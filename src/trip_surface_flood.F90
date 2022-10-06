!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
SUBROUTINE TRIP_SURFACE_FLOOD (TPST,KLISTING,PTSTEP,OPRINT,       &
                               PVEL,PHS,PSOURCE,PFLOOD_STO2,      &
                               PQFR,PQRF,PVFIN,PVFOUT,PHSF,       &
                               PFSTO_ALL,PFSTO2_ALL,PSOURCE_ALL,  &
                               PFIN_ALL,PFOUT_ALL,PHF_ALL,PFF_ALL )
!
!     ################################################################
!
!!****  *TRIP_SURFACE_FLOOD*
!!
!!    PURPOSE
!!    -------
!
!     Calculate the flood storage in the next time step based on storages
!     of current time step using the Manning equation.
!
!     Decharme et al., Clim. Dyn., 2012
!
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!
!     None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/01/14
!!      S. Munier   28/03/2020 CTRIP-12D and parallelization
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_STATE, ONLY : TRIP_STATE_t
!
USE MODN_TRIP,       ONLY : LAPPROXVEL
USE MODD_TRIP_PAR,   ONLY : XRHOLW, XM, XUNDEF
!
USE MODE_POWER_2THIRD
!
USE MODI_ABORT_TRIP
USE MODI_FLOOD_UPDATE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(TRIP_STATE_t), INTENT(INOUT) :: TPST
!
INTEGER, INTENT(IN)               :: KLISTING
REAL,    INTENT(IN)               :: PTSTEP ! Trip timestep value (10800s)
LOGICAL, INTENT(IN)               :: OPRINT   !Printable budget key
!
REAL,DIMENSION(:), INTENT(IN)     :: PVEL       ! river velocity    [m/s]
REAL,DIMENSION(:), INTENT(IN)     :: PHS        ! river channel height [m]
REAL,DIMENSION(:), INTENT(IN)     :: PSOURCE    ! precip-infiltration-evaporation [kg/s]
!
REAL,DIMENSION(:), INTENT(INOUT)  :: PFLOOD_STO2! Floodplain water storage at t+1  [kg]
!
REAL,DIMENSION(:), INTENT(OUT)    :: PQFR  ! Flood flow to river          [kg/s]
REAL,DIMENSION(:), INTENT(OUT)    :: PQRF  ! River flow to floodplain     [kg/s]
REAL,DIMENSION(:), INTENT(OUT)    :: PVFIN ! River flow to flood velocity [m/s]
REAL,DIMENSION(:), INTENT(OUT)    :: PVFOUT! Flood flow to river velocity [m/s]
REAL,DIMENSION(:), INTENT(OUT)    :: PHSF  ! River-Floodplain depth comparison [m] during dt
!
REAL,              INTENT(OUT)    :: PFSTO_ALL,PFSTO2_ALL,PSOURCE_ALL, &
                                     PFIN_ALL,PFOUT_ALL,PHF_ALL,PFF_ALL
!                                    Final budget variable
!
!*      0.2    declarations of local variables
!
REAL, PARAMETER                  :: ZLEN_MIN = 1.E-6
!
REAL, DIMENSION(TPST%NNSTATE_P) :: ZMF
REAL, DIMENSION(TPST%NNSTATE_P) :: ZSLOPE
REAL, DIMENSION(TPST%NNSTATE_P) :: ZRADIUS
REAL, DIMENSION(TPST%NNSTATE_P) :: ZDIST
REAL, DIMENSION(TPST%NNSTATE_P) :: ZDELTA
REAL, DIMENSION(TPST%NNSTATE_P) :: ZMF_IN
REAL, DIMENSION(TPST%NNSTATE_P) :: ZMF_OUT
REAL, DIMENSION(TPST%NNSTATE_P) :: ZFLD_LEN
REAL, DIMENSION(TPST%NNSTATE_P) :: ZAREA_SG
REAL, DIMENSION(TPST%NNSTATE_P) :: ZVRIV
REAL, DIMENSION(TPST%NNSTATE_P) :: ZVFLD
REAL, DIMENSION(TPST%NNSTATE_P) :: ZVINER
!
REAL    :: ZAREA
!
INTEGER :: ISTATE, JSTATE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! * Init
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_SURFACE_FLOOD',0,ZHOOK_HANDLE)
!
ISTATE = TPST%NSTATE_LEN_P
!
PFLOOD_STO2(:) = 0.0
PHSF       (:) = 0.0
PVFIN      (:) = 0.0
PVFOUT     (:) = 0.0
PQFR       (:) = 0.0
PQRF       (:) = 0.0
!
ZMF        (:) = 0.0
ZSLOPE     (:) = 0.0
ZRADIUS    (:) = 0.0
ZDIST      (:) = 0.0
ZDELTA     (:) = 0.0
ZMF_IN     (:) = 0.0
ZMF_OUT    (:) = 0.0
ZFLD_LEN   (:) = 0.0
ZAREA_SG   (:) = 0.0
ZVRIV      (:) = 0.0
ZVFLD      (:) = 0.0
ZVINER     (:) = 0.0
!
WHERE(TPST%GMASK_FLD(:))
  ZAREA_SG(:) = MAX(TPST%XAREA(:)-(TPST%XLEN(:)*TPST%XWIDTH(:)),ZLEN_MIN*ZLEN_MIN)
ELSEWHERE
  ZAREA_SG(:) = TPST%XAREA(:)
ENDWHERE
!
!-------------------------------------------------------------------------------
! * Update the floodplain storage due to source (Precip inter - LEf - Infil)
!-------------------------------------------------------------------------------
!
WHERE(TPST%GMASK_FLD(:))
  PFLOOD_STO2(:)=TPST%XFLOOD_STO(:)+PTSTEP*PSOURCE(:)
ENDWHERE
!
!------------------------------------------------------------------
! * Update the floodplain geomorphological properties
!------------------------------------------------------------------
!
CALL FLOOD_UPDATE(TPST,ZAREA_SG,PFLOOD_STO2)
!
ZFLD_LEN(:)=MAX(ZLEN_MIN,TPST%XFLOOD_LEN(:))
!
!------------------------------------------------------------------
! * Update the floodplain storage due to inflow or outflow
!------------------------------------------------------------------
!
DO JSTATE=1,ISTATE
!
  IF(TPST%GMASK_FLD(JSTATE))THEN
!
!   ------------------------------------------------------------------
!   Calculate the potential water mass exchange
!
    PHSF  (JSTATE) = PHS(JSTATE)-TPST%XHC_BED(JSTATE)-TPST%XHFLOOD(JSTATE)
!
    ZMF   (JSTATE) = PHSF(JSTATE)*ZFLD_LEN(JSTATE)*TPST%XWIDTH(JSTATE)*XRHOLW
    ZDIST (JSTATE) = 0.5*(TPST%XWIDTH(JSTATE)+TPST%XWFLOOD(JSTATE))
    ZSLOPE(JSTATE) = PHSF(JSTATE)/ZDIST(JSTATE)
!
  ENDIF
!
  IF(ZMF(JSTATE)>0.0.AND.TPST%XFFLOOD(JSTATE)<1.0)THEN
!
!   ------------------------------------------------------------------
!   Floodplain inflow case
!
    IF(LAPPROXVEL)THEN
      ! Approximate computation of power 2/3
      CALL POWER_2THIRD(PHS(JSTATE)-TPST%XHC_BED(JSTATE),ZRADIUS(JSTATE))
    ELSE
      ! Exact computation of power 2/3
      ZRADIUS(JSTATE) = PHS(JSTATE)-TPST%XHC_BED(JSTATE)
      ZRADIUS(JSTATE) = EXP(XM*LOG(ZRADIUS(JSTATE)))
    ENDIF
!
    ZVFLD (JSTATE) = ZRADIUS(JSTATE)*SQRT(ZSLOPE(JSTATE))/TPST%XN_FLOOD(JSTATE)
    ZVRIV (JSTATE) = MAX(PVEL(JSTATE),ZVFLD(JSTATE))
    ZVINER(JSTATE) = SQRT(ZVRIV(JSTATE)*ZVFLD(JSTATE))
    PVFIN (JSTATE) = MIN(ZVINER(JSTATE),ZDIST(JSTATE)/PTSTEP)
    PVFOUT(JSTATE) = 0.0
!
    ZMF_IN (JSTATE) = ZMF(JSTATE)
    ZMF_OUT(JSTATE) = 0.0
!
  ENDIF
!
  IF(ZMF(JSTATE)<0.0.AND.TPST%XFFLOOD(JSTATE)>0.0)THEN
!
!   ------------------------------------------------------------------
!   Floodplain outflow case
!
    ZRADIUS(JSTATE) = TPST%XHFLOOD(JSTATE)
    ZRADIUS(JSTATE) = EXP(XM*LOG(ZRADIUS(JSTATE)))
!
    PVFIN (JSTATE) = 0.0
    ZVFLD (JSTATE) = ZRADIUS(JSTATE)*SQRT(-1.0*ZSLOPE(JSTATE))/TPST%XN_FLOOD(JSTATE)
    PVFOUT(JSTATE) = MIN(ZVFLD(JSTATE),ZDIST(JSTATE)/PTSTEP)
!
    ZMF_IN (JSTATE) = 0.0
    ZMF_OUT(JSTATE) = ABS(ZMF(JSTATE))
!
    ZDELTA(JSTATE) = MERGE(1.0,0.0,PFLOOD_STO2(JSTATE)<ZMF_OUT(JSTATE))
!
  ENDIF
!
  IF(TPST%GMASK_FLD(JSTATE).AND.ZMF(JSTATE)/=0.0)THEN
!
!   ------------------------------------------------------------------
!   Update the floodplain storage
!
    PQRF(JSTATE) = ZMF_IN (JSTATE)*PVFIN (JSTATE)/ZDIST(JSTATE)
!
    PQFR(JSTATE) = (1.0-ZDELTA(JSTATE)) * ZMF_OUT(JSTATE)*PVFOUT(JSTATE)/ZDIST(JSTATE) &
                    +   ZDELTA(JSTATE)  * PFLOOD_STO2(JSTATE)/PTSTEP
!
    PFLOOD_STO2(JSTATE) = PFLOOD_STO2(JSTATE)+PTSTEP*(PQRF(JSTATE)-PQFR(JSTATE))
!
  ENDIF
!
ENDDO
!
!------------------------------------------------------------------
! * Update the floodplain geomorphological properties
!------------------------------------------------------------------
!
CALL FLOOD_UPDATE(TPST,ZAREA_SG,PFLOOD_STO2)
!
!-------------------------------------------------------------------------------
! * Budget calculation
!-------------------------------------------------------------------------------
!
IF(OPRINT)THEN
!
  PFSTO_ALL   = 0.0
  PFSTO2_ALL  = 0.0
  PSOURCE_ALL = 0.0
  PFIN_ALL    = 0.0
  PFOUT_ALL   = 0.0
  PHF_ALL     = 0.0
  PFF_ALL     = 0.0
  ZAREA       = 0.0
!
  DO JSTATE=1,ISTATE
    IF(TPST%GMASK_FLD(JSTATE))THEN
      PFSTO_ALL  = PFSTO_ALL  + TPST%XFLOOD_STO(JSTATE) / TPST%XAREA(JSTATE)
      PFSTO2_ALL = PFSTO2_ALL + PFLOOD_STO2    (JSTATE) / TPST%XAREA(JSTATE)
      PFIN_ALL   = PFIN_ALL   + PQRF           (JSTATE) / TPST%XAREA(JSTATE)
      PFOUT_ALL  = PFOUT_ALL  + PQFR           (JSTATE) / TPST%XAREA(JSTATE)
      PSOURCE_ALL= PSOURCE_ALL+ PSOURCE        (JSTATE) / TPST%XAREA(JSTATE)
      PHF_ALL    = PHF_ALL    + TPST%XHFLOOD   (JSTATE) * TPST%XAREA(JSTATE)
      PFF_ALL    = PFF_ALL    + TPST%XFFLOOD   (JSTATE) * TPST%XAREA(JSTATE)
      ZAREA      = ZAREA      + TPST%XAREA     (JSTATE)
    ENDIF
  ENDDO
!
  IF(ZAREA>0.0)THEN
    PHF_ALL = PHF_ALL / ZAREA
    PFF_ALL = PFF_ALL / ZAREA
  ENDIF
!
ENDIF
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('TRIP_SURFACE_FLOOD',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_SURFACE_FLOOD
