!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!     #########
SUBROUTINE TRIP_SURFACE_WATER (TPST, &
                               KLISTING,OPRINT,PTSTEP,                  &
                               PRUNOFF,PGOUT,PQFR,PQRF,PHS,PVEL,        &
                               PSURF_STO2,PSIN,PSOUT,                   &
                               PSSTO_ALL,PSSTO2_ALL,PSIN_ALL,PDRUN_ALL, &
                               PSOUT_ALL,PVEL_ALL,PHS_ALL               )
!     ################################################################
!
!!****  *TRIP_SURFACE_WATER*
!!
!!    PURPOSE
!!    -------
!
!     Calculate the river storage in the next time step based on the storage of current time step
!     Where OMASK_VEL=true the Manning equation is used to compute a variable flow velocity.
!
!
!!**  METHOD
!!    ------
!
!     RK Ordre 4 Rang 4
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
!!    B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/02/09
!!      S. Munier   28/03/2020 CTRIP-12D and parallelization
!!      T. Guinaldo 04/2020    Add MLake
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_STATE, ONLY : TRIP_STATE_t
!
USE MODN_TRIP,       ONLY : CLAKE, LBACKWATER, XBANKSLOPE
!
USE MODI_TRIP_MASSLAKE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABORT_TRIP
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(TRIP_STATE_t),    INTENT(INOUT) :: TPST
!
INTEGER,               INTENT(IN)    :: KLISTING
LOGICAL,               INTENT(IN)    :: OPRINT     ! Printable budget key
!
REAL,                  INTENT(IN)    :: PTSTEP     ! Trip timestep value (10800s)
!
REAL,    DIMENSION(:), INTENT(IN)    :: PRUNOFF    ! Surface runoff from ISBA     [kg/s]
REAL,    DIMENSION(:), INTENT(IN)    :: PGOUT      ! Ground water outflow         [kg/s]
REAL,    DIMENSION(:), INTENT(IN)    :: PQFR       ! Flood flow to river          [kg/s]
REAL,    DIMENSION(:), INTENT(IN)    :: PQRF       ! River flow to floodplain     [kg/s]
REAL,    DIMENSION(:), INTENT(INOUT) :: PHS        ! River channel or lake height [m]
REAL,    DIMENSION(:), INTENT(IN)    :: PVEL       ! River channel velocity       [m/s]
!
REAL,    DIMENSION(:), INTENT(OUT)   :: PSURF_STO2 ! river channel or lake storage at t+1 [kg]
!
REAL,    DIMENSION(:), INTENT(OUT)   :: PSIN  ! Inflow to the surface river or lake reservoir [kg/s]
REAL,    DIMENSION(:), INTENT(OUT)   :: PSOUT ! Outflow from the surface river or lake reservoir [kg/s]
!
REAL,                  INTENT(OUT)   :: PSSTO_ALL, PSSTO2_ALL, PSIN_ALL, &
                                        PDRUN_ALL, PSOUT_ALL, PVEL_ALL,  &
                                        PHS_ALL
!                                       Final budget variable
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PRUNOFF,1)) :: ZQIN
REAL :: ZQOUT, ZSTOMAX, ZAREA
REAL :: ZELEV, ZELEV_NEXT, ZALPHA
INTEGER :: JSTATE, JSTATE_NEXT
REAL :: ZT,ZP,ZA,ZC,ZTR,ZF2,ZCHI,ZKAPPA,ZFKAPPA,ZK,ZTRMAX
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! * Init
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_SURFACE_WATER',0,ZHOOK_HANDLE)
!
PSURF_STO2 (:) = 0.0
PSIN       (:) = 0.0
PSOUT      (:) = 0.0
!
ZQIN       (:) = 0.0
!
!-------------------------------------------------------------------------------
! * Sequence loop (optimized computation)
!-------------------------------------------------------------------------------
!
ZTRMAX = 0.
DO JSTATE = 1,TPST%NSTATE_LEN_P
!
! ---------------------------------------------------------------------
! inflow calculation
!
  ZQIN(JSTATE) = ZQIN(JSTATE)+PRUNOFF(JSTATE)+PGOUT(JSTATE)+PQFR(JSTATE)-PQRF(JSTATE)
  PSIN(JSTATE) = ZQIN(JSTATE)
!
  IF(CLAKE=='MLK'.AND.TPST%GMASK_LAKE(JSTATE))THEN
!
! ---------------------------------------------------------------------
! lake case
!
    CALL TRIP_MASSLAKE(PTSTEP,TPST%XAREA(JSTATE),                   &
                       TPST%XWEIR_Z(JSTATE),TPST%XWEIR_W(JSTATE),   &
                       TPST%XSURF_STO(JSTATE),ZQIN(JSTATE),         &
                       PSURF_STO2(JSTATE),PSOUT(JSTATE),PHS(JSTATE) )
!
  ELSE
!
! ---------------------------------------------------------------------
! river case
!
!   river channel storage calculation
    ZSTOMAX = TPST%XSURF_STO(JSTATE)+ZQIN(JSTATE)*PTSTEP
    PSURF_STO2(JSTATE) = ZSTOMAX/(1.0+PTSTEP*PVEL(JSTATE)/TPST%XLEN(JSTATE))
!
!   supress numerical artifacs
    PSURF_STO2(JSTATE) = MIN(ZSTOMAX,PSURF_STO2(JSTATE))
!
!   river channel outflow calculation and supress numerical artifacs
    ZQOUT = (TPST%XSURF_STO(JSTATE)-PSURF_STO2(JSTATE))/PTSTEP+ZQIN(JSTATE)
    PSOUT(JSTATE) = MAX(ZQOUT,0.0)
    PSURF_STO2(JSTATE) = PSURF_STO2(JSTATE) + (PSOUT(JSTATE)-ZQOUT)*PTSTEP
!
!   backwater effect
    IF(LBACKWATER.AND.TPST%NNEXTST(JSTATE)>0)THEN
      JSTATE_NEXT = TPST%NNEXTST(JSTATE)
      !ZELEV      = PHS(JSTATE)      + TPST%XTOPO_RIV(JSTATE)
      !ZELEV_NEXT = PHS(JSTATE_NEXT) + TPST%XTOPO_RIV(JSTATE_NEXT)
      !IF(ZELEV<ZELEV_NEXT)THEN
      !  PSOUT(JSTATE) = 0.0
      !  PSURF_STO2(JSTATE) = ZSTOMAX
      !ENDIF
      ZALPHA = (PHS(JSTATE_NEXT)-PHS(JSTATE))/(TPST%XSLOPEBED(JSTATE)*TPST%XLEN(JSTATE))
      ZALPHA = MIN(ZALPHA,1.)
      IF(ZALPHA>0)THEN
        PSURF_STO2(JSTATE) = PSURF_STO2(JSTATE) + ZALPHA*PSOUT(JSTATE)*PTSTEP
        PSOUT(JSTATE) = (1.-ZALPHA)*PSOUT(JSTATE)
      ENDIF
    ENDIF
!
  ENDIF
!
! ------------------------------------------------------------------
! propagate downstream
!
  IF(TPST%NNEXTST(JSTATE)>0)THEN
    ZQIN(TPST%NNEXTST(JSTATE)) = ZQIN(TPST%NNEXTST(JSTATE))+PSOUT(JSTATE)
  ENDIF
!
ENDDO
! write(*,*) ZTRMAX/3600.
!
!-------------------------------------------------------------------------------
! * Budget calculation
!-------------------------------------------------------------------------------
!
IF(OPRINT)THEN
!
  PDRUN_ALL   = 0.0
  PSSTO_ALL   = 0.0
  PSSTO2_ALL  = 0.0
  PSIN_ALL    = 0.0
  PSOUT_ALL   = 0.0
  PVEL_ALL    = 0.0
  PHS_ALL     = 0.0
  ZAREA       = 0.0
!
  DO JSTATE = 1,SIZE(PRUNOFF,1)
    PDRUN_ALL  = PDRUN_ALL  + PRUNOFF(JSTATE)+PGOUT(JSTATE)+PQFR(JSTATE)-PQRF(JSTATE)
    PSSTO_ALL  = PSSTO_ALL  + TPST%XSURF_STO(JSTATE) / TPST%XAREA(JSTATE)
    PSSTO2_ALL = PSSTO2_ALL + PSURF_STO2    (JSTATE) / TPST%XAREA(JSTATE)
    PSIN_ALL   = PSIN_ALL   + ZQIN          (JSTATE) / TPST%XAREA(JSTATE)
    PSOUT_ALL  = PSOUT_ALL  + PSOUT         (JSTATE) / TPST%XAREA(JSTATE)
    IF(TPST%GMASK_VEL(JSTATE))THEN
      PVEL_ALL = PVEL_ALL   + PVEL (JSTATE) * TPST%XAREA(JSTATE)
      PHS_ALL  = PHS_ALL    + PHS  (JSTATE) * TPST%XAREA(JSTATE)
      ZAREA    = ZAREA      + TPST%XAREA(JSTATE)
    ENDIF
  ENDDO
!
  IF(ZAREA>0.0)THEN
    PVEL_ALL = PVEL_ALL / ZAREA
    PHS_ALL  = PHS_ALL  / ZAREA
  ENDIF
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP_SURFACE_WATER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_SURFACE_WATER
