!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!#################################################################
SUBROUTINE TRIP_INTERFACE (TPDG, TP, TPG, TPLK, TPST,          &
                           KLISTING,OPRINT,                    &
                           KSTATE,KLON,KLAT,KLAKE_NUM,         &
                           PTIME,PTIMEC,                       &
                           KNB_TSTEP_RUN,KNB_TSTEP_DIAG,       &
                           PTSTEP_RUN,PTSTEP_DIAG,PRUNOFF,     &
                           PDRAIN,PCALVING,PSRC_FLOOD,OXIOS    )
!#################################################################
!
!!****  *TRIP*
!!
!!    PURPOSE
!!    -------
!
!     Driver for the TRIP river routing.
!     Here, we call the physical and the diag routines
!
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
!!      Original    01/02/05
!!      Modif.      28/05/08
!!      B. Decharme 10/2016  bug surface/groundwater coupling
!!      S.Sénési    08/11/16 : interface to XIOS
!!      S. Munier   28/03/2020 CTRIP-12D and parallelization
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
!
USE MODD_TRIP_DIAG,  ONLY : TRIP_DIAG_t
USE MODD_TRIP,       ONLY : TRIP_t
USE MODD_TRIP_GRID,  ONLY : TRIP_GRID_t
USE MODD_TRIP_LAKE,  ONLY : TRIP_LAKE_t
USE MODD_TRIP_STATE, ONLY : TRIP_STATE_t
!
USE MODN_TRIP_RUN,  ONLY : LDIAG_MISC, LWR_DIAG
!
USE MODN_TRIP,      ONLY : CGROUNDW, LFLOOD, XTSTEP, CVIT, CLAKE
!
USE MODD_TRIP_PAR,  ONLY : XRHOLW, XUNDEF
USE MODD_TRIP_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_TRIP_HS_VEL
USE MODI_TRIP
USE MODI_TRIP_DIAG_INIT
USE MODI_TRIP_DIAG
USE MODI_TRIP_DIAG_GATHER
USE MODI_TRIP_DIAG_WRITE
USE MODI_TRIP_DIAG_CPL_ESM
!
USE MODI_GWF_STATE
USE MODI_GWF_CPL_UPDATE_STATE
!
USE MODI_ABORT_TRIP
!
USE MODI_GWF
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1    declarations of arguments
!
!
!
TYPE(TRIP_DIAG_t),    INTENT(INOUT) :: TPDG
TYPE(TRIP_t),         INTENT(INOUT) :: TP
TYPE(TRIP_GRID_t),    INTENT(INOUT) :: TPG
TYPE(TRIP_LAKE_t),    INTENT(INOUT) :: TPLK
TYPE(TRIP_STATE_t),   INTENT(INOUT) :: TPST
!
INTEGER,              INTENT(IN)    :: KLISTING       !Output file id
LOGICAL,              INTENT(IN)    :: OPRINT         !print option          [-]
INTEGER,              INTENT(IN)    :: KSTATE         !Size of state vector
INTEGER,              INTENT(IN)    :: KLON           !Number of longitude
INTEGER,              INTENT(IN)    :: KLAT           !Number of latitude
INTEGER,              INTENT(IN)    :: KLAKE_NUM      !Number of lakes
REAL,                 INTENT(INOUT) :: PTIME          !Current time          (s)
REAL,                 INTENT(INOUT) :: PTIMEC         !Cumulated time        (s)
INTEGER,              INTENT(IN)    :: KNB_TSTEP_RUN  !TSTEP_RUN counter     [-]
REAL,                 INTENT(IN)    :: PTSTEP_RUN     !Run  timestep         [s]
REAL,                 INTENT(IN)    :: PTSTEP_DIAG    !Diag timestep         [s]
INTEGER,              INTENT(INOUT) :: KNB_TSTEP_DIAG !DIAG call counter     [-]
LOGICAL,              INTENT(IN)    :: OXIOS          !Do we use XIOS
!
REAL, DIMENSION(:),   INTENT(IN)    :: PRUNOFF   !Input surface runoff            [kg/s]
REAL, DIMENSION(:),   INTENT(IN)    :: PDRAIN    !Input free drainage             [kg/s]
REAL, DIMENSION(:),   INTENT(IN)    :: PCALVING  !Input claving flux from glacier [kg/s]
REAL, DIMENSION(:),   INTENT(IN)    :: PSRC_FLOOD! Input P-E-I flood source term  [kg/s]
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(KSTATE) :: ZRUNOFF    !Input surface runoff          [kg/s]
REAL, DIMENSION(KSTATE) :: ZDRAIN     !Input drainage + recharge     [kg/s]
REAL, DIMENSION(KSTATE) :: ZSRC_FLOOD !Input P-E-I flood source term [kg/s]
!
REAL, DIMENSION(KSTATE) :: ZSOUT      !streamflow                    [kg/s]
REAL, DIMENSION(KSTATE) :: ZSIN       !grid-cell input streamflow    [kg/s]
REAL, DIMENSION(KSTATE) :: ZVEL       !river velocity                [m/s]
REAL, DIMENSION(KSTATE) :: ZHS        !River heigh                   [m]
REAL, DIMENSION(KSTATE) :: ZGOUT      !Groundwater outflow           [kg/s]
REAL, DIMENSION(KSTATE) :: ZGNEG      !Groundwater intflow (neg)     [kg/s]
REAL, DIMENSION(KSTATE) :: ZWTD       !Water table depth for coupling[m]
REAL, DIMENSION(KSTATE) :: ZFWTD      !fraction of water table to rise
REAL, DIMENSION(KSTATE) :: ZQGCELL    !lateral groundwater exchanges [kg/s]
REAL, DIMENSION(KSTATE) :: ZHGHS      !groundwater minus river heigh [m]
REAL, DIMENSION(KSTATE) :: ZQFR       !floodplains to river exchange [kg/s]
REAL, DIMENSION(KSTATE) :: ZQRF       !river to floodplains exchange [kg/s]
REAL, DIMENSION(KSTATE) :: ZVFIN      !QRF velocity                  [m/s]
REAL, DIMENSION(KSTATE) :: ZVFOUT     !QFR velocity                  [m/s]
REAL, DIMENSION(KSTATE) :: ZHSF       !river minus flodd heigh       [m]
REAL, DIMENSION(KSTATE) :: ZDISCHARGE !river discharges              [kg]
REAL, DIMENSION(KSTATE) :: ZHG_OLD    !Water table depth at t-1      [m]
!
REAL                    :: ZGSTO_ALL  !Global groundwater storage at t    [kg]
REAL                    :: ZGSTO2_ALL !Global groundwater storage at t-1  [kg]
REAL                    :: ZGIN_ALL   !Global gw recharge + lateral input [kg/m2/s]
REAL                    :: ZGOUT_ALL  !Global gw outflow                  [kg/m2/s]
!
INTEGER :: JTSTEP, ITSTEP
INTEGER :: IERR
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_INTERFACE',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!*       0.     Initialize local variables:
!               ---------------------------
!
ZSOUT     (:) = 0.0
ZSIN      (:) = 0.0
ZVEL      (:) = 0.0
ZHS       (:) = 0.0
ZGOUT     (:) = 0.0
ZGNEG     (:) = 0.0
ZWTD      (:) = 0.0
ZFWTD     (:) = 0.0
ZQGCELL   (:) = 0.0
ZHGHS     (:) = 0.0
ZQFR      (:) = 0.0
ZQRF      (:) = 0.0
ZVFIN     (:) = 0.0
ZVFOUT    (:) = 0.0
ZHSF      (:) = 0.0
ZDISCHARGE(:) = 0.0
ZHG_OLD   (:) = 0.0
!
!-------------------------------------------------------------------------------
!
ZGSTO_ALL  = 0.0
ZGSTO2_ALL = 0.0
ZGIN_ALL   = 0.0
ZGOUT_ALL  = 0.0
!
!-------------------------------------------------------------------------------
!
! Surface runoff treatment
!
ZRUNOFF(:) = PRUNOFF(:)
!
! Drainage and Calving treatment
! calving over greenland and antarctica directly to ocean
!
WHERE(TPST%GMASK(:).AND..NOT.TPST%GMASK_GRE(:).AND..NOT.TPST%GMASK_ANT(:))
  ZDRAIN(:) = PDRAIN(:)+PCALVING(:)
ELSEWHERE
  ZDRAIN(:) = PDRAIN(:)
ENDWHERE
!
! Flood treatment
!
IF(LFLOOD)THEN
  WHERE(TPST%XFFLOOD(:)==1.0.AND.ZSRC_FLOOD(:)>0.0)
    ZRUNOFF   (:) = ZRUNOFF(:) + PSRC_FLOOD(:)
    ZSRC_FLOOD(:) = 0.0
  ELSEWHERE
    ZSRC_FLOOD(:) = PSRC_FLOOD(:)
  ENDWHERE
ELSE
  ZSRC_FLOOD(:) = 0.0
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       1.     Initialize local diag :
!               -----------------------
!
CALL TRIP_DIAG_INIT(TPST, &
                    ZSOUT,ZSIN,ZVEL,ZHS,ZGOUT,ZGNEG,ZHG_OLD, &
                    ZWTD,ZFWTD,ZQGCELL,ZHGHS,                &
                    ZQFR,ZQRF,ZVFIN,ZVFOUT,ZHSF,ZDISCHARGE,  &
                    ZGSTO_ALL,ZGSTO2_ALL,ZGIN_ALL,ZGOUT_ALL  )
!
!-------------------------------------------------------------------------------
!
!*       2.     Initialize river height and velocity :
!               --------------------------------------
!
CALL TRIP_HS_VEL(TPST,XTSTEP,ZHS,ZVEL)
!
!-------------------------------------------------------------------------------
!
!*       3.     Call Groundwater dynamic :
!               --------------------------
!
IF(CGROUNDW=='DIF')THEN
!
!  * Groundwater actualization
!
  CALL GWF_STATE(TP,TPG,TPST, &
                 KLON,KLAT,KSTATE,OPRINT,PTSTEP_RUN,XTSTEP,                &
                 ZDRAIN,ZHS,ZHG_OLD,PQGCELL=ZQGCELL,PWTD=ZWTD,PFWTD=ZFWTD, &
                 PHGHS=ZHGHS,PGOUT=ZGOUT,PGNEG=ZGNEG,                      &
                 PGSTO_ALL=ZGSTO_ALL,PGSTO2_ALL=ZGSTO2_ALL,                &
                 PGIN_ALL=ZGIN_ALL,PGOUT_ALL=ZGOUT_ALL                     )
!
!  * Velocity actualization
!
  CALL TRIP_HS_VEL(TPST,XTSTEP,ZHS,ZVEL)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       4.     Call Trip river routines and actualisation of diagnostic :
!               ----------------------------------------------------------
!
ITSTEP = INT(PTSTEP_RUN/XTSTEP)
!
DO JTSTEP=1,ITSTEP !TRIP time step loop
!
  CALL TRIP(TPST, &
            KLISTING,OPRINT,KNB_TSTEP_RUN,JTSTEP,ITSTEP,XTSTEP, &
            ZDRAIN,ZRUNOFF,ZSRC_FLOOD,ZHS,ZVEL,                 &
            ZSOUT,ZSIN,ZGOUT,ZQFR,ZQRF,ZVFIN,ZVFOUT,ZHSF,       &
            ZGSTO_ALL,ZGSTO2_ALL,ZGIN_ALL,ZGOUT_ALL             )
!
!  * Actualisation of diagnostic
!
  IF(CGROUNDW=='DIF')THEN
    CALL GWF_CPL_UPDATE_STATE(TPST,ZHG_OLD,ZWTD,ZFWTD)
  ENDIF
!
  CALL TRIP_DIAG(TPDG, TP, TPST,                            &
                 XTSTEP,ZSOUT,ZSIN,ZVEL,ZHS,ZGOUT,ZGNEG,    &
                 ZWTD,ZFWTD,ZQGCELL,ZHGHS,                  &
                 ZQFR,ZQRF,ZVFIN,ZVFOUT,ZHSF,ZSRC_FLOOD,    &
                 ZDRAIN,ZRUNOFF,ZDISCHARGE                  )
!
!  * Velocity actualization
!
  CALL TRIP_HS_VEL(TPST,XTSTEP,ZHS,ZVEL)
!
!  * Time actualization
!
  PTIME  = PTIME  + XTSTEP
  PTIMEC = PTIMEC + XTSTEP
!
!  * Write diagnostic
!
  IF (LWR_DIAG.AND.MOD(PTIMEC,PTSTEP_DIAG)==0.) THEN
    KNB_TSTEP_DIAG = KNB_TSTEP_DIAG + 1
    CALL TRIP_DIAG_GATHER(TPLK, TPDG, TPST)
    IF (NRANK==NPIO) THEN
      CALL TRIP_DIAG_WRITE(TPDG, TPG,                                                    &
                           KLISTING,KLON,KLAT,KLAKE_NUM,KNB_TSTEP_DIAG,PTSTEP_DIAG,OXIOS )
    ENDIF
  ENDIF
!
!  * end
!
ENDDO ! * End TRIP time step loop
!
!-------------------------------------------------------------------------------
!
!*       5.      Actualisation of coupling diagnostic:
!               --------------------------------------
!
CALL TRIP_DIAG_CPL_ESM(TPST, &
                       PTSTEP_RUN,ZDISCHARGE,PCALVING,ZWTD,ZFWTD)
!
IF (LHOOK) CALL DR_HOOK('TRIP_INTERFACE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
END SUBROUTINE TRIP_INTERFACE
