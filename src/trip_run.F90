!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
SUBROUTINE TRIP_RUN (TPDG, TP, TPG, TPLK, TPST,       &
                     OOASIS, OXIOS, KLISTING,         &
                     KSTATE,KNB_TSTEP_RUN,PRUNTIME,   &
                     KLON,KLAT,KNB,                   &
                     KYEAR,KMONTH,KDAY,PTIME          )
!#############################################
!
!!****  *TRIP_RUN*
!!
!!    PURPOSE
!!    -------
!!
!!    Run trip
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
!!      Original    06/08
!!      B. Decharme 10/2016  bug surface/groundwater coupling
!!      S.Sénési    08/11/16 : interface to XIOS
!!      S. Munier   28/03/2020 CTRIP-12D and parallelization
!!      T. Guinaldo 04/2020    Add MLake
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_DIAG,  ONLY : TRIP_DIAG_t
USE MODD_TRIP,       ONLY : TRIP_t
USE MODD_TRIP_GRID,  ONLY : TRIP_GRID_t
USE MODD_TRIP_LAKE,  ONLY : TRIP_LAKE_t
USE MODD_TRIP_STATE, ONLY : TRIP_STATE_t
!
USE MODD_TRIP_MPI
!
USE MODN_TRIP_RUN, ONLY : LRESTART, LPRINT,   &
                          XTSTEP_RUN, XTSTEP_DIAG
USE MODN_TRIP    , ONLY : CLAKE
!
USE MODD_TRIP_PAR, ONLY : XUNDEF, NUNDEF, XDAY
!
USE MODE_TRIP_GRID_STATE
USE MODE_TRIP_LAKE
!
USE MODI_TRIP_FORCING_OFFLINE
USE MODI_TRIP_FORCING_OASIS
USE MODI_TRIP_INTERFACE
USE MODI_TRIP_DATE
!
USE MODI_TRIP_OASIS_SEND
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!-------------------------------------------------------------------------------
!
!*      0.1    declarations of arguments
!
!
TYPE(TRIP_DIAG_t),  INTENT(INOUT) :: TPDG
TYPE(TRIP_t),       INTENT(INOUT) :: TP
TYPE(TRIP_GRID_t),  INTENT(INOUT) :: TPG
TYPE(TRIP_LAKE_t),  INTENT(INOUT) :: TPLK
TYPE(TRIP_STATE_t), INTENT(INOUT) :: TPST
!
LOGICAL, INTENT(IN)  :: OOASIS        ! Oasis coupling or not
LOGICAL, INTENT(IN)  :: OXIOS         ! Do we use XIOS
!
INTEGER, INTENT(IN)  :: KLISTING      ! Listing ID
INTEGER, INTENT(IN)  :: KSTATE        ! Size of state vector
INTEGER, INTENT(IN)  :: KNB_TSTEP_RUN ! number of time step in the run
REAL,    INTENT(IN)  :: PRUNTIME      ! total simulated time
!
INTEGER, INTENT(IN)  :: KLON          ! Number of longitude if forcing offline
INTEGER, INTENT(IN)  :: KLAT          ! Number of latittude if forcing offline
INTEGER, INTENT(IN)  :: KNB           ! number of time step if forcing offline
!
INTEGER, INTENT(OUT) :: KYEAR         ! current year         (UTC)
INTEGER, INTENT(OUT) :: KMONTH        ! current month        (UTC)
INTEGER, INTENT(OUT) :: KDAY          ! current day          (UTC)
REAL,    INTENT(OUT) :: PTIME         ! current time           (s)
!
!-------------------------------------------------------------------------------
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(KSTATE,KNB) :: ZDRAIN_OL         ! Drainage from the forcing file          (kg)
REAL, DIMENSION(KSTATE,KNB) :: ZRUNOFF_OL        ! Surface runoff from the forcing file    (kg)
REAL, DIMENSION(KSTATE,KNB) :: ZSRC_FLOOD_OL     ! Flood source term from the forcing file (kg)
!
REAL, DIMENSION(KSTATE) :: ZRUNOFF           ! Surface runoff               (kg/s)
REAL, DIMENSION(KSTATE) :: ZDRAIN            ! Drainage                     (kg/s)
REAL, DIMENSION(KSTATE) :: ZCALVING          ! Calving flux                 (kg/s)
REAL, DIMENSION(KSTATE) :: ZSRC_FLOOD        ! Input P-E-I flood source term(kg/s)
!
REAL                    :: ZTIMEC            ! cumulated current time (s)
REAL                    :: ZTIME_CPL         ! Coupling time
INTEGER                 :: JNB_TSTEP_RUN     ! TSTEP_RUN counter
INTEGER                 :: JNB_TSTEP_DIAG    ! DIAG call counter
INTEGER                 :: ICOUNT
INTEGER                 :: ILAKE_NUM
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! --------------------------------------------------------------------------------------
! * 1. Initialize
! --------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_RUN',0,ZHOOK_HANDLE)
!
ZRUNOFF   (:) = XUNDEF
ZDRAIN    (:) = XUNDEF
ZCALVING  (:) = XUNDEF
ZSRC_FLOOD(:) = XUNDEF
!
! --------------------------------------------------------------------------------------
! * 2. Read and prepare drainage and runoff if offline
! --------------------------------------------------------------------------------------
!
IF(.NOT.OOASIS)THEN
!
! Read forcings from TRIP_FORCING.nc
!
  CALL TRIP_FORCING_OFFLINE(TPG, TPLK, TPST,                                  &
                            KLISTING, KSTATE, KNB_TSTEP_RUN, KLON, KLAT, KNB, &
                            ZDRAIN_OL, ZRUNOFF_OL, ZSRC_FLOOD_OL              )
!
ENDIF
!
! --------------------------------------------------------------------------------------
! * 3. Temporal loops
! --------------------------------------------------------------------------------------
!
ZTIMEC         = 0
ICOUNT         = 0
JNB_TSTEP_DIAG = 0
ILAKE_NUM = TPLK%NLAKE_NUM
!
DO JNB_TSTEP_RUN = 1, KNB_TSTEP_RUN
  !IF (NRANK==NPIO.AND.LPRINT) THEN
  !  WRITE(*,'(A16,I5,A2,I5)')'TRIP TSTEP_RUN :',JNB_TSTEP_RUN,' /',KNB_TSTEP_RUN
  !ENDIF
!
! * TRIP INPUT FLUXES (kg/s)
!
  IF(OOASIS)THEN
!
    CALL TRIP_FORCING_OASIS(TP, TPG, TPLK, TPST,                  &
                            KLISTING, KSTATE, KLON, KLAT, ZTIMEC, &
                            ZDRAIN, ZRUNOFF, ZSRC_FLOOD, ZCALVING )
!
  ELSE
    ZDRAIN    (:) = ZDRAIN_OL    (:,JNB_TSTEP_RUN) / XTSTEP_RUN
    ZRUNOFF   (:) = ZRUNOFF_OL   (:,JNB_TSTEP_RUN) / XTSTEP_RUN
    ZSRC_FLOOD(:) = ZSRC_FLOOD_OL(:,JNB_TSTEP_RUN) / XTSTEP_RUN
    ZCALVING  (:) = 0.0
  ENDIF
!
! * TRIP PHYSIC CALL
!
  CALL TRIP_INTERFACE(TPDG, TP, TPG, TPLK, TPST,       &
                      KLISTING,LPRINT,                 &
                      KSTATE,KLON,KLAT,ILAKE_NUM,      &
                      PTIME,ZTIMEC,                    &
                      JNB_TSTEP_RUN,JNB_TSTEP_DIAG,    &
                      XTSTEP_RUN,XTSTEP_DIAG,ZRUNOFF,  &
                      ZDRAIN,ZCALVING,ZSRC_FLOOD,OXIOS )
!
! * TRIP OUTPUT FLUXES
!
  IF(OOASIS)THEN
    ZTIME_CPL = ZTIMEC-XTSTEP_RUN
    CALL TRIP_OASIS_SEND(TP, TPG, TPST, &
                         KLISTING,KLON,KLAT,KSTATE,ZTIME_CPL)
  ENDIF
!
  IF (NRANK==NPIO.AND.LPRINT.AND.MOD(ZTIMEC,XDAY)==0.0) THEN
    ICOUNT = ICOUNT +1
    !WRITE(*,'(A28)') '============================'
    WRITE(*,'(A10,I5,A2,I5)') 'TRIP DAY :',ICOUNT,' /',INT(PRUNTIME/XDAY)
    !WRITE(*,'(A28)') '============================'
  ENDIF
!
! * TRIP DATE INCREMENT
!
  CALL TRIP_DATE(KYEAR,KMONTH,KDAY,PTIME)
!
ENDDO
!
! --------------------------------------------------------------------------------------
! * 4. End TRIP run
! --------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_RUN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_RUN
