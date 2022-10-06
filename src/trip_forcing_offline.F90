!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
SUBROUTINE TRIP_FORCING_OFFLINE(TPG, TPLK, TPST,                                  &
                                KLISTING, KSTATE, KNB_TSTEP_RUN, KLON, KLAT, KNB, &
                                PDRAIN_OL, PRUNOFF_OL, PSRC_FLOOD_OL              )
!#############################################
!
!!****  *TRIP_FORCING_OFFLINE*
!!
!!    PURPOSE
!!    -------
!!
!!    Get forcing from TRIP_FORCING.nc file (offline mode)
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      S. Munier
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2020
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_GRID,  ONLY : TRIP_GRID_t
USE MODD_TRIP_LAKE,  ONLY : TRIP_LAKE_t
USE MODD_TRIP_STATE, ONLY : TRIP_STATE_t
!
USE MODD_TRIP_LISTING
USE MODD_TRIP_MPI
!
USE MODN_TRIP    , ONLY : CLAKE
!
USE MODD_TRIP_PAR, ONLY : XUNDEF
!
USE MODE_TRIP_GRID_STATE
USE MODE_TRIP_LAKE
!
USE MODI_TRIP_FORCING
USE MODI_WAITING_MPI
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
TYPE(TRIP_GRID_t),  INTENT(INOUT) :: TPG
TYPE(TRIP_LAKE_t),  INTENT(INOUT) :: TPLK
TYPE(TRIP_STATE_t), INTENT(INOUT) :: TPST
!
INTEGER, INTENT(IN)  :: KLISTING      ! Listing ID
INTEGER, INTENT(IN)  :: KSTATE        ! Size of state vector
INTEGER, INTENT(IN)  :: KNB_TSTEP_RUN ! number of time step in the run
!
INTEGER, INTENT(IN)  :: KLON          ! Number of longitude if forcing offline
INTEGER, INTENT(IN)  :: KLAT          ! Number of latittude if forcing offline
INTEGER, INTENT(IN)  :: KNB           ! number of time step if forcing offline
!
REAL, DIMENSION(KSTATE,KNB), INTENT(OUT) :: PDRAIN_OL         ! Drainage from the forcing file          (kg)
REAL, DIMENSION(KSTATE,KNB), INTENT(OUT) :: PRUNOFF_OL        ! Surface runoff from the forcing file    (kg)
REAL, DIMENSION(KSTATE,KNB), INTENT(OUT) :: PSRC_FLOOD_OL     ! Flood source term from the forcing file (kg)
!
!-------------------------------------------------------------------------------
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZDRAIN_OL_READ        ! Drainage from the forcing file          (kg)
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZRUNOFF_OL_READ       ! Surface runoff from the forcing file    (kg)
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZSRC_FLOOD_OL_READ    ! Flood source term from the forcing file (kg)
REAL, DIMENSION(:),     ALLOCATABLE :: ZVECTOR
!
INTEGER :: JNB_TSTEP_RUN     ! TSTEP_RUN counter
INTEGER :: IERR
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! --------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_FORCING_OFFLINE',0,ZHOOK_HANDLE)
!
! Read forcings from TRIP_FORCING.nc
!
PDRAIN_OL    (:,:) = XUNDEF
PRUNOFF_OL   (:,:) = XUNDEF
PSRC_FLOOD_OL(:,:) = XUNDEF
!
IF(NRANK==NPIO)THEN
  ALLOCATE(ZDRAIN_OL_READ(KLON,KLAT,KNB_TSTEP_RUN))
  ALLOCATE(ZRUNOFF_OL_READ(KLON,KLAT,KNB_TSTEP_RUN))
  ALLOCATE(ZSRC_FLOOD_OL_READ(KLON,KLAT,KNB_TSTEP_RUN))
  ALLOCATE(ZVECTOR(TPST%NNSTATE))
!
  CALL TRIP_FORCING(TPG, &
                    KLISTING,KLON,KLAT,KNB_TSTEP_RUN, &
                    ZDRAIN_OL_READ,ZRUNOFF_OL_READ,ZSRC_FLOOD_OL_READ)
ELSE
  ALLOCATE(ZVECTOR(0))
ENDIF
!
CALL WAITING_MPI('TRIP_FORCING_OFFLINE')
!
DO JNB_TSTEP_RUN = 1,KNB_TSTEP_RUN
  IF(NRANK==NPIO)THEN
    IF (CLAKE=='MLK') THEN
      CALL TRIP_GRID_TO_LAKE(TPST%NSTATE_IND,TPLK%NLAKE_ID_IN,TPST%NLAKE_STATE,         &
                              TPLK%XFRAC_LAKE,ZDRAIN_OL_READ(:,:,JNB_TSTEP_RUN),ZVECTOR )
    ELSE
      CALL TRIP_GRID_TO_STATE(TPST%NSTATE_LON,TPST%NSTATE_LAT,ZDRAIN_OL_READ(:,:,JNB_TSTEP_RUN),ZVECTOR)
    ENDIF
  ENDIF
  CALL MPI_SCATTER(ZVECTOR,KSTATE,MPI_DOUBLE,PDRAIN_OL(:,JNB_TSTEP_RUN),KSTATE,MPI_DOUBLE,NPIO,NCOMM,IERR)
!
  IF(NRANK==NPIO)THEN
    IF (CLAKE=='MLK') THEN
      CALL TRIP_GRID_TO_LAKE(TPST%NSTATE_IND,TPLK%NLAKE_ID_IN,TPST%NLAKE_STATE,          &
                              TPLK%XFRAC_LAKE,ZRUNOFF_OL_READ(:,:,JNB_TSTEP_RUN),ZVECTOR )
    ELSE
      CALL TRIP_GRID_TO_STATE(TPST%NSTATE_LON,TPST%NSTATE_LAT,ZRUNOFF_OL_READ(:,:,JNB_TSTEP_RUN),ZVECTOR)
    ENDIF
  ENDIF
  CALL MPI_SCATTER(ZVECTOR,KSTATE,MPI_DOUBLE,PRUNOFF_OL(:,JNB_TSTEP_RUN),KSTATE,MPI_DOUBLE,NPIO,NCOMM,IERR)
!
  IF(NRANK==NPIO)THEN
    IF (CLAKE=='MLK') THEN
      CALL TRIP_GRID_TO_LAKE(TPST%NSTATE_IND,TPLK%NLAKE_ID_IN,TPST%NLAKE_STATE,             &
                              TPLK%XFRAC_LAKE,ZSRC_FLOOD_OL_READ(:,:,JNB_TSTEP_RUN),ZVECTOR )
    ELSE
      CALL TRIP_GRID_TO_STATE(TPST%NSTATE_LON,TPST%NSTATE_LAT,ZSRC_FLOOD_OL_READ(:,:,JNB_TSTEP_RUN),ZVECTOR)
    ENDIF
  ENDIF
  CALL MPI_SCATTER(ZVECTOR,KSTATE,MPI_DOUBLE,PSRC_FLOOD_OL(:,JNB_TSTEP_RUN),KSTATE,MPI_DOUBLE,NPIO,NCOMM,IERR)
ENDDO
!
IF(NRANK==NPIO)THEN
  DEALLOCATE(ZDRAIN_OL_READ)
  DEALLOCATE(ZRUNOFF_OL_READ)
  DEALLOCATE(ZSRC_FLOOD_OL_READ)
ENDIF
DEALLOCATE(ZVECTOR)
!
! --------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_FORCING_OFFLINE',1,ZHOOK_HANDLE)
!
END SUBROUTINE TRIP_FORCING_OFFLINE
