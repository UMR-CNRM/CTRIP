!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!######################
MODULE MODE_TRIP_GRID_STATE
!######################
!
!!****  *MODE_TRIP_GRID_STATE*
!!
!!    PURPOSE
!!    -------
!
!      The purpose of this routine is to transform grid variable to state variable.
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!       NONE
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      S. Munier       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2019
!--------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TRIP_PAR, ONLY : NUNDEF, XUNDEF
!
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE PARKIND1, ONLY : JPRB
!
INTERFACE TRIP_GRID_TO_STATE
    MODULE PROCEDURE TRIP_GRID_TO_STATE_INTEGER
    MODULE PROCEDURE TRIP_GRID_TO_STATE_REAL_X2
    MODULE PROCEDURE TRIP_GRID_TO_STATE_REAL_X3
    MODULE PROCEDURE TRIP_GRID_TO_STATE_LOGICAL
END INTERFACE
!
INTERFACE TRIP_STATE_TO_GRID
    MODULE PROCEDURE TRIP_STATE_TO_GRID_INTEGER
    MODULE PROCEDURE TRIP_STATE_TO_GRID_REAL_X2
    MODULE PROCEDURE TRIP_STATE_TO_GRID_REAL_X3
    MODULE PROCEDURE TRIP_STATE_TO_GRID_LOGICAL
END INTERFACE
!
!-------------------------------------------------------------------------------
!
CONTAINS
!
!-------------------------------------------------------------------------------
!
!#########################################################################
SUBROUTINE TRIP_GRID_TO_STATE_INTEGER(KSTATE_LON,KSTATE_LAT,KGRID,KSTATE)
!#########################################################################
!
IMPLICIT NONE
!
!*      declarations of arguments
!
INTEGER, DIMENSION(:),   INTENT(IN)  :: KSTATE_LON
INTEGER, DIMENSION(:),   INTENT(IN)  :: KSTATE_LAT
INTEGER, DIMENSION(:,:), INTENT(IN)  :: KGRID
!
INTEGER, DIMENSION(:),   INTENT(OUT) :: KSTATE
!
!*      declarations of local variables
!
INTEGER :: ISTATE,JSTATE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_GRID_STATE:TRIP_GRID_TO_STATE_INTEGER',0,ZHOOK_HANDLE)
!
!*      procedure
!
ISTATE = SIZE(KSTATE)
KSTATE(:) = NUNDEF
DO JSTATE = 1,ISTATE
  IF (KSTATE_LON(JSTATE)>0) THEN
    KSTATE(JSTATE) = KGRID(KSTATE_LON(JSTATE),KSTATE_LAT(JSTATE))
  ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_GRID_STATE:TRIP_GRID_TO_STATE_INTEGER',1,ZHOOK_HANDLE)
!
END SUBROUTINE TRIP_GRID_TO_STATE_INTEGER
!
!-------------------------------------------------------------------------------
!
!#########################################################################
SUBROUTINE TRIP_GRID_TO_STATE_REAL_X2(KSTATE_LON,KSTATE_LAT,PGRID,PSTATE)
!#########################################################################
!
IMPLICIT NONE
!
!*      declarations of arguments
!
INTEGER, DIMENSION(:),   INTENT(IN)  :: KSTATE_LON
INTEGER, DIMENSION(:),   INTENT(IN)  :: KSTATE_LAT
REAL,    DIMENSION(:,:), INTENT(IN)  :: PGRID
!
REAL,    DIMENSION(:),   INTENT(OUT) :: PSTATE
!
!*      declarations of local variables
!
INTEGER :: ISTATE,JSTATE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_GRID_STATE:TRIP_GRID_TO_STATE_REAL_X2',0,ZHOOK_HANDLE)
!
!*      procedure
!
ISTATE = SIZE(PSTATE,1)
PSTATE(:) = XUNDEF
DO JSTATE = 1,ISTATE
  IF (KSTATE_LON(JSTATE)>0) THEN
    PSTATE(JSTATE) = PGRID(KSTATE_LON(JSTATE),KSTATE_LAT(JSTATE))
  ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_GRID_STATE:TRIP_GRID_TO_STATE_REAL_X2',1,ZHOOK_HANDLE)
!
END SUBROUTINE TRIP_GRID_TO_STATE_REAL_X2
!
!-------------------------------------------------------------------------------
!
!#########################################################################
SUBROUTINE TRIP_GRID_TO_STATE_REAL_X3(KSTATE_LON,KSTATE_LAT,PGRID,PSTATE)
!#########################################################################
!
IMPLICIT NONE
!
!*      declarations of arguments
!
INTEGER, DIMENSION(:),     INTENT(IN)  :: KSTATE_LON
INTEGER, DIMENSION(:),     INTENT(IN)  :: KSTATE_LAT
REAL,    DIMENSION(:,:,:), INTENT(IN)  :: PGRID
!
REAL,    DIMENSION(:,:),   INTENT(OUT) :: PSTATE
!
!*      declarations of local variables
!
INTEGER :: ISTATE,JSTATE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_GRID_STATE:TRIP_GRID_TO_STATE_REAL_X3',0,ZHOOK_HANDLE)
!
!*      procedure
!
ISTATE = SIZE(PSTATE,2)
PSTATE(:,:) = XUNDEF
DO JSTATE = 1,ISTATE
  IF (KSTATE_LON(JSTATE)>0) THEN
    PSTATE(:,JSTATE) = PGRID(KSTATE_LON(JSTATE),KSTATE_LAT(JSTATE),:)
  ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_GRID_STATE:TRIP_GRID_TO_STATE_REAL_X3',1,ZHOOK_HANDLE)
!
END SUBROUTINE TRIP_GRID_TO_STATE_REAL_X3
!
!-------------------------------------------------------------------------------
!
!#########################################################################
SUBROUTINE TRIP_GRID_TO_STATE_LOGICAL(KSTATE_LON,KSTATE_LAT,OGRID,OSTATE)
!#########################################################################
!
IMPLICIT NONE
!
!*      declarations of arguments
!
INTEGER, DIMENSION(:),   INTENT(IN)  :: KSTATE_LON
INTEGER, DIMENSION(:),   INTENT(IN)  :: KSTATE_LAT
LOGICAL, DIMENSION(:,:), INTENT(IN)  :: OGRID
!
LOGICAL, DIMENSION(:),   INTENT(OUT) :: OSTATE
!
!*      declarations of local variables
!
INTEGER :: ISTATE,JSTATE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_GRID_STATE:TRIP_GRID_TO_STATE_LOGICAL',0,ZHOOK_HANDLE)
!
!*      procedure
!
ISTATE = SIZE(OSTATE)
OSTATE(:) = .FALSE.
DO JSTATE = 1,ISTATE
  IF (KSTATE_LON(JSTATE)>0) THEN
    OSTATE(JSTATE) = OGRID(KSTATE_LON(JSTATE),KSTATE_LAT(JSTATE))
  ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_GRID_STATE:TRIP_GRID_TO_STATE_LOGICAL',1,ZHOOK_HANDLE)
!
END SUBROUTINE TRIP_GRID_TO_STATE_LOGICAL
!
!-------------------------------------------------------------------------------
!
!#########################################################################
SUBROUTINE TRIP_STATE_TO_GRID_INTEGER(KSTATE_LON,KSTATE_LAT,KSTATE,KGRID)
!#########################################################################
!
IMPLICIT NONE
!
!*      declarations of arguments
!
INTEGER, DIMENSION(:),   INTENT(IN)  :: KSTATE_LON
INTEGER, DIMENSION(:),   INTENT(IN)  :: KSTATE_LAT
INTEGER, DIMENSION(:),   INTENT(IN)  :: KSTATE
!
INTEGER, DIMENSION(:,:), INTENT(OUT) :: KGRID
!
!*      declarations of local variables
!
INTEGER :: ISTATE,JSTATE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_GRID_STATE:TRIP_STATE_TO_GRID_INTEGER',0,ZHOOK_HANDLE)
!
!*      procedure
!
ISTATE = SIZE(KSTATE)
KGRID(:,:) = NUNDEF
DO JSTATE = 1,ISTATE
  IF (KSTATE_LON(JSTATE)>0) THEN
    KGRID(KSTATE_LON(JSTATE),KSTATE_LAT(JSTATE)) = KSTATE(JSTATE)
  ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_GRID_STATE:TRIP_STATE_TO_GRID_INTEGER',1,ZHOOK_HANDLE)
!
END SUBROUTINE TRIP_STATE_TO_GRID_INTEGER
!
!-------------------------------------------------------------------------------
!
!#########################################################################
SUBROUTINE TRIP_STATE_TO_GRID_REAL_X2(KSTATE_LON,KSTATE_LAT,PSTATE,PGRID)
!#########################################################################
!
IMPLICIT NONE
!
!*      declarations of arguments
!
INTEGER, DIMENSION(:),   INTENT(IN)  :: KSTATE_LON
INTEGER, DIMENSION(:),   INTENT(IN)  :: KSTATE_LAT
REAL,    DIMENSION(:),   INTENT(IN)  :: PSTATE
!
REAL,    DIMENSION(:,:), INTENT(OUT) :: PGRID
!
!*      declarations of local variables
!
INTEGER :: ISTATE,JSTATE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_GRID_STATE:TRIP_STATE_TO_GRID_REAL_X2',0,ZHOOK_HANDLE)
!
!*      procedure
!
ISTATE = SIZE(PSTATE)
PGRID(:,:) = XUNDEF
DO JSTATE = 1,ISTATE
  IF (KSTATE_LON(JSTATE)>0) THEN
    PGRID(KSTATE_LON(JSTATE),KSTATE_LAT(JSTATE)) = PSTATE(JSTATE)
  ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_GRID_STATE:TRIP_STATE_TO_GRID_REAL_X2',1,ZHOOK_HANDLE)
!
END SUBROUTINE TRIP_STATE_TO_GRID_REAL_X2
!
!-------------------------------------------------------------------------------
!
!#########################################################################
SUBROUTINE TRIP_STATE_TO_GRID_REAL_X3(KSTATE_LON,KSTATE_LAT,PSTATE,PGRID)
!#########################################################################
!
IMPLICIT NONE
!
!*      declarations of arguments
!
INTEGER, DIMENSION(:),     INTENT(IN)  :: KSTATE_LON
INTEGER, DIMENSION(:),     INTENT(IN)  :: KSTATE_LAT
INTEGER, DIMENSION(:,:),   INTENT(IN)  :: PSTATE
!
INTEGER, DIMENSION(:,:,:), INTENT(OUT) :: PGRID
!
!*      declarations of local variables
!
INTEGER :: ISTATE,JSTATE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_GRID_STATE:TRIP_STATE_TO_GRID_REAL_X3',0,ZHOOK_HANDLE)
!
!*      procedure
!
ISTATE = SIZE(PSTATE)
PGRID(:,:,:) = XUNDEF
DO JSTATE = 1,ISTATE
  IF (KSTATE_LON(JSTATE)>0) THEN
    PGRID(KSTATE_LON(JSTATE),KSTATE_LAT(JSTATE),:) = PSTATE(JSTATE,:)
  ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_GRID_STATE:TRIP_STATE_TO_GRID_REAL_X3',1,ZHOOK_HANDLE)
!
END SUBROUTINE TRIP_STATE_TO_GRID_REAL_X3
!
!-------------------------------------------------------------------------------
!
!#########################################################################
SUBROUTINE TRIP_STATE_TO_GRID_LOGICAL(KSTATE_LON,KSTATE_LAT,OSTATE,OGRID)
!#########################################################################
!
IMPLICIT NONE
!
!*      declarations of arguments
!
INTEGER, DIMENSION(:),   INTENT(IN)  :: KSTATE_LON
INTEGER, DIMENSION(:),   INTENT(IN)  :: KSTATE_LAT
LOGICAL, DIMENSION(:),   INTENT(IN)  :: OSTATE
!
LOGICAL, DIMENSION(:,:), INTENT(OUT) :: OGRID
!
!*      declarations of local variables
!
INTEGER :: ISTATE,JSTATE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_GRID_STATE:TRIP_STATE_TO_GRID_LOGICAL',0,ZHOOK_HANDLE)
!
!*      procedure
!
ISTATE = SIZE(OSTATE)
OGRID(:,:) = .FALSE.
DO JSTATE = 1,ISTATE
  IF (KSTATE_LON(JSTATE)>0) THEN
    OGRID(KSTATE_LON(JSTATE),KSTATE_LAT(JSTATE)) = OSTATE(JSTATE)
  ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_GRID_STATE:TRIP_STATE_TO_GRID_LOGICAL',1,ZHOOK_HANDLE)
!
END SUBROUTINE TRIP_STATE_TO_GRID_LOGICAL
!
!-------------------------------------------------------------------------------
!
END MODULE MODE_TRIP_GRID_STATE
