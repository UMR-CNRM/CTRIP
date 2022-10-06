!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!######################
MODULE MODE_TRIP_LAKE
!######################
!
!!****  *MODE_TRIP_LAKE*
!!
!!    PURPOSE
!!    -------
!
!      The purpose of this routine is to transform grid variable to lake variable.
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
!!      Original    04/2020
!!      T. Guinaldo 04/2020    Add MLake
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TRIP_PAR, ONLY : XUNDEF
!
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE PARKIND1, ONLY : JPRB
!
!-------------------------------------------------------------------------------
!
CONTAINS
!
!-------------------------------------------------------------------------------
!
!#########################################################################
SUBROUTINE TRIP_LAKE_TO_GRID(KLAKE_ID, KLAKE_STATE, PSTATE, PGRID)
!#########################################################################
!
IMPLICIT NONE
!
!*      declarations of arguments
!
INTEGER, DIMENSION(:,:), INTENT(IN)    :: KLAKE_ID
INTEGER, DIMENSION(:),   INTENT(IN)    :: KLAKE_STATE
!
REAL,    DIMENSION(:),   INTENT(IN)    :: PSTATE
REAL,    DIMENSION(:,:), INTENT(INOUT) :: PGRID
!
!*      declarations of local variables
!
INTEGER :: JLON, JLAT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_LAKE:TRIP_LAKE_TO_GRID',0,ZHOOK_HANDLE)
!
!*      procedure
!
DO JLAT = 1,SIZE(PGRID,2)
  DO JLON = 1,SIZE(PGRID,1)
    IF(KLAKE_ID(JLON,JLAT)>0)THEN
      PGRID(JLON,JLAT) = PSTATE(KLAKE_STATE(KLAKE_ID(JLON,JLAT)))
    ENDIF
  ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_LAKE:TRIP_LAKE_TO_GRID',1,ZHOOK_HANDLE)
!
END SUBROUTINE TRIP_LAKE_TO_GRID
!
!-------------------------------------------------------------------------------
!
!#########################################################################
SUBROUTINE TRIP_GRID_TO_LAKE(KSTATE_IND, KLAKE_ID, KLAKE_STATE, &
                             PFRAC_LAKE, PGRID, PSTATE)
!#########################################################################
!
IMPLICIT NONE
!
!*      declarations of arguments
!
INTEGER, DIMENSION(:,:), INTENT(IN)  :: KSTATE_IND
INTEGER, DIMENSION(:,:), INTENT(IN)  :: KLAKE_ID
INTEGER, DIMENSION(:),   INTENT(IN)  :: KLAKE_STATE
REAL,    DIMENSION(:,:), INTENT(IN)  :: PFRAC_LAKE
!
REAL,    DIMENSION(:,:), INTENT(IN)  :: PGRID
REAL,    DIMENSION(:),   INTENT(OUT) :: PSTATE
!
!*      declarations of local variables
!
INTEGER :: JLON, JLAT, JSTATE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_LAKE:TRIP_GRID_TO_LAKE',0,ZHOOK_HANDLE)
!
!*      procedure
!
PSTATE(:) = 0.
!
DO JLAT = 1,SIZE(PGRID,2)
  DO JLON = 1,SIZE(PGRID,1)
    IF(KLAKE_ID(JLON,JLAT)>0)THEN
      JSTATE = KLAKE_STATE(KLAKE_ID(JLON,JLAT))
      PSTATE(JSTATE) = PSTATE(JSTATE) +     PFRAC_LAKE(JLON,JLAT)  * PGRID(JLON,JLAT)
      JSTATE = KSTATE_IND(JLON,JLAT)
      PSTATE(JSTATE) = PSTATE(JSTATE) + (1.-PFRAC_LAKE(JLON,JLAT)) * PGRID(JLON,JLAT)
    ELSEIF(KSTATE_IND(JLON,JLAT)>0)THEN
      JSTATE = KSTATE_IND(JLON,JLAT)
      PSTATE(JSTATE) = PGRID(JLON,JLAT)
    ENDIF
  ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_LAKE:TRIP_GRID_TO_LAKE',1,ZHOOK_HANDLE)
!
END SUBROUTINE TRIP_GRID_TO_LAKE
!
!-------------------------------------------------------------------------------
!
!#########################################################################
SUBROUTINE TRIP_STATE_TO_LAKE(KLAKE_STATE, PSTATE, PLAKE)
!#########################################################################
!
IMPLICIT NONE
!
!*      declarations of arguments
!
INTEGER, DIMENSION(:), INTENT(IN)  :: KLAKE_STATE
!
REAL,    DIMENSION(:), INTENT(IN)  :: PSTATE
REAL,    DIMENSION(:), INTENT(OUT) :: PLAKE
!
!*      declarations of local variables
!
INTEGER :: JLAKE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_LAKE:TRIP_STATE_TO_LAKE',0,ZHOOK_HANDLE)
!
!*      procedure
!
DO JLAKE = 1,SIZE(PLAKE,1)
  PLAKE(JLAKE) = PSTATE(KLAKE_STATE(JLAKE))
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_LAKE:TRIP_STATE_TO_LAKE',1,ZHOOK_HANDLE)
!
END SUBROUTINE TRIP_STATE_TO_LAKE
!
!-------------------------------------------------------------------------------
!
END MODULE MODE_TRIP_LAKE
