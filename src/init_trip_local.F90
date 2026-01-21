!     #########
      SUBROUTINE INIT_TRIP_LOCAL (TPST, KLISTING, KLON, KLAT, KPNT)
!     #######################################################################
!
!!****  *INIT_TRIP_LOCAL*
!!
!!    PURPOSE
!!    -------
!
!     Prepare local mask from file
!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      S. Munier
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    28/09/2021
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_STATE, ONLY : TRIP_STATE_t
USE MODD_TRIP_ANALYSIS, ONLY : TRIP_LOCAL_NULLIFY, TPLOC
!
!USE MODN_TRIP_ASSIM, ONLY : CLOCAL_FILE
!
USE MODE_RW_TRIP
!
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE PARKIND1, ONLY : JPRB
!
IMPLICIT NONE
!
!
!*      0.1    declarations of arguments
!
TYPE(TRIP_STATE_t), INTENT(INOUT) :: TPST
!
INTEGER, INTENT(IN) :: KLISTING
INTEGER, INTENT(IN) :: KLON
INTEGER, INTENT(IN) :: KLAT
INTEGER, INTENT(IN) :: KPNT
!
!*      0.2    declarations of local variables
!
CHARACTER(LEN=13), PARAMETER         :: YFILE_PARAM  ='TRIP_PARAM.nc'
CHARACTER(LEN=20) :: YVAR
REAL, DIMENSION(KLON,KLAT) :: ZREAD
INTEGER :: JSTATE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_TRIP_LOCAL',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
! * Allocate
!-------------------------------------------------------------------------------
!
ALLOCATE(TPLOC(KPNT))
DO JSTATE = 1,KPNT
  CALL TRIP_LOCAL_NULLIFY(TPLOC(JSTATE))
ENDDO
!
!-------------------------------------------------------------------------------
! * Read sequential local files
!-------------------------------------------------------------------------------
!
YVAR = 'LOCAL_LEN'
CALL READ_TRIP(KLISTING,YFILE_PARAM,YVAR,ZREAD)
!
DO JSTATE = 1,KPNT
  TPLOC(JSTATE)%NLEN = INT(ZREAD(TPST%NSTATE_LON(JSTATE),TPST%NSTATE_LAT(JSTATE)))
ENDDO
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_TRIP_LOCAL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE INIT_TRIP_LOCAL
