!#########
SUBROUTINE READ_NAM_TRIP_RUN(KLISTING)
!#######################################################
!
!!****  *READ_NAM_TRIP_RUN* - routine to read the configuration for TRIP
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2008 
!!      S.Munier    12/2020 : selection of output variables
!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODN_TRIP_RUN
!
USE MODI_TRIP_POSNAM
USE MODI_ABORT_TRIP
USE MODI_TEST_NAM_VAR_TRIP
USE MODI_OPEN_TRIP_NAMELIST
USE MODI_CLOSE_TRIP_NAMELIST
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, INTENT(IN) :: KLISTING
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!
LOGICAL         :: GFOUND         ! Return code when searching namelist
INTEGER         :: INAM           ! logical unit of namelist file
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_TRIP_RUN',0,ZHOOK_HANDLE)
!
!* open namelist file
!
 CALL OPEN_TRIP_NAMELIST(INAM)
!
!* reading of namelist
!  -------------------
!
CSELECT(:) = '            '
!
CALL TRIP_POSNAM(INAM,'NAM_TRIP_RUN',GFOUND,KLISTING)
IF (GFOUND) THEN
   READ(UNIT=INAM,NML=NAM_TRIP_RUN)
ELSE
   WRITE(KLISTING,*)'READ_NAM_TRIP_RUN: NAM_TRIP_RUN not found in namelist'
   CALL ABORT_TRIP('READ_NAM_TRIP_RUN: NAM_TRIP_RUN not found in namelist')
ENDIF
!
CALL TEST_NAM_VAR_TRIP(KLISTING,'CREADFRC',CREADFRC,'VECTOR','LATLON')
!
!* close namelist file
!
CALL CLOSE_TRIP_NAMELIST(INAM)
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_TRIP_RUN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_TRIP_RUN
