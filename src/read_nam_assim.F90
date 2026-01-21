SUBROUTINE READ_NAM_ASSIM(KLISTING)
!####################################################################
!
!!****  *TRIP_RUN_CONF* - prepare the dimenssions (xt or xyt) of a run
!!
!!    PURPOSE
!!    -------
!!
!!    AUTHOR
!!    ------
!!      S. Munier   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!

USE MODI_TRIP_POSNAM
USE MODI_ABORT_TRIP
USE MODI_OPEN_TRIP_NAMELIST
USE MODI_CLOSE_TRIP_NAMELIST
!
USE MODN_TRIP_ASSIM
!
USE MODE_TRIP_GRID
!
USE YOMHOOK
USE PARKIND1  ,ONLY : JPRB

IMPLICIT NONE

INTEGER,          INTENT(IN)     :: KLISTING

INTEGER :: ILUNAM ! namelist file  logical unit
LOGICAL :: GFOUND
REAL    :: ZWORK
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('READ_NAM_ASSIM',0,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
!*       1.    opening of namelist
!
CALL OPEN_TRIP_NAMELIST(ILUNAM)
!
!---------------------------------------------------------------------------
!
!*       2.    Reading of projection parameters
!              --------------------------------
!
CALL TRIP_POSNAM(ILUNAM,'NAM_TRIP_ASSIM',GFOUND,KLISTING)
IF (GFOUND) THEN
   READ(UNIT=ILUNAM,NML=NAM_TRIP_ASSIM)
ELSE
   WRITE(KLISTING,*)'READ_NAM_ASSIM: NAM_TRIP_ASSIM not found in namelist'
   WRITE(KLISTING,*)'Default values are used            '
ENDIF

CALL CLOSE_TRIP_NAMELIST(ILUNAM)

IF (LHOOK) CALL DR_HOOK('READ_NAM_ASSIM',1,ZHOOK_HANDLE)

END SUBROUTINE READ_NAM_ASSIM
