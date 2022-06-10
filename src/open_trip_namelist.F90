!     ######
      SUBROUTINE OPEN_TRIP_NAMELIST(KLUNAM)
!     #####################################
!
!!****  *OPEN_TRIP_NAMELIST* - routine to open a namelist file
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
!!      Original    01/2013 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODN_TRIP_RUN, ONLY : CNAMELIST
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, INTENT(OUT) :: KLUNAM   ! logical unit of namelist
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('OPEN_TRIP_NAMELIST',0,ZHOOK_HANDLE)
!
KLUNAM=11
OPEN(KLUNAM,FILE=CNAMELIST,ACTION='READ',FORM="FORMATTED",POSITION="REWIND")
!
IF (LHOOK) CALL DR_HOOK('OPEN_TRIP_NAMELIST',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE OPEN_TRIP_NAMELIST
