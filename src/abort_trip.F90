!     #############################################################
      SUBROUTINE ABORT_TRIP(YTEXT)
!     #############################################################
!
!!****  *ABORT_TRIP* - abort subroutine
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
!!      Original    06/2013 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TRIP_LISTING, ONLY : NLISTING, CLISTING
!     
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*),  INTENT(IN)  :: YTEXT
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* get output listing file logical unit
!
IF (LHOOK) CALL DR_HOOK('ABORT_TRIP',0,ZHOOK_HANDLE)
!      
WRITE(*,*)YTEXT
WRITE(*,*)'---------------------------------------------------------------------------'
WRITE(*,*) 'MORE DETAILS ABOUT THE CRASH IN THE OUTPUT LISTING:',CLISTING
WRITE(*,*)'---------------------------------------------------------------------------'
!
WRITE(NLISTING,*) '---------------------------------------------------------------------------'
WRITE(NLISTING,*) '---------------------------------------------------------------------------'
WRITE(NLISTING,*) '--------------------   FATAL ERROR in TRIP  -------------------------------'
WRITE(NLISTING,*) '---------------------------------------------------------------------------'
WRITE(NLISTING,*) '---------------------------------------------------------------------------'
WRITE(NLISTING,*) '-                                                                         -'
WRITE(NLISTING,*)YTEXT
WRITE(NLISTING,*) '-                                                                         -'
WRITE(NLISTING,*) '---------------------------------------------------------------------------'
WRITE(NLISTING,*) '---------------------------------------------------------------------------'
 CLOSE(NLISTING)
!
 CALL ABORT
STOP
!
IF (LHOOK) CALL DR_HOOK('ABORT_TRIP',1,ZHOOK_HANDLE)
!
END SUBROUTINE ABORT_TRIP
