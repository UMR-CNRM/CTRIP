!     #########
SUBROUTINE WAITING_MPI(HWAITING)
!     ################################################################
!
!!****  *WAITING_MPI*
!!
!!    PURPOSE
!!    -------
!
!     Calculate the river storage in the next time step based on the storage of current time step
!     Where OMASK_VEL=true the Manning equation is used to compute a variable flow velocity.
!
!
!!**  METHOD
!!    ------
!
!     RK Ordre 4 Rang 4
!
!!    EXTERNAL
!!    --------
!
!     None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!    B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/02/09
!!      S. Munier   28/03/2020 CTRIP-12D and parallelization
!!      T. Guinaldo 04/2020    Add MLake
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_MPI, ONLY : NCOMM
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
CHARACTER(LEN=*), INTENT(IN) :: HWAITING
CHARACTER(LEN=255) :: YWAITING
INTEGER :: IERR
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
YWAITING = 'WAITING_MPI:'//TRIM(HWAITING)
IF (LHOOK) CALL DR_HOOK(YWAITING,0,ZHOOK_HANDLE)
!
CALL MPI_BARRIER(NCOMM,IERR)
!
IF (LHOOK) CALL DR_HOOK(YWAITING,1,ZHOOK_HANDLE)
!
END SUBROUTINE WAITING_MPI
