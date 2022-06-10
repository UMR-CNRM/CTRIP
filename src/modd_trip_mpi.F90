!     ######################
      MODULE MODD_TRIP_MPI
!     ######################
!-------------------------------------------------------------------------------
!
!##################
!
!!****  *MODD_TRIP_MPI - declaration of MPI communication variables
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      S. Munier   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       07/2019
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!-------------------------------------------------------------------------------
!
INTEGER :: NPROC = 1
INTEGER :: NRANK = 0
INTEGER :: NCOMM = -1
INTEGER :: NPIO  = 0
CHARACTER(LEN=3) :: CRANK
!
END MODULE MODD_TRIP_MPI

