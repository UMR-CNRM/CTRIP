!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
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

