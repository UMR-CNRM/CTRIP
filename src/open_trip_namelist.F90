!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
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
