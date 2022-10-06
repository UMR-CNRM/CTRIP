!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!#########
SUBROUTINE TRIP_OASIS_END(OOASIS,OXIOS)
!########################
!
!!****  *TRIP_OASIS_END* - end coupling TRIP - OASIS
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
!!      Original    10/2013
!!      S.Sénési    08/11/16 : interface to XIOS
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
#ifdef CPLOASIS
USE MOD_OASIS
#endif
!
#ifdef WXIOS
USE XIOS
#endif
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
LOGICAL, INTENT(IN)           :: OOASIS      ! key to use OASIS
LOGICAL, INTENT(IN)           :: OXIOS       ! key to use XIOS
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                    :: IERR   ! Error info
!
IF (OXIOS) THEN
!-------------------------------------------------------------------------------
#ifdef WXIOS
!-------------------------------------------------------------------------------
   CALL XIOS_CONTEXT_FINALIZE()
   CALL XIOS_FINALIZE()
!-------------------------------------------------------------------------------
#endif
!-------------------------------------------------------------------------------
ELSE

  IF (OOASIS) THEN
  !
  !-------------------------------------------------------------------------------
#ifdef CPLOASIS
!-------------------------------------------------------------------------------
!
    CALL OASIS_TERMINATE(IERR)
    IF (IERR/=OASIS_OK) THEN
      WRITE(*,'(A)'   )'Error OASIS terminate'
      WRITE(*,'(A,I4)')'Return code from oasis_terminate : ',IERR
      CALL ABORT
      STOP
    ENDIF
!
!-------------------------------------------------------------------------------
#endif
  ENDIF
!-------------------------------------------------------------------------------
ENDIF
!-------------------------------------------------------------------------------
!
END SUBROUTINE TRIP_OASIS_END
