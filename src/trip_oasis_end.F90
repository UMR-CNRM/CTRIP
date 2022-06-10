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
