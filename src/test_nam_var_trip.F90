!     #########################################################
      SUBROUTINE TEST_NAM_VAR_TRIP(KLISTING,HNAME,HVAR,       &
                                     HVALUE1,HVALUE2,HVALUE3, &
                                     HVALUE4,HVALUE5,HVALUE6, &
                                     HVALUE7,HVALUE8,HVALUE9  )  
!     #########################################################
!
!!****  *TEST_NAM_VARC0* - routine to test the value of a character var.
!!
!!    PURPOSE
!!    -------
!
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!      FM_READ
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      B. Decharme      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original form V. Masson         17/04/98
!-------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABORT_TRIP
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
INTEGER,          INTENT(IN)           ::KLISTING   ! output listing logical unit
 CHARACTER(LEN=*) ,INTENT(IN)           ::HNAME    ! name of the variable to test
 CHARACTER(LEN=*) ,INTENT(IN)           ::HVAR     ! variable to test

 CHARACTER(LEN=*) ,INTENT(IN), OPTIONAL ::HVALUE1  ! first possible value
 CHARACTER(LEN=*) ,INTENT(IN), OPTIONAL ::HVALUE2  ! second possible value
 CHARACTER(LEN=*) ,INTENT(IN), OPTIONAL ::HVALUE3  ! third possible value
 CHARACTER(LEN=*) ,INTENT(IN), OPTIONAL ::HVALUE4  ! fourth possible value
 CHARACTER(LEN=*) ,INTENT(IN), OPTIONAL ::HVALUE5  ! fiveth possible value
 CHARACTER(LEN=*) ,INTENT(IN), OPTIONAL ::HVALUE6  ! sixth possible value
 CHARACTER(LEN=*) ,INTENT(IN), OPTIONAL ::HVALUE7  ! seventh possible value
 CHARACTER(LEN=*) ,INTENT(IN), OPTIONAL ::HVALUE8  ! eightth possible value
 CHARACTER(LEN=*) ,INTENT(IN), OPTIONAL ::HVALUE9  ! nineth possible value
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.2   Declarations of local variables
!
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TEST_NAM_VAR_TRIP',0,ZHOOK_HANDLE)
IF ( PRESENT (HVALUE1) ) THEN
  IF ( HVAR==HVALUE1  .AND. LHOOK) &
        CALL DR_HOOK('TEST_NAM_VAR_TRIP',1,ZHOOK_HANDLE)
  IF ( HVAR==HVALUE1 ) RETURN
END IF
!
IF ( PRESENT (HVALUE2) ) THEN
  IF ( HVAR==HVALUE2  .AND. LHOOK) &
        CALL DR_HOOK('TEST_NAM_VAR_TRIP',1,ZHOOK_HANDLE)
  IF ( HVAR==HVALUE2 ) RETURN
END IF
!
IF ( PRESENT (HVALUE3) ) THEN
  IF ( HVAR==HVALUE3  .AND. LHOOK) &
        CALL DR_HOOK('TEST_NAM_VAR_TRIP',1,ZHOOK_HANDLE)
  IF ( HVAR==HVALUE3 ) RETURN
END IF
!
IF ( PRESENT (HVALUE4) ) THEN
  IF ( HVAR==HVALUE4  .AND. LHOOK) &
        CALL DR_HOOK('TEST_NAM_VAR_TRIP',1,ZHOOK_HANDLE)
  IF ( HVAR==HVALUE4 ) RETURN
END IF
!
IF ( PRESENT (HVALUE5) ) THEN
  IF ( HVAR==HVALUE5  .AND. LHOOK) &
        CALL DR_HOOK('TEST_NAM_VAR_TRIP',1,ZHOOK_HANDLE)
  IF ( HVAR==HVALUE5 ) RETURN
END IF
!
IF ( PRESENT (HVALUE6) ) THEN
  IF ( HVAR==HVALUE6  .AND. LHOOK) &
        CALL DR_HOOK('TEST_NAM_VAR_TRIP',1,ZHOOK_HANDLE)
  IF ( HVAR==HVALUE6 ) RETURN
END IF
!
IF ( PRESENT (HVALUE7) ) THEN
  IF ( HVAR==HVALUE7  .AND. LHOOK) &
        CALL DR_HOOK('TEST_NAM_VAR_TRIP',1,ZHOOK_HANDLE)
  IF ( HVAR==HVALUE7 ) RETURN
END IF
!
IF ( PRESENT (HVALUE8) ) THEN
  IF ( HVAR==HVALUE8  .AND. LHOOK) &
        CALL DR_HOOK('TEST_NAM_VAR_TRIP',1,ZHOOK_HANDLE)
  IF ( HVAR==HVALUE8 ) RETURN
END IF
!
IF ( PRESENT (HVALUE9) ) THEN
  IF ( HVAR==HVALUE9  .AND. LHOOK) &
        CALL DR_HOOK('TEST_NAM_VAR_TRIP',1,ZHOOK_HANDLE)
  IF ( HVAR==HVALUE9 ) RETURN
END IF
!
!-------------------------------------------------------------------------------
!
WRITE (KLISTING,*) ' '
WRITE (KLISTING,*) 'FATAL ERROR:'
WRITE (KLISTING,*) '-----------'
WRITE (KLISTING,*) ' '
WRITE (KLISTING,*) 'Value "',HVAR,'" is not allowed for variable ',HNAME
WRITE (KLISTING,*) ' '
WRITE (KLISTING,*) 'Possible values are:'
IF ( PRESENT (HVALUE1) ) WRITE (KLISTING,*) '"',HVALUE1,'"'
IF ( PRESENT (HVALUE2) ) WRITE (KLISTING,*) '"',HVALUE2,'"'
IF ( PRESENT (HVALUE3) ) WRITE (KLISTING,*) '"',HVALUE3,'"'
IF ( PRESENT (HVALUE4) ) WRITE (KLISTING,*) '"',HVALUE4,'"'
IF ( PRESENT (HVALUE5) ) WRITE (KLISTING,*) '"',HVALUE5,'"'
IF ( PRESENT (HVALUE6) ) WRITE (KLISTING,*) '"',HVALUE6,'"'
IF ( PRESENT (HVALUE7) ) WRITE (KLISTING,*) '"',HVALUE7,'"'
IF ( PRESENT (HVALUE8) ) WRITE (KLISTING,*) '"',HVALUE8,'"'
IF ( PRESENT (HVALUE9) ) WRITE (KLISTING,*) '"',HVALUE9,'"'
!
 CALL ABORT_TRIP('TEST_NAM_VAR_TRIP: (1) CHARACTER VALUE NOT ALLOWED')
IF (LHOOK) CALL DR_HOOK('TEST_NAM_VAR_TRIP',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE TEST_NAM_VAR_TRIP
