SUBROUTINE TRIP_DATE(KYEAR,KMONTH,KDAY,PTIME)
!#######################################################
!
!!****  *TRIP_DATE* - current date and hour
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
!
INTEGER, INTENT(INOUT) :: KYEAR  ! year of date
INTEGER, INTENT(INOUT) :: KMONTH ! month of date
INTEGER, INTENT(INOUT) :: KDAY   ! day of date
REAL,    INTENT(INOUT) :: PTIME  ! number of seconds since date at 00 UTC
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
INTEGER :: IDAYS ! number of days in KMONTH
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       1.    Return condition: less than one day to add
!              ------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_DATE',0,ZHOOK_HANDLE)
!
DO 
  IF (86400.-PTIME > 1.E-6) EXIT
!
!-------------------------------------------------------------------------------
!
!*       2.    Adding one day
!              --------------
!
  PTIME=PTIME-86400.
!
!*       2.1   Number of days in a month
!              -------------------------
!
  SELECT CASE (KMONTH)
    CASE(4,6,9,11)
      IDAYS=30
    CASE(1,3,5,7:8,10,12)
      IDAYS=31
    CASE(2)
      IF( ((MOD(KYEAR,4)==0).AND.(MOD(KYEAR,100)/=0)) .OR. (MOD(KYEAR,400)==0))THEN
        IDAYS=29
      ELSE
        IDAYS=28
      ENDIF
  END SELECT
!
!*       2.2   Last day of the month
!              ---------------------
!
  IF (KDAY==IDAYS) THEN
    IF (KMONTH==12) THEN
      KDAY=1
      KMONTH=1
      KYEAR=KYEAR+1
    ELSE
      KDAY=1   
      KMONTH=KMONTH+1
    ENDIF
! 
!*       2.3   Other days
!              ----------
  ELSE
    KDAY=KDAY+1
  ENDIF
!
ENDDO
!
IF (LHOOK) CALL DR_HOOK('TRIP_DATE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE TRIP_DATE
