MODULE MODE_TRIP_DATE_NETCDF
!
! Module to read correctly time in a netcdf file
! Author : Matthieu Lafaysse
! Creation : 2012-11-12
! Modifications
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
INTEGER,PARAMETER::IYEAR_GREGOIRE=1582 !Année où le pape Grégoire XIII décida de passer du calendrier julien au calendrier grégorien
!Avant cette date tous les années multiples de 4 sont des années bissextiles
!Au-delà de cette date les années multiples de 100 ne sont pas bissextiles sauf les années multiples de 400 qui le sont
!De plus les journées du 5 au 14 octobre 1582 ont été supprimées pour compenser le retard accumulé
!
CONTAINS
!
!----------------------------------------------------------------------------------------------------------------
!
SUBROUTINE READ_DATE(PTIME,HUNITS,KYEAR,KMONTH,KDAY,PTIME_DATE)
!
!*      0.1   Declarations of arguments
!
REAL,DIMENSION(:),INTENT(IN)  :: PTIME
CHARACTER(*),     INTENT(IN)  :: HUNITS
!
INTEGER,          INTENT(OUT) :: KYEAR
INTEGER,          INTENT(OUT) :: KMONTH
INTEGER,          INTENT(OUT) :: KDAY
REAL,             INTENT(OUT) :: PTIME_DATE
!
!*      0.2   Declarations of local variables
!
INTEGER,DIMENSION(SIZE(PTIME)) :: ITIMEHOURS
REAL,   DIMENSION(SIZE(PTIME)) :: ZREST
!
INTEGER :: IYEAR
INTEGER :: IMONTH
INTEGER :: IDAY
INTEGER :: IHOUR
REAL    :: ZTIME
!
CHARACTER(LEN=14) :: YHEADER
CHARACTER(LEN=100) :: YFMT
CHARACTER :: YC1,YC2,YC3
!
LOGICAL :: GHEADER
!
INTEGER :: ICARACUNITS,ICARACHOUR,ICARACDAY,ICARACMONTH,ICARACYEAR
INTEGER :: IERROR
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_DATE_NETCDF:READ_DATE',0,ZHOOK_HANDLE)
!
SELECT CASE (HUNITS(1:4))
  CASE ("days")
    ICARACUNITS = 11
    ITIMEHOURS(:) = INT(PTIME(:)*24.)
    ZREST(:) = PTIME(:)*24.-ITIMEHOURS(:)
  CASE ("hour")
    ICARACUNITS = 12
    ITIMEHOURS(:) = INT(PTIME(:))
    ZREST(:) = PTIME(:)-ITIMEHOURS(:)
  CASE ("minu")
    ICARACUNITS = 14
    ITIMEHOURS(:) = INT(PTIME(:)/60.)
    ZREST(:) = (PTIME(:)/60.)-ITIMEHOURS(:)
  CASE ("seco")
    ICARACUNITS = 14
    ITIMEHOURS(:) = INT(PTIME(:)/3600.)
    ZREST(:) = (PTIME(:)/3600.)-ITIMEHOURS(:)
  CASE DEFAULT
    PRINT*, "ERROR date_netcdf.F90 : Can't read time units :"
    PRINT*,TRIM(HUNITS)
    STOP "Error units time"
END SELECT
!
! * Read reference date
!
SELECT CASE (HUNITS(1:4))
  CASE ("days","hour","minu","seco")
    boucles_hours:DO ICARACHOUR=1,2 !Hour written by 1 or 2 char
      DO ICARACDAY=1,2 !Day written by 1 or 2 char
        DO ICARACMONTH=1,2 !Month written by 1 or 2 char
          DO ICARACYEAR=1,4 !Year written by 1 to 4 char
            WRITE(YFMT,FMT='("(A",I2,",",2("I",I1,",A1,"),"I",I1,",X,I",I1,",A1)")')&
              ICARACUNITS,ICARACYEAR,ICARACMONTH,ICARACDAY,ICARACHOUR
            READ(HUNITS,FMT=YFMT,IOSTAT=IERROR)YHEADER,IYEAR,YC1,IMONTH,YC2,IDAY,IHOUR,YC3
            IF (IERROR==0) THEN
              GHEADER=(TRIM(YHEADER)=='hours since') .OR. (TRIM(YHEADER)=='minutes since') .OR. &
                      (TRIM(YHEADER)=='seconds since') .OR. (TRIM(YHEADER)=='days since')
              IF (GHEADER.AND.(YC1=='-').AND.(YC2=='-').AND.(YC3==':')) THEN
                 EXIT boucles_hours
              END IF
            END IF
          END DO
        END DO
      END DO
    END DO boucles_hours
  CASE DEFAULT
    STOP "Error units time"
END SELECT
!
! * Check successful reading
!
IF (IERROR>0) THEN
  PRINT*, "ERROR date_netcdf.F90 : Can't read time units :"
  PRINT*,TRIM(HUNITS)
  STOP "Error units time"
END IF
!
!Initialiaze the date to the reference date
!
ZTIME = REAL(IHOUR)+ZREST(1)
!
!Add the number of hours to a date
!
CALL ADDTIME(ITIMEHOURS(1),IYEAR,IMONTH,IDAY,ZTIME)
!
! Current date
!
KYEAR      = IYEAR
KMONTH     = IMONTH
KDAY       = IDAY
PTIME_DATE = ZTIME*3600
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_DATE_NETCDF:READ_DATE',1,ZHOOK_HANDLE)

END SUBROUTINE READ_DATE
!
!----------------------------------------------------------------------------------------------------------------
!
SUBROUTINE ADDTIME (KNHOURS,KYEAR,KMONTH,KDAY,PTIME)
!
!*      0.1   Declarations of arguments
!
INTEGER, INTENT(IN)    :: KNHOURS ! number of hours
!
INTEGER, INTENT(INOUT) :: KYEAR
INTEGER, INTENT(INOUT) :: KMONTH
INTEGER, INTENT(INOUT) :: KDAY
REAL,    INTENT(INOUT) :: PTIME
!
!*      0.2   Declarations of local variables
!
INTEGER,DIMENSION(12) :: INBDM          !Number of days per months
INTEGER               :: IREMAININGDAYS !Number of remaining days to add
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_DATE_NETCDF:ADDTIME',0,ZHOOK_HANDLE)
!
!special case of year 1582
!
IF (KYEAR==IYEAR_GREGOIRE) THEN
  INBDM=(/31,28,31,30,31,30,31,31,30,21,30,31/)
ELSE
  IF (LEAPYEAR(KYEAR)) THEN
    INBDM=(/31,29,31,30,31,30,31,31,30,31,30,31/)
  ELSE
    INBDM=(/31,28,31,30,31,30,31,31,30,31,30,31/)
  ENDIF
ENDIF
!
IF (KNHOURS>=0) THEN
  !Increase hour
  IF ((INT(PTIME)+KNHOURS)<=23) THEN
    PTIME=PTIME+KNHOURS
  ELSE
    IREMAININGDAYS=(INT(PTIME)+KNHOURS)/24 !Number of remaining days
    PTIME=MOD(INT(PTIME)+KNHOURS,24)+PTIME-INT(PTIME) ! new hour (+minutes)
    !Increase days
    DO
      IF ((KDAY+IREMAININGDAYS)<=INBDM(KMONTH)) THEN
        KDAY=KDAY+IREMAININGDAYS
        EXIT
      ELSE
        IREMAININGDAYS=IREMAININGDAYS-(INBDM(KMONTH)-KDAY+1)
        IF (KMONTH<12) THEN
          KMONTH=KMONTH+1 !Month change
          KDAY=1
        ELSE
          !Year change
          KYEAR=KYEAR+1
          KMONTH=1
          KDAY=1
          !Update february number of days
          IF (LEAPYEAR(KYEAR)) INBDM(2)=29
          IF (LEAPYEAR(KYEAR-1)) INBDM(2)=28
          !Update october month for 1582 and 1583
          IF (KYEAR==IYEAR_GREGOIRE) INBDM(10)=21
          IF (KYEAR==IYEAR_GREGOIRE+1) INBDM(10)=31
        ENDIF
      ENDIF
    ENDDO
  ENDIF
ELSE
  IF ((INT(PTIME)+KNHOURS)>=0) THEN
    PTIME=PTIME+KNHOURS
  ELSE
    IREMAININGDAYS=(INT(PTIME)-KNHOURS)/24+1
    PTIME=MOD(INT(PTIME)-KNHOURS,24)+PTIME-INT(PTIME) ! new hour
    ! decrease days
    DO
      IF  ((KDAY-IREMAININGDAYS)>=1) THEN
        KDAY=KDAY-IREMAININGDAYS
        EXIT
      ELSE
        IREMAININGDAYS=IREMAININGDAYS-KDAY
        IF (KMONTH>=1) THEN
          KMONTH=KMONTH-1 !Month change
          KDAY=INBDM(KMONTH)
        ELSE
          !Year change
          KYEAR=KYEAR-1
          KMONTH=12
          KDAY=INBDM(KMONTH)
          !Update february number of days
          IF (LEAPYEAR(KYEAR)) INBDM(2)=29
          IF (LEAPYEAR(KYEAR+1)) INBDM(2)=28
          !Update october month for 1582 and 1583
          IF (KYEAR==IYEAR_GREGOIRE) INBDM(10)=21
          IF (KYEAR==IYEAR_GREGOIRE-1) INBDM(10)=31
        END IF
      END IF
    END DO
  END IF
END IF
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_DATE_NETCDF:ADDTIME',1,ZHOOK_HANDLE)
!
END SUBROUTINE ADDTIME
!
!--------------------------------------------------------------------------------------------------------------
!
LOGICAL FUNCTION LEAPYEAR (PYEAR)
INTEGER, INTENT(IN) :: PYEAR ! Is the year a leap year ?
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_DATE_NETCDF:LEAPYEAR',0,ZHOOK_HANDLE)
!
IF (PYEAR>IYEAR_GREGOIRE) THEN
  LEAPYEAR = (((MOD(PYEAR,4)==0).AND.(MOD(PYEAR,100)/=0)).OR.(MOD(PYEAR,400)==0))
ELSE
  LEAPYEAR = (MOD(PYEAR,4)==0)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_DATE_NETCDF:LEAPYEAR',1,ZHOOK_HANDLE)
!
END FUNCTION LEAPYEAR
!
!--------------------------------------------------------------------------------------------------------------
!
END MODULE MODE_TRIP_DATE_NETCDF
