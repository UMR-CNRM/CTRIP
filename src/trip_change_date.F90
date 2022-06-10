!#########
PROGRAM TRIP_CHANGE_DATE
!############################################
!
!!****  *TRIP_CHANGE_DATE*  
!!
!!    PURPOSE
!!    -------
!
!     Change the current date in the TRIP restart.
!     Important for spin up procedure
!     
!!      
!!    AUTHOR
!!    ------
!!      B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/14 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_PAR, ONLY : LNCPRINT
!
USE NETCDF
USE MODE_TRIP_NETCDF
!
USE MODI_INIT_TRIP_PAR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of local variables
!
 CHARACTER(LEN=NF90_MAX_NAME), PARAMETER :: YFILE    = 'TRIP_RESTART.nc'
 CHARACTER(LEN=NF90_MAX_NAME), PARAMETER :: YDATE    = 'date'
INTEGER,                      PARAMETER :: ILISTING = 6
LOGICAL,                      PARAMETER :: LRW      = .TRUE.
!
REAL*4, DIMENSION(4) :: ZDATE_OLD, ZDATE
REAL,   DIMENSION(4) :: ZREAD
!
INTEGER            :: IYEAR
INTEGER            :: IMONTH
INTEGER            :: IDAY
INTEGER            :: ITIME
!
INTEGER            :: IC, IDATEID, INCID
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_CHANGE_DATE',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
 CALL INIT_TRIP_PAR
!-------------------------------------------------------------------------------
!
! * Read current date
!
OPEN(UNIT=21, FILE='date_trip', FORM='formatted')
READ(21, *) IYEAR, IMONTH, IDAY, ITIME
 CLOSE(21)
!
ZDATE(1) = REAL(IYEAR)
ZDATE(2) = REAL(IMONTH)
ZDATE(3) = REAL(IDAY)
ZDATE(4) = REAL(ITIME)
!
! * Open netcdf file
!
 CALL NCOPEN(ILISTING,LRW,LNCPRINT,YFILE,INCID)
!
! * read date in restart file
!
 CALL NCREAD(ILISTING,INCID,YDATE,ZREAD(:),LNCPRINT)
ZDATE_OLD(:) = ZREAD(:)
!
WRITE(ILISTING,*)'Change date in trip restart :'
WRITE(ILISTING,*)'Year  :',INT(ZDATE_OLD(1)),'to',IYEAR
WRITE(ILISTING,*)'Month :',INT(ZDATE_OLD(2)),'to',IMONTH
WRITE(ILISTING,*)'Day   :',INT(ZDATE_OLD(3)),'to',IDAY
WRITE(ILISTING,*)'Time  :',INT(ZDATE_OLD(4)),'to',ITIME

! * Write current date in restart file
!
IC = NF90_INQ_VARID(INCID,YDATE,IDATEID)
IF(IC/=NF90_NOERR)THEN
  WRITE(ILISTING,*)'TRIP_CHANGE_DATE: NF90_INQ_VARID problem'
  STOP
ENDIF
!
IC = NF90_PUT_VAR(INCID,IDATEID,ZDATE)
IF(IC/=NF90_NOERR)THEN
  WRITE(ILISTING,*)'TRIP_CHANGE_DATE: NF90_PUT_VAR problem'
  STOP
ENDIF
!
WRITE(ILISTING,*)'Sucess in writting current date'
!
! * Close netcdf file
!
 CALL NCCLOSE(ILISTING,LNCPRINT,YFILE,INCID)
!
IF (LHOOK) CALL DR_HOOK('TRIP_CHANGE_DATE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END PROGRAM TRIP_CHANGE_DATE
