SUBROUTINE TRIP_XIOS_INIT(TPG,KLOCAL_COMM,KLON,KLAT,KYEAR,KMONTH,KDAY,PTIME)
!!
!!
!!     PURPOSE
!!     --------
!!
!!     Initialize Xios context and declare Trip domain to XIOS
!!
!!
!!     METHOD
!!     ------
!!
!!
!!     EXTERNAL
!!     --------
!!
!!     XIOS library
!!
!!     REFERENCE
!!     ---------
!!
!!     Y.Meurdesoif, 2015 : XIOS ....
!!
!!
!!     AUTHOR
!!     ------
!!
!!     S.Senesi - CNRM
!!
!!     MODIFICATION
!!     --------------
!!
!!     Original    10/2016
!!     S.Sénési    08/11/16 : interface to XIOS
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TRIP_PAR, ONLY : XUNDEF
USE MODN_TRIP_RUN, ONLY : CMODEL_NAME, XTSTEP_DIAG
USE MODD_TRIP_GRID, ONLY : TRIP_GRID_t
!
USE MODI_GET_LONLAT_TRIP
!
#ifdef WXIOS
USE XIOS
#endif
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(TRIP_GRID_t), INTENT(INOUT)  :: TPG
INTEGER, INTENT(IN)               :: KLOCAL_COMM ! value of local communicator
INTEGER, INTENT(IN)               :: KLON        ! Number of longitude
INTEGER, INTENT(IN)               :: KLAT        ! Number of latittude
INTEGER, INTENT(IN)               :: KYEAR   !date UTC
INTEGER, INTENT(IN)               :: KMONTH  !date UTC
INTEGER, INTENT(IN)               :: KDAY    !date UTC
REAL   , INTENT(IN)               :: PTIME   ! current time (s)
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(:),ALLOCATABLE    :: ZLON
REAL, DIMENSION(:),ALLOCATABLE    :: ZLAT
!
#ifdef WXIOS
INTEGER              :: IREFYEAR
TYPE(XIOS_DURATION)  :: DTIME
TYPE(XIOS_DATE)      :: TDATE
#endif
INTEGER              :: NHOURS,NMINUTES,NSECONDS
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_XIOS_INIT',0,ZHOOK_HANDLE)
!
#ifdef WXIOS
!
CALL XIOS_CONTEXT_INITIALIZE(CMODEL_NAME,KLOCAL_COMM)
CALL XIOS_SET_CURRENT_CONTEXT(CMODEL_NAME)
!
NHOURS=INT(PTIME/3600)
NMINUTES=INT((PTIME - NHOURS*3600)/60)
NSECONDS=INT(PTIME - NHOURS*3600 -NMINUTES*60)
IF (.NOT.(XIOS_GETVAR('ref_year',IREFYEAR))) IREFYEAR=1850
CALL XIOS_DEFINE_CALENDAR("Gregorian", &
     start_date=xios_date(KYEAR,KMONTH,KDAY,NHOURS,NMINUTES,NSECONDS), &
     time_origin=xios_date(IREFYEAR,1,1,0,0,0))
!
DTIME%SECOND=XTSTEP_DIAG
!
CALL XIOS_SET_TIMESTEP(DTIME)
!
CALL XIOS_SET_DOMAIN_ATTR("trip_grid",DATA_DIM=2,TYPE="rectilinear")
CALL XIOS_SET_DOMAIN_ATTR("trip_grid",NI_GLO=KLON,NI=KLON,IBEGIN=0)
CALL XIOS_SET_DOMAIN_ATTR("trip_grid",NJ_GLO=KLAT,NJ=KLAT,JBEGIN=0)
!
ALLOCATE(ZLON(KLON),ZLAT(KLAT))
ZLON(:)=XUNDEF ; ZLAT(:)=XUNDEF
CALL GET_LONLAT_TRIP(TPG, KLON,KLAT,ZLON,ZLAT)
CALL XIOS_SET_DOMAIN_ATTR("trip_grid",LONVALUE_1D=ZLON,LATVALUE_1D=ZLAT)
DEALLOCATE(ZLON,ZLAT)
!
CALL XIOS_CLOSE_CONTEXT_DEFINITION()
!
#endif
!
IF (LHOOK) CALL DR_HOOK('TRIP_XIOS_INIT',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_XIOS_INIT
