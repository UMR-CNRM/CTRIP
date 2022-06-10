!     #########
      SUBROUTINE TRIP_NEAREST(KNI,KCODE,PLON,PLAT,PFIELD)
!     #########################################################
!
!!**** *TRIP_NEAREST*
!!
!!    PURPOSE
!!    -------
!!
!!    Approximation : Pythagora theorem is used on an  
!!                    equirectangular projection for 
!!                    computing performance.
!!
!!    The points are all on only one grid (defined with the coordinates
!!    of all the points). The code to apply for each point is:
!!
!!    KCODE>0 : data point (with field valid for interpolation)
!!    KCODE=-1: point to ignore
!!    KCODE=0 : point to interpolate
!!
!!
!!
!!    METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    B. Decharme          Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    10/2016
!!    Modification
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_TRIP_PAR, ONLY : XUNDEF, XRAD, XPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                 INTENT(IN)     :: KNI      ! 
INTEGER,DIMENSION(KNI),  INTENT(INOUT)  :: KCODE    ! code for each point
                                                    ! >0 point used for interpolation
                                                    !  0 point to interpolate
                                                    ! -1 point not used
REAL,   DIMENSION(KNI),  INTENT(IN)      :: PLON      ! x of each grid mesh.
REAL,   DIMENSION(KNI),  INTENT(IN)      :: PLAT      ! y of each grid mesh.
REAL,   DIMENSION(KNI),  INTENT(INOUT)   :: PFIELD  ! pgd field on grid mesh.
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                               :: JD ! data point index
INTEGER                               :: JS ! loop counter on data points
INTEGER                               :: JL ! loop counter on points to initialize
!
REAL,   DIMENSION(KNI)                :: ZDIST   ! square distance between two interpolating and interpolated points
REAL,   DIMENSION(KNI)                :: ZNDIST  ! nearest square distances
REAL,   DIMENSION(KNI)                :: ZNVAL   ! corresponding field values
REAL,   DIMENSION(KNI)                :: ZLON
REAL,   DIMENSION(KNI)                :: ZLAT
REAL,   DIMENSION(KNI)                :: ZDLON
REAL                                  :: ZRAD
!
INTEGER                               :: ISCAN          ! number of points to scan
INTEGER, DIMENSION(KNI)               :: IINDEX       ! list of index to scan
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_NEAREST',0,ZHOOK_HANDLE)
!
ZRAD = XPI/180.
!
ZLON(:) = PLON(:)*ZRAD
ZLAT(:) = PLAT(:)*ZRAD
!
IINDEX(:) = 0
!
ISCAN = COUNT(KCODE(:)>0)
!
JS = 0
DO JD=1,KNI
  IF (KCODE(JD)>0) THEN
    JS = JS+1
    IINDEX(JS) = JD
  END IF
END DO
!
DO JL=1,KNI
  !
  IF (KCODE(JL)/=0) CYCLE
  !
  ZNDIST(JL) = XUNDEF
  ZNVAL (JL) = 0.0
  !
  DO JS=1,ISCAN
    !
    JD = IINDEX(JS)
    !
    ZDLON(JL)= ZLON(JD)-ZLON(JL) 
    IF(ZDLON(JL)>=XPI)ZDLON(JL) = ABS(2.0*XPI-ZDLON(JL))

    ZDIST(JL)= (ZDLON(JL)*COS(0.5*(ZLAT(JD)+ZLAT(JL))))**2 + (ZLAT(JD)-ZLAT(JL))**2    
    !
    IF ( ZDIST(JL)>ZNDIST(JL) ) CYCLE
    !
    ZNDIST(JL) = ZDIST (JL)
    ZNVAL (JL) = PFIELD(JD)
    !
  ENDDO
  !
  PFIELD(JL) = ZNVAL(JL)
  !
END DO
!
IF (LHOOK) CALL DR_HOOK('TRIP_NEAREST',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE TRIP_NEAREST
