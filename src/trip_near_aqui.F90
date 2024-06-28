!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!     #########
      SUBROUTINE TRIP_NEAR_AQUI(TP, TPG,   &
                              KLON, KLAT )
!     #########################################################
!
!!**** *TRIP_NEAR_AQUI*
!!
!!    PURPOSE
!!    -------
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
!!    S. Munier            Meteo-France
!!
!!    Original    10/2016
!!    Modification
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_TRIP,       ONLY : TRIP_t
USE MODD_TRIP_GRID,  ONLY : TRIP_GRID_t
!
USE MODD_TRIP_PAR,   ONLY : XUNDEF
!
USE MODE_TRIP_GRID
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
TYPE(TRIP_t),      INTENT(INOUT) :: TP
TYPE(TRIP_GRID_t), INTENT(INOUT) :: TPG
!
INTEGER,           INTENT(IN)    :: KLON
INTEGER,           INTENT(IN)    :: KLAT
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL,   DIMENSION(KLON)      :: ZLON
REAL,   DIMENSION(KLAT)      :: ZLAT
INTEGER,DIMENSION(KLON*KLAT) :: ICODE
REAL,   DIMENSION(KLON*KLAT) :: ZNEAR
REAL,   DIMENSION(KLON*KLAT) :: ZX
REAL,   DIMENSION(KLON*KLAT) :: ZY
!
INTEGER :: JLON, JLAT, IWORK
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_NEAR_AQUI',0,ZHOOK_HANDLE)
!
CALL GET_TRIP_GRID(TPG%XTRIP_GRID,PLON=ZLON,PLAT=ZLAT)
!
TP%XNEAR_AQUI(:,:)=XUNDEF
ZNEAR(:)=XUNDEF
ICODE(:)=-1
!
IWORK=0
DO JLAT=1,KLAT
  DO JLON=1,KLON 
    IF(TPG%GMASK(JLON,JLAT))THEN
      IWORK=IWORK+1
      ICODE(IWORK)=0
      ZX   (IWORK)=ZLON(JLON)
      ZY   (IWORK)=ZLAT(JLAT)
      ZNEAR(IWORK)=TP%XNUM_AQUI(JLON,JLAT)
      IF(TPG%GMASK_GW(JLON,JLAT)) ICODE(IWORK)=1
    ENDIF
  ENDDO
ENDDO
!
CALL TRIP_NEAREST(IWORK,ICODE(1:IWORK),ZX(1:IWORK),ZY(1:IWORK),ZNEAR(1:IWORK))
!
IWORK=0
DO JLAT=1,KLAT
  DO JLON=1,KLON
      IF(TPG%GMASK(JLON,JLAT))THEN
        IWORK=IWORK+1
        TP%XNEAR_AQUI(JLON,JLAT)=ZNEAR(IWORK)
      ENDIF
  ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('TRIP_NEAR_AQUI',1,ZHOOK_HANDLE)
!
CONTAINS
!
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
INTEGER,DIMENSION(KNI),  INTENT(IN)     :: KCODE    ! code for each point
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
END SUBROUTINE TRIP_NEAREST
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE TRIP_NEAR_AQUI
