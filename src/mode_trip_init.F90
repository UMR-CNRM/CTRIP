!###################
MODULE MODE_TRIP_INIT
!###################
!
!!****  *MODE_TRIP_INIT*
!!
!!    PURPOSE
!!    -------
!    
!      The purpose of this routine is to store here all routines 
!      used by INIT_TRIP.
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!       NONE          
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      B. Decharme       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    15/04/08
!--------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_TRIP_FUNCTION
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!-------------------------------------------------------------------------------
!
 CONTAINS
!-------------------------------------------------------------------------------
!
!     #############################################################
      SUBROUTINE SETNEXT(KLON,KLAT,KGRCN,KNEXTX,KNEXTY,GMLON,GMLAT)
!     #############################################################
!
!!    PURPOSE
!!    -------
!    
!     set the destination grid point
!
!     (i, j) ===>  (inextx(i,j), inexty(i,j))
!     at river mouth : pointing itself
!     at sea         : 0
!
IMPLICIT NONE
!
!*      declarations of arguments
!
INTEGER, INTENT(IN)                    :: KLON, KLAT
!
INTEGER, DIMENSION(:,:), INTENT(INOUT) :: KGRCN
!
INTEGER, DIMENSION(:,:), INTENT(OUT)   :: KNEXTX, KNEXTY
!
LOGICAL, INTENT(IN), OPTIONAL          :: GMLON,GMLAT
!
!*      declarations of local variables
!
INTEGER :: JLON, JLAT, IDIR
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      procedure
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_INIT:SETNEXT',0,ZHOOK_HANDLE)
!
IF(PRESENT(GMLON))THEN
  IF(GMLON)THEN
    DO JLAT=1,KLAT
       IF(KGRCN(KLON,JLAT)==2..OR.KGRCN(KLON,JLAT)==3.OR.KGRCN(KLON,JLAT)==4.)KGRCN(KLON,JLAT)=15
       IF(KGRCN(   1,JLAT)==6..OR.KGRCN(   1,JLAT)==7.OR.KGRCN(   1,JLAT)==8.)KGRCN(   1,JLAT)=15
    ENDDO
  ENDIF
ENDIF
!
IF(PRESENT(GMLAT))THEN
  IF(GMLAT)THEN
    DO JLON=1,KLON
       IF(KGRCN(JLON,   1)==4..OR.KGRCN(JLON,   1)==5.OR.KGRCN(JLON,   1)==6.)KGRCN(JLON,   1)=15
       IF(KGRCN(JLON,KLAT)==1..OR.KGRCN(JLON,KLAT)==2.OR.KGRCN(JLON,KLAT)==8.)KGRCN(JLON,KLAT)=15
    ENDDO
  ENDIF
ENDIF
!
DO JLAT=1,KLAT
   DO JLON=1,KLON
!
      IDIR=KGRCN(JLON,JLAT)
!
      IF((IDIR>=1).AND.(IDIR<=8))THEN              
        KNEXTX(JLON,JLAT)=IRNXTX(JLON,KLON,IDIR)
        KNEXTY(JLON,JLAT)=IRNXTY(JLAT,KLAT,IDIR)        
      ELSEIF(IDIR>=9)THEN     
        KNEXTX(JLON,JLAT)=JLON
        KNEXTY(JLON,JLAT)=JLAT     
      ELSE             
        KNEXTX(JLON,JLAT)=0
        KNEXTY(JLON,JLAT)=0  
      ENDIF
!
      IF(IDIR>0)THEN
        IF (KGRCN(KNEXTX(JLON,JLAT),KNEXTY(JLON,JLAT))==0) KGRCN(JLON,JLAT)=9
      ENDIF
!
   ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_INIT:SETNEXT',1,ZHOOK_HANDLE)
!
END SUBROUTINE SETNEXT
!
!-------------------------------------------------------------------------------
!
!     #################################################
      SUBROUTINE SETAREA(KLAT,PLATMIN,PRES,PAREA)
!     #################################################
!
!!    PURPOSE
!!    -------
!    
!     set area [m²] of each grid box
!
USE MODD_TRIP_PAR, ONLY : XPI, XRAD
!
IMPLICIT NONE
!
!*      declarations of arguments
!
INTEGER, INTENT(IN)                :: KLAT
REAL, INTENT(IN)                   :: PRES
REAL, INTENT(IN)                   :: PLATMIN
!
REAL, DIMENSION(:,:), INTENT(OUT)  :: PAREA
!
!*      declarations of local variables
!
REAL    :: ZDLAT, ZDLON, ZLAT
!
INTEGER :: JI, JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      procedure
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_INIT:SETAREA',0,ZHOOK_HANDLE)
ZDLON=PRES
ZDLAT=PRES
!
ZLAT=PLATMIN-(PRES/2.)
!
DO JI=1,KLAT
   ZLAT=ZLAT+PRES
   DO JJ=1,SIZE(PAREA,1)
      PAREA(JJ,JI) = XRAD * XRAD * XPI/180.*(ZDLON)              &
          * (SIN((ZLAT+ZDLAT/2.)*XPI/180.)-SIN((ZLAT-ZDLAT/2.)*XPI/180.))  
   ENDDO
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_INIT:SETAREA',1,ZHOOK_HANDLE)
!
END SUBROUTINE SETAREA
!
!-------------------------------------------------------------------------------
!
!     #############################################################
      SUBROUTINE SETLEN(KLON,KLAT,KGRCN,KNEXTX,KNEXTY,PRATMED,PLEN)
!     #############################################################
!
!!    PURPOSE
!!    -------
!    
!     length from (i, j) to the destination in [m]
!     river mouth : distance to 1 grid north
!     sea         : 0.0
!
IMPLICIT NONE
!
!*      declarations of arguments
!
INTEGER, INTENT(IN)                 :: KLON, KLAT
REAL, INTENT(IN)                    :: PRATMED
!
INTEGER, DIMENSION(:,:), INTENT(IN) :: KGRCN
!
INTEGER, DIMENSION(:,:), INTENT(IN) :: KNEXTX, KNEXTY
!
REAL, DIMENSION(:,:), INTENT(OUT)   :: PLEN
!
!*      declarations of local variables
!
REAL    :: ZLON, ZLAT, ZLON_N, ZLAT_N
!
INTEGER :: JLON, JLAT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      procedure
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_INIT:SETLEN',0,ZHOOK_HANDLE)
!
ZLON=0.0
ZLAT=0.0
!
DO JLAT=1,KLAT
!
   ZLAT=GETLAT(KLAT-JLAT+1,KLAT)
!
   DO JLON=1,KLON
!
      ZLON=GETLON(JLON,KLON)
!
      IF(KGRCN(JLON,JLAT)>=1.AND.KGRCN(JLON,JLAT)<=8)THEN
        ZLON_N = GETLON(KNEXTX(JLON,JLAT),KLON)
        ZLAT_N = GETLAT(KLAT-KNEXTY(JLON,JLAT)+1,KLAT)
        PLEN(JLON,JLAT) = GIVELEN(ZLON,ZLAT,ZLON_N,ZLAT_N) * 1000.0
      ELSEIF(KGRCN(JLON,JLAT)>=9)THEN
        ZLAT_N = GETLAT(KLAT-(JLAT-1)+1,KLAT)
        PLEN(JLON,JLAT) = GIVELEN(ZLON,ZLAT,ZLON,ZLAT_N) * 1000.0
      ELSE
        PLEN(JLON,JLAT) = 0.0
      ENDIF
!
   ENDDO
!   
ENDDO
!
PLEN=PLEN*PRATMED
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_INIT:SETLEN',1,ZHOOK_HANDLE)
!
END SUBROUTINE SETLEN
!
!-------------------------------------------------------------------------------
!
END MODULE MODE_TRIP_INIT      
