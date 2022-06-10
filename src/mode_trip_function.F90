!########################
MODULE MODE_TRIP_FUNCTION
!########################
!
!!****  *MODE_TRIP_FUNCTION*
!!
!!    PURPOSE
!!    -------
!    
!      The purpose of this routine is to store here all functions 
!      used by MODE_TRIP_INIT.
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
!
!-------------------------------------------------------------------------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
 CONTAINS
!-------------------------------------------------------------------------------
!
!     ###############################################
      FUNCTION IRNXTX(IX,NX,IRIV) RESULT(KNEXTX)
!     ###############################################
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)  :: IX,NX,IRIV 
INTEGER              :: KNEXTX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:IRNXTX',0,ZHOOK_HANDLE)
IF(IRIV==1.OR.IRIV==5)THEN
  KNEXTX = IX 
ELSEIF(IRIV==8.OR.IRIV==7.OR.IRIV==6)THEN
  IF(IX==1)THEN
    KNEXTX = NX
  ELSE
    KNEXTX = IX-1
  ENDIF
ELSEIF(IRIV==2.OR.IRIV==3.OR.IRIV==4)THEN
  IF(IX==NX)THEN
    KNEXTX = 1
  ELSE
    KNEXTX = IX+1
  ENDIF
ELSE
    KNEXTX = 0
ENDIF
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:IRNXTX',1,ZHOOK_HANDLE)
!
END FUNCTION IRNXTX
!
!-------------------------------------------------------------------------------
!
!     ###############################################
      FUNCTION IRNXTY(IY,NY,IRIV) RESULT(KNEXTY)
!     ###############################################
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)  :: IY,NY,IRIV 
INTEGER              :: KNEXTY
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:IRNXTY',0,ZHOOK_HANDLE)
IF(IRIV==7.OR.IRIV==3)THEN
  KNEXTY = IY 
ELSEIF(IRIV==6.OR.IRIV==5.OR.IRIV==4)THEN
  KNEXTY = IY-1
ELSEIF(IRIV==8.OR.IRIV==1.OR.IRIV==2)THEN
  IF(IY==NY)THEN
    KNEXTY = 0
  ELSE
    KNEXTY = IY+1
  ENDIF
ELSE
  KNEXTY = 0
ENDIF
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:IRNXTY',1,ZHOOK_HANDLE)
!
END FUNCTION IRNXTY
!
!-------------------------------------------------------------------------------
!
!     ###############################################
      FUNCTION GETLON(IX,NX) RESULT(PLON0)
!     ###############################################
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)  :: IX,NX 
REAL                 :: PLON0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GETLON',0,ZHOOK_HANDLE)
PLON0 = 360.0 * (REAL(IX)-0.5) / REAL(NX) - 180.0
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GETLON',1,ZHOOK_HANDLE)
!
END FUNCTION GETLON
!
!-------------------------------------------------------------------------------
!
!     ###############################################
      FUNCTION GETLAT(IY,NY) RESULT(PLAT0)
!     ###############################################
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)  :: IY,NY 
REAL                 :: PLAT0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GETLAT',0,ZHOOK_HANDLE)
PLAT0 = 180.0 * (REAL(IY)-0.5) / REAL(NY) - 90.0
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GETLAT',1,ZHOOK_HANDLE)
!
END FUNCTION GETLAT
!
!-------------------------------------------------------------------------------
!
!     ###############################################
      FUNCTION GIVELON(ZY) RESULT(PDLON)
!     ###############################################
!
USE MODD_TRIP_PAR, ONLY : XPI, XRAD
!
IMPLICIT NONE
!
REAL, INTENT(IN)  :: ZY
REAL              :: PDLON
!
REAL, PARAMETER   :: ZE2 = 0.006694470
REAL :: ZR, ZY_RAD, ZRA
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GIVELON',0,ZHOOK_HANDLE)
ZRA = XRAD/1000.0
!
ZY_RAD = ZY * XPI / 180.
!
PDLON = XPI / 180.0 * ZRA * COS(ZY_RAD) / SQRT(1.0 - ZE2 * SIN(ZY_RAD) * SIN(ZY_RAD))
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GIVELON',1,ZHOOK_HANDLE)
!
END FUNCTION GIVELON
!
!-------------------------------------------------------------------------------
!
!     ###############################################
      FUNCTION GIVELAT(ZY) RESULT(PDLAT)
!     ###############################################
!
USE MODD_TRIP_PAR, ONLY : XPI, XRAD
!
IMPLICIT NONE
!
REAL, INTENT(IN)  :: ZY
REAL              :: PDLAT
!
REAL, PARAMETER   :: ZE2 = 0.006694470
REAL :: ZR, ZY_RAD, ZRA
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GIVELAT',0,ZHOOK_HANDLE)
ZRA = XRAD/1000.0
!
ZY_RAD = ZY * XPI / 180.
!
PDLAT = XPI / 180.0 * ZRA * (1.0-ZE2) / SQRT( (1.0 - ZE2 * SIN(ZY_RAD) * SIN(ZY_RAD))**3 )
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GIVELAT',1,ZHOOK_HANDLE)
!
END FUNCTION GIVELAT
!
!-------------------------------------------------------------------------------
!
!     ###############################################
      FUNCTION GIVERAD(ZY) RESULT(PRAD)
!     ###############################################
!
USE MODD_TRIP_PAR, ONLY : XRAD, XPI
!
IMPLICIT NONE
!
REAL, INTENT(IN)  :: ZY
REAL              :: PRAD
!
REAL, PARAMETER   :: ZE2 = 0.006694470
REAL :: ZR, ZY_RAD, ZRN, ZRA
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GIVERAD',0,ZHOOK_HANDLE)
ZRA = XRAD/1000.0
!
ZY_RAD = ZY * XPI / 180.
!
ZRN = ZRA / SQRT(1.0 - ZE2 *  SIN(ZY_RAD) * SIN(ZY_RAD) )
!
PRAD = ZRN * SQRT( 1.0 - ZE2 * SIN(ZY_RAD) + ZE2 * ZE2 * SIN(ZY_RAD) * SIN(ZY_RAD) )
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GIVERAD',1,ZHOOK_HANDLE)
!
END FUNCTION GIVERAD
!
!-------------------------------------------------------------------------------
!
!     ###############################################
      FUNCTION GIVELEN(ZX,ZY,ZX_N,ZY_N) RESULT(PLEN0)
!     ###############################################
!
IMPLICIT NONE
!
REAL, INTENT(IN)  :: ZX,ZY,ZX_N,ZY_N
REAL              :: PLEN0
!
REAL :: ZLAT, ZDX, ZDY, ZRAD, ZDLON, ZDLAT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GIVELEN',0,ZHOOK_HANDLE)
ZDLON = ABS(ZX-ZX_N)
ZDLAT = ABS(ZY-ZY_N)
!
IF(ZDLON>=180.0)ZDLON = ABS(360.0 - ZDLON)
!
PLEN0 = 0.0
!
IF(ZX==ZX_N)THEN
  ZLAT  = (ZY+ZY_N) / 2.0
  PLEN0 = GIVELAT(ZLAT) * ZDLAT
ELSEIF(ZY==ZY_N)THEN
  ZLAT  = ZY
  PLEN0 = GIVELON(ZLAT) * ZDLON
ELSE
  ZLAT  = (ZY+ZY_N) / 2.0
  ZRAD  = GIVERAD(ZLAT)
  ZDX   = GIVELON(ZLAT) * ZDLON / ZRAD
  ZDY   = GIVELAT(ZLAT) * ZDLAT / ZRAD
  PLEN0 = ACOS(COS(ZDX)*COS(ZDY)) * ZRAD
ENDIF
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GIVELEN',1,ZHOOK_HANDLE)
!
END FUNCTION GIVELEN
!
!-------------------------------------------------------------------------------
!
END MODULE MODE_TRIP_FUNCTION   
