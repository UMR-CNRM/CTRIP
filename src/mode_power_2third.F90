!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!     #########
MODULE MODE_POWER_2THIRD
!     ################################################################
!
USE MODD_TRIP_PAR, ONLY : XUNDEF, XM
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
INTEGER, PARAMETER :: NDIM = 100000
REAL,    PARAMETER :: XVALUE_MAX = 100.
REAL, DIMENSION(NDIM) :: XTAB_VALUE
REAL, DIMENSION(NDIM) :: XTAB_POWER
REAL :: XDV
!
CONTAINS
!
!-------------------------------------------------------------------------------
!
SUBROUTINE POWER_2THIRD_INIT()
!
!-------------------------------------------------------------------------------
!
INTEGER :: I
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('POWER_2THIRD_INIT',0,ZHOOK_HANDLE)
!
XDV = XVALUE_MAX/REAL(NDIM)
DO I = 1,NDIM
  XTAB_VALUE(I) = I*XDV
  XTAB_POWER(I) = EXP(XM*LOG(XTAB_VALUE(I)))
ENDDO
!
IF (LHOOK) CALL DR_HOOK('POWER_2THIRD_INIT',1,ZHOOK_HANDLE)
!
ENDSUBROUTINE POWER_2THIRD_INIT
!
!-------------------------------------------------------------------------------
!
SUBROUTINE POWER_2THIRD(PVALUE,PPOWER)
!
!-------------------------------------------------------------------------------
!
REAL, INTENT(IN)  :: PVALUE
REAL, INTENT(OUT) :: PPOWER
INTEGER :: ITAB
REAL :: ZFRAC
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('POWER_2THIRD',0,ZHOOK_HANDLE)
!
IF (PVALUE<=0) THEN
  PPOWER = XUNDEF
ELSEIF (PVALUE<XDV) THEN
  PPOWER = XTAB_POWER(1)
ELSEIF (PVALUE<XVALUE_MAX) THEN
  ITAB = INT(PVALUE/XDV)
  ZFRAC = PVALUE-XTAB_VALUE(ITAB)
  PPOWER = XTAB_POWER(ITAB)+ZFRAC*(XTAB_POWER(ITAB+1)-XTAB_POWER(ITAB)) / &
                                  (XTAB_VALUE(ITAB+1)-XTAB_VALUE(ITAB))
ELSE
  PPOWER = EXP(XM*LOG(PVALUE))
ENDIF
!
IF (LHOOK) CALL DR_HOOK('POWER_2THIRD',1,ZHOOK_HANDLE)
!
END SUBROUTINE POWER_2THIRD
!
!-------------------------------------------------------------------------------
!
END MODULE MODE_POWER_2THIRD
