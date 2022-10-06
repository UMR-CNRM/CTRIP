!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!     #########
SUBROUTINE TRIP_HS_VEL (TPST, &
                        PTSTEP,PHS,PVEL)
!     ################################################################
!
!!****  *TRIP_HS_VEL*
!!
!!    PURPOSE
!!    -------
!
!     Calculate the river height and velocity
!     Where OMASK_VEL=true the Manning equation is used
!
!
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!
!     None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/02/09
!!      S. Munier   28/03/2020 CTRIP-12D and parallelization
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_STATE, ONLY : TRIP_STATE_t
!
USE MODN_TRIP,     ONLY : CVIT, XCVEL, LAPPROXVEL, XBANKSLOPE
USE MODD_TRIP_PAR, ONLY : XUNDEF, XM, XVELMIN, &
                          XHSMIN, XRHOLW
USE MODE_POWER_2THIRD
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABORT_TRIP
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(TRIP_STATE_t), INTENT(INOUT) :: TPST
!
REAL,               INTENT(IN)    :: PTSTEP ! Trip timestep value (10800s)
!
REAL, DIMENSION(:), INTENT(OUT)   :: PHS    ! river channel height [m]
REAL, DIMENSION(:), INTENT(OUT)   :: PVEL   ! River channel velocity  [m/s]
!
!*      0.2    declarations of local variables
!
REAL, PARAMETER :: ZLOG_MIN = 1.E-12
!
REAL :: ZRADIUS, ZHS, ZVEL
REAL :: ZWETAREA, ZWETPERIMETER, ZBOTWIDTH
!
INTEGER :: JSTATE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! * Init
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_HS_VEL',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
! * River channel velocity
!-------------------------------------------------------------------------------
!
!Constant streamflow velocity
!
IF(CVIT == 'VAR')THEN
  DO JSTATE = 1,TPST%NSTATE_LEN_P
    IF(TPST%GMASK_VEL(JSTATE))THEN
      !Variable streamflow velocity
      IF(XBANKSLOPE==0)THEN
        PHS(JSTATE) = TPST%XSURF_STO(JSTATE)/(XRHOLW*TPST%XLEN(JSTATE)*TPST%XWIDTH(JSTATE))
        ZHS = MAX(PHS(JSTATE),ZLOG_MIN)
        ZRADIUS = TPST%XWIDTH(JSTATE)*ZHS/(TPST%XWIDTH(JSTATE)+2.0*ZHS)
      ELSE
        ZWETAREA = TPST%XSURF_STO(JSTATE)/(XRHOLW*TPST%XLEN(JSTATE))
        ZWETAREA = MAX(ZWETAREA,0.0)
        ZBOTWIDTH = TPST%XWIDTH(JSTATE)-XBANKSLOPE*TPST%XHC_BED(JSTATE)
        ZBOTWIDTH = MAX(ZBOTWIDTH,0.0)
        PHS(JSTATE) = (SQRT(ZBOTWIDTH*ZBOTWIDTH+4.*XBANKSLOPE*ZWETAREA)-ZBOTWIDTH)/(2.*XBANKSLOPE)
        ZHS = MAX(PHS(JSTATE),ZLOG_MIN)
        ZWETPERIMETER = ZBOTWIDTH+2*ZHS*SQRT(1.+XBANKSLOPE*XBANKSLOPE)
        ZRADIUS = (ZBOTWIDTH+XBANKSLOPE*ZHS)*ZHS/ZWETPERIMETER
      ENDIF
      !
      IF(LAPPROXVEL)THEN
        ! Approximate computation of power 2/3
        CALL POWER_2THIRD(ZRADIUS,ZVEL)
      ELSE
        ! Exact computation of power 2/3
        ZVEL = EXP(XM*LOG(ZRADIUS))
      ENDIF
      !
      ZVEL = ZVEL*SQRT(TPST%XSLOPEBED(JSTATE))/TPST%XN(JSTATE)
      ZVEL = MAX(XVELMIN,ZVEL)
      ZVEL = MIN(ZVEL,TPST%XLEN(JSTATE)/PTSTEP)
      !Velocity limitation if the river is very dry
      PVEL(JSTATE) = ZVEL*MIN(1.0,MAX(0.0,(PHS(JSTATE)-XHSMIN))/XHSMIN)
    ELSE
      PHS (JSTATE) = XUNDEF
      PVEL(JSTATE) = XUNDEF
    ENDIF
  ENDDO
ELSE
  PHS (:) = XUNDEF
  PVEL(:) = XCVEL
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP_HS_VEL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE TRIP_HS_VEL
