!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!     #########
SUBROUTINE TRIP_UPDATE_AND_CONSERV (TPST, &
                                    OPRINT,PSURF_STO2,PFLOOD_STO2,PGROUND_STO2,PRECUP_ALL)
!     ################################################################
!
!!****  *TRIP_UPDATE_AND_CONSERV*
!!
!!    PURPOSE
!!    -------
!
! Update all reservoir and conserve water mass as possible
!
!!**  METHOD
!!    ------
!
!     Direct calculation
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
!!      Original    01/12/13
!!      S. Munier   28/03/2020 CTRIP-12D and parallelization
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_STATE, ONLY : TRIP_STATE_t
!
USE MODN_TRIP,     ONLY : LFLOOD, CGROUNDW
USE MODD_TRIP_PAR, ONLY : XRHOLW
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(TRIP_STATE_t),    INTENT(INOUT) :: TPST
!
LOGICAL,               INTENT(IN)    :: OPRINT       ! Printable budget key
REAL,    DIMENSION(:), INTENT(IN)    :: PSURF_STO2   ! river channel storage           [kg]
REAL,    DIMENSION(:), INTENT(IN)    :: PFLOOD_STO2  ! Floodplain water storage        [kg]
REAL,    DIMENSION(:), INTENT(IN)    :: PGROUND_STO2 ! groundwater storage             [kg]
!
REAL,                  INTENT(OUT)   :: PRECUP_ALL   ! Global none conserved water mass[kg/m2]
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSURF_STO2,1)) :: ZRECUP_FLD  ! ensure water conservation        [kg]
REAL, DIMENSION(SIZE(PSURF_STO2,1)) :: ZRECUP_SURF ! ensure water conservation        [kg]
REAL, DIMENSION(SIZE(PSURF_STO2,1)) :: ZRECUP_FINAL! ensure water conservation        [kg]
!
INTEGER :: JSTATE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_UPDATE_AND_CONSERV',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
! * Update and conserve water mass
!-------------------------------------------------------------------------------
!
! Surface water conservation
!
IF(LFLOOD)THEN
!
  ZRECUP_FLD     (:) = MIN(0.0,PFLOOD_STO2(:))
  TPST%XFLOOD_STO(:) = MAX(0.0,PFLOOD_STO2(:))
  ZRECUP_SURF    (:) = MIN(0.0,PSURF_STO2(:)+ZRECUP_FLD(:))
  TPST%XSURF_STO (:) = MAX(0.0,PSURF_STO2(:)+ZRECUP_FLD(:))
!
ELSE
!
  IF(CGROUNDW/='DEF')THEN
    ZRECUP_SURF   (:) = MIN(0.0,PSURF_STO2(:))
    TPST%XSURF_STO(:) = MAX(0.0,PSURF_STO2(:))
  ELSE
    ZRECUP_FINAL  (:) = MIN(0.0,PSURF_STO2(:))
    TPST%XSURF_STO(:) = MAX(0.0,PSURF_STO2(:))
  ENDIF
!
ENDIF
!
! Groundwater conservation
!
IF(CGROUNDW=='CST')THEN
!
  WHERE(TPST%GMASK_GW(:))
    ZRECUP_FINAL    (:) = MIN(0.0,PGROUND_STO2(:)+ZRECUP_SURF(:))
    TPST%XGROUND_STO(:) = MAX(0.0,PGROUND_STO2(:)+ZRECUP_SURF(:))
  ELSEWHERE
    ZRECUP_FINAL    (:) = ZRECUP_SURF(:)
    TPST%XGROUND_STO(:) = 0.0
  ENDWHERE
!
ELSEIF(CGROUNDW=='DIF')THEN
!
  WHERE(TPST%GMASK_GW(:).AND.ZRECUP_SURF(:)<0.0)
    ZRECUP_FINAL (:) = 0.0
    TPST%XHGROUND(:) = TPST%XHGROUND(:)+ZRECUP_SURF(:)/(TPST%XWEFF(:)*TPST%XAREA(:)*XRHOLW)
  ELSEWHERE
    ZRECUP_FINAL (:) = ZRECUP_SURF(:)
  ENDWHERE
!
ENDIF
!
!-------------------------------------------------------------------------------
! * Global unconserved water mass calculation
!-------------------------------------------------------------------------------
!
IF(OPRINT)THEN
!
  PRECUP_ALL = 0.0
!
  DO JSTATE=1,SIZE(PSURF_STO2,1)
    PRECUP_ALL = PRECUP_ALL + ZRECUP_FINAL(JSTATE)/TPST%XAREA(JSTATE)
  ENDDO
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP_UPDATE_AND_CONSERV',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_UPDATE_AND_CONSERV
