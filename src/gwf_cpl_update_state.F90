SUBROUTINE GWF_CPL_UPDATE_STATE(TPST, &
                                PHG_OLD,PWTD,PFWTD   )
!     ##########################################################################
!
!!****  *GWF_CPL_UPDATE_STATE*
!!
!!    PURPOSE
!!    -------
!
!     update groundwater diag
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
!!      Original    01/11/06
!!      S. Munier   28/03/2020 CTRIP-12D and parallelization
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_STATE, ONLY : TRIP_STATE_t
!
USE MODN_TRIP, ONLY : LGWSUBF, XGWSUBD
!
USE MODI_ABORT_TRIP
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(TRIP_STATE_t),      INTENT(INOUT) :: TPST
!
REAL,    DIMENSION(:),   INTENT(INOUT) :: PHG_OLD
REAL,    DIMENSION(:),   INTENT(OUT)   :: PWTD
REAL,    DIMENSION(:),   INTENT(OUT)   :: PFWTD
!
!*      0.2    declarations of local variables
!
INTEGER, DIMENSION(TPST%NNSTATE_P) :: ISUP
INTEGER, DIMENSION(TPST%NNSTATE_P) :: IINF
LOGICAL, DIMENSION(TPST%NNSTATE_P) :: LMASK
REAL,    DIMENSION(TPST%NNSTATE_P) :: ZSLOPE
REAL,    DIMENSION(TPST%NNSTATE_P) :: ZHGROUND
!
INTEGER         :: JSTATE, JFRAC, INFRAC
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('GWF_CPL_UPDATE_STATE',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
! * Init
!-------------------------------------------------------------------------------
!
INFRAC = SIZE(TPST%XTABGW_H,2)
!
ZHGROUND(:) = 0.0
ZSLOPE  (:) = 0.0

LMASK(:) = (TPST%GMASK_GW(:).AND.TPST%XHGROUND(:)/=PHG_OLD(:))
!
!-------------------------------------------------------------------------------
! * Evolution of water table depth
!-------------------------------------------------------------------------------
!
WHERE(LMASK(:))
  PWTD (:) = TPST%XHGROUND(:)-TPST%XTOPO_RIV(:)
ENDWHERE
!
!-------------------------------------------------------------------------------
! * Evolution of the fraction of water table to rise
!-------------------------------------------------------------------------------
!
IF(.NOT.LGWSUBF)THEN
!
! No sub-grid fraction
!
  WHERE(TPST%GMASK_GW(:))
    PFWTD(:) = 1.0
  ENDWHERE
!
ELSE
!
! Adjust depth before computed sub-grid fraction
!
  WHERE(LMASK(:))
    ZSLOPE  (:) = MIN(1.0,MAX(0.0,TPST%XHGROUND(:)-(TPST%XTOPO_RIV(:)-TPST%XHC_BED(:)))/TPST%XHC_BED(:))
    ZHGROUND(:) = TPST%XHGROUND(:) + ZSLOPE(:)*XGWSUBD
  ENDWHERE
!
! Compute sub-grid fraction as in Verges et al., JGR, 2014
!
  WHERE(LMASK(:).AND.ZHGROUND(:)<=TPST%XTABGW_H(:,1))
    PFWTD(:) = MIN(1.0,TPST%XTABGW_F(:,1))
    LMASK(:) = .FALSE.
  ELSEWHERE(LMASK(:).AND.ZHGROUND(:)>=TPST%XTABGW_H(:,INFRAC))
    PFWTD(:) = MIN(1.0,TPST%XTABGW_F(:,INFRAC))
    LMASK(:) = .FALSE.
  ENDWHERE
!
  ISUP (:)=0
  IINF (:)=0
!
  DO JSTATE = 1,TPST%NSTATE_LEN_P
    IF(LMASK(JSTATE))THEN
      DO JFRAC = 1,INFRAC-1
        IF(ZHGROUND(JSTATE)>=TPST%XTABGW_H(JSTATE,JFRAC))THEN
          ISUP(JSTATE)=JFRAC+1
          IINF(JSTATE)=JFRAC
        ENDIF
      ENDDO
      IF(IINF(JSTATE)==0.or.ISUP(JSTATE)==0)then
        WRITE(6,*)'IINF,ISUP,JSTATE',IINF(JSTATE),ISUP(JSTATE),JSTATE
        CALL FLUSH(6)
        WRITE(6,*)'JFRAC   PHGROUND   ZHGROUND   PTABGW_H(JFRAC)'
        WRITE(6,*)JFRAC,TPST%XHGROUND(JSTATE),ZHGROUND(JSTATE),TPST%XTABGW_H(JSTATE,JFRAC)
        CALL FLUSH(6)
        CALL ABORT_TRIP('GWF_CPL_UPDATE_STATE:Problem with IINF or ISUP')
      ENDIF
    ENDIF
  ENDDO
!
  DO JSTATE = 1,TPST%NSTATE_LEN_P
    IF(LMASK(JSTATE))THEN
      PFWTD(JSTATE) = TPST%XTABGW_F(JSTATE,IINF(JSTATE))                                   &
                      + (ZHGROUND(JSTATE             )-TPST%XTABGW_H(JSTATE,IINF(JSTATE))) &
                      * (TPST%XTABGW_F(JSTATE,ISUP(JSTATE))-TPST%XTABGW_F(JSTATE,IINF(JSTATE))) &
                      / (TPST%XTABGW_H(JSTATE,ISUP(JSTATE))-TPST%XTABGW_H(JSTATE,IINF(JSTATE)))
      PFWTD(JSTATE) = MIN(1.0,PFWTD(JSTATE))
    ENDIF
  ENDDO
!
ENDIF
!
!-------------------------------------------------------------------------------
! * Update the old groundwater height
!-------------------------------------------------------------------------------
!
PHG_OLD(:)=TPST%XHGROUND(:)
!
IF (LHOOK) CALL DR_HOOK('GWF_CPL_UPDATE_STATE',1,ZHOOK_HANDLE)
!
END SUBROUTINE GWF_CPL_UPDATE_STATE

