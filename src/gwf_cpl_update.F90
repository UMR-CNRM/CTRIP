      SUBROUTINE GWF_CPL_UPDATE(PTABGW_H,PTABGW_F,OMASK_GW,PTOPO_RIV, &
                                PHC_BED,PHGROUND,PHG_OLD,PWTD,PFWTD   )
!     ##########################################################################
!
!!****  *GWF_CPL_UPDATE*  
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
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
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
REAL,    DIMENSION(:,:,:), INTENT(IN)    :: PTABGW_H
REAL,    DIMENSION(:,:,:), INTENT(IN)    :: PTABGW_F
LOGICAL, DIMENSION(:,:),   INTENT(IN)    :: OMASK_GW
REAL,    DIMENSION(:,:),   INTENT(IN)    :: PTOPO_RIV
REAL,    DIMENSION(:,:),   INTENT(IN)    :: PHC_BED
REAL,    DIMENSION(:,:),   INTENT(IN)    :: PHGROUND
!
REAL,    DIMENSION(:,:),   INTENT(INOUT) :: PHG_OLD
REAL,    DIMENSION(:,:),   INTENT(OUT)   :: PWTD
REAL,    DIMENSION(:,:),   INTENT(OUT)   :: PFWTD
!
!*      0.2    declarations of local variables
!
INTEGER, DIMENSION(SIZE(PTABGW_H,1),SIZE(PTABGW_H,2)) :: ISUP
INTEGER, DIMENSION(SIZE(PTABGW_H,1),SIZE(PTABGW_H,2)) :: IINF
LOGICAL, DIMENSION(SIZE(PTABGW_H,1),SIZE(PTABGW_H,2)) :: LMASK
REAL,    DIMENSION(SIZE(PTABGW_H,1),SIZE(PTABGW_H,2)) :: ZSLOPE
REAL,    DIMENSION(SIZE(PTABGW_H,1),SIZE(PTABGW_H,2)) :: ZHGROUND
!
INTEGER         :: ILON, ILAT, JLON, JLAT, JFRAC, INFRAC
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('GWF_CPL_UPDATE',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
! * Init
!-------------------------------------------------------------------------------
!
ILON   = SIZE(PTABGW_H,1)
ILAT   = SIZE(PTABGW_H,2)
INFRAC = SIZE(PTABGW_H,3)
!
ZHGROUND(:,:) = 0.0
ZSLOPE  (:,:) = 0.0

LMASK(:,:) = (OMASK_GW(:,:).AND.PHGROUND(:,:)/=PHG_OLD(:,:))
!
!-------------------------------------------------------------------------------
! * Evolution of water table depth
!-------------------------------------------------------------------------------
!
WHERE(LMASK(:,:))
      PWTD (:,:) = PHGROUND(:,:)-PTOPO_RIV(:,:)
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
  WHERE(OMASK_GW(:,:))
        PFWTD   (:,:) = 1.0
  ENDWHERE
!
ELSE
!
! Adjust depth before computed sub-grid fraction
!
  WHERE(LMASK(:,:))
    ZSLOPE  (:,:) = MIN(1.0,MAX(0.0,PHGROUND(:,:)-(PTOPO_RIV(:,:)-PHC_BED(:,:)))/PHC_BED(:,:))
    ZHGROUND(:,:) = PHGROUND(:,:) + ZSLOPE(:,:)*XGWSUBD
  ENDWHERE
!
! Compute sub-grid fraction as in Verges et al., JGR, 2014
!
  WHERE(LMASK(:,:).AND.ZHGROUND(:,:)<=PTABGW_H(:,:,1))
        PFWTD(:,:) = MIN(1.0,PTABGW_F(:,:,1))
        LMASK(:,:) = .FALSE.
  ELSEWHERE(LMASK(:,:).AND.ZHGROUND(:,:)>=PTABGW_H(:,:,INFRAC))
        PFWTD(:,:) = MIN(1.0,PTABGW_F(:,:,INFRAC))
        LMASK(:,:) = .FALSE.
  ENDWHERE
!
  ISUP (:,:)=0
  IINF (:,:)=0
!
  DO JLAT=1,ILAT
     DO JLON=1,ILON 
        IF(LMASK(JLON,JLAT))THEN
          DO JFRAC=1,INFRAC-1
             IF(ZHGROUND(JLON,JLAT)>=PTABGW_H(JLON,JLAT,JFRAC))THEN
               ISUP(JLON,JLAT)=JFRAC+1
               IINF(JLON,JLAT)=JFRAC
             ENDIF          
          ENDDO
          IF(IINF(JLON,JLAT)==0.or.ISUP(JLON,JLAT)==0)then
            WRITE(6,*)'IINF,ISUP,JLON,JLAT',IINF(JLON,JLAT),ISUP(JLON,JLAT),JLON,JLAT
            CALL FLUSH(6)
            WRITE(6,*)'JFRAC   PHGROUND   ZHGROUND   PTABGW_H(JFRAC)'
            WRITE(6,*)JFRAC,PHGROUND(JLON,JLAT),ZHGROUND(JLON,JLAT),PTABGW_H(JLON,JLAT,JFRAC)
            CALL FLUSH(6)
            CALL ABORT_TRIP('GWF_CPL_UPDATE:Problem with IINF or ISUP')
          ENDIF        
        ENDIF
     ENDDO
  ENDDO
!
  DO JLAT=1,ILAT
     DO JLON=1,ILON      
        IF(LMASK(JLON,JLAT))THEN
          PFWTD(JLON,JLAT) = PTABGW_F(JLON,JLAT,IINF(JLON,JLAT))                                      &
                           + (ZHGROUND(JLON,JLAT                )-PTABGW_H(JLON,JLAT,IINF(JLON,JLAT))) &
                           * (PTABGW_F(JLON,JLAT,ISUP(JLON,JLAT))-PTABGW_F(JLON,JLAT,IINF(JLON,JLAT))) &
                           / (PTABGW_H(JLON,JLAT,ISUP(JLON,JLAT))-PTABGW_H(JLON,JLAT,IINF(JLON,JLAT)))
          PFWTD(JLON,JLAT) = MIN(1.0,PFWTD(JLON,JLAT))
        ENDIF
     ENDDO
  ENDDO
!
ENDIF
!
!-------------------------------------------------------------------------------
! * Update the old groundwater height
!-------------------------------------------------------------------------------
!
PHG_OLD(:,:)=PHGROUND(:,:)
!
IF (LHOOK) CALL DR_HOOK('GWF_CPL_UPDATE',1,ZHOOK_HANDLE)
!
END SUBROUTINE GWF_CPL_UPDATE

