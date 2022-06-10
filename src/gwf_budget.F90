!####################################################################
SUBROUTINE GWF_BUDGET(KLON,KLAT,OMASK,PHGROUND,PHDRAIN_RIV,  &
                      PGWDEEP,PCR,PCC,PCRIV,PQGCELL,PQDRAIN  )
!####################################################################
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_PAR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,               INTENT(IN)    :: KLON
INTEGER,               INTENT(IN)    :: KLAT
!
LOGICAL,  DIMENSION(:,:), INTENT(IN) :: OMASK
!
REAL,  DIMENSION(:,:), INTENT(IN)    :: PHGROUND
REAL,  DIMENSION(:,:), INTENT(IN)    :: PHDRAIN_RIV
REAL,  DIMENSION(:,:), INTENT(IN)    :: PGWDEEP
REAL,  DIMENSION(:,:), INTENT(IN)    :: PCR
REAL,  DIMENSION(:,:), INTENT(IN)    :: PCC
REAL,  DIMENSION(:,:), INTENT(IN)    :: PCRIV
!
REAL,  DIMENSION(:,:), INTENT(INOUT) :: PQGCELL
REAL,  DIMENSION(:,:), INTENT(INOUT) :: PQDRAIN
!
!*      0.2    declarations of local variables
!
INTEGER :: JLON,JLAT,IX,IY
!
REAL, DIMENSION(KLON,KLAT) :: ZTOP
REAL, DIMENSION(KLON,KLAT) :: ZBOTTOM
REAL, DIMENSION(KLON,KLAT) :: ZLEFT
REAL, DIMENSION(KLON,KLAT) :: ZRIGHT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GWF_BUDGET',0,ZHOOK_HANDLE)
!
! *     1.    CALCULATE FLOW BETWEEN ADJACENT CELL
!            -------------------------------------
!
ZTOP   (:,:) = 0.0
ZBOTTOM(:,:) = 0.0
ZLEFT  (:,:) = 0.0
ZRIGHT (:,:) = 0.0
!
DO JLAT = 1,KLAT
    DO JLON = 1,KLON
!
!       Skip no-flow and constant-head cells
        IF (OMASK(JLON,JLAT)) THEN

!          Calculate flow through the right face
           IX=MIN(JLON+1,KLON)
           ZRIGHT(JLON,JLAT) = PCR(JLON,JLAT) *(PHGROUND(IX,JLAT)-PHGROUND(JLON,JLAT)) 
!
!          Calculate flow through the left face
           IX=MAX(JLON-1,1)
           ZLEFT(JLON,JLAT) = PCR(IX,JLAT)*(PHGROUND(IX,JLAT)-PHGROUND(JLON,JLAT))
!
!          Calculate flow through the bottom face
           IY=MAX(JLAT-1,1)
           ZBOTTOM(JLON,JLAT) = PCC(JLON,IY)*(PHGROUND(JLON,IY)-PHGROUND(JLON,JLAT))
!
!          Calculate flow through the top face
           IY=MIN(JLAT+1,KLAT)
           ZTOP(JLON,JLAT) = PCC(JLON,JLAT) *(PHGROUND(JLON,IY)-PHGROUND(JLON,JLAT))
!
           PQGCELL(JLON,JLAT) = (ZTOP(JLON,JLAT)+ZBOTTOM(JLON,JLAT)+ZLEFT(JLON,JLAT)+ZRIGHT(JLON,JLAT))*XRHOLW
!
        ENDIF
!
    ENDDO
ENDDO
!
! *     2. CALCULATE FLOW TO AQUIFER [kg/s]
!          --------------------------------
!
PQDRAIN(:,:) = 0.0
WHERE(OMASK(:,:))
      PQDRAIN(:,:) = (PCRIV(:,:)*(PHGROUND(:,:)-PHDRAIN_RIV(:,:))-PGWDEEP(:,:))*XRHOLW
ENDWHERE
!
IF (LHOOK) CALL DR_HOOK('GWF_BUDGET',1,ZHOOK_HANDLE)
!
END SUBROUTINE GWF_BUDGET


