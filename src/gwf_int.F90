!     #################################################################
      SUBROUTINE GWF_INT(KLON,KLAT,PGRID_RES,PLAT,OMASK,PNUM_AQUI,PTRANS,PCR,PCC)
!     #################################################################
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_PAR, ONLY : XPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,                 INTENT(IN)    :: KLON
INTEGER,                 INTENT(IN)    :: KLAT
REAL,                    INTENT(IN)    :: PGRID_RES
REAL, DIMENSION(:  ),    INTENT(IN)    :: PLAT
REAL, DIMENSION(:,:),    INTENT(IN)    :: PNUM_AQUI
REAL, DIMENSION(:,:),    INTENT(IN)    :: PTRANS
!
LOGICAL, DIMENSION(:,:), INTENT(IN)    :: OMASK
!
REAL,    DIMENSION(:,:), INTENT(INOUT) :: PCR
REAL,    DIMENSION(:,:), INTENT(INOUT) :: PCC
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(KLON,KLAT)     :: ZT
REAL                           :: ZDLAT
INTEGER                        :: JLON, JLAT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GWF_INT',0,ZHOOK_HANDLE)
!
! *      1.   CALCULATE TRANSMISSIVITY
!             ------------------------
!
ZDLAT = PGRID_RES/2.0
!
ZT(:,:)=0.0 
WHERE(OMASK(:,:))
      ZT(:,:)=PTRANS(:,:)
ENDWHERE
!
! *      2.   CALCULATE HORIZONTAL CONDUCTANCES TO THE RIGHT (CR)
!             AND TO THE TOP(CC)
!             ------------------
!
DO JLAT = 1,KLAT
    DO JLON = 1,KLON
!
! CALCULATE CONDUCTANCE TO THE RIGHT (CR)
!
       IF(OMASK(JLON,JLAT).AND.JLON<KLON)THEN
!
           IF(PNUM_AQUI(JLON,JLAT)==PNUM_AQUI(JLON+1,JLAT))THEN
!              variable head cells adjacent
               PCR(JLON,JLAT) =  SQRT(ZT(JLON+1,JLAT)*ZT(JLON,JLAT))/(COS(PLAT(JLAT)*XPI/180.))
           ELSEIF(PNUM_AQUI(JLON,JLAT)==0.0)THEN
!              adjacent constant head cells (flux to ocean)
               PCR(JLON,JLAT) = ZT(JLON+1,JLAT)/(COS(PLAT(JLAT)*XPI/180.))
           ELSEIF(PNUM_AQUI(JLON+1,JLAT)==0.0)THEN
!              adjacent constant head cells (flux to ocean)
               PCR(JLON,JLAT) = ZT(JLON,JLAT)/(COS(PLAT(JLAT)*XPI/180.))
           ENDIF
!
       ENDIF
!
! CALCULATE CONDUCTANCE TO THE TOP (CC)
!
       IF(OMASK(JLON,JLAT).AND.JLAT<KLAT)THEN
!
           IF(PNUM_AQUI(JLON,JLAT)==PNUM_AQUI(JLON,JLAT+1))THEN
!              variable head cells adjacent
               PCC(JLON,JLAT) = SQRT(ZT(JLON,JLAT)*ZT(JLON,JLAT+1))*COS((PLAT(JLAT)+ZDLAT)*XPI/180.)
           ELSEIF(PNUM_AQUI(JLON,JLAT)==0.0)THEN
!              adjacent constant head cells (flux to ocean)
               PCC(JLON,JLAT) = ZT(JLON,JLAT+1)*COS((PLAT(JLAT)+ZDLAT)*XPI/180.)
           ELSEIF(PNUM_AQUI(JLON,JLAT+1)==0.0)THEN
!              adjacent constant head cells (flux to ocean)
               PCC(JLON,JLAT) = ZT(JLON,JLAT)*COS((PLAT(JLAT)+ZDLAT)*XPI/180.)
           ENDIF
!
       ENDIF
    ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('GWF_INT',1,ZHOOK_HANDLE)
!
END SUBROUTINE GWF_INT
!
