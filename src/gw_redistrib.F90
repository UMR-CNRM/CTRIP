!     #########
      SUBROUTINE GW_REDISTRIB (TP, TPG,             &
                               KLON,KLAT,PREAD,PFOUT)  
!     #####################################################################
!
!!****  *GW_REDISTRIB*  
!!
!!    PURPOSE
!!    -------
!
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
!!      Original    01/10/16 
!!      S. Munier   03/2020 CTRIP-12D and parallelization
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP,      ONLY : TRIP_t
USE MODD_TRIP_GRID, ONLY : TRIP_GRID_t
!
USE MODD_TRIP_LISTING
!
USE MODD_TRIP_PAR, ONLY : XUNDEF
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
!
TYPE(TRIP_t),      INTENT(INOUT) :: TP
TYPE(TRIP_GRID_t), INTENT(INOUT) :: TPG
!
INTEGER, INTENT(IN)               :: KLON
INTEGER, INTENT(IN)               :: KLAT
!
REAL, DIMENSION(:,:), INTENT(IN ) :: PREAD ![kg/m2/s]
REAL, DIMENSION(:,:), INTENT(OUT) :: PFOUT ![kg/m2/s]
!
!
!*      0.2    declarations of local variables
!
!
REAL,    DIMENSION(10000)     :: ZBAS_FNEG
REAL,    DIMENSION(10000)     :: ZBAS_AREA
!
LOGICAL, DIMENSION(KLON,KLAT) :: GMASK_NOGW
!
REAL,    DIMENSION(KLON,KLAT) :: ZFNEG
REAL,    DIMENSION(KLON,KLAT) :: ZRATIO
!
INTEGER, DIMENSION(KLON,KLAT) :: INUM_AQUI
!
REAL :: ZFLUXE_IN
REAL :: ZFLUXE_OUT
REAL :: ZAREA_TOT, ZTOT_STO
REAL :: ZBILAN
!
INTEGER :: JBAS, JLON, JLAT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GW_REDISTRIB',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
! * Init none groundwater mask
!-------------------------------------------------------------------------------
!
WHERE(TPG%GMASK(:,:).AND.(.NOT.TPG%GMASK_GW(:,:)))
  GMASK_NOGW(:,:)=.TRUE.
ELSEWHERE
  GMASK_NOGW(:,:)=.FALSE.
ENDWHERE
!
!-------------------------------------------------------------------------------
! * Init flux fileds
!-------------------------------------------------------------------------------
!
ZBILAN     = 0.0
ZFNEG(:,:) = 0.0
PFOUT(:,:) = PREAD(:,:)
!
DO JLAT=1,KLAT
   DO JLON=1,KLON
      IF(GMASK_NOGW(JLON,JLAT).AND.PREAD(JLON,JLAT)<0.0)THEN
        PFOUT(JLON,JLAT) = 0.0
        ZFNEG(JLON,JLAT) = PREAD(JLON,JLAT)
        ZBILAN           = ZBILAN + PREAD(JLON,JLAT)
      ENDIF
   ENDDO
ENDDO
!
IF(ZBILAN==0.0)THEN
! If no negative fluxes over none groundwater mask, return
  IF (LHOOK) CALL DR_HOOK('GW_REDISTRIB',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!-------------------------------------------------------------------------------
!  * Redistribute negative field over aquifer basins
!-------------------------------------------------------------------------------
!
WHERE(TPG%GMASK(:,:))
  INUM_AQUI(:,:)=INT(TP%XNEAR_AQUI(:,:))
ELSEWHERE
  INUM_AQUI(:,:)=0
ENDWHERE
!
ZBAS_FNEG(:) = 0.0
ZBAS_AREA(:) = 0.0
!
DO JLAT=1,KLAT
   DO JLON=1,KLON
      JBAS=INUM_AQUI(JLON,JLAT)
      IF(TPG%GMASK_GW(JLON,JLAT))THEN        
        ZBAS_AREA(JBAS)=ZBAS_AREA(JBAS)+TPG%XAREA(JLON,JLAT)
      ENDIF
      IF(GMASK_NOGW(JLON,JLAT))THEN        
        ZBAS_FNEG(JBAS)=ZBAS_FNEG(JBAS)+TPG%XAREA(JLON,JLAT)*ZFNEG(JLON,JLAT) ! kg/s
      ENDIF
   ENDDO
ENDDO
!
DO JLAT=1,KLAT
   DO JLON=1,KLON
      JBAS = INUM_AQUI(JLON,JLAT)
      IF(TPG%GMASK_GW(JLON,JLAT))THEN
         PFOUT(JLON,JLAT) = PFOUT(JLON,JLAT) + ZBAS_FNEG(JBAS)/ZBAS_AREA(JBAS) ! kg/m2/s
      ENDIF
   ENDDO
ENDDO     
!
!-------------------------------------------------------------------------------
! * Comput cumulated Fluxes (kg/s) and flooded areas (-)
!-------------------------------------------------------------------------------
!
ZFLUXE_IN  = 0.0
ZFLUXE_OUT = 0.0
ZAREA_TOT  = 0.0
!
DO JLAT=1,KLAT
   DO JLON=1,KLON
      IF(TPG%GMASK(JLON,JLAT))THEN
        ZAREA_TOT  = ZAREA_TOT  + TPG%XAREA(JLON,JLAT)
        ZFLUXE_IN  = ZFLUXE_IN  + TPG%XAREA(JLON,JLAT) * PREAD(JLON,JLAT)
        ZFLUXE_OUT = ZFLUXE_OUT + TPG%XAREA(JLON,JLAT) * PFOUT(JLON,JLAT)
      ENDIF   
   ENDDO
ENDDO
!
ZBILAN=(ZFLUXE_IN-ZFLUXE_OUT)/ZAREA_TOT
!
IF(ABS(ZBILAN)>1.E-12)THEN
  WRITE(NLISTING,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(NLISTING,*)'Redistribution of negative recharge sources has a problem'
  WRITE(NLISTING,*)'BILAN = ', ZBILAN, ZFLUXE_IN/ZAREA_TOT, ZFLUXE_OUT/ZAREA_TOT
  WRITE(NLISTING,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  CALL ABORT_TRIP('GW_REDISTRIB: Redistribution of negative recharge sources has a problem')        
ENDIF
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GW_REDISTRIB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GW_REDISTRIB
