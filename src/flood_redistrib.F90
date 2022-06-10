!     #########
      SUBROUTINE FLOOD_REDISTRIB (TP, TPG,                              &
                                  KLON,KLAT,PTSTEP,PREAD,PSRCFLD,PRESIDU)  
!     #####################################################################
!
!!****  *FLOOD_REDISTRIB*  
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
!!      Original    01/12/13 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP, ONLY : TRIP_t
USE MODD_TRIP_GRID, ONLY : TRIP_GRID_t
!
USE MODD_TRIP_LISTING
!
USE MODD_TRIP_PAR, ONLY : XUNDEF, XRHOLW
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
REAL,    INTENT(IN)               :: PTSTEP
!
REAL, DIMENSION(:,:), INTENT(IN ) :: PREAD   ![kg/m2/s]
REAL, DIMENSION(:,:), INTENT(OUT) :: PSRCFLD ![kg/m2/s]
REAL, DIMENSION(:,:), INTENT(OUT) :: PRESIDU ![kg/m2/s]
!
!
!*      0.2    declarations of local variables
!
REAL,    PARAMETER              :: ZNEG = -1.0
REAL,    PARAMETER              :: ZLIM = -0.95
REAL,    PARAMETER              :: ZSTO =  0.1  ! kg/m2
!
LOGICAL, DIMENSION(0:TPG%NBASMAX) :: LBAS_FLD
!
REAL,    DIMENSION(TPG%NBASMAX) :: ZBAS_REMAIN
REAL,    DIMENSION(TPG%NBASMAX) :: ZBAS_AREA
REAL,    DIMENSION(TPG%NBASMAX) :: ZBAS_STO
!
REAL,    DIMENSION(KLON,KLAT) :: ZFLDBUDGET
REAL,    DIMENSION(KLON,KLAT) :: ZRATIO
REAL,    DIMENSION(KLON,KLAT) :: ZREMAIN
!
REAL :: ZFLUXE_IN
REAL :: ZFLUXE_OUT
REAL :: ZAREA_TOT
REAL :: ZTOT_STO
REAL :: ZBILAN
!
INTEGER :: JBAS, JLON, JLAT, ICOUNT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('FLOOD_REDISTRIB',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
! * Init
!-------------------------------------------------------------------------------
!
ZFLDBUDGET(:,:) = XUNDEF
!
PRESIDU   (:,:) = 0.0
PSRCFLD   (:,:) = 0.0
!
IF(ALL(PREAD(:,:)==0.0))THEN
! If no fluxes, return
  IF (LHOOK) CALL DR_HOOK('FLOOD_REDISTRIB',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!-------------------------------------------------------------------------------
! * Localized water budget 
!-------------------------------------------------------------------------------
!
WHERE(TPG%GMASK_FLD(:,:).AND.PREAD(:,:)/=0.0)
   ZFLDBUDGET(:,:) = TP%XFLOOD_STO(:,:)+PREAD(:,:)*TPG%XAREA(:,:)*PTSTEP ! kg
ENDWHERE
!
WHERE(TP%XFLOOD_STO(:,:)>0.0.AND.ZFLDBUDGET(:,:)>=0.0.AND.ZFLDBUDGET(:,:)/=XUNDEF)
  PSRCFLD(:,:) = PREAD(:,:)
  PRESIDU(:,:) = 0.0
ENDWHERE
!
WHERE(TP%XFLOOD_STO(:,:)>0.0.AND.ZFLDBUDGET(:,:)<0.0.AND.ZFLDBUDGET(:,:)/=XUNDEF)
  PSRCFLD(:,:) = ZNEG*TP%XFLOOD_STO(:,:)/(TPG%XAREA(:,:)*PTSTEP)                     ! kg/m2/s
  PRESIDU(:,:) = MAX(ZLIM*TP%XSURF_STO(:,:),ZFLDBUDGET(:,:))/(TPG%XAREA(:,:)*PTSTEP) ! kg/m2/s
ENDWHERE
!
ZREMAIN(:,:) = PREAD(:,:) - PSRCFLD(:,:) - PRESIDU(:,:)
!
!-------------------------------------------------------------------------------
!  * If some residue remains, redistribute the redidue 
!-------------------------------------------------------------------------------
!
LBAS_FLD (:) = .FALSE.
!
ZFLUXE_IN  = 0.0
ZTOT_STO   = 0.0
!
DO JLAT=1,KLAT
   DO JLON=1,KLON
      JBAS = TPG%NBASID(JLON,JLAT)
      IF(TPG%GMASK_FLD(JLON,JLAT).AND.(.NOT.LBAS_FLD(JBAS)))THEN
        LBAS_FLD(JBAS)=(TP%XFLOOD_STO(JLON,JLAT)>0.0)
      ENDIF
      IF(TPG%GMASK_FLD(JLON,JLAT))THEN              
        ZFLUXE_IN  = ZFLUXE_IN  + TPG%XAREA(JLON,JLAT) * ZREMAIN(JLON,JLAT) ! kg/s
        ZTOT_STO   = ZTOT_STO   + TP%XSURF_STO(JLON,JLAT)
      ENDIF      
   ENDDO
ENDDO
!
ZBAS_AREA  (:) = 0.0
ZBAS_STO   (:) = 0.0
!
DO JLAT=1,KLAT
   DO JLON=1,KLON
      JBAS=TPG%NBASID(JLON,JLAT)
      IF(TPG%GMASK_FLD(JLON,JLAT).AND.LBAS_FLD(JBAS).AND.TP%XFLOOD_STO(JLON,JLAT)>0.0)THEN        
        ZBAS_AREA  (JBAS)=ZBAS_AREA(JBAS)+TPG%XAREA   (JLON,JLAT)
        ZBAS_STO   (JBAS)=ZBAS_STO (JBAS)+TP%XSURF_STO(JLON,JLAT)
      ENDIF
   ENDDO
ENDDO
!
ICOUNT = 0
!
DO JBAS=TPG%NBASMIN,TPG%NBASMAX
    IF(LBAS_FLD(JBAS))THEN
     IF((ZBAS_STO(JBAS)/ZBAS_AREA(JBAS))<=ZSTO)THEN
       !WRITE(NLISTING,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
       !WRITE(NLISTING,*)'A basin is empty :',JBAS
       !WRITE(NLISTING,*)'MASS (kg/m2) = ', ZBAS_STO(JBAS)/ZBAS_AREA(JBAS)
       !WRITE(NLISTING,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
       LBAS_FLD(JBAS)=.FALSE.
     ELSE
       ICOUNT=ICOUNT+1
     ENDIF
   ENDIF
ENDDO
!
IF(ZFLUXE_IN/=0.0.AND.ICOUNT==0)THEN
!
!------------------------------------------------------------------------------------------
!  * If there is no flooded areas, redistribute the redidue globaly
!------------------------------------------------------------------------------------------
!
  DO JLAT=1,KLAT
     DO JLON=1,KLON
        IF(TPG%GMASK_FLD(JLON,JLAT))THEN
          ZRATIO (JLON,JLAT) = TP%XSURF_STO(JLON,JLAT)/TPG%XAREA(JLON,JLAT)
          PRESIDU(JLON,JLAT) = PRESIDU(JLON,JLAT) + ZFLUXE_IN * ZRATIO(JLON,JLAT) / ZTOT_STO ! kg/m2/s
        ENDIF
     ENDDO
  ENDDO
!
ELSE
!
!-------------------------------------------------------------------------------
!  * Redistribute the redidue  over each basin where there is flood
!-------------------------------------------------------------------------------
!
  ZBAS_REMAIN(:) = 0.0
  ZBAS_STO   (:) = 0.0
!
  ZFLUXE_IN  = 0.0
  ZTOT_STO   = 0.0
!
  DO JLAT=1,KLAT
     DO JLON=1,KLON
        JBAS=TPG%NBASID(JLON,JLAT)
        IF(TPG%GMASK_FLD(JLON,JLAT).AND.LBAS_FLD(JBAS).AND.TP%XFLOOD_STO(JLON,JLAT)>0.0)THEN
          ZTOT_STO      = ZTOT_STO      + TP%XFLOOD_STO(JLON,JLAT)
          ZBAS_STO(JBAS)=ZBAS_STO(JBAS) + TP%XFLOOD_STO(JLON,JLAT)
        ENDIF
        IF(TPG%GMASK_FLD(JLON,JLAT).AND.LBAS_FLD(JBAS))THEN
          ZBAS_REMAIN(JBAS)=ZBAS_REMAIN(JBAS)+TPG%XAREA(JLON,JLAT)*ZREMAIN(JLON,JLAT) ! kg/s
        ENDIF
        IF(TPG%GMASK_FLD(JLON,JLAT).AND.(.NOT.LBAS_FLD(JBAS)))THEN
          ZFLUXE_IN  = ZFLUXE_IN  + TPG%XAREA(JLON,JLAT) * ZREMAIN(JLON,JLAT) ! kg/s
        ENDIF
     ENDDO
  ENDDO
!
  DO JLAT=1,KLAT
     DO JLON=1,KLON
        JBAS = TPG%NBASID(JLON,JLAT)
        IF(TPG%GMASK_FLD(JLON,JLAT).AND.LBAS_FLD(JBAS).AND.TP%XFLOOD_STO(JLON,JLAT)>0.0)THEN
          ZRATIO (JLON,JLAT) = TP%XFLOOD_STO(JLON,JLAT)/TPG%XAREA(JLON,JLAT)
          PSRCFLD(JLON,JLAT) = PSRCFLD(JLON,JLAT) + ZBAS_REMAIN(JBAS)*ZRATIO(JLON,JLAT)/ZBAS_STO(JBAS) & ! kg/m2/s
                                                  + ZFLUXE_IN        *ZRATIO(JLON,JLAT)/ZTOT_STO         ! kg/m2/s
        ENDIF
     ENDDO
  ENDDO     
!
ENDIF
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
      IF(TPG%GMASK_FLD(JLON,JLAT))THEN
        ZAREA_TOT  = ZAREA_TOT  + TPG%XAREA(JLON,JLAT)
        ZFLUXE_IN  = ZFLUXE_IN  + TPG%XAREA(JLON,JLAT) * PREAD(JLON,JLAT)
        ZFLUXE_OUT = ZFLUXE_OUT + TPG%XAREA(JLON,JLAT) * (PSRCFLD(JLON,JLAT)+PRESIDU(JLON,JLAT))
      ENDIF   
   ENDDO
ENDDO
!
ZBILAN=(ZFLUXE_IN-ZFLUXE_OUT)/ZAREA_TOT
!
IF(ABS(ZBILAN)>1.E-12)THEN
  WRITE(NLISTING,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(NLISTING,*)'Redistribution of flood sources has a problem'
  WRITE(NLISTING,*)'BILAN = ', ZBILAN, ZFLUXE_IN/ZAREA_TOT, ZFLUXE_OUT/ZAREA_TOT
  WRITE(NLISTING,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  CALL ABORT_TRIP('FLOOD_REDISTRIB: Redistribution of flood sources has a problem')        
ENDIF
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('FLOOD_REDISTRIB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE FLOOD_REDISTRIB
