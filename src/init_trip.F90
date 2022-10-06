!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!####################################################################################
SUBROUTINE INIT_TRIP (TPDG, TP, TPG, TPLK, &
                      KYEAR,KMONTH,KDAY,PTIME,KLON,KLAT,KLAKE_NUM, &
                      PTSTEP_RUN,PTSTEP_DIAG,ORESTART,OXIOS)
!####################################################################################
!
!!****  *INIT_TRIP*
!!
!!    PURPOSE
!!    -------
!
!     Initialize TRIP variables and parameters.
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
!!      Original    01/02/05
!!      For surfex  21/05/08
!!      S. Munier   03/2020    CTRIP-12D and parallelization
!!      T. Guinaldo 04/2020    Add MLake
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_DIAG, ONLY : TRIP_DIAG_t
USE MODD_TRIP,      ONLY : TRIP_t
USE MODD_TRIP_GRID, ONLY : TRIP_GRID_t
USE MODD_TRIP_LAKE, ONLY : TRIP_LAKE_t
!
USE MODN_TRIP, ONLY : CGROUNDW, CVIT, LFLOOD, CLAKE, LCALCRIVLEN, &
                      XCVEL, XRATMED, XTSTEP, LBACKWATER, &
                      LGWSUBF, XGWSUBD, LAPPROXVEL, XBANKSLOPE
!
USE MODD_TRIP_PAR
USE MODD_TRIP_LISTING, ONLY : NLISTING
USE MODD_TRIP_MPI
!
USE MODE_TRIP_GRID
USE MODE_TRIP_INIT
USE MODE_TRIP_FUNCTION
USE MODE_RW_TRIP
USE MODE_POWER_2THIRD, ONLY : POWER_2THIRD_INIT
!
USE MODI_ABORT_TRIP
USE MODI_TRIP_HS_VEL
USE MODI_INIT_TRIP_DIAG
USE MODI_INIT_RESTART_TRIP
USE MODI_GET_LONLAT_TRIP
USE MODI_INIT_TRIP_CPL_ESM
USE MODI_ALLOC_TRIP_DIAG
USE MODI_READ_DIMLEN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
!*      0.1    declarations of arguments
!
!
TYPE(TRIP_DIAG_t), INTENT(INOUT) :: TPDG
TYPE(TRIP_t),      INTENT(INOUT) :: TP
TYPE(TRIP_GRID_t), INTENT(INOUT) :: TPG
TYPE(TRIP_LAKE_t), INTENT(INOUT) :: TPLK
!
INTEGER,          INTENT(OUT) :: KYEAR   !date UTC
INTEGER,          INTENT(OUT) :: KMONTH  !date UTC
INTEGER,          INTENT(OUT) :: KDAY    !date UTC
REAL,             INTENT(OUT) :: PTIME   !date UTC
!
INTEGER,          INTENT(OUT) :: KLON      ! number of points in longitude
INTEGER,          INTENT(OUT) :: KLAT      ! number of points in latitude
INTEGER,          INTENT(OUT) :: KLAKE_NUM ! number of points in latitude
!
REAL,             INTENT(IN)  :: PTSTEP_RUN
REAL,             INTENT(IN)  :: PTSTEP_DIAG
!
LOGICAL,          INTENT(IN)  :: ORESTART
LOGICAL,          INTENT(IN)  :: OXIOS
!
!-------------------------------------------------------------------------------
!
!*      0.2    declarations of local variables
!
CHARACTER(LEN=13), PARAMETER         :: YFILE_PARAM  ='TRIP_PARAM.nc'
CHARACTER(LEN=25)                    :: YFILE_INIT   ='TRIP_PREP.nc'
CHARACTER(LEN=25)                    :: YFILE_RESTART='TRIP_RESTART.nc'
CHARACTER(LEN=25)                    :: YDIAG        ='TRIP_DIAG.nc'
CHARACTER(LEN=25)                    :: YRUN         ='TRIP_DIAG_RUN.nc'
!
CHARACTER(LEN=6)                     :: YTIME
CHARACTER(LEN=50)                    :: YFILE
CHARACTER(LEN=20)                    :: YVAR
!
REAL,DIMENSION(4)                    :: ZDATE
!
REAL,DIMENSION(:,:,:),ALLOCATABLE    :: ZREAD3D
REAL,DIMENSION(:),  ALLOCATABLE      :: ZREAD1D
!
REAL,DIMENSION(:,:),ALLOCATABLE      :: ZREAD
! REAL,DIMENSION(:,:),ALLOCATABLE      :: ZHSTREAM
! REAL,DIMENSION(:,:),ALLOCATABLE      :: ZVEL
REAL,DIMENSION(:,:),ALLOCATABLE      :: ZWORK
REAL,DIMENSION(:,:),ALLOCATABLE      :: ZGW_STO
REAL,DIMENSION(:,:),ALLOCATABLE      :: ZWTD
!
REAL, DIMENSION(:),ALLOCATABLE       :: ZLON
REAL, DIMENSION(:),ALLOCATABLE       :: ZLAT
!
REAL    :: ZLONMIN   ! minimum longitude (degrees)
REAL    :: ZLONMAX   ! maximum longitude (degrees)
REAL    :: ZLATMIN   ! minimum latitude  (degrees)
REAL    :: ZLATMAX   ! maximum latitude  (degrees)
REAL    :: ZGRID_RES ! 1° or 0.5° or 0.083333° resolution
!
INTEGER :: IWORK, IFLOOD, INI, JLON, JLAT
INTEGER :: IDIMLEN(1)
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! * Output attribut for netcdf diag file
!-------------------------------------------------------------------------------
!
CHARACTER(LEN=50) :: YTITLE, YUNITTIME
!                    YTITLE    = Title of each output file
!                    YUNITTIME = Time unit in each output file if present
!
!-------------------------------------------------------------------------------
! * Initilyse TRIP
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_TRIP',0,ZHOOK_HANDLE)
!
WRITE(NLISTING,*)''
WRITE(NLISTING,*)''
WRITE(NLISTING,*)'!'
WRITE(NLISTING,*)'! TRIP RUN !!!!!!!!!!!!!'
WRITE(NLISTING,*)'!'
!
WRITE(NLISTING,*)''
WRITE(NLISTING,*)'        INITIALYSE TRIP            '
WRITE(NLISTING,*)''
!
!-------------------------------------------------------------------------------
! * Read date
!-------------------------------------------------------------------------------
!
YVAR='date'
CALL READ_TRIP(NLISTING,YFILE_INIT,YVAR,ZDATE)
!
KYEAR  = INT(ZDATE(1))
KMONTH = INT(ZDATE(2))
KDAY   = INT(ZDATE(3))
PTIME  = ZDATE(4)
!
!-------------------------------------------------------------------------------
!
! * Get TRIP grid configuration
!
CALL GET_TRIP_GRID(TPG%XTRIP_GRID,ZLONMIN,ZLONMAX,ZLATMIN,ZLATMAX,ZGRID_RES,KLON,KLAT)
KLAKE_NUM = 0
IF(CLAKE=='MLK') THEN
  CALL READ_DIMLEN(NLISTING,YFILE_PARAM,'LAKE_A',1,IDIMLEN)
  KLAKE_NUM = IDIMLEN(1)
ENDIF
TPLK%NLAKE_NUM = KLAKE_NUM
!
ALLOCATE(ZLON(KLON))
ALLOCATE(ZLAT(KLAT))
ZLON(:)=XUNDEF
ZLAT(:)=XUNDEF
!
CALL GET_LONLAT_TRIP(TPG, &
                     KLON,KLAT,ZLON,ZLAT)
!
IF(CVIT == 'VAR' .AND. LAPPROXVEL) CALL POWER_2THIRD_INIT()
!
! Only IO processus does continue this routine
IF(NRANK/=NPIO) THEN
  IF (LHOOK) CALL DR_HOOK('INIT_TRIP',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!-------------------------------------------------------------------------------
! * Check options
!-------------------------------------------------------------------------------
!
IF(CVIT == 'VAR')THEN
   IF(XCVEL/=0.5)THEN
      XCVEL=0.5
      WRITE(NLISTING,*)'!!! ATTENTION : You use the velocity scheme and XCVEL is not 0.5 m/s !!!'
      WRITE(NLISTING,*)'!!! ATTENTION : XCVEL put at 0.5 m/s !!!'
  ENDIF
ELSE
   IF(XCVEL<0.1)THEN
      WRITE(NLISTING,*)'!!! ATTENTION : XCVEL < 0.1 m/s !!! Not good !!!'
      CALL ABORT_TRIP('INIT_TRIP: ATTENTION : XCVEL < 0.1 m/s !!! Not good !!!')
  ENDIF
ENDIF
!
IF(CGROUNDW=='DIF')THEN
  IF(CVIT /= 'VAR')THEN
    WRITE(NLISTING,*)'! You cannot use the groundwater scheme without the variable velocity scheme !!!'
    CALL ABORT_TRIP('INIT_TRIP: You cannot use the groundwater scheme without the variable velocity scheme !!!')
  ENDIF
  IF(ZGRID_RES>0.5)THEN
    WRITE(NLISTING,*)'! You cannot use the groundwater scheme with another resolution than 0.5 or 1/12 !!!'
    CALL ABORT_TRIP('INIT_TRIP: You cannot use the groundwater scheme with another resolution than 0.5 or 1/12 !!!')
  ENDIF
ENDIF
!
IF(LFLOOD)THEN
  IF(CGROUNDW=='DEF')THEN
    WRITE(NLISTING,*)'! ATTENTION : You use the flooding scheme without groundwater scheme !!!'
  ENDIF
  IF(CVIT /= 'VAR')THEN
    WRITE(NLISTING,*)'! You cannot use the flooding scheme without the variable velocity scheme !!!'
    CALL ABORT_TRIP('INIT_TRIP: You cannot use the flooding scheme without the variable velocity scheme !!!')
  ENDIF
  IF(XTSTEP>3600.)THEN
    WRITE(NLISTING,*)'!'
    WRITE(NLISTING,*)'! For flooding, the TRIP time step is too big      !!!'
    WRITE(NLISTING,*)'! XTSTEP must be equal or inferior to 3600s        !!!'
    WRITE(NLISTING,*)'!'
    CALL ABORT_TRIP('INIT_TRIP: For flooding, the TRIP time step is too big      !!!')
  ENDIF
ENDIF
!
IF(CLAKE=='MLK')THEN
  IF(ZGRID_RES>=0.5)THEN
    WRITE(NLISTING,*)'! You cannot use the MLake scheme with a resolution different than 1/12° !!!'
    CALL ABORT_TRIP('INIT_TRIP: You cannot use the MLake scheme with a resolution different than 1/12° !!!')
  ENDIF
ENDIF
!
IF(MOD(PTSTEP_RUN,XTSTEP)*MOD(XTSTEP,PTSTEP_RUN)/=0.)THEN
  WRITE(NLISTING,*)'! XTSTEP_RUN and XTSTEP are not good !!!'
  WRITE(NLISTING,*)'! XTSTEP_RUN =', PTSTEP_RUN
  WRITE(NLISTING,*)'! XTSTEP     =', XTSTEP
  CALL ABORT_TRIP('INIT_TRIP: PTSTEP_RUN and XTSTEP are not good !!!')
ENDIF
!
IF(MOD(PTSTEP_DIAG,XTSTEP)*MOD(XTSTEP,PTSTEP_DIAG)/=0.)THEN
  WRITE(NLISTING,*)'! PTSTEP_DIAG and XTSTEP are not good !!!'
  WRITE(NLISTING,*)'! XTSTEP_DIAG =', PTSTEP_DIAG
  WRITE(NLISTING,*)'! XTSTEP      =', XTSTEP
  CALL ABORT_TRIP('INIT_TRIP: PTSTEP_DIAG and XTSTEP are not good !!!')
ENDIF
!
WRITE(NLISTING,*)'! ',ZGRID_RES,'° TRIP run !!!'
!
IF(ZGRID_RES<0.5.AND.XRATMED==1.4)THEN
     WRITE(NLISTING,*)'! meandering ratio is 1.4 at 0.5° or 1° resolution !!!'
     WRITE(NLISTING,*)'! for other resolution change XRATMED in namelist  !!!'
     CALL ABORT_TRIP('INIT_TRIP: meandering ratio is 1.4 at 0.5° or 1° resolution !!!')
ENDIF
!
IF(LGWSUBF.AND.XGWSUBD>30.)THEN
     WRITE(NLISTING,*)'!! XGWSUBD too large (must be <=30), check your namelist  !!!'
     CALL ABORT_TRIP('INIT_TRIP: XGWSUBD too large (must be <=30), check your namelist !!!')
ENDIF
!
!-------------------------------------------------------------------------------
! * Allocate trip grid arguments
!-------------------------------------------------------------------------------
!
ALLOCATE(TPG%XAREA    (KLON,KLAT))
ALLOCATE(TPG%XLEN     (KLON,KLAT))
ALLOCATE(TPG%NGRCN    (KLON,KLAT))
ALLOCATE(TPG%NSEQ     (KLON,KLAT))
ALLOCATE(TPG%NNEXTX   (KLON,KLAT))
ALLOCATE(TPG%NNEXTY   (KLON,KLAT))
ALLOCATE(TPG%NBASID   (KLON,KLAT))
!
ALLOCATE(TPG%GMASK    (KLON,KLAT))
ALLOCATE(TPG%GMASK_VEL(KLON,KLAT))
ALLOCATE(TPG%GMASK_FLD(KLON,KLAT))
ALLOCATE(TPG%GMASK_GW (KLON,KLAT))
ALLOCATE(TPG%GMASK_GRE(KLON,KLAT))
ALLOCATE(TPG%GMASK_ANT(KLON,KLAT))
ALLOCATE(TPG%GMASK_LAKE_IN(KLON,KLAT))
ALLOCATE(TPG%GMASK_LAKE_NW(KLON,KLAT))
!
TPG%GMASK    (:,:) = .FALSE.
TPG%GMASK_VEL(:,:) = .FALSE.
TPG%GMASK_FLD(:,:) = .FALSE.
TPG%GMASK_GW (:,:) = .FALSE.
TPG%GMASK_GRE(:,:) = .FALSE.
TPG%GMASK_ANT(:,:) = .FALSE.
TPG%GMASK_LAKE_IN(:,:) = .FALSE.
TPG%GMASK_LAKE_NW(:,:) = .FALSE.
!
!-------------------------------------------------------------------------------
! * Allocate trip arguments
!-------------------------------------------------------------------------------
!
ALLOCATE(TP%XSURF_STO(KLON,KLAT))
!
IF(CGROUNDW/='DEF')THEN
  ALLOCATE(TP%XTAUG(KLON,KLAT))
ELSE
  ALLOCATE(TP%XTAUG(0,0))
ENDIF
!
IF(CGROUNDW=='CST')THEN
  ALLOCATE(TP%XGROUND_STO(KLON,KLAT))
ELSE
  ALLOCATE(TP%XGROUND_STO(0,0))
ENDIF
!
IF(CGROUNDW=='DIF')THEN
  ALLOCATE(TP%XHGROUND (KLON,KLAT))
  ALLOCATE(TP%XWEFF    (KLON,KLAT))
  ALLOCATE(TP%XTRANS   (KLON,KLAT))
  ALLOCATE(TP%XNUM_AQUI(KLON,KLAT))
ELSE
  ALLOCATE(TP%XHGROUND (0,0))
  ALLOCATE(TP%XWEFF    (0,0))
  ALLOCATE(TP%XTRANS   (0,0))
  ALLOCATE(TP%XNUM_AQUI(0,0))
ENDIF
!
IF(CGROUNDW=='DIF'.OR.LBACKWATER)THEN
  ALLOCATE(TP%XTOPO_RIV(KLON,KLAT))
ELSE
  ALLOCATE(TP%XTOPO_RIV(0,0))
ENDIF
!
IF(LFLOOD.OR.CGROUNDW=='DIF'.OR.XBANKSLOPE>0.0)THEN
  ALLOCATE(TP%XHC_BED(KLON,KLAT))
ELSE
  ALLOCATE(TP%XHC_BED(0,0))
ENDIF
!
IF(LFLOOD)THEN
  ALLOCATE(TP%XN_FLOOD  (KLON,KLAT))
  ALLOCATE(TP%XFLOOD_STO(KLON,KLAT))
  ALLOCATE(TP%XWFLOOD   (KLON,KLAT))
  ALLOCATE(TP%XFLOOD_LEN(KLON,KLAT))
  ALLOCATE(TP%XHFLOOD   (KLON,KLAT))
  ALLOCATE(TP%XFFLOOD   (KLON,KLAT))
ELSE
  ALLOCATE(TP%XN_FLOOD  (0,0))
  ALLOCATE(TP%XFLOOD_STO(0,0))
  ALLOCATE(TP%XWFLOOD   (0,0))
  ALLOCATE(TP%XFLOOD_LEN(0,0))
  ALLOCATE(TP%XHFLOOD   (0,0))
  ALLOCATE(TP%XFFLOOD   (0,0))
ENDIF
!
IF(CVIT=='VAR')THEN
  ALLOCATE(TP%XSLOPEBED(KLON,KLAT))
  ALLOCATE(TP%XWIDTH   (KLON,KLAT))
  ALLOCATE(TP%XN       (KLON,KLAT))
ELSE
  ALLOCATE(TP%XSLOPEBED(0,0))
  ALLOCATE(TP%XWIDTH   (0,0))
  ALLOCATE(TP%XN       (0,0))
ENDIF
!
IF(CLAKE=='MLK')THEN
  ALLOCATE(TPLK%NLAKE_ID_IN(KLON,KLAT))
  ALLOCATE(TPLK%NLAKE_ID_NW(KLON,KLAT))
  ALLOCATE(TPLK%XFRAC_LAKE (KLON,KLAT))
  ALLOCATE(TPLK%XLAKE_A    (KLAKE_NUM))
  ALLOCATE(TPLK%XWEIR_Z    (KLAKE_NUM))
  ALLOCATE(TPLK%XWEIR_W    (KLAKE_NUM))
  ALLOCATE(TPLK%XLAKE_STO  (KLAKE_NUM))
  ALLOCATE(TP%XWEIR_Z      (KLON,KLAT))
  ALLOCATE(TP%XWEIR_W      (KLON,KLAT))
ELSE
  ALLOCATE(TPLK%NLAKE_ID_IN(0,0))
  ALLOCATE(TPLK%NLAKE_ID_NW(0,0))
  ALLOCATE(TPLK%XFRAC_LAKE (0,0))
  ALLOCATE(TPLK%XLAKE_A    (0))
  ALLOCATE(TPLK%XWEIR_Z    (0))
  ALLOCATE(TPLK%XWEIR_W    (0))
  ALLOCATE(TPLK%XLAKE_STO  (0))
  ALLOCATE(TP%XWEIR_Z      (0,0))
  ALLOCATE(TP%XWEIR_W      (0,0))
ENDIF
!
!-------------------------------------------------------------------------------
! * Compute and Read TRIP parameter
!-------------------------------------------------------------------------------
!
ALLOCATE(ZREAD(KLON,KLAT))
!
! * Flow direction
!
YVAR ='FLOWDIR'
CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,ZREAD)
WHERE(ZREAD==XUNDEF)ZREAD=0.0
TPG%NGRCN(:,:)=INT(ZREAD(:,:))
WHERE(TPG%NGRCN(:,:)>0)TPG%GMASK(:,:)=.TRUE.
!
! * Rriver sequence
!
YVAR ='RIVSEQ'
CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,ZREAD)
WHERE(ZREAD==XUNDEF)ZREAD=0.0
TPG%NSEQ(:,:)=INT(ZREAD(:,:))
!
! * Maximum river sequence value
!
TPG%NSEQMAX = MAXVAL(TPG%NSEQ(:,:))
!
! * Basin number id
!
YVAR ='NUM_BAS'
CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,ZREAD)
WHERE(ZREAD==XUNDEF)ZREAD=0.0
TPG%NBASID(:,:)=INT(ZREAD(:,:))
!
TPG%NBASMIN = MINVAL(TPG%NBASID(:,:),TPG%NBASID(:,:)>0)
TPG%NBASMAX = MAXVAL(TPG%NBASID(:,:),TPG%NBASID(:,:)>0)
!
! * Set down stream
!
CALL SETNEXT(KLON,KLAT,TPG%NGRCN,TPG%NNEXTX,TPG%NNEXTY)
!
! * Set area size
!
YVAR ='CELL_AREA'
CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,TPG%XAREA)
WHERE(.NOT.TPG%GMASK(:,:))TPG%XAREA(:,:)=XUNDEF
!
! * Distance between grids with the meandering ratio
!
YVAR ='RIVLEN'
CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,TPG%XLEN)
WHERE(.NOT.TPG%GMASK(:,:))TPG%XLEN(:,:)=XUNDEF
!
! * Meandering ration
!
IF (ZGRID_RES < 0.5.AND..NOT.LCALCRIVLEN)THEN
  XRATMED = 1.0
ENDIF
!
! * Land mask for Greenland and Antarctica
!
YVAR ='GREEN_ANT'
CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,ZREAD)
DO JLAT=1,KLAT
  DO JLON=1,KLON
    IF(ZREAD(JLON,JLAT)==2.0)THEN
          TPG%GMASK_ANT(JLON,JLAT)=.TRUE.
    ELSEIF(ZREAD(JLON,JLAT)==1.0)THEN
          TPG%GMASK_GRE(JLON,JLAT)=.TRUE.
    ENDIF
  ENDDO
ENDDO
!
! * Read lake parameters
!
IF(CLAKE=='MLK')THEN
!
  YVAR ='LAKE_ID_IN'
  CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,ZREAD)
  TPLK%NLAKE_ID_IN(:,:) = 0
  WHERE(ZREAD/=XUNDEF) TPLK%NLAKE_ID_IN = INT(ZREAD)
  WHERE(TPLK%NLAKE_ID_IN(:,:)>0) TPG%GMASK_LAKE_IN(:,:)=.TRUE.
!
  YVAR ='LAKE_ID_NW'
  CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,ZREAD)
  TPLK%NLAKE_ID_NW(:,:) = 0
  WHERE(ZREAD/=XUNDEF) TPLK%NLAKE_ID_NW = INT(ZREAD)
  WHERE(TPLK%NLAKE_ID_NW(:,:)>0) TPG%GMASK_LAKE_NW(:,:)=.TRUE.
!
  YVAR ='FRAC_LAKE'
  CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,TPLK%XFRAC_LAKE)
!
  WHERE(TPG%GMASK_LAKE_IN .AND. .NOT.TPG%GMASK_LAKE_NW)
    TPLK%XFRAC_LAKE = MIN(TPLK%XFRAC_LAKE,0.95)
  ENDWHERE
!
  YVAR ='LAKE_A'
  CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,TPLK%XLAKE_A)
!
  YVAR ='WEIR_Z'
  CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,TPLK%XWEIR_Z)
!
  YVAR ='WEIR_W'
  CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,TPLK%XWEIR_W)
!
  DO JLAT = 1,KLAT
    DO JLON = 1,KLON
      IF (TPG%GMASK_LAKE_NW(JLON,JLAT)) THEN
        TP%XWEIR_Z(JLON,JLAT) = TPLK%XWEIR_Z(TPLK%NLAKE_ID_NW(JLON,JLAT))
        TP%XWEIR_W(JLON,JLAT) = TPLK%XWEIR_W(TPLK%NLAKE_ID_NW(JLON,JLAT))
      ENDIF
    ENDDO
  ENDDO
!
ENDIF
!
DEALLOCATE(ZREAD)
DEALLOCATE(ZLON)
DEALLOCATE(ZLAT)
!
! * Variable velocity schemes variables
!
IF(CVIT == 'VAR')THEN
!
  YVAR ='N_RIV'
  CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,TP%XN)
!
  YVAR ='WIDTHRIV'
  CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,TP%XWIDTH)
!
  YVAR ='SLOPERIV'
  CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,TP%XSLOPEBED)
!
  TPG%GMASK_VEL(:,:)=TPG%GMASK(:,:)
  WHERE(TPG%NSEQ(:,:)==0.OR.TP%XWIDTH(:,:)>=XUNDEF-1.0.OR.TPG%GMASK_LAKE_NW(:,:))
        TP%XSLOPEBED(:,:)= 0.0
        TP%XWIDTH   (:,:)= 0.0
        TP%XN       (:,:)= 0.0
        TPG%GMASK_VEL(:,:)=.FALSE.
  ENDWHERE
!
ENDIF
!
! * Groundwater parameters
!
IF(CGROUNDW/='DEF')THEN
!
! * Calculate the groundwater transfert time
!
  YVAR ='TAUG'
  CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,TP%XTAUG)
!
  WHERE(TPG%NSEQ(:,:)==0)
        TP%XTAUG(:,:)=XUNDEF
  ENDWHERE
!
  TPG%GMASK_GW(:,:)=TPG%GMASK(:,:)
  WHERE(TP%XTAUG(:,:)/=XUNDEF.AND..NOT.TPG%GMASK_LAKE_NW(:,:))
        TP%XTAUG(:,:)=TP%XTAUG(:,:)*XDAY
        TPG%GMASK_GW(:,:)=.TRUE.
  ELSEWHERE
        TPG%GMASK_GW(:,:)=.FALSE.
  ENDWHERE
!
ENDIF
!
IF(CGROUNDW == 'DIF')THEN
!
! * Diffusive groundwater scheme variables
!
  YVAR = 'NUM_AQUI'
  CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,TP%XNUM_AQUI)
!
  YVAR = 'WEFF'
  CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,TP%XWEFF)
!
  YVAR = 'TRANS'
  CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,TP%XTRANS)
!
  ALLOCATE(TP%XTABGW_F (KLON,KLAT,NDIMTAB))
  ALLOCATE(TP%XTABGW_H (KLON,KLAT,NDIMTAB))
!
  YVAR ='TABGW_F'
  CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,TP%XTABGW_F)
!
  YVAR ='TABGW_H'
  CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,TP%XTABGW_H)
!
ELSE
!
  ALLOCATE(TP%XTABGW_F (0,0,0))
  ALLOCATE(TP%XTABGW_H (0,0,0))
!
ENDIF
!
IF(CGROUNDW=='DIF'.OR.LBACKWATER)THEN
!
  YVAR = 'TOPO_RIV'
  CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,TP%XTOPO_RIV)
!
ENDIF
!
IF (LFLOOD.OR.CGROUNDW=='DIF'.OR.XBANKSLOPE>0.0) THEN
!
  YVAR ='RIVDEPTH'
  CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,TP%XHC_BED)
!
ENDIF
!
! * Calculate floodplains parameters
!
IF(LFLOOD)THEN
!
  YVAR ='NFLOOD'
  CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,TP%XN_FLOOD)
!
  TPG%GMASK_FLD(:,:)=TPG%GMASK_VEL(:,:)
  WHERE(TP%XN_FLOOD(:,:)==XUNDEF.OR.TPG%GMASK_LAKE_NW(:,:))
        TPG%GMASK_FLD(:,:)=.FALSE.
  ENDWHERE
!
  ALLOCATE(ZREAD3D(KLON,KLAT,NDIMTAB))
!
  ALLOCATE(TP%XTAB_F (KLON,KLAT,NDIMTAB+1))
  ALLOCATE(TP%XTAB_H (KLON,KLAT,NDIMTAB+1))
  ALLOCATE(TP%XTAB_VF(KLON,KLAT,NDIMTAB+1))
!
  TP%XTAB_F (:,:,:)=XUNDEF
  TP%XTAB_H (:,:,:)=XUNDEF
  TP%XTAB_VF(:,:,:)=XUNDEF
!
  WHERE(TPG%GMASK_FLD(:,:))
      TP%XTAB_F (:,:,1)=0.0
      TP%XTAB_H (:,:,1)=0.0
      TP%XTAB_VF(:,:,1)=0.0
  ENDWHERE
!
  YVAR ='TABF'
  CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,ZREAD3D)
  TP%XTAB_F(:,:,2:NDIMTAB+1)=ZREAD3D(:,:,:)
!
  YVAR ='TABH'
  CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,ZREAD3D)
  TP%XTAB_H(:,:,2:NDIMTAB+1)=ZREAD3D(:,:,:)
!
  YVAR ='TABVF'
  CALL READ_TRIP(NLISTING,YFILE_PARAM,YVAR,ZREAD3D)
  TP%XTAB_VF(:,:,2:NDIMTAB+1)=ZREAD3D(:,:,:)
!
  DEALLOCATE(ZREAD3D)
!
ELSE
!
  ALLOCATE(TP%XTAB_F (0,0,0))
  ALLOCATE(TP%XTAB_H (0,0,0))
  ALLOCATE(TP%XTAB_VF(0,0,0))
!
ENDIF
!
!-------------------------------------------------------------------------------
! * Read initial and historical variables
!-------------------------------------------------------------------------------
!
YVAR ='SURF_STO'
CALL READ_TRIP(NLISTING,YFILE_INIT,YVAR,TP%XSURF_STO)
!
IF(CGROUNDW=='CST')THEN
!
  YVAR ='GROUND_STO'
  CALL READ_TRIP(NLISTING,YFILE_INIT,YVAR,TP%XGROUND_STO)
!
ELSEIF(CGROUNDW=='DIF')THEN
!
  YVAR ='HGROUND'
  CALL READ_TRIP(NLISTING,YFILE_INIT,YVAR,TP%XHGROUND)
!
ENDIF
!
IF(LFLOOD)THEN
!
  YVAR ='FLOOD_STO'
  CALL READ_TRIP(NLISTING,YFILE_INIT,YVAR,TP%XFLOOD_STO)
!
  YVAR ='FFLOOD'
  CALL READ_TRIP(NLISTING,YFILE_INIT,YVAR,TP%XFFLOOD)
!
  YVAR ='HFLOOD'
  CALL READ_TRIP(NLISTING,YFILE_INIT,YVAR,TP%XHFLOOD)
!
ENDIF
!
IF(CLAKE=='MLK')THEN
!
  YVAR = 'LAKE_STO'
  CALL READ_TRIP(NLISTING,YFILE_INIT,YVAR,TPLK%XLAKE_STO)
!
  DO JLAT = 1,KLAT
    DO JLON = 1,KLON
      IF(TPG%GMASK_LAKE_NW(JLON,JLAT))THEN
        TP%XSURF_STO(JLON,JLAT) = TPLK%XLAKE_STO(TPLK%NLAKE_ID_NW(JLON,JLAT))
      ENDIF
    ENDDO
  ENDDO
!
ENDIF
!
!-------------------------------------------------------------------------------
! * Initial Conditions
!-------------------------------------------------------------------------------
!
ALLOCATE(ZGW_STO (KLON,KLAT))
ALLOCATE(ZWTD    (KLON,KLAT))
! ALLOCATE(ZHSTREAM(KLON,KLAT))
! ALLOCATE(ZVEL    (KLON,KLAT))
ZWTD    (:,:) = 0.0
ZGW_STO (:,:) = 0.0
! ZHSTREAM(:,:) = 0.0
! ZVEL    (:,:) = 0.0
!
! IF(CVIT == 'VAR')THEN
!   CALL TRIP_HS_VEL(XTSTEP,TPG%GMASK_VEL,            &
!                    TPG%XLEN,TP%XWIDTH,TP%XSLOPEBED, &
!                    TP%XN,TP%XSURF_STO,ZHSTREAM,ZVEL )
! ENDIF
!
IF(CGROUNDW=='DIF')THEN
  WHERE(TPG%GMASK_GW(:,:))
    ZGW_STO(:,:)=(XGWDZMAX+TP%XHGROUND(:,:)-TP%XTOPO_RIV(:,:))*TP%XWEFF(:,:)*XRHOLW
    ZWTD   (:,:)=TP%XHGROUND(:,:)-TP%XTOPO_RIV(:,:)
  ENDWHERE
ENDIF
!
! * Fraction, width, water depth of floodplains
!
IWORK=0
!
IF(LFLOOD)THEN
!
  WHERE(TPG%GMASK(:,:).AND.TPG%GMASK_FLD(:,:))
        TP%XFLOOD_LEN(:,:) = XRATMED*SQRT(TP%XFFLOOD(:,:)*TPG%XAREA(:,:))
        TP%XFLOOD_LEN(:,:) = MIN(TPG%XLEN(:,:),TP%XFLOOD_LEN(:,:))

  ELSEWHERE
        TP%XFLOOD_LEN(:,:) = 0.0
        TP%XWFLOOD   (:,:) = 0.0
        TP%XFFLOOD   (:,:) = 0.0
        TP%XHFLOOD   (:,:) = 0.0
        TP%XFLOOD_STO(:,:) = 0.0
  ENDWHERE
!
!
  WHERE(TP%XFFLOOD(:,:)>0.0.AND.TPG%GMASK_FLD(:,:))
        TP%XWFLOOD(:,:) = TPG%XAREA(:,:) * TP%XFFLOOD(:,:) / TP%XFLOOD_LEN(:,:)
  ELSEWHERE
        TP%XWFLOOD(:,:) = 0.0
  ENDWHERE
!
  INI   =COUNT(TPG%GMASK)
  IWORK =COUNT(TPG%GMASK_FLD)
  IFLOOD=COUNT(TP%XFFLOOD>0.0)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
WRITE(NLISTING,*)'Coupling_time_step :           ',PTSTEP_RUN
WRITE(NLISTING,*)'Output_time_step :             ',PTSTEP_DIAG
WRITE(NLISTING,*)'TRIP_time_step :               ',XTSTEP
WRITE(NLISTING,*)''
WRITE(NLISTING,*)'Sequence max :                 ',TPG%NSEQMAX
WRITE(NLISTING,*)''
WRITE(NLISTING,*)'MEANDERING RATIO FIXED TO      ',XRATMED
WRITE(NLISTING,*)'CELL LENGTH MIN, MAX (km):     ',MINVAL(TPG%XLEN/1.E3, TPG%GMASK),  &
                                                   MAXVAL(TPG%XLEN/1.E3, TPG%GMASK)

WRITE(NLISTING,*)'CELL AREA MIN, MAX (km²):      ',MINVAL(TPG%XAREA/1.E6,TPG%GMASK),  &
                                                   MAXVAL(TPG%XAREA/1.E6,TPG%GMASK)
WRITE(NLISTING,*)''
IF(CGROUNDW=='CST')THEN
  WRITE(NLISTING,*)''
  WRITE(NLISTING,*)'Ground transf. time MIN, MAX : ',MINVAL(TP%XTAUG/XDAY,TPG%GMASK_GW),  &
                                                     MAXVAL(TP%XTAUG/XDAY,TPG%GMASK_GW)
ELSEIF(CGROUNDW=='DIF')THEN
  WRITE(NLISTING,*)''
  WRITE(NLISTING,*)'Ground transf. time MIN, MAX : ',MINVAL(TP%XTAUG/XDAY,TPG%GMASK_GW),  &
                                                     MAXVAL(TP%XTAUG/XDAY,TPG%GMASK_GW)
  WRITE(NLISTING,*)'Effective porosity MIN, MAX  : ',MINVAL(TP%XWEFF,     TPG%GMASK_GW),  &
                                                     MAXVAL(TP%XWEFF,     TPG%GMASK_GW)
  WRITE(NLISTING,*)'Transmissivity      MIN, MAX : ',MINVAL(TP%XTRANS,    TPG%GMASK_GW),  &
                                                     MAXVAL(TP%XTRANS,    TPG%GMASK_GW)
ENDIF
IF(CVIT == 'VAR')THEN
  WRITE(NLISTING,*)'WIDTH_RIVER MIN, MAX :         ',MINVAL(TP%XWIDTH,   TPG%GMASK_VEL),  &
                                                     MAXVAL(TP%XWIDTH,   TPG%GMASK_VEL)
  WRITE(NLISTING,*)'N MANNING COEF MIN, MAX :      ',MINVAL(TP%XN,       TPG%GMASK_VEL),  &
                                                     MAXVAL(TP%XN,       TPG%GMASK_VEL)
  WRITE(NLISTING,*)'RIVER SLOPE MIN, MAX :         ',MINVAL(TP%XSLOPEBED,TPG%GMASK_VEL),  &
                                                     MAXVAL(TP%XSLOPEBED,TPG%GMASK_VEL)
!   WRITE(NLISTING,*)''
!   WRITE(NLISTING,*)'Initial river height         : ',MINVAL(ZHSTREAM,  TPG%GMASK_VEL),  &
!                                                      MAXVAL(ZHSTREAM,  TPG%GMASK_VEL)
ENDIF
WRITE(NLISTING,*)''
WRITE(NLISTING,*)'Initial river storage        : ',MINVAL(TP%XSURF_STO/TPG%XAREA, TPG%GMASK),  &
                                                   MAXVAL(TP%XSURF_STO/TPG%XAREA, TPG%GMASK)
IF(CGROUNDW=='CST')THEN
  WRITE(NLISTING,*)''
  WRITE(NLISTING,*)'Initial ground storage       : ',MINVAL(TP%XGROUND_STO/TPG%XAREA,TPG%GMASK_GW),  &
                                                     MAXVAL(TP%XGROUND_STO/TPG%XAREA,TPG%GMASK_GW)
ELSEIF(CGROUNDW=='DIF')THEN
  WRITE(NLISTING,*)''
  WRITE(NLISTING,*)'Initial gw elevation         : ',MINVAL(TP%XHGROUND,TPG%GMASK_GW),  &
                                                     MAXVAL(TP%XHGROUND,TPG%GMASK_GW)
  WRITE(NLISTING,*)'Initial water table depth    : ',MINVAL(ZWTD,       TPG%GMASK_GW),  &
                                                     MAXVAL(ZWTD,       TPG%GMASK_GW)
  WRITE(NLISTING,*)'Initial gw storage           : ',MINVAL(ZGW_STO,TPG%GMASK_GW),  &
                                                     MAXVAL(ZGW_STO,TPG%GMASK_GW)
ENDIF
WRITE(NLISTING,*)''

IF(LFLOOD)THEN
  WRITE(NLISTING,*)'N FLOOD FIXED TO               ',MINVAL(TP%XN_FLOOD,TPG%GMASK_FLD)
  WRITE(NLISTING,*)'RIVER DEPTH MIN, MAX :         ',MINVAL(TP%XHC_BED, TPG%GMASK_FLD),  &
                                                     MAXVAL(TP%XHC_BED, TPG%GMASK_FLD)
  WRITE(NLISTING,*)''
  WRITE(NLISTING,*)'Number of potential flood cell : ',IWORK,'on',INI
  WRITE(NLISTING,*)'          %                      ',100.0*(FLOAT(IWORK)/FLOAT(INI))
  WRITE(NLISTING,*)'Number of actual flood cell :    ',IFLOOD,'on',IWORK
  WRITE(NLISTING,*)'          %                      ',100.0*(FLOAT(IFLOOD)/FLOAT(IWORK))
  WRITE(NLISTING,*)'% of flooded area in the domain :',100.0*SUM(TP%XFFLOOD*TPG%XAREA)/SUM(TPG%XAREA)
  WRITE(NLISTING,*)''
  WRITE(NLISTING,*)'Initial flood depth (m) :      ',MINVAL(TP%XHFLOOD,                 TPG%GMASK_FLD), &
                                                     MAXVAL(TP%XHFLOOD,                 TPG%GMASK_FLD)
  WRITE(NLISTING,*)'Initial flood fraction :       ',MINVAL(TP%XFFLOOD,                 TPG%GMASK_FLD), &
                                                     MAXVAL(TP%XFFLOOD,                 TPG%GMASK_FLD)
  WRITE(NLISTING,*)'Initial flood volume m3/1E9 :  ',MINVAL(TP%XFLOOD_STO/(XRHOLW*1.E9),TPG%GMASK_FLD), &
                                                     MAXVAL(TP%XFLOOD_STO/(XRHOLW*1.E9),TPG%GMASK_FLD)
  WRITE(NLISTING,*)'Initial flood length (km):     ',MINVAL(TP%XFLOOD_LEN/1.E3,         TPG%GMASK_FLD), &
                                                     MAXVAL(TP%XFLOOD_LEN/1.E3,         TPG%GMASK_FLD)
  WRITE(NLISTING,*)'Initial flood WIDTH (km) :     ',MINVAL(TP%XWFLOOD/1.E3,            TPG%GMASK_FLD), &
                                                     MAXVAL(TP%XWFLOOD/1.E3,            TPG%GMASK_FLD)
  WRITE(NLISTING,*)''
ENDIF
!
! DEALLOCATE(ZVEL)
! DEALLOCATE(ZHSTREAM)
DEALLOCATE(ZGW_STO)
DEALLOCATE(ZWTD)
!
!-------------------------------------------------------------------------------
! * Alloc diag
!-------------------------------------------------------------------------------
!
CALL ALLOC_TRIP_DIAG(TPDG,KLON,KLAT,KLAKE_NUM)
!
!-------------------------------------------------------------------------------
! * Create diag files
!-------------------------------------------------------------------------------
!
IF (.NOT.OXIOS) THEN
  !
  YFILE  = YDIAG
  YTITLE = 'TRIP high frequency outputs'
  CALL OUTPUT_DATE(YUNITTIME,XTIME_DIAG)
  !
  CALL INIT_TRIP_DIAG(TPDG, TPG, &
                      NLISTING,YFILE,KLON,KLAT,KLAKE_NUM,YTITLE,YUNITTIME,.TRUE.)
  !
  !-------------------------------------------------------------------------------
  ! * Create run mean diag file
  !-------------------------------------------------------------------------------
  !
  YFILE     = YRUN
  YTITLE    = 'TRIP run mean outputs'
  WRITE(YTIME,'(i4.4,i2.2)') KYEAR, KMONTH
  YUNITTIME = 'months since '//YTIME(1:4)//'-'//YTIME(5:LEN_TRIM(YTIME))//'-15'
  CALL INIT_TRIP_DIAG(TPDG, TPG, &
                      NLISTING,YFILE,KLON,KLAT,KLAKE_NUM,YTITLE,YUNITTIME,.TRUE.)
!
ENDIF
!
!-------------------------------------------------------------------------------
! * Create restart file
!-------------------------------------------------------------------------------
!
IF(ORESTART)THEN
  YTITLE   ='TRIP restart variables'
  YUNITTIME='-'
  CALL INIT_RESTART_TRIP(TPG, &
                         NLISTING,YFILE_RESTART,KLON,KLAT,KLAKE_NUM,YTITLE,YUNITTIME,.FALSE.)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('INIT_TRIP',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
! * CONTAINS
!-------------------------------------------------------------------------------
!
CONTAINS
!
SUBROUTINE OUTPUT_DATE(HTIMEUNIT,PTIME_DIAG)
!
IMPLICIT NONE
!
CHARACTER(LEN=*), INTENT(OUT) :: HTIMEUNIT
REAL,             INTENT(OUT) :: PTIME_DIAG
!
INTEGER, DIMENSION(3)         :: ITIME, IDATE
INTEGER                       :: INDAYS
INTEGER                       :: ISEC
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('INIT_TRIP:OUTPUT_DATE',0,ZHOOK_HANDLE)
!
IF (PTSTEP_DIAG == FLOOR(PTSTEP_DIAG/86400.)*86400) THEN
  HTIMEUNIT ='days since '
  PTIME_DIAG=86400.
ELSEIF (PTSTEP_DIAG == FLOOR(PTSTEP_DIAG/3600.)*3600) THEN
  HTIMEUNIT ='hours since '
  PTIME_DIAG=3600.
ELSEIF (PTSTEP_DIAG == FLOOR(PTSTEP_DIAG/60.)*60) THEN
  HTIMEUNIT ='minutes since '
  PTIME_DIAG=60.
ELSE
  HTIMEUNIT ='seconds since '
  PTIME_DIAG=1.
ENDIF
!
IDATE(1) = KYEAR
IDATE(2) = KMONTH
IDATE(3) = KDAY
!
ISEC=PTIME
ITIME(1)=FLOOR(ISEC/3600.)
ITIME(2)=FLOOR((ISEC-ITIME(1)*3600)/60.)
ITIME(3)=ISEC-ITIME(1)*3600-ITIME(2)*60
!
CALL WRITE_TIME(IDATE(1),1,"-",HTIMEUNIT)
CALL WRITE_TIME(IDATE(2),0,"-",HTIMEUNIT)
CALL WRITE_TIME(IDATE(3),0,"",HTIMEUNIT)
CALL WRITE_TIME(ITIME(1),1,":",HTIMEUNIT)
CALL WRITE_TIME(ITIME(2),0,":",HTIMEUNIT)
CALL WRITE_TIME(ITIME(3),0,"",HTIMEUNIT)
!
IF (LHOOK) CALL DR_HOOK('INIT_TRIP:OUTPUT_DATE',1,ZHOOK_HANDLE)
!
END SUBROUTINE OUTPUT_DATE
!
SUBROUTINE WRITE_TIME(ITIME,ISPACE,HSEP,HTDATE)
!
INTEGER, INTENT(IN)             :: ITIME
INTEGER, INTENT(IN)             :: ISPACE
CHARACTER(LEN=*), INTENT(IN)    :: HSEP
CHARACTER(LEN=*), INTENT(INOUT) :: HTDATE
CHARACTER(LEN=10)               :: YPAS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('INIT_TRIP:WRITE_TIME',0,ZHOOK_HANDLE)
IF (ITIME.LT.10) THEN
  WRITE(YPAS,'(i1)') ITIME
  IF (ISPACE==1) THEN
    HTDATE=trim(HTDATE)//" 0"//trim(YPAS)//HSEP
  ELSE
    HTDATE=trim(HTDATE)//"0"//trim(YPAS)//HSEP
  ENDIF
ELSE
  IF (ITIME.LT.100) THEN
    WRITE(YPAS,'(i2)') ITIME
  ELSE
    WRITE(YPAS,'(i4)') ITIME
  ENDIF
  IF (ISPACE==1) THEN
    HTDATE=trim(HTDATE)//" "//trim(YPAS)//HSEP
  ELSE
    HTDATE=trim(HTDATE)//trim(YPAS)//HSEP
  ENDIF
ENDIF
IF (LHOOK) CALL DR_HOOK('INIT_TRIP:WRITE_TIME',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_TIME
!
!-------------------------------------------------------------------------------
! * END
!-------------------------------------------------------------------------------
END SUBROUTINE INIT_TRIP





