!##################
MODULE MODD_TRIP_STATE
!##################
!
!!****  *MODD_TRIP_STATE - declaration of state for TRIP scheme
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      S. Munier   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       07/2019
!
!*       0.   DECLARATIONS
!             ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
TYPE TRIP_STATE_t
!
!-------------------------------------------------------------------------------
!
INTEGER                          :: NNSTATE      ! size of state vector
INTEGER                          :: NNSTATE_P    ! size of state vector on each proc
INTEGER, POINTER, DIMENSION(:)   :: NSTATE_LEN   ! state vector size of all procs
INTEGER                          :: NSTATE_LEN_P ! state vector size for each proc
INTEGER, POINTER, DIMENSION(:)   :: NSTATE_POS   ! start index in state vector for each proc
INTEGER, POINTER, DIMENSION(:,:) :: NSTATE_IND   ! indices of grid pixels in state vector
INTEGER, POINTER, DIMENSION(:,:) :: NSTATE_IND_P ! indices of grid pixels in state vector for each proc
INTEGER, POINTER, DIMENSION(:)   :: NSTATE_LON   ! longitude indices of state elements
INTEGER, POINTER, DIMENSION(:)   :: NSTATE_LAT   ! latitude indices of state elements
INTEGER, POINTER, DIMENSION(:)   :: NSTATE_LON_P ! longitude indices of state elements for each proc
INTEGER, POINTER, DIMENSION(:)   :: NSTATE_LAT_P ! latitude indices of state elements for each proc
!
!-------------------------------------------------------------------------------
!
INTEGER, POINTER, DIMENSION(:)   :: NGRCN        ! Flow direction (1->8)
INTEGER, POINTER, DIMENSION(:)   :: NSEQ         ! River sequence
INTEGER                          :: NSEQMAX      ! maximum down flow
INTEGER, POINTER, DIMENSION(:)   :: NNEXTST      ! next state
INTEGER, POINTER, DIMENSION(:,:) :: NPREVST      ! previous states
INTEGER, POINTER, DIMENSION(:)   :: NBASID       ! basin number id
INTEGER                          :: NBASMIN      ! minimum basin number
INTEGER                          :: NBASMAX      ! maximum basin number
!
!-------------------------------------------------------------------------------
!
REAL,    POINTER, DIMENSION(:)   :: XAREA        ! 2d grid area [m*m]
REAL,    POINTER, DIMENSION(:)   :: XLEN         ! distance between grids       [m]
!
!-------------------------------------------------------------------------------
!
LOGICAL, POINTER, DIMENSION(:)   :: GMASK        ! Logical Mask for TRIP grid
LOGICAL, POINTER, DIMENSION(:)   :: GMASK_VEL    ! Logical Mask for variable velocity scheme
LOGICAL, POINTER, DIMENSION(:)   :: GMASK_GW     ! Logical Mask for Groundwater grid
LOGICAL, POINTER, DIMENSION(:)   :: GMASK_FLD    ! Logical Mask for floodplain scheme
LOGICAL, POINTER, DIMENSION(:)   :: GMASK_LAKE   ! Logical Mask for lake grid cells
LOGICAL, POINTER, DIMENSION(:)   :: GMASK_GRE    ! Logical Mask for Greenland grid
LOGICAL, POINTER, DIMENSION(:)   :: GMASK_ANT    ! Logical Mask for Antartactic grid
!
!-------------------------------------------------------------------------------
!
! Input river geometry Parameters :
!
REAL, POINTER, DIMENSION(:) :: XTAUG          ! ground water transfer time   [s]
REAL, POINTER, DIMENSION(:) :: XSLOPEBED      ! river bed slopes             [m/m]
REAL, POINTER, DIMENSION(:) :: XWIDTH         ! river widths                 [m]
REAL, POINTER, DIMENSION(:) :: XN             ! Manning roughness coeficient [-] (0.03 to 0.065)
REAL, POINTER, DIMENSION(:) :: XN_FLOOD       ! Manning coeficient over floodplains  [-] (currently 0.1)
REAL, POINTER, DIMENSION(:) :: XHC_BED        ! River bed depth              [m]
REAL, POINTER, DIMENSION(:) :: XWEFF          ! Porosité efficace
REAL, POINTER, DIMENSION(:) :: XTRANS         ! Transmissivité
REAL, POINTER, DIMENSION(:) :: XNUM_AQUI      ! Numéro aquifère
REAL, POINTER, DIMENSION(:) :: XNEAR_AQUI     ! Nearest aquifer
REAL, POINTER, DIMENSION(:) :: XTOPO_RIV      ! River elevation              [m]
!
!-------------------------------------------------------------------------------
!
! Time varing variables :
!
REAL, POINTER, DIMENSION(:) :: XSURF_STO        ! river channel storage        [kg]
REAL, POINTER, DIMENSION(:) :: XGROUND_STO      ! groundwater storage          [kg]
REAL, POINTER, DIMENSION(:) :: XFLOOD_STO       ! Floodplain water storage     [kg]
REAL, POINTER, DIMENSION(:) :: XHGROUND         ! Groudwater height            [m]
REAL, POINTER, DIMENSION(:) :: XHFLOOD          ! Floodplain water depth       [m]
REAL, POINTER, DIMENSION(:) :: XFFLOOD          ! Floodplain grid-cell fraction [-]
REAL, POINTER, DIMENSION(:) :: XWFLOOD          ! Floodplain width             [m]
REAL, POINTER, DIMENSION(:) :: XFLOOD_LEN       ! Floodplain lenght            [m]
!
!-------------------------------------------------------------------------------
!
! Floodplain fonctions :
!
REAL, POINTER, DIMENSION(:,:) :: XTAB_F         ! Flood fraction array
REAL, POINTER, DIMENSION(:,:) :: XTAB_H         ! Topo height array
REAL, POINTER, DIMENSION(:,:) :: XTAB_VF        ! Flood volume array
!
!-------------------------------------------------------------------------------
!
! Groundwater fonctions :
!
REAL, POINTER, DIMENSION(:,:) :: XTABGW_F       ! Groundwater fraction array
REAL, POINTER, DIMENSION(:,:) :: XTABGW_H       ! Topo height array
!
!-------------------------------------------------------------------------------
!
! Lake parameter :
!
INTEGER, POINTER, DIMENSION(:) :: NLAKE_STATE   ! State index of each lake
REAL,    POINTER, DIMENSION(:) :: XWEIR_Z       ! Lake weir height             [m]
REAL,    POINTER, DIMENSION(:) :: XWEIR_W       ! Lake weir width              [m]
!
!-------------------------------------------------------------------------------
!
! Coupling variable with other models :
!
REAL, POINTER, DIMENSION(:) :: XCPL_FWTD         ! grid-cell fraction of water table to rise
REAL, POINTER, DIMENSION(:) :: XCPL_WTD          ! Water table depth            [m]
!
REAL, POINTER, DIMENSION(:) :: XCPL_FFLOOD       ! Flood fraction    [-]
REAL, POINTER, DIMENSION(:) :: XCPL_PIFLOOD      ! Floodplains potential infiltration  [kg/m2]
!
REAL, POINTER, DIMENSION(:) :: XCPL_RIVDIS       ! River discharges             [kg/m2]
REAL, POINTER, DIMENSION(:) :: XCPL_CALVGRE      ! Calving flux over greenland  [kg/m2]
REAL, POINTER, DIMENSION(:) :: XCPL_CALVANT      ! Calving flux over antarctica [kg/m2]
!
!-------------------------------------------------------------------------------
!
END TYPE TRIP_STATE_t
!
CONTAINS
!
SUBROUTINE TRIP_STATE_NULLIFY(YTRIP_STATE)
TYPE(TRIP_STATE_t), INTENT(INOUT) :: YTRIP_STATE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK("MODD_TRIP_STATE:TRIP_STATE_NULLIFY",0,ZHOOK_HANDLE)
!
YTRIP_STATE%NNSTATE=0
YTRIP_STATE%NNSTATE_P=0
YTRIP_STATE%NSTATE_LEN_P=0
YTRIP_STATE%NSEQMAX=0
YTRIP_STATE%NBASMIN=0
YTRIP_STATE%NBASMAX=0
!
NULLIFY(YTRIP_STATE%NSTATE_LEN)
NULLIFY(YTRIP_STATE%NSTATE_POS)
NULLIFY(YTRIP_STATE%NSTATE_IND)
NULLIFY(YTRIP_STATE%NSTATE_IND_P)
NULLIFY(YTRIP_STATE%NSTATE_LON)
NULLIFY(YTRIP_STATE%NSTATE_LAT)
NULLIFY(YTRIP_STATE%NSTATE_LON_P)
NULLIFY(YTRIP_STATE%NSTATE_LAT_P)
!
NULLIFY(YTRIP_STATE%NGRCN)
NULLIFY(YTRIP_STATE%NSEQ)
NULLIFY(YTRIP_STATE%NNEXTST)
NULLIFY(YTRIP_STATE%NPREVST)
NULLIFY(YTRIP_STATE%NBASID)
!
NULLIFY(YTRIP_STATE%XAREA)
NULLIFY(YTRIP_STATE%XLEN)
!
NULLIFY(YTRIP_STATE%GMASK)
NULLIFY(YTRIP_STATE%GMASK_VEL)
NULLIFY(YTRIP_STATE%GMASK_GW)
NULLIFY(YTRIP_STATE%GMASK_FLD)
NULLIFY(YTRIP_STATE%GMASK_LAKE)
NULLIFY(YTRIP_STATE%GMASK_GRE)
NULLIFY(YTRIP_STATE%GMASK_ANT)
!
NULLIFY(YTRIP_STATE%XTAUG)
NULLIFY(YTRIP_STATE%XSLOPEBED)
NULLIFY(YTRIP_STATE%XWIDTH)
NULLIFY(YTRIP_STATE%XN)
NULLIFY(YTRIP_STATE%XN_FLOOD)
NULLIFY(YTRIP_STATE%XHC_BED)
NULLIFY(YTRIP_STATE%XWEFF)
NULLIFY(YTRIP_STATE%XTRANS)
NULLIFY(YTRIP_STATE%XNUM_AQUI)
NULLIFY(YTRIP_STATE%XNEAR_AQUI)
NULLIFY(YTRIP_STATE%XTOPO_RIV)
!
NULLIFY(YTRIP_STATE%XSURF_STO)
NULLIFY(YTRIP_STATE%XGROUND_STO)
NULLIFY(YTRIP_STATE%XFLOOD_STO)
NULLIFY(YTRIP_STATE%XHGROUND)
NULLIFY(YTRIP_STATE%XHFLOOD)
NULLIFY(YTRIP_STATE%XFFLOOD)
NULLIFY(YTRIP_STATE%XWFLOOD)
NULLIFY(YTRIP_STATE%XFLOOD_LEN)
!
NULLIFY(YTRIP_STATE%XTAB_F)
NULLIFY(YTRIP_STATE%XTAB_H)
NULLIFY(YTRIP_STATE%XTAB_VF)
!
NULLIFY(YTRIP_STATE%XTABGW_F)
NULLIFY(YTRIP_STATE%XTABGW_H)
!
NULLIFY(YTRIP_STATE%NLAKE_STATE)
NULLIFY(YTRIP_STATE%XWEIR_Z)
NULLIFY(YTRIP_STATE%XWEIR_W)
!
NULLIFY(YTRIP_STATE%XCPL_FWTD)
NULLIFY(YTRIP_STATE%XCPL_WTD)
!
NULLIFY(YTRIP_STATE%XCPL_FFLOOD)
NULLIFY(YTRIP_STATE%XCPL_PIFLOOD)
!
NULLIFY(YTRIP_STATE%XCPL_RIVDIS)
NULLIFY(YTRIP_STATE%XCPL_CALVGRE)
NULLIFY(YTRIP_STATE%XCPL_CALVANT)
!
IF (LHOOK) CALL DR_HOOK("MODD_TRIP_STATE:TRIP_STATE_NULLIFY",1,ZHOOK_HANDLE)
!
END SUBROUTINE TRIP_STATE_NULLIFY


END MODULE MODD_TRIP_STATE
