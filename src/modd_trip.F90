!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!###############
MODULE MODD_TRIP
!###############
!
!!****  *MODD_TRIP - declaration of surface variable for TRIP RRM
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
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       21/05/08
!!      B. Decharme 10/2016  bug surface/groundwater coupling
!!      T. Guinaldo 04/2020    Add MLake
!
!*       0.   DECLARATIONS
!             ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE TRIP_t
!
!-------------------------------------------------------------------------------
!
! Input river geometry Parameters :
!
REAL, POINTER, DIMENSION(:,:) :: XTAUG          ! ground water transfer time   [s]
REAL, POINTER, DIMENSION(:,:) :: XSLOPEBED      ! river bed slopes             [m/m]
REAL, POINTER, DIMENSION(:,:) :: XWIDTH         ! river widths                 [m]
REAL, POINTER, DIMENSION(:,:) :: XN             ! Manning roughness coeficient [-] (0.03 to 0.065)
REAL, POINTER, DIMENSION(:,:) :: XN_FLOOD       ! Manning coeficient over floodplains  [-] (currently 0.1)
REAL, POINTER, DIMENSION(:,:) :: XHC_BED        ! River bed depth              [m]
REAL, POINTER, DIMENSION(:,:) :: XWEFF          ! Porosité efficace
REAL, POINTER, DIMENSION(:,:) :: XTRANS         ! Transmissivité
REAL, POINTER, DIMENSION(:,:) :: XNUM_AQUI      ! Numéro aquifère
REAL, POINTER, DIMENSION(:,:) :: XNEAR_AQUI     ! Nearest aquifer
REAL, POINTER, DIMENSION(:,:) :: XTOPO_RIV      ! River elevation              [m]
!
!-------------------------------------------------------------------------------
!
! Time varing variables :
!
REAL, POINTER, DIMENSION(:,:) :: XSURF_STO        ! river channel storage        [kg]
REAL, POINTER, DIMENSION(:,:) :: XGROUND_STO      ! groundwater storage          [kg]
REAL, POINTER, DIMENSION(:,:) :: XFLOOD_STO       ! Floodplain water storage     [kg]
REAL, POINTER, DIMENSION(:,:) :: XHGROUND         ! Groudwater height            [m]
REAL, POINTER, DIMENSION(:,:) :: XHFLOOD          ! Floodplain water depth       [m]
REAL, POINTER, DIMENSION(:,:) :: XFFLOOD          ! Floodplain grid-cell fraction [-]
REAL, POINTER, DIMENSION(:,:) :: XWFLOOD          ! Floodplain width             [m]
REAL, POINTER, DIMENSION(:,:) :: XFLOOD_LEN       ! Floodplain lenght            [m]
!
!-------------------------------------------------------------------------------
!
! Floodplain fonctions :
!
REAL, POINTER, DIMENSION(:,:,:) :: XTAB_F         ! Flood fraction array
REAL, POINTER, DIMENSION(:,:,:) :: XTAB_H         ! Topo height array
REAL, POINTER, DIMENSION(:,:,:) :: XTAB_VF        ! Flood volume array
!
!-------------------------------------------------------------------------------
!
! Groundwater fonctions :
!
REAL, POINTER, DIMENSION(:,:,:) :: XTABGW_F       ! Groundwater fraction array
REAL, POINTER, DIMENSION(:,:,:) :: XTABGW_H       ! Topo height array
!
!-------------------------------------------------------------------------------
!
! Groundwater fonctions :
!
REAL, POINTER, DIMENSION(:,:) :: XWEIR_Z          ! Lake weir height      [m]
REAL, POINTER, DIMENSION(:,:) :: XWEIR_W          ! Lake weir width       [m]
!
!-------------------------------------------------------------------------------
!
! Coupling variable with other models :
!
REAL, POINTER, DIMENSION(:,:) :: XCPL_FWTD         ! grid-cell fraction of water table to rise
REAL, POINTER, DIMENSION(:,:) :: XCPL_WTD          ! Water table depth            [m]
!
REAL, POINTER, DIMENSION(:,:) :: XCPL_FFLOOD       ! Flood fraction    [-]
REAL, POINTER, DIMENSION(:,:) :: XCPL_PIFLOOD      ! Floodplains potential infiltration  [kg/m2]
!
REAL, POINTER, DIMENSION(:,:) :: XCPL_RIVDIS       ! River discharges             [kg/m2]
REAL, POINTER, DIMENSION(:,:) :: XCPL_CALVGRE      ! Calving flux over greenland  [kg/m2]
REAL, POINTER, DIMENSION(:,:) :: XCPL_CALVANT      ! Calving flux over antarctica [kg/m2]
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
END TYPE TRIP_t
!
 CONTAINS
!
SUBROUTINE TRIP_INIT(YTRIP)
TYPE(TRIP_t), INTENT(INOUT) :: YTRIP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TRIP:TRIP_INIT",0,ZHOOK_HANDLE)
!
  NULLIFY(YTRIP%XTAUG)
  NULLIFY(YTRIP%XSLOPEBED)
  NULLIFY(YTRIP%XWIDTH)
  NULLIFY(YTRIP%XN)
  NULLIFY(YTRIP%XN_FLOOD)
  NULLIFY(YTRIP%XHC_BED)
  NULLIFY(YTRIP%XSURF_STO)
  NULLIFY(YTRIP%XGROUND_STO)
  NULLIFY(YTRIP%XFLOOD_STO)
  NULLIFY(YTRIP%XHFLOOD)
  NULLIFY(YTRIP%XFFLOOD)
  NULLIFY(YTRIP%XWFLOOD)
  NULLIFY(YTRIP%XFLOOD_LEN)
  NULLIFY(YTRIP%XWEFF)
  NULLIFY(YTRIP%XTRANS)
  NULLIFY(YTRIP%XNUM_AQUI)
  NULLIFY(YTRIP%XNEAR_AQUI)
  NULLIFY(YTRIP%XTOPO_RIV)
  NULLIFY(YTRIP%XTABGW_H)
  NULLIFY(YTRIP%XHGROUND)
  NULLIFY(YTRIP%XTAB_F)
  NULLIFY(YTRIP%XTAB_H)
  NULLIFY(YTRIP%XTAB_VF)
  NULLIFY(YTRIP%XTABGW_F)
  NULLIFY(YTRIP%XTABGW_H)
  NULLIFY(YTRIP%XWEIR_Z)
  NULLIFY(YTRIP%XWEIR_W)
!
  NULLIFY(YTRIP%XCPL_WTD)
  NULLIFY(YTRIP%XCPL_FWTD)
  NULLIFY(YTRIP%XCPL_RIVDIS)
  NULLIFY(YTRIP%XCPL_FFLOOD)
  NULLIFY(YTRIP%XCPL_PIFLOOD)
  NULLIFY(YTRIP%XCPL_CALVGRE)
  NULLIFY(YTRIP%XCPL_CALVANT)
!
IF (LHOOK) CALL DR_HOOK("MODD_TRIP:TRIP_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE TRIP_INIT

!
END MODULE MODD_TRIP
