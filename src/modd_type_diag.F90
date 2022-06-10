!######################
MODULE MODD_TYPE_DIAG
!######################
!
!!****  *MODD_TYPE_DIAG - declaration of diagnostics for TRIP scheme
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
!!      Original       08/13
!!      S. Munier   03/2020    CTRIP-12D and parallelization
!!      T. Guinaldo 04/2020    Add MLake
!
!*       0.   DECLARATIONS
!             ------------
!
!
IMPLICIT NONE
!
TYPE DIAG
!-------------------------------------------------------------------------------
!
  REAL, DIMENSION(:,:), POINTER :: XSURF_STO   ! River storage             [kg m-2]
  REAL, DIMENSION(:,:), POINTER :: XGROUND_STO ! Groundwater storage       [kg m-2]
  REAL, DIMENSION(:,:), POINTER :: XFLOOD_STO  ! Floodplains storage       [kg m-2]
  REAL, DIMENSION(:,:), POINTER :: XQDIS       ! Discharge                 [m3 s-1]
  REAL, DIMENSION(:,:), POINTER :: XQGF        ! Groundwater flow to river [m3 s-1]
  REAL, DIMENSION(:,:), POINTER :: XVEL        ! Stream flow velocity      [m s-1 ]
  REAL, DIMENSION(:,:), POINTER :: XHS         ! Stream river depth        [m     ]
  REAL, DIMENSION(:,:), POINTER :: XFF         ! TRIP flooded fraction     [-]
  REAL, DIMENSION(:,:), POINTER :: XHF         ! Flood depth               [m     ]
!
  REAL, DIMENSION(:,:), POINTER :: XQFR        ! Flood flow to river          [m3 s-1]
  REAL, DIMENSION(:,:), POINTER :: XQRF        ! River flow to floodplain     [m3 s-1]
  REAL, DIMENSION(:,:), POINTER :: XQIN        ! Inflow to the river          [m3 s-1]
  REAL, DIMENSION(:,:), POINTER :: XVFIN       ! River flow to flood velocity [m s-1]
  REAL, DIMENSION(:,:), POINTER :: XVFOUT      ! Flood flow to river velocity [m s-1]
  REAL, DIMENSION(:,:), POINTER :: XWF         ! Flood width during dt        [m]
  REAL, DIMENSION(:,:), POINTER :: XLF         ! Flood lenght during dt       [m]
  REAL, DIMENSION(:,:), POINTER :: XHSF        ! River-Flood depth comparison [m]
!
  REAL, DIMENSION(:,:), POINTER :: XRUNOFF     ! Input surface runoff            [kg m-2]
  REAL, DIMENSION(:,:), POINTER :: XDRAIN      ! Input deep drainage or recharge [kg m-2]
  REAL, DIMENSION(:,:), POINTER :: XSOURCE     ! Floodplains source (Pf-Ef-If)   [kg m-2]
!
  REAL, DIMENSION(:,:), POINTER :: XHGROUND    ! Groudwater height       [m]
  REAL, DIMENSION(:,:), POINTER :: XQGCELL     ! Grid-cell fluxes budget [m3 s-1]
  REAL, DIMENSION(:,:), POINTER :: XWTD        ! Water table depth       [m]
  REAL, DIMENSION(:,:), POINTER :: XFWTD       ! grid-cell fraction of water table to rise
  REAL, DIMENSION(:,:), POINTER :: XHGHS       ! Hground - Hstream       [m]
!
  REAL, DIMENSION(:),   POINTER :: XLAKE_STO   ! Lake storage            [kg m-2]
  REAL, DIMENSION(:),   POINTER :: XLAKE_OUT   ! Lake outflow            [m3 s-1]
  REAL, DIMENSION(:),   POINTER :: XLAKE_IN    ! Lake inflow             [m3 s-1]
  REAL, DIMENSION(:),   POINTER :: XLAKE_H     ! Lake depth              [m]
!
!-------------------------------------------------------------------------------
!
END TYPE DIAG
!
TYPE DIAG_ST
!-------------------------------------------------------------------------------
!
  REAL, DIMENSION(:), POINTER :: XSURF_STO   ! River storage             [kg m-2]
  REAL, DIMENSION(:), POINTER :: XGROUND_STO ! Groundwater storage       [kg m-2]
  REAL, DIMENSION(:), POINTER :: XFLOOD_STO  ! Floodplains storage       [kg m-2]
  REAL, DIMENSION(:), POINTER :: XQDIS       ! Discharge                 [m3 s-1]
  REAL, DIMENSION(:), POINTER :: XQGF        ! Groundwater flow to river [m3 s-1]
  REAL, DIMENSION(:), POINTER :: XVEL        ! Stream flow velocity      [m s-1 ]
  REAL, DIMENSION(:), POINTER :: XHS         ! Stream river depth        [m     ]
  REAL, DIMENSION(:), POINTER :: XFF         ! TRIP flooded fraction     [-]
  REAL, DIMENSION(:), POINTER :: XHF         ! Flood depth               [m     ]
!
  REAL, DIMENSION(:), POINTER :: XQFR        ! Flood flow to river          [m3 s-1]
  REAL, DIMENSION(:), POINTER :: XQRF        ! River flow to floodplain     [m3 s-1]
  REAL, DIMENSION(:), POINTER :: XQIN        ! Inflow to the river          [m3 s-1]
  REAL, DIMENSION(:), POINTER :: XVFIN       ! River flow to flood velocity [m s-1]
  REAL, DIMENSION(:), POINTER :: XVFOUT      ! Flood flow to river velocity [m s-1]
  REAL, DIMENSION(:), POINTER :: XWF         ! Flood width during dt        [m]
  REAL, DIMENSION(:), POINTER :: XLF         ! Flood lenght during dt       [m]
  REAL, DIMENSION(:), POINTER :: XHSF        ! River-Flood depth comparison [m]
!
  REAL, DIMENSION(:), POINTER :: XRUNOFF     ! Input surface runoff            [kg m-2]
  REAL, DIMENSION(:), POINTER :: XDRAIN      ! Input deep drainage or recharge [kg m-2]
  REAL, DIMENSION(:), POINTER :: XSOURCE     ! Floodplains source (Pf-Ef-If)   [kg m-2]
!
  REAL, DIMENSION(:), POINTER :: XHGROUND    ! Groudwater height       [m]
  REAL, DIMENSION(:), POINTER :: XQGCELL     ! Grid-cell fluxes budget [m3 s-1]
  REAL, DIMENSION(:), POINTER :: XWTD        ! Water table depth       [m]
  REAL, DIMENSION(:), POINTER :: XFWTD       ! grid-cell fraction of water table to rise
  REAL, DIMENSION(:), POINTER :: XHGHS       ! Hground - Hstream       [m]
!
!-------------------------------------------------------------------------------
!
END TYPE DIAG_ST
!
!
END MODULE MODD_TYPE_DIAG
