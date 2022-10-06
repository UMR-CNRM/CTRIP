!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!##################
MODULE MODD_TRIP_GRID
!##################
!
!!****  *MODD_TRIP_GRID - declaration of grid for TRIP scheme
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
!!      Original       05/2008
!!      T. Guinaldo 04/2020    Add MLake
!
!*       0.   DECLARATIONS
!             ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
TYPE TRIP_GRID_t
!
!-------------------------------------------------------------------------------
!
INTEGER, POINTER, DIMENSION(:,:) :: NGRCN       ! Flow direction (1->8)
INTEGER, POINTER, DIMENSION(:,:) :: NSEQ        ! River sequence
INTEGER                          :: NSEQMAX     ! maximum down flow
INTEGER, POINTER, DIMENSION(:,:) :: NNEXTX      ! returns x and y point
INTEGER, POINTER, DIMENSION(:,:) :: NNEXTY      ! of destination grid:
!                                                        8 1 2
!                                                        7   3
!                                                        6 5 4
INTEGER, POINTER, DIMENSION(:,:) :: NBASID      ! basin number id
INTEGER                          :: NBASMIN     ! minimum basin number
INTEGER                          :: NBASMAX     ! maximum basin number
!
!-------------------------------------------------------------------------------
!
REAL, POINTER,  DIMENSION(:)     :: XTRIP_GRID ! list of parameters used to define the grid
!
!-------------------------------------------------------------------------------
!
REAL, POINTER, DIMENSION(:,:)    :: XAREA      ! 2d grid area [m*m]
REAL, POINTER, DIMENSION(:,:)    :: XLEN       ! distance between grids       [m]
!
!-------------------------------------------------------------------------------
!
LOGICAL, POINTER, DIMENSION(:,:) :: GMASK      !Logical Mask for TRIP grid
LOGICAL, POINTER, DIMENSION(:,:) :: GMASK_VEL  !Logical Mask for variable velocity scheme
LOGICAL, POINTER, DIMENSION(:,:) :: GMASK_GW   !Logical Mask for Groundwater grid
LOGICAL, POINTER, DIMENSION(:,:) :: GMASK_FLD  !Logical Mask for floodplain scheme
LOGICAL, POINTER, DIMENSION(:,:) :: GMASK_GRE  !Logical Mask for Greenland grid
LOGICAL, POINTER, DIMENSION(:,:) :: GMASK_ANT  !Logical Mask for Antartactic grid
LOGICAL, POINTER, DIMENSION(:,:) :: GMASK_LAKE_IN ! Lake mask grid for runoff inflow
LOGICAL, POINTER, DIMENSION(:,:) :: GMASK_LAKE_NW ! Lake mask grid for river network
!
!-------------------------------------------------------------------------------
!
END TYPE TRIP_GRID_t
!
CONTAINS
!
SUBROUTINE TRIP_GRID_NULLIFY(YTRIP_GRID)
TYPE(TRIP_GRID_t), INTENT(INOUT) :: YTRIP_GRID
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK("MODD_TRIP_GRID:TRIP_GRID_NULLIFY",0,ZHOOK_HANDLE)
!
NULLIFY(YTRIP_GRID%NGRCN)
NULLIFY(YTRIP_GRID%NSEQ)
NULLIFY(YTRIP_GRID%NNEXTX)
NULLIFY(YTRIP_GRID%NNEXTY)
NULLIFY(YTRIP_GRID%NBASID)
NULLIFY(YTRIP_GRID%XTRIP_GRID)
NULLIFY(YTRIP_GRID%XAREA)
NULLIFY(YTRIP_GRID%XLEN)
NULLIFY(YTRIP_GRID%GMASK)
NULLIFY(YTRIP_GRID%GMASK_VEL)
NULLIFY(YTRIP_GRID%GMASK_GW)
NULLIFY(YTRIP_GRID%GMASK_FLD)
NULLIFY(YTRIP_GRID%GMASK_GRE)
NULLIFY(YTRIP_GRID%GMASK_ANT)
NULLIFY(YTRIP_GRID%GMASK_LAKE_IN)
NULLIFY(YTRIP_GRID%GMASK_LAKE_NW)
!
YTRIP_GRID%NSEQMAX=0
YTRIP_GRID%NBASMIN=0
YTRIP_GRID%NBASMAX=0
!
IF (LHOOK) CALL DR_HOOK("MODD_TRIP_GRID:TRIP_GRID_NULLIFY",1,ZHOOK_HANDLE)
!
END SUBROUTINE TRIP_GRID_NULLIFY


END MODULE MODD_TRIP_GRID
