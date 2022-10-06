!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!##############################
     MODULE MODD_TRIP_LAKE
!##############################
!
!!**** MODD_TRIP_LAKE
!!
!!     PURPOSE
!!     -------
!
!!
!!**   AUTHOR
!!     ------
!!     T.Guinaldo     *Meteo-France*
!!
!!
!!**   MODIFICATIONS
!!     -------------
!!     Original     09/19
!!
!!--------------------------------------
!*     0.  DECLARATIONS
!!     ---------------------------------
!
!
USE YOMHOOK   ,ONLY: LHOOK, DR_HOOK
USE PARKIND1  ,ONLY: JPRB
!
IMPLICIT NONE
!
TYPE TRIP_LAKE_t
!
!---------------------------------------
!
!Input lake scheme parameters:
!
INTEGER                          :: NLAKE_NUM      ! Total number of lakes
!
INTEGER, POINTER, DIMENSION(:,:) :: NLAKE_ID_IN    ! Lake ids used in the inflow calculation
INTEGER, POINTER, DIMENSION(:,:) :: NLAKE_ID_NW    ! Lake ids used in the network calculation
!
REAL,    POINTER, DIMENSION(:,:) :: XFRAC_LAKE     ! Lake fraction with a grid-cell
!
REAL,    POINTER, DIMENSION(:)   :: XLAKE_A        ! Lake area
REAL,    POINTER, DIMENSION(:)   :: XWEIR_Z        ! Weir height
REAL,    POINTER, DIMENSION(:)   :: XWEIR_W        ! Weir width
! REAL,    POINTER, DIMENSION(:)   :: XZ_EQ          ! Lake equivalent depth
! REAL,    POINTER, DIMENSION(:)   :: XNSIGMA        ! Lake n*sigma
!
!Lake time variables
REAL,    POINTER, DIMENSION(:)   :: XLAKE_STO      ! Lake storage
!
!--------------------------------------------
!--------------------------------------------
END TYPE TRIP_LAKE_t
!
CONTAINS
!
SUBROUTINE TRIP_LAKE_NULLIFY(YTRIP)
TYPE(TRIP_LAKE_t), INTENT(INOUT) :: YTRIP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF(LHOOK) CALL DR_HOOK('MODD_TRIP_LAKE:TRIP_LAKE_NULLIFY',0,ZHOOK_HANDLE)
!
YTRIP%NLAKE_NUM = 0
!
NULLIFY(YTRIP%NLAKE_ID_IN)
NULLIFY(YTRIP%NLAKE_ID_NW)
NULLIFY(YTRIP%XFRAC_LAKE)
NULLIFY(YTRIP%XLAKE_A)
NULLIFY(YTRIP%XWEIR_Z)
NULLIFY(YTRIP%XWEIR_W)
! NULLIFY(YTRIP%XZ_EQ)
! NULLIFY(YTRIP%XNSIGMA)
!
NULLIFY(YTRIP%XLAKE_STO)
!
IF(LHOOK) CALL DR_HOOK('MODD_TRIP_LAKE:TRIP_LAKE_NULLIFY',1,ZHOOK_HANDLE)
!
END SUBROUTINE TRIP_LAKE_NULLIFY
!
END MODULE MODD_TRIP_LAKE
