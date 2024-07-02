!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
SUBROUTINE INIT_TRIP_CPL_ESM (TP, TPG, TPST, &
                              KLON,KLAT,KSTATE)
!     ##################################
!
!!****  *INIT_TRIP_CPL_ESM*
!!
!!    PURPOSE
!!    -------
!
!     TRIP cpl diag compuation
!     
!!      
!!    AUTHOR
!!    ------
!!      B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/12/13
!!      B. Decharme 10/2016  bug surface/groundwater coupling
!!      S. Munier   28/03/2020 CTRIP-12D and parallelization
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_TRIP,       ONLY : TRIP_t
USE MODD_TRIP_GRID,  ONLY : TRIP_GRID_t
USE MODD_TRIP_STATE, ONLY : TRIP_STATE_t
!
USE MODD_TRIP_OASIS, ONLY : LCPL_SEA, LCPL_LAND, LCPL_GW, &
                            LCPL_FLOOD, LCPL_CALVSEA
!
USE MODD_TRIP_PAR,   ONLY : XUNDEF
!
USE MODD_TRIP_MPI
!
USE MODE_TRIP_GRID
USE MODE_TRIP_GRID_STATE, ONLY : TRIP_GRID_TO_STATE
!
USE MODI_GWF_CPL_UPDATE_STATE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1    declarations of arguments
!
!
TYPE(TRIP_t), INTENT(INOUT) :: TP
TYPE(TRIP_GRID_t), INTENT(INOUT) :: TPG
TYPE(TRIP_STATE_t), INTENT(INOUT) :: TPST
!
INTEGER, INTENT(IN) :: KLON
INTEGER, INTENT(IN) :: KLAT
INTEGER, INTENT(IN) :: KSTATE
!
!*      0.2    declarations of local variables
!
REAL,   DIMENSION(KSTATE) :: ZHG_OLD    !Water table elevation at t-1    [m]
REAL,   DIMENSION(KSTATE) :: ZWTD       !Water table depth               [m]
REAL,   DIMENSION(KSTATE) :: ZFWTD      !Fraction of Water table to rise
!
REAL,   DIMENSION(KLON)   :: ZLON
REAL,   DIMENSION(KLAT)   :: ZLAT
INTEGER,DIMENSION(:), ALLOCATABLE :: ICODE
REAL,   DIMENSION(:), ALLOCATABLE :: ZNEAR
REAL,   DIMENSION(:), ALLOCATABLE :: ZX
REAL,   DIMENSION(:), ALLOCATABLE :: ZY
REAL,   DIMENSION(:), ALLOCATABLE :: ZSTATE
!
INTEGER :: IWORK, JLON, JLAT
INTEGER :: IERR
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_TRIP_CPL_ESM',0,ZHOOK_HANDLE)
!
ZHG_OLD (:) = XUNDEF
ZWTD    (:) = XUNDEF
ZFWTD   (:) = XUNDEF
!
!-------------------------------------------------------------------------------
! * Allocate coupling variables
!-------------------------------------------------------------------------------
!
IF(LCPL_SEA)THEN
  IF(NRANK==NPIO)ALLOCATE(TP%XCPL_RIVDIS(KLON,KLAT))
  IF(NRANK==NPIO)TP%XCPL_RIVDIS(:,:) = XUNDEF
  ALLOCATE(TPST%XCPL_RIVDIS(KSTATE))
  TPST%XCPL_RIVDIS(:) = XUNDEF
ELSE
  IF(NRANK==NPIO)ALLOCATE(TP%XCPL_RIVDIS(0,0))
  ALLOCATE(TPST%XCPL_RIVDIS(0))
ENDIF
!
IF(LCPL_CALVSEA)THEN
  IF(NRANK==NPIO)THEN
    ALLOCATE(TP%XCPL_CALVGRE(KLON,KLAT))
    ALLOCATE(TP%XCPL_CALVANT(KLON,KLAT))
    TP%XCPL_CALVGRE(:,:) = XUNDEF
    TP%XCPL_CALVANT(:,:) = XUNDEF
  ENDIF
  ALLOCATE(TPST%XCPL_CALVGRE(KSTATE))
  ALLOCATE(TPST%XCPL_CALVANT(KSTATE))
  TPST%XCPL_CALVGRE(:) = XUNDEF
  TPST%XCPL_CALVANT(:) = XUNDEF
ELSE
  IF(NRANK==NPIO)ALLOCATE(TP%XCPL_CALVGRE(0,0))
  IF(NRANK==NPIO)ALLOCATE(TP%XCPL_CALVANT(0,0))
  ALLOCATE(TPST%XCPL_CALVGRE(0))
  ALLOCATE(TPST%XCPL_CALVANT(0))
ENDIF
!
IF(LCPL_LAND)THEN
  IF(LCPL_GW)THEN
    IF(NRANK==NPIO)THEN
      ALLOCATE(TP%XCPL_FWTD (KLON,KLAT))
      ALLOCATE(TP%XCPL_WTD  (KLON,KLAT))
      TP%XCPL_FWTD (:,:) = XUNDEF
      TP%XCPL_WTD  (:,:) = XUNDEF
    ENDIF
    ALLOCATE(TPST%XCPL_FWTD (KSTATE))
    ALLOCATE(TPST%XCPL_WTD  (KSTATE))
    ALLOCATE(TPST%XNEAR_AQUI(KSTATE))
    TPST%XCPL_FWTD (:) = XUNDEF
    TPST%XCPL_WTD  (:) = XUNDEF
    TPST%XNEAR_AQUI(:) = XUNDEF
  ELSE
    IF(NRANK==NPIO)THEN
      ALLOCATE(TP%XCPL_FWTD (0,0))
      ALLOCATE(TP%XCPL_WTD  (0,0))
    ENDIF
    ALLOCATE(TPST%XCPL_FWTD (0))
    ALLOCATE(TPST%XCPL_WTD  (0))
    ALLOCATE(TPST%XNEAR_AQUI(0))
  ENDIF
  IF(LCPL_FLOOD)THEN
    IF(NRANK==NPIO)THEN
      ALLOCATE(TP%XCPL_FFLOOD (KLON,KLAT))
      ALLOCATE(TP%XCPL_PIFLOOD(KLON,KLAT))
      TP%XCPL_FFLOOD (:,:) = XUNDEF
      TP%XCPL_PIFLOOD(:,:) = XUNDEF
    ENDIF
    ALLOCATE(TPST%XCPL_FFLOOD (KSTATE))
    ALLOCATE(TPST%XCPL_PIFLOOD(KSTATE))
    TPST%XCPL_FFLOOD (:) = XUNDEF
    TPST%XCPL_PIFLOOD(:) = XUNDEF
  ELSE
    IF(NRANK==NPIO)ALLOCATE(TP%XCPL_FFLOOD (0,0))
    IF(NRANK==NPIO)ALLOCATE(TP%XCPL_PIFLOOD(0,0))
    ALLOCATE(TPST%XCPL_FFLOOD (0))
    ALLOCATE(TPST%XCPL_PIFLOOD(0))
  ENDIF
ELSE
  IF(NRANK==NPIO)THEN
    ALLOCATE(TP%XCPL_FFLOOD (0,0))
    ALLOCATE(TP%XCPL_PIFLOOD(0,0))
    ALLOCATE(TP%XCPL_FWTD   (0,0))
    ALLOCATE(TP%XCPL_WTD    (0,0))
  ENDIF
  ALLOCATE(TPST%XCPL_FFLOOD (0))
  ALLOCATE(TPST%XCPL_PIFLOOD(0))
  ALLOCATE(TPST%XCPL_FWTD   (0))
  ALLOCATE(TPST%XCPL_WTD    (0))
  ALLOCATE(TPST%XNEAR_AQUI  (0))
ENDIF
!
!-------------------------------------------------------------------------------
! * Actualisation of coupling diagnostic:
!-------------------------------------------------------------------------------
!
IF(LCPL_SEA)THEN
  WHERE(TPST%NGRCN(:)==9.OR.TPST%NGRCN(:)==12)
    TPST%XCPL_RIVDIS(:) = 0.0
  ENDWHERE
ENDIF
!
IF(LCPL_CALVSEA)THEN
  WHERE(TPST%GMASK_GRE(:))
    TPST%XCPL_CALVGRE(:) = 0.0
  ENDWHERE
  WHERE(TPST%GMASK_ANT(:))
    TPST%XCPL_CALVANT(:) = 0.0
  ENDWHERE
ENDIF
!
IF(LCPL_LAND)THEN
!
  IF(LCPL_GW)THEN
!
!   Water table depth and fraction of water table to rise
!
    CALL GWF_CPL_UPDATE_STATE(TPST, &
                              ZHG_OLD,ZWTD,ZFWTD   )
!
    WHERE(TPST%GMASK_GW(:))
          TPST%XCPL_WTD (:) = ZWTD (:)
          TPST%XCPL_FWTD(:) = ZFWTD(:)
    ELSEWHERE(TPST%GMASK(:))
          TPST%XCPL_WTD (:) = XUNDEF
          TPST%XCPL_FWTD(:) = 0.0
    ENDWHERE
!
!   Broadcast XNEAR_AQUI
!
    IF(NRANK==NPIO) THEN
      ALLOCATE(ZSTATE(TPST%NNSTATE))
      CALL TRIP_GRID_TO_STATE(TPST%NSTATE_LON,TPST%NSTATE_LAT,TP%XNEAR_AQUI,ZSTATE)
    ENDIF
    CALL MPI_SCATTER(ZSTATE,KSTATE,MPI_DOUBLE,TPST%XNEAR_AQUI,KSTATE,MPI_DOUBLE,NPIO,NCOMM,IERR)
    IF(NRANK==NPIO)DEALLOCATE(ZSTATE)
!
  ENDIF
!
! Flood fraction [-] and potential infiltration [kg/m2]
! no flood for very smal flooded area (<0.1% of grid-cell)
!
  IF(LCPL_FLOOD)THEN
     TPST%XCPL_FFLOOD (:) = TPST%XFFLOOD    (:)
     TPST%XCPL_PIFLOOD(:) = TPST%XFLOOD_STO (:) / TPST%XAREA(:)
  ENDIF
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('INIT_TRIP_CPL_ESM',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE INIT_TRIP_CPL_ESM
