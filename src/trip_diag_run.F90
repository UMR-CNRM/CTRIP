!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!     #########
      SUBROUTINE TRIP_DIAG_RUN (TPDG, TPG, &
                                KLISTING,KLON,KLAT,KLAKE_NUM,PRUNTIME)
!     #####################################################
!
!!****  *TRIP_DIAG_RUN*
!!
!!    PURPOSE
!!    -------
!
!     TRIP river routing run mean outputs.
!
!!
!!    AUTHOR
!!    ------
!!      B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    28/05/05
!!      T. Guinaldo 04/2020    Add MLake
!!      S.Munier    12/2020    selection of output variables
!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_TRIP_DIAG, ONLY : TRIP_DIAG_t
USE MODD_TRIP_GRID, ONLY : TRIP_GRID_t
!
USE MODN_TRIP,       ONLY : CGROUNDW, CVIT, LFLOOD, CLAKE
USE MODN_TRIP_RUN,   ONLY : LDIAG_MISC
USE MODD_TRIP_OASIS, ONLY : LCPL_LAND
!
USE MODD_TRIP_MPI,   ONLY : CRANK
!
USE MODE_RW_TRIP
USE MODI_TEST_DIAG_WRITE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(TRIP_DIAG_t), INTENT(INOUT) :: TPDG
TYPE(TRIP_GRID_t), INTENT(INOUT) :: TPG
!
INTEGER, INTENT(IN)             :: KLISTING
INTEGER, INTENT(IN)             :: KLON
INTEGER, INTENT(IN)             :: KLAT
INTEGER, INTENT(IN)             :: KLAKE_NUM
!
REAL, INTENT(IN)                :: PRUNTIME
!
!*      0.2    declarations of local variables
!
CHARACTER(LEN=50), PARAMETER         :: YRUN   = 'TRIP_DIAG_RUN.nc'
CHARACTER(LEN=50)                    :: YFILE
CHARACTER(LEN=10)                    :: YVNAME
!
REAL,   DIMENSION(KLON,KLAT)         :: ZWRITE
LOGICAL,DIMENSION(KLON,KLAT)         :: LMASK
LOGICAL,DIMENSION(KLON,KLAT)         :: LMASK_GW
REAL,   DIMENSION(KLAKE_NUM)         :: ZWRITE_LAKE
LOGICAL,DIMENSION(KLAKE_NUM)         :: LMASK_LAKE
!
INTEGER :: ITNUM, ITVAL
LOGICAL :: LWRITE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_DIAG_RUN',0,ZHOOK_HANDLE)
!
! * Trip mask
!
LMASK(:,:) = TPG%GMASK(:,:)
!
! * Groundwater specific mask
!
IF(CGROUNDW/='DEF')THEN
  LMASK_GW(:,:) = TPG%GMASK_GW(:,:)
ENDIF
!
! * Lake mask
!
IF(CLAKE=='MLK') LMASK_LAKE(:) = .TRUE.
!
! * Recup diag file
!
ITNUM = 1
ITVAL = 0
YFILE = YRUN
!
! * Store output in diag file
!
YVNAME = 'SURF_STO'
CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
IF(LWRITE)THEN
  ZWRITE = TPDG%TDIAG_RUN%XSURF_STO / PRUNTIME
  CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,ZWRITE,ITNUM,ITVAL)
ENDIF
!
YVNAME = 'QDIS'
CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
IF(LWRITE)THEN
  ZWRITE = TPDG%TDIAG_RUN%XQDIS / PRUNTIME
  CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,ZWRITE,ITNUM,ITVAL)
ENDIF
!
IF(LDIAG_MISC)THEN
!
  YVNAME = 'QSIN'
  CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
  IF(LWRITE)THEN
    ZWRITE = TPDG%TDIAG_RUN%XQIN / PRUNTIME
    CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,ZWRITE,ITNUM,ITVAL)
  ENDIF
!
ENDIF
!
IF(LCPL_LAND.AND.LDIAG_MISC)THEN
!
  YVNAME = 'RUNOFF'
  CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
  IF(LWRITE)THEN
    ZWRITE = TPDG%TDIAG_RUN%XRUNOFF !mm of water
    CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,ZWRITE,ITNUM,ITVAL)
  ENDIF
!
  YVNAME = 'DRAIN'
  CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
  IF(LWRITE)THEN
    ZWRITE = TPDG%TDIAG_RUN%XDRAIN !mm of water
    CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,ZWRITE,ITNUM,ITVAL)
  ENDIF
!
ENDIF
!
IF(CGROUNDW/='DEF')THEN
!
  YVNAME = 'QGF'
  CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
  IF(LWRITE)THEN
    ZWRITE = TPDG%TDIAG_RUN%XQGF / PRUNTIME
    CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK_GW,ZWRITE,ITNUM,ITVAL)
  ENDIF
!
  YVNAME = 'GROUND_STO'
  CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
  IF(LWRITE)THEN
    ZWRITE = TPDG%TDIAG_RUN%XGROUND_STO / PRUNTIME
    CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK_GW,ZWRITE,ITNUM,ITVAL)
  ENDIF
!
ENDIF
!
IF(CGROUNDW=='DIF')THEN
!
  YVNAME = 'HGROUND'
  CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
  IF(LWRITE)THEN
    ZWRITE = TPDG%TDIAG_RUN%XHGROUND / PRUNTIME
    CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK_GW,ZWRITE,ITNUM,ITVAL)
  ENDIF
!
  YVNAME = 'FWTD'
  CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
  IF(LWRITE)THEN
    ZWRITE = TPDG%TDIAG_RUN%XFWTD / PRUNTIME
    CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK_GW,ZWRITE,ITNUM,ITVAL)
  ENDIF
!
  YVNAME = 'WTD'
  CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
  IF(LWRITE)THEN
    ZWRITE = TPDG%TDIAG_RUN%XWTD / PRUNTIME
    CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK_GW,ZWRITE,ITNUM,ITVAL)
  ENDIF
!
  IF(LDIAG_MISC)THEN
!
    YVNAME = 'QGCELL'
    CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
    IF(LWRITE)THEN
      ZWRITE = TPDG%TDIAG_RUN%XQGCELL / PRUNTIME
      CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK_GW,ZWRITE,ITNUM,ITVAL)
    ENDIF
!
    YVNAME = 'HGHRIV'
    CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
    IF(LWRITE)THEN
      ZWRITE = TPDG%TDIAG_RUN%XHGHS / PRUNTIME
      CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK_GW,ZWRITE,ITNUM,ITVAL)
    ENDIF
!
  ENDIF
!
ENDIF
!
IF(CVIT=='VAR')THEN
!
  YVNAME = 'VEL'
  CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
  IF(LWRITE)THEN
    ZWRITE = TPDG%TDIAG_RUN%XVEL / PRUNTIME
    CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,ZWRITE,ITNUM,ITVAL)
  ENDIF
!
  YVNAME = 'HSTREAM'
  CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
  IF(LWRITE)THEN
    ZWRITE = TPDG%TDIAG_RUN%XHS / PRUNTIME
    CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,ZWRITE,ITNUM,ITVAL)
  ENDIF
!
ENDIF
!
IF(LFLOOD)THEN
!
  YVNAME = 'FFLOOD'
  CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
  IF(LWRITE)THEN
    ZWRITE = TPDG%TDIAG_RUN%XFF / PRUNTIME
    CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,ZWRITE,ITNUM,ITVAL)
  ENDIF
!
  YVNAME = 'FLOOD_STO'
  CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
  IF(LWRITE)THEN
    ZWRITE = TPDG%TDIAG_RUN%XFLOOD_STO / PRUNTIME
    CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,ZWRITE,ITNUM,ITVAL)
  ENDIF
!
  YVNAME = 'HFLOOD'
  CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
  IF(LWRITE)THEN
    ZWRITE = TPDG%TDIAG_RUN%XHF / PRUNTIME
    CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,ZWRITE,ITNUM,ITVAL)
  ENDIF
!
  IF(LDIAG_MISC)THEN
!
    YVNAME = 'FSOURCE'
    CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
    IF(LWRITE)THEN
      ZWRITE = TPDG%TDIAG_RUN%XSOURCE !mm of water
      CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,ZWRITE,ITNUM,ITVAL)
    ENDIF
!
    YVNAME = 'QFR'
    CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
    IF(LWRITE)THEN
      ZWRITE = TPDG%TDIAG_RUN%XQFR / PRUNTIME
      CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,ZWRITE,ITNUM,ITVAL)
    ENDIF
!
    YVNAME = 'QRF'
    CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
    IF(LWRITE)THEN
      ZWRITE = TPDG%TDIAG_RUN%XQRF / PRUNTIME
      CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,ZWRITE,ITNUM,ITVAL)
    ENDIF
!
    YVNAME = 'VFIN'
    CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
    IF(LWRITE)THEN
      ZWRITE = TPDG%TDIAG_RUN%XVFIN / PRUNTIME
      CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,ZWRITE,ITNUM,ITVAL)
    ENDIF
!
    YVNAME = 'VFOUT'
    CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
    IF(LWRITE)THEN
      ZWRITE = TPDG%TDIAG_RUN%XVFOUT / PRUNTIME
      CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,ZWRITE,ITNUM,ITVAL)
    ENDIF
!
    YVNAME = 'HSF'
    CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
    IF(LWRITE)THEN
      ZWRITE = TPDG%TDIAG_RUN%XHSF / PRUNTIME
      CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,ZWRITE,ITNUM,ITVAL)
    ENDIF
!
    YVNAME = 'WF'
    CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
    IF(LWRITE)THEN
      ZWRITE = TPDG%TDIAG_RUN%XWF / PRUNTIME
      CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,ZWRITE,ITNUM,ITVAL)
    ENDIF
!
    YVNAME = 'LF'
    CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
    IF(LWRITE)THEN
      ZWRITE = TPDG%TDIAG_RUN%XLF / PRUNTIME
      CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK,ZWRITE,ITNUM,ITVAL)
    ENDIF
!
  ENDIF
!
ENDIF
!
IF(CLAKE=='MLK')THEN
!
  YVNAME = 'LAKE_STO'
  CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
  IF(LWRITE)THEN
    ZWRITE_LAKE = TPDG%TDIAG_RUN%XLAKE_STO / PRUNTIME
    CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK_LAKE,ZWRITE_LAKE,ITNUM,ITVAL)
  ENDIF
!
  YVNAME = 'LAKE_OUT'
  CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
  IF(LWRITE)THEN
    ZWRITE_LAKE = TPDG%TDIAG_RUN%XLAKE_OUT / PRUNTIME
    CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK_LAKE,ZWRITE_LAKE,ITNUM,ITVAL)
  ENDIF
!
  YVNAME = 'LAKE_IN'
  CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
  IF(LWRITE)THEN
    ZWRITE_LAKE = TPDG%TDIAG_RUN%XLAKE_IN  / PRUNTIME
    CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK_LAKE,ZWRITE_LAKE,ITNUM,ITVAL)
  ENDIF
!
  YVNAME = 'LAKE_H'
  CALL TEST_DIAG_WRITE(YVNAME,LWRITE)
  IF(LWRITE)THEN
    ZWRITE_LAKE = TPDG%TDIAG_RUN%XLAKE_H / PRUNTIME
    CALL WRITE_TRIP(KLISTING,YFILE,YVNAME,LMASK_LAKE,ZWRITE_LAKE,ITNUM,ITVAL)
  ENDIF
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP_DIAG_RUN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_DIAG_RUN
