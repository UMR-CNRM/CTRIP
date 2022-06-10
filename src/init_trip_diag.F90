!     #########
      SUBROUTINE INIT_TRIP_DIAG (TPDG, TPG, &
                                  KLISTING,HFILE,KLON,KLAT,KLAKE_NUM,HTITLE,HTIMEUNIT,OTIME)
!     #######################################################################
!
!!****  *INIT_TRIP_DIAG*
!!
!!    PURPOSE
!!    -------
!
!     Define the name and unit of each trip output variable.
!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    27/05/08
!!      T. Guinaldo 04/2020   Add MLake
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
!
USE MODD_TRIP_DIAG, ONLY : TRIP_DIAG_t
USE MODD_TRIP_GRID, ONLY : TRIP_GRID_t
!
USE MODE_TRIP_NETCDF
!
USE MODN_TRIP_RUN,   ONLY : LDIAG_MISC
USE MODN_TRIP,       ONLY : CGROUNDW, CVIT, LFLOOD, CLAKE
!
USE MODD_TRIP_OASIS, ONLY : LCPL_LAND
!
USE MODD_TRIP_PAR, ONLY : XUNDEF, LNCPRINT
!
USE MODI_TEST_DIAG_WRITE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE NETCDF
!
USE MODI_GET_LONLAT_TRIP
!
IMPLICIT NONE
!
!
!*      0.1    declarations of arguments
!
!
!
TYPE(TRIP_DIAG_t), INTENT(INOUT) :: TPDG
TYPE(TRIP_GRID_t), INTENT(INOUT) :: TPG
!
CHARACTER(LEN=*), INTENT(IN) :: HFILE, HTITLE, HTIMEUNIT
!
INTEGER, INTENT(IN)          :: KLISTING, KLON, KLAT, KLAKE_NUM
!
LOGICAL, INTENT(IN)          :: OTIME
!
!*      0.2    declarations of output variables
!
CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(:), ALLOCATABLE :: YVNAME  !Name of each output variable
CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(:), ALLOCATABLE :: YVLNAME !Long name of each output variable
CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(:), ALLOCATABLE :: YUNIT   !Unit of each output variable
LOGICAL,                      DIMENSION(:), ALLOCATABLE :: LLAKE   !Lake dimension of each variable
!
CHARACTER(LEN=NF90_MAX_NAME) :: YFILE,YTITLE,YTIMEUNIT
!
REAL,    DIMENSION(:), ALLOCATABLE :: ZLON
REAL,    DIMENSION(:), ALLOCATABLE :: ZLAT
!
INTEGER :: INDIAG, INCID, INUM
LOGICAL :: GWRITE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
! * Number of output variable
!
IF (LHOOK) CALL DR_HOOK('INIT_TRIP_DIAG',0,ZHOOK_HANDLE)
!
INUM   = 0
INDIAG = 2
IF(LDIAG_MISC) INDIAG = INDIAG + 1
IF(LCPL_LAND.AND.LDIAG_MISC) INDIAG = INDIAG + 2
IF(CVIT=='VAR')     INDIAG = INDIAG + 1
IF(CVIT=='VAR'.OR.CLAKE=='MLK') INDIAG = INDIAG + 1
IF(CGROUNDW/='DEF') INDIAG = INDIAG + 2
IF(CGROUNDW=='DIF')THEN
  INDIAG = INDIAG + 3
  IF(LDIAG_MISC)INDIAG = INDIAG + 2
ENDIF
IF(LFLOOD)THEN
  INDIAG = INDIAG + 3
  IF(LDIAG_MISC) INDIAG = INDIAG + 8
ENDIF
IF(CLAKE=='MLK') INDIAG = INDIAG + 4
!
! * Allocate netcdf file attributs
!
ALLOCATE(YVNAME  (INDIAG))
ALLOCATE(YVLNAME (INDIAG))
ALLOCATE(YUNIT   (INDIAG))
ALLOCATE(LLAKE   (INDIAG))
LLAKE(:) = .FALSE.
!
ALLOCATE(ZLON(KLON))
ALLOCATE(ZLAT(KLAT))
!
! * Initialyse netcdf file attributs
!
CALL TEST_DIAG_WRITE('SURF_STO',GWRITE)
IF(GWRITE)THEN
  INUM = INUM + 1
  YVNAME (INUM) = 'SURF_STO                  '
  YVLNAME(INUM) = 'River storage             '
  YUNIT  (INUM) = 'kg m-2                    '
ENDIF
!
CALL TEST_DIAG_WRITE('QDIS',GWRITE)
IF(GWRITE)THEN
  INUM = INUM + 1
  YVNAME (INUM) = 'QDIS                      '
  YVLNAME(INUM) = 'Discharge                 '
  YUNIT  (INUM) = 'm3 s-1                    '
ENDIF
!
IF(LDIAG_MISC)THEN
!
  CALL TEST_DIAG_WRITE('QSIN',GWRITE)
  IF(GWRITE)THEN
    INUM = INUM + 1
    YVNAME (INUM) = 'QSIN                      '
    YVLNAME(INUM) = 'Inflow to the river       '
    YUNIT  (INUM) = 'm3 s-1                    '
  ENDIF
!
ENDIF
!
IF(LCPL_LAND.AND.LDIAG_MISC)THEN
!
  CALL TEST_DIAG_WRITE('RUNOFF',GWRITE)
  IF(GWRITE)THEN
    INUM = INUM + 1
    YVNAME (INUM) = 'RUNOFF                    '
    YVLNAME(INUM) = 'Input surface runoff (can be used to force TRIP offline)'
    YUNIT  (INUM) = 'mm of water               '
  ENDIF
!
  CALL TEST_DIAG_WRITE('DRAIN',GWRITE)
  IF(GWRITE)THEN
    INUM = INUM + 1
    YVNAME (INUM) = 'DRAIN                     '
    YVLNAME(INUM) = 'Input drainage or recharge (can be used to force TRIP offline)'
    YUNIT  (INUM) = 'mm of water               '
  ENDIF
!
ENDIF
!
IF(CGROUNDW/='DEF')THEN
!
  CALL TEST_DIAG_WRITE('QGF',GWRITE)
  IF(GWRITE)THEN
    INUM = INUM + 1
    YVNAME (INUM) = 'QGF'
    YVLNAME(INUM) = 'Groundwater-river exchange'
    YUNIT  (INUM) = 'm3 s-1                    '
  ENDIF
!
  CALL TEST_DIAG_WRITE('GROUND_STO',GWRITE)
  IF(GWRITE)THEN
    INUM = INUM + 1
    YVNAME (INUM) = 'GROUND_STO                '
    IF(CGROUNDW=='CST')THEN
      YVLNAME(INUM) = 'Groundwater storage     '
    ELSEIF(CGROUNDW=='DIF')THEN
      YVLNAME(INUM) = 'Groundwater mass equivalent'
    ENDIF
    YUNIT  (INUM) = 'kg m-2                    '
  ENDIF
!
ENDIF
!
IF(CGROUNDW=='DIF')THEN
!
  CALL TEST_DIAG_WRITE('HGROUND',GWRITE)
  IF(GWRITE)THEN
    INUM = INUM + 1
    YVNAME (INUM) = 'HGROUND                   '
    YVLNAME(INUM) = 'Groundwater height        '
    YUNIT  (INUM) = 'm                         '
  ENDIF
!
  CALL TEST_DIAG_WRITE('FWTD',GWRITE)
  IF(GWRITE)THEN
    INUM = INUM + 1
    YVNAME (INUM) = 'FWTD                      '
    YVLNAME(INUM) = 'grid-cell fraction of wtd '
    YUNIT  (INUM) = '-                         '
  ENDIF
!
  CALL TEST_DIAG_WRITE('WTD',GWRITE)
  IF(GWRITE)THEN
    INUM = INUM + 1
    YVNAME (INUM) = 'WTD                       '
    YVLNAME(INUM) = 'Wat Tab Depth for coupling'
    YUNIT  (INUM) = 'm                         '
  ENDIF
!
  IF(LDIAG_MISC)THEN
!
    CALL TEST_DIAG_WRITE('QGCELL',GWRITE)
    IF(GWRITE)THEN
      INUM = INUM + 1
      YVNAME (INUM) = 'QGCELL                    '
      YVLNAME(INUM) = 'Grid-cell fluxes budget   '
      YUNIT  (INUM) = 'm3 s-1                    '
    ENDIF
!
    CALL TEST_DIAG_WRITE('HGHRIV',GWRITE)
    IF(GWRITE)THEN
      INUM = INUM + 1
      YVNAME (INUM)= 'HGHRIV                     '
      YVLNAME(INUM)= 'Hground - Hriver           '
      YUNIT  (INUM)= 'm                          '
    ENDIF
!
  ENDIF
!
ENDIF
!
IF(CVIT=='VAR')THEN
!
  CALL TEST_DIAG_WRITE('VEL',GWRITE)
  IF(GWRITE)THEN
    INUM = INUM + 1
    YVNAME (INUM) = 'VEL                       '
    YVLNAME(INUM) = 'Stream flow velocity      '
    YUNIT  (INUM) = 'm s-1                     '
  ENDIF
!
ENDIF
!
IF(CVIT=='VAR'.OR.CLAKE=='MLK')THEN
!
  CALL TEST_DIAG_WRITE('HSTREAM',GWRITE)
  IF(GWRITE)THEN
    INUM = INUM + 1
    YVNAME (INUM) = 'HSTREAM                   '
    YVLNAME(INUM) = 'Stream river height       '
    YUNIT  (INUM) = 'm                         '
  ENDIF
!
ENDIF
!
IF(LFLOOD)THEN
!
  CALL TEST_DIAG_WRITE('FLOOD_STO',GWRITE)
  IF(GWRITE)THEN
    INUM = INUM + 1
    YVNAME (INUM) = 'FLOOD_STO                 '
    YVLNAME(INUM) = 'Floodplain storage        '
    YUNIT  (INUM) = 'kg m-2                    '
  ENDIF
!
  CALL TEST_DIAG_WRITE('FFLOOD',GWRITE)
  IF(GWRITE)THEN
    INUM = INUM + 1
    YVNAME (INUM) = 'FFLOOD                    '
    YVLNAME(INUM) = 'TRIP flooded fraction     '
    YUNIT  (INUM) = '-                         '
  ENDIF
!
  CALL TEST_DIAG_WRITE('HFLOOD',GWRITE)
  IF(GWRITE)THEN
    INUM = INUM + 1
    YVNAME (INUM) = 'HFLOOD                    '
    YVLNAME(INUM) = 'Flood depth               '
    YUNIT  (INUM) = 'm                         '
  ENDIF
!
  IF(LDIAG_MISC)THEN
!
    CALL TEST_DIAG_WRITE('FSOURCE',GWRITE)
    IF(GWRITE)THEN
      INUM = INUM + 1
      YVNAME (INUM)= 'FSOURCE                      '
      YVLNAME(INUM)= 'Floodplains source (Pf-Ef-If) (can be used to force TRIP offline)'
      YUNIT  (INUM)= 'mm of water                  '
    ENDIF
!
    CALL TEST_DIAG_WRITE('VFIN',GWRITE)
    IF(GWRITE)THEN
      INUM = INUM + 1
      YVNAME (INUM)= 'VFIN                      '
      YVLNAME(INUM)= 'River to flood velocity   '
      YUNIT  (INUM)= 'm s-1                     '
    ENDIF
!
    CALL TEST_DIAG_WRITE('QRF',GWRITE)
    IF(GWRITE)THEN
      INUM = INUM + 1
      YVNAME (INUM)= 'QRF                       '
      YVLNAME(INUM)= 'River flow to floodplain  '
      YUNIT  (INUM)= 'm3 s-1                    '
    ENDIF
!
    CALL TEST_DIAG_WRITE('VFOUT',GWRITE)
    IF(GWRITE)THEN
      INUM = INUM + 1
      YVNAME (INUM)= 'VFOUT                     '
      YVLNAME(INUM)= 'Flood to river velocity   '
      YUNIT  (INUM)= 'm s-1                     '
    ENDIF
!
    CALL TEST_DIAG_WRITE('QFR',GWRITE)
    IF(GWRITE)THEN
      INUM = INUM + 1
      YVNAME (INUM)= 'QFR                       '
      YVLNAME(INUM)= 'Flood flow to river       '
      YUNIT  (INUM)= 'm3 s-1                    '
    ENDIF
!
    CALL TEST_DIAG_WRITE('HSF',GWRITE)
    IF(GWRITE)THEN
      INUM = INUM + 1
      YVNAME (INUM)= 'HSF                         '
      YVLNAME(INUM)= 'River-Flood depth comparison'
      YUNIT  (INUM)= 'm                           '
    ENDIF
!
    CALL TEST_DIAG_WRITE('WF',GWRITE)
    IF(GWRITE)THEN
      INUM = INUM + 1
      YVNAME (INUM)= 'WF                          '
      YVLNAME(INUM)= 'Flood width during dt       '
      YUNIT  (INUM)= 'm                           '
    ENDIF
!
    CALL TEST_DIAG_WRITE('LF',GWRITE)
    IF(GWRITE)THEN
      INUM = INUM + 1
      YVNAME (INUM)= 'LF                          '
      YVLNAME(INUM)= 'Flood lenght during dt      '
      YUNIT  (INUM)= 'm                           '
    ENDIF
!
  ENDIF
!
ENDIF
!
IF(CLAKE=='MLK')THEN
!
  CALL TEST_DIAG_WRITE('LAKE_STO',GWRITE)
  IF(GWRITE)THEN
    INUM = INUM + 1
    YVNAME (INUM)= 'LAKE_STO                    '
    YVLNAME(INUM)= 'Lake storage                '
    YUNIT  (INUM)= 'kg m-2                      '
    LLAKE  (INUM) = .TRUE.
  ENDIF
!
  CALL TEST_DIAG_WRITE('LAKE_OUT',GWRITE)
  IF(GWRITE)THEN
    INUM = INUM + 1
    YVNAME (INUM)= 'LAKE_OUT                    '
    YVLNAME(INUM)= 'Lake outflow                '
    YUNIT  (INUM)= 'm3 s-1                      '
    LLAKE  (INUM) = .TRUE.
  ENDIF
!
  CALL TEST_DIAG_WRITE('LAKE_IN',GWRITE)
  IF(GWRITE)THEN
    INUM = INUM + 1
    YVNAME (INUM)= 'LAKE_IN                     '
    YVLNAME(INUM)= 'Lake inflow                 '
    YUNIT  (INUM)= 'm3 s-1                      '
    LLAKE  (INUM) = .TRUE.
  ENDIF
!
  CALL TEST_DIAG_WRITE('LAKE_H',GWRITE)
  IF(GWRITE)THEN
    INUM = INUM + 1
    YVNAME (INUM)= 'LAKE_H                      '
    YVLNAME(INUM)= 'Lake depth                  '
    YUNIT  (INUM)= 'm                           '
    LLAKE  (INUM) = .TRUE.
  ENDIF
!
ENDIF
!
! * Create netcdf file
!
YFILE     = HFILE(1:LEN_TRIM(HFILE))
YTITLE    = HTITLE(1:LEN_TRIM(HTITLE))
YTIMEUNIT = HTIMEUNIT(1:LEN_TRIM(HTIMEUNIT))
!
CALL GET_LONLAT_TRIP(TPG, &
                     KLON,KLAT,ZLON,ZLAT)
!
IF(CLAKE=='MLK') THEN
  CALL NCCREATE(KLISTING,YFILE,YTITLE,YTIMEUNIT,INUM,YVNAME,YVLNAME,YUNIT,   &
                ZLON,ZLAT,XUNDEF,LNCPRINT,INCID,OTIME,OLAKE=LLAKE,KLAKE_NUM=KLAKE_NUM)
ELSE
  CALL NCCREATE(KLISTING,YFILE,YTITLE,YTIMEUNIT,INUM,YVNAME,YVLNAME,YUNIT,   &
                ZLON,ZLAT,XUNDEF,LNCPRINT,INCID,OTIME)
ENDIF
!
CALL NCCLOSE(KLISTING,LNCPRINT,YFILE,INCID)
!
! * Deallocate netcdf file attributs
!
DEALLOCATE(YVNAME  )
DEALLOCATE(YVLNAME )
DEALLOCATE(YUNIT   )
DEALLOCATE(LLAKE   )
DEALLOCATE(ZLON    )
DEALLOCATE(ZLAT    )
!
IF (LHOOK) CALL DR_HOOK('INIT_TRIP_DIAG',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE INIT_TRIP_DIAG
