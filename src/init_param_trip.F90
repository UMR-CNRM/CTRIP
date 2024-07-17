!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!     #########
      SUBROUTINE INIT_PARAM_TRIP (TPG, &
                                   KLISTING,HFILE,KLON,KLAT,KLAKE_NUM,HTITLE,HTIMEUNIT)
!     #######################################################################
!
!!****  *INIT_PARAM_TRIP*
!!
!!    PURPOSE
!!    -------
!
!     Define the name and unit of each trip parameter.
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
USE MODD_TRIP_GRID, ONLY : TRIP_GRID_t
!
USE MODE_TRIP_NETCDF
!
USE MODN_TRIP,       ONLY : CGROUNDW, CVIT, LFLOOD, CLAKE, XBANKSLOPE, LBACKWATER
USE MODD_TRIP_PAR,   ONLY : XUNDEF, NDIMTAB, LNCPRINT
USE MODD_TRIP_OASIS, ONLY : LCPL_LAND, LCPL_GW
!
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
TYPE(TRIP_GRID_t), INTENT(INOUT) :: TPG
!
CHARACTER(LEN=*), INTENT(IN) :: HFILE, HTITLE, HTIMEUNIT
!
INTEGER, INTENT(IN)          :: KLISTING, KLON, KLAT, KLAKE_NUM
!
!*      0.2    declarations of output variables
!
 CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(:), ALLOCATABLE :: YVNAME  !Name of each output variable
 CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(:), ALLOCATABLE :: YVLNAME !Long name of each output variables
 CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(:), ALLOCATABLE :: YUNIT   !Unit of each output variable
!
 CHARACTER(LEN=NF90_MAX_NAME) :: YFILE,YTITLE,YTIMEUNIT
!
LOGICAL, DIMENSION(:), ALLOCATABLE ::  LZLEN
!
REAL,    DIMENSION(:), ALLOCATABLE ::  ZLON
REAL,    DIMENSION(:), ALLOCATABLE ::  ZLAT
LOGICAL, DIMENSION(:), ALLOCATABLE ::  LDOUBLE
LOGICAL, DIMENSION(:), ALLOCATABLE ::  LLAKE
!
INTEGER :: INPARAM, INCID, INUM
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
! * Number of output variable
!
IF (LHOOK) CALL DR_HOOK('INIT_PARAM_TRIP',0,ZHOOK_HANDLE)
!
INPARAM = 7
IF(CGROUNDW/='DEF'          )INPARAM = INPARAM + 1
IF(CVIT=='VAR'              )INPARAM = INPARAM + 3
IF(CGROUNDW=='DIF'          )INPARAM = INPARAM + 5
IF(CGROUNDW=='DIF'.AND.LCPL_LAND.AND.LCPL_GW)INPARAM = INPARAM + 1
IF(LFLOOD.OR.CGROUNDW=='DIF'.OR.XBANKSLOPE>0.0)INPARAM = INPARAM + 1
IF(LBACKWATER.OR.CGROUNDW=='DIF')INPARAM = INPARAM + 1
IF(LFLOOD                   )INPARAM = INPARAM + 4
IF(CLAKE=='MLK'             )INPARAM = INPARAM + 6
!
!
! * Allocate netcdf file attributs
!
ALLOCATE(YVNAME  (INPARAM))
ALLOCATE(YVLNAME (INPARAM))
ALLOCATE(YUNIT   (INPARAM))
ALLOCATE(LZLEN   (INPARAM))
ALLOCATE(LDOUBLE (INPARAM))
ALLOCATE(LLAKE   (INPARAM))
!
ALLOCATE(ZLON(KLON))
ALLOCATE(ZLAT(KLAT))
!
LLAKE(:) = .FALSE.
!
! * Initialise netcdf file attributs
!
YVNAME (1) = 'FLOWDIR                    '
YVLNAME(1) = 'Flow direction             '
YUNIT  (1) = '-                          '
LZLEN  (1) = .FALSE.
LDOUBLE(1) = .FALSE.
!
YVNAME (2) = 'RIVSEQ                     '
YVLNAME(2) = 'River sequence             '
YUNIT  (2) = '-                          '
LZLEN  (2) = .FALSE.
LDOUBLE(2) = .FALSE.
!
YVNAME (3) = 'RIVLEN                     '
YVLNAME(3) = 'River length               '
YUNIT  (3) = 'm                          '
LZLEN  (3) = .FALSE.
LDOUBLE(3) = .TRUE.
!
YVNAME (4) = 'NUM_BAS                    '
YVLNAME(4) = 'Trip basin reference number'
YUNIT  (4) = '-                          '
LZLEN  (4) = .FALSE.
LDOUBLE(4) = .FALSE.
!
YVNAME (5) = 'CELL_AREA                  '
YVLNAME(5) = 'Trip cell area             '
YUNIT  (5) = 'm2                         '
LZLEN  (5) = .FALSE.
LDOUBLE(5) = .TRUE.
!
YVNAME (6) = 'GREEN_ANT                  '
YVNAME (6) = 'GREEN_ANT                  '
YVLNAME(6) = 'Greenland/Antarctic mask   '
YUNIT  (6) = '-                          '
LZLEN  (6) = .FALSE.
LDOUBLE(6) = .FALSE.
!
YVNAME (7) = 'DR_AREA                    '
YVLNAME(7) = 'Trip drainage area         '
YUNIT  (7) = 'm2                         '
LZLEN  (7) = .FALSE.
LDOUBLE(7) = .TRUE.
!
INUM = 7
!
IF(CLAKE=='MLK')THEN
!
INUM = INUM + 1
!
YVNAME (INUM) = 'LAKE_ID_IN                 '
YVLNAME(INUM) = 'Lake ID on each lake cell  '
YUNIT  (INUM) = '-                          '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
INUM = INUM + 1
!
YVNAME (INUM) = 'LAKE_ID_NW                 '
YVLNAME(INUM) = 'Lake ID in the network mask'
YUNIT  (INUM) = '-                          '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
INUM = INUM + 1
!
YVNAME (INUM) = 'FRAC_LAKE                  '
YVLNAME(INUM) = 'Lake cell fraction         '
YUNIT  (INUM) = '-                          '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
! INUM = INUM + 1
! !
! YVNAME (INUM) = 'LAKE_Z                     '
! YVLNAME(INUM) = 'Equivalent lake depth      '
! YUNIT  (INUM) = 'm                          '
! LZLEN  (INUM) = .FALSE.
! LDOUBLE(INUM) = .FALSE.
! LLAKE  (INUM) = .TRUE.
! !
INUM = INUM + 1
!
YVNAME (INUM) = 'LAKE_A                     '
YVLNAME(INUM) = 'Equivalent lake area       '
YUNIT  (INUM) = 'm2                         '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
LLAKE  (INUM) = .TRUE.
!
INUM = INUM + 1
!
YVNAME (INUM) = 'WEIR_Z                     '
YVLNAME(INUM) = 'Lake weir height           '
YUNIT  (INUM) = 'm                          '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
LLAKE  (INUM) = .TRUE.
!
INUM = INUM + 1
!
YVNAME (INUM) = 'WEIR_W                     '
YVLNAME(INUM) = 'Lake weir width            '
YUNIT  (INUM) = 'm'
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
LLAKE  (INUM) = .TRUE.
!
ENDIF
!
IF(CGROUNDW/='DEF')THEN
!
INUM = INUM + 1
!
YVNAME (INUM) = 'TAUG                      '
YVLNAME(INUM) = 'Groundwater transfert time (0=permafrost area)'
YUNIT  (INUM) = 'days                      '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
ENDIF
!
IF(CVIT=='VAR')THEN
!
INUM = INUM + 1
YVNAME (INUM) = 'N_RIV                     '
YVLNAME(INUM) = 'Manning coefficient       '
YUNIT  (INUM) = '-                         '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
INUM = INUM + 1
YVNAME (INUM) = 'WIDTHRIV                  '
YVLNAME(INUM) = 'Stream river width        '
YUNIT  (INUM) = 'm                         '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
INUM = INUM + 1
YVNAME (INUM) = 'SLOPERIV                  '
YVLNAME(INUM) = 'Stream River slope        '
YUNIT  (INUM) = 'm/m                       '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .TRUE.
!
ENDIF
!
IF(LBACKWATER.OR.CGROUNDW=='DIF')THEN
!
INUM = INUM + 1
YVNAME (INUM) = 'TOPO_RIV                  '
YVLNAME(INUM) = 'River elevation           '
YUNIT  (INUM) = '                          '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
ENDIF
!
IF(CGROUNDW=='DIF')THEN
!
INUM = INUM + 1
YVNAME (INUM) = 'WEFF                      '
YVLNAME(INUM) = 'Effective porosity        '
YUNIT  (INUM) = '                          '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
INUM = INUM + 1
YVNAME (INUM) = 'TRANS                     '
YVLNAME(INUM) = 'Transmissivity'
YUNIT  (INUM) = 'm2/s                      '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
INUM = INUM + 1
YVNAME (INUM) = 'NUM_AQUI                  '
YVLNAME(INUM) = 'Aquifer number            '
YUNIT  (INUM) = '                          '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
IF(LCPL_LAND.AND.LCPL_GW)THEN
!
INUM = INUM + 1
YVNAME (INUM) = 'NEAR_AQUI                 '
YVLNAME(INUM) = 'Nearest aquifer number    '
YUNIT  (INUM) = '                          '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
ENDIF
!
INUM = INUM + 1
YVNAME (INUM) = 'TABGW_F                      '
YVLNAME(INUM) = 'Potential fraction of wt rise'
YUNIT  (INUM) = '-                            '
LZLEN  (INUM) = .TRUE.
LDOUBLE(INUM) = .FALSE.
!
INUM = INUM + 1
YVNAME (INUM) = 'TABGW_H                      '
YVLNAME(INUM) = 'Subgrid elevation height     '
YUNIT  (INUM) = 'm                            '
LZLEN  (INUM) = .TRUE.
LDOUBLE(INUM) = .FALSE.
!
ENDIF
!
IF(LFLOOD.OR.CGROUNDW=='DIF'.OR.XBANKSLOPE>0.0)THEN
!
INUM = INUM + 1
YVNAME (INUM) = 'RIVDEPTH                  '
YVLNAME(INUM) = 'Stream River Depth (Hc)   '
YUNIT  (INUM) = 'm                         '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
ENDIF
!
IF(LFLOOD)THEN
!
INUM = INUM + 1
YVNAME (INUM) = 'NFLOOD                    '
YVLNAME(INUM) = 'Manning coef for flood    '
YUNIT  (INUM) = '-                         '
LZLEN  (INUM) = .FALSE.
LDOUBLE(INUM) = .FALSE.
!
INUM = INUM + 1
YVNAME (INUM) = 'TABF                      '
YVLNAME(INUM) = 'Potential flood fraction  '
YUNIT  (INUM) = '-                         '
LZLEN  (INUM) = .TRUE.
LDOUBLE(INUM) = .FALSE.
!
INUM = INUM + 1
YVNAME (INUM) = 'TABH                      '
YVLNAME(INUM) = 'Topographic height        '
YUNIT  (INUM) = 'm                         '
LZLEN  (INUM) = .TRUE.
LDOUBLE(INUM) = .FALSE.
!
INUM = INUM + 1
YVNAME (INUM) = 'TABVF                     '
YVLNAME(INUM) = 'Potential flood volume    '
YUNIT  (INUM) = 'kg/m2                     '
LZLEN  (INUM) = .TRUE.
LDOUBLE(INUM) = .FALSE.
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

IF(ALL(.NOT.LZLEN(:)))THEN
  IF(CLAKE=='MLK') THEN
    CALL NCCREATE(KLISTING,YFILE,YTITLE,YTIMEUNIT,INUM,YVNAME,YVLNAME,YUNIT,&
                  ZLON,ZLAT,XUNDEF,LNCPRINT,INCID,.FALSE.,                  &
                  ODOUBLE=LDOUBLE,OLAKE=LLAKE,KLAKE_NUM=KLAKE_NUM           )
  ELSE
    CALL NCCREATE(KLISTING,YFILE,YTITLE,YTIMEUNIT,INUM,YVNAME,YVLNAME,YUNIT,&
                  ZLON,ZLAT,XUNDEF,LNCPRINT,INCID,.FALSE.,ODOUBLE=LDOUBLE   )
  ENDIF
ELSE
  IF(CLAKE=='MLK') THEN
    CALL NCCREATE(KLISTING,YFILE,YTITLE,YTIMEUNIT,INUM,YVNAME,YVLNAME,YUNIT,&
                  ZLON,ZLAT,XUNDEF,LNCPRINT,INCID,.FALSE.,NDIMTAB,LZLEN,    &
                  ODOUBLE=LDOUBLE,OLAKE=LLAKE,KLAKE_NUM=KLAKE_NUM           )
  ELSE
    CALL NCCREATE(KLISTING,YFILE,YTITLE,YTIMEUNIT,INUM,YVNAME,YVLNAME,YUNIT,&
                  ZLON,ZLAT,XUNDEF,LNCPRINT,INCID,.FALSE.,NDIMTAB,LZLEN,    &
                  ODOUBLE=LDOUBLE                                           )
  ENDIF
ENDIF
!
CALL NCCLOSE(KLISTING,LNCPRINT,YFILE,INCID)
!
! * Deallocate netcdf file attributs
!
DEALLOCATE(YVNAME  )
DEALLOCATE(YVLNAME )
DEALLOCATE(YUNIT   )
DEALLOCATE(ZLON    )
DEALLOCATE(ZLAT    )
!
IF (LHOOK) CALL DR_HOOK('INIT_PARAM_TRIP',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE INIT_PARAM_TRIP
