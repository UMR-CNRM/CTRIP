!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!     #########
      SUBROUTINE INIT_RESTART_TRIP (TPG, &
                                     KLISTING,HFILE,KLON,KLAT,KLAKE_NUM,HTITLE,HTIMEUNIT,OTIME)
!     #######################################################################
!
!!****  *INIT_RESTART_TRIP*  
!!
!!    PURPOSE
!!    -------
!
!     Define the name and unit of each trip restart variables.
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
USE MODD_TRIP_GRID, ONLY : TRIP_GRID_t
!
USE MODE_TRIP_NETCDF
!
USE MODN_TRIP,     ONLY : CGROUNDW, LFLOOD, CLAKE
USE MODD_TRIP_PAR, ONLY : XUNDEF, LNCPRINT
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
TYPE(TRIP_GRID_t), INTENT(INOUT) :: TPG
!
 CHARACTER(LEN=*), INTENT(IN) :: HFILE, HTITLE, HTIMEUNIT
!
INTEGER, INTENT(IN)          :: KLISTING, KLON, KLAT, KLAKE_NUM
!
LOGICAL, INTENT(IN)          :: OTIME
!
!*      0.2    declarations of restart variables
!
 CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(:), ALLOCATABLE :: YVNAME  !Name of each restart variable
 CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(:), ALLOCATABLE :: YVLNAME !Long name of each restart variables
 CHARACTER(LEN=NF90_MAX_NAME), DIMENSION(:), ALLOCATABLE :: YUNIT   !Unit of each restart variable
!
 CHARACTER(LEN=NF90_MAX_NAME) :: YFILE,YTITLE,YTIMEUNIT
!
REAL,    DIMENSION(:), ALLOCATABLE ::  ZLON
REAL,    DIMENSION(:), ALLOCATABLE ::  ZLAT
LOGICAL, DIMENSION(:), ALLOCATABLE ::  LDOUBLE
LOGICAL, DIMENSION(:), ALLOCATABLE ::  LLAKE
!
INTEGER :: IND, INCID, INUM
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
! * Number of restart variable
!
IF (LHOOK) CALL DR_HOOK('INIT_RESTART_TRIP',0,ZHOOK_HANDLE)
IND = 1
IF(CGROUNDW=='CST'.OR.CGROUNDW=='DIF') IND = IND + 1
IF(LFLOOD)IND = IND + 3
IF(CLAKE=='MLK')IND  = IND + 1
!
! * Allocate netcdf file attributs
!
ALLOCATE(YVNAME  (IND))
ALLOCATE(YVLNAME (IND))
ALLOCATE(YUNIT   (IND))
ALLOCATE(LDOUBLE (IND))
ALLOCATE(LLAKE(IND))
!
ALLOCATE(ZLON(KLON))
ALLOCATE(ZLAT(KLAT))
!
! * Initialyse netcdf file attributs
!
YVNAME (1) = 'SURF_STO                  '
YVLNAME(1) = 'River storage             '
YUNIT  (1) = 'kg                        '
LDOUBLE(1)=.TRUE.
LLAKE(1) = .FALSE.
!
!
INUM = 1
!
IF(CGROUNDW=='CST')THEN
!        
INUM = INUM + 1
YVNAME (INUM) = 'GROUND_STO                '
YVLNAME(INUM) = 'Groundwater storage       '
YUNIT  (INUM) = 'kg                        '
LDOUBLE(INUM)=.TRUE.
LLAKE(INUM) = .FALSE.
!
!
ELSEIF(CGROUNDW=='DIF')THEN
!        
INUM = INUM + 1
YVNAME (INUM) = 'HGROUND                   '
YVLNAME(INUM) = 'Groundwater height        '
YUNIT  (INUM) = 'm                         '
LDOUBLE(INUM)=.TRUE.
LLAKE(INUM) = .FALSE.
!
!
ENDIF
!
IF(LFLOOD)THEN
!
INUM = INUM + 1
YVNAME (INUM) = 'FLOOD_STO                 '
YVLNAME(INUM) = 'Floodplain storage        '
YUNIT  (INUM) = 'kg                        '
LDOUBLE(INUM)=.TRUE.
LLAKE(INUM) = .FALSE.
!
!
INUM = INUM + 1
YVNAME (INUM) = 'FFLOOD                    '
YVLNAME(INUM) = 'TRIP flooded fraction     '
YUNIT  (INUM) = '-                         '
LDOUBLE(INUM)=.TRUE.
LLAKE(INUM) = .FALSE.
!
!
INUM = INUM + 1
YVNAME (INUM) = 'HFLOOD                    '
YVLNAME(INUM) = 'Flood depth               '
YUNIT  (INUM) = 'm                         '
LDOUBLE(INUM)=.TRUE.
LLAKE(INUM) = .FALSE.

!
!
ENDIF
!
IF(CLAKE=='MLK')THEN
!
INUM = INUM +1
YVNAME(INUM) = 'LAKE_STO'
YVLNAME(INUM)= 'Lake storage'
YUNIT(INUM)  = 'kg'
LDOUBLE(INUM)=.TRUE.
LLAKE(INUM)  = .TRUE.
!
!INUM = INUM +1
!YVNAME(INUM) = 'H_LAKE'
!YVLNAME(INUM)= 'Lake height'
!YUNIT(INUM)  = 'm'
!LDOUBLE(INUM)=.TRUE.
!LLAKE(INUM)  = .TRUE.
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
                ZLON,ZLAT,XUNDEF,LNCPRINT,INCID,OTIME,ODOUBLE=LDOUBLE,OLAKE=LLAKE,KLAKE_NUM=KLAKE_NUM)
ELSE
  CALL NCCREATE(KLISTING,YFILE,YTITLE,YTIMEUNIT,INUM,YVNAME,YVLNAME,YUNIT,ZLON,ZLAT, &
                XUNDEF,LNCPRINT,INCID,OTIME,ODOUBLE=LDOUBLE)
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
IF (LHOOK) CALL DR_HOOK('INIT_RESTART_TRIP',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE INIT_RESTART_TRIP
