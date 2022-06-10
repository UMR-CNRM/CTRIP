!#########
SUBROUTINE TRIP_OASIS_PREP (TPG, &
                            KLISTING,KLON,KLAT)
!###############################################
!
!!****  *TRIP_OASIS_PREP* - Definitions for exchange of coupling fields
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2013
!!      B. Decharme 10/2016  bug surface/groundwater coupling   
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
USE MODD_TRIP_GRID, ONLY : TRIP_GRID_t
!
USE MODN_TRIP_OASIS
USE MODD_TRIP_OASIS
!
USE MODD_TRIP_PAR, ONLY : NUNDEF
!
USE MODD_TRIP_OASIS
!
USE MODE_TRIP_GRID
!
USE MODI_ABORT_TRIP
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef CPLOASIS
USE MOD_OASIS
#endif
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(TRIP_GRID_t), INTENT(INOUT) :: TPG
!
INTEGER, INTENT(IN)    :: KLISTING 
INTEGER, INTENT(IN)    :: KLON
INTEGER, INTENT(IN)    :: KLAT
!
!*       0.2   Declarations of local parameter
!              -------------------------------
!
INTEGER,               PARAMETER  :: INCORNER     = 4       ! Number of grid-cell corners
INTEGER,               PARAMETER  :: IG_PARSIZE   = 3       ! Size of array decomposition
INTEGER,               PARAMETER  :: IG_NSEGMENTS = 1       ! Number of segments of process decomposition
INTEGER, DIMENSION(2), PARAMETER  :: IVAR_NODIMS  = (/2,1/) ! rank and number of bundles in coupling field
!
 CHARACTER(LEN=4)                  :: YCPL_LAND = 'tlan'
 CHARACTER(LEN=4)                  :: YCPL_GW   = 'tgw '
 CHARACTER(LEN=4)                  :: YCPL_FLD  = 'tfld'
 CHARACTER(LEN=4)                  :: YCPL_SEA  = 'tsea'
 CHARACTER(LEN=4)                  :: YCPL_GRE  = 'tgre'
 CHARACTER(LEN=4)                  :: YCPL_ANT  = 'tant'
!
!*       0.3   Declarations of local variables
!              -------------------------------
!
REAL                                    :: ZRES
REAL,    DIMENSION(KLON)                :: ZLON1D
REAL,    DIMENSION(KLAT)                :: ZLAT1D
REAL,    DIMENSION(KLON,KLAT)           :: ZLON
REAL,    DIMENSION(KLON,KLAT)           :: ZLAT
REAL,    DIMENSION(KLON,KLAT)           :: ZAREA
INTEGER, DIMENSION(KLON,KLAT)           :: IMASK
!
REAL,    DIMENSION(KLON,KLAT,INCORNER)  :: ZCORNER_LON
REAL,    DIMENSION(KLON,KLAT,INCORNER)  :: ZCORNER_LAT
!
INTEGER, DIMENSION(IG_PARSIZE) :: IPARAL      ! Decomposition for each proc
INTEGER, DIMENSION(2,2)        :: IVAR_SHAPE  ! indexes for the coupling field local dimension
!
INTEGER                        :: IPART_ID ! Local partition ID
INTEGER                        :: IERR     ! Error info
!
LOGICAL                        :: GFOUND   ! Return code when searching namelist
INTEGER                        :: INAM     ! logical unit of namelist file
!
INTEGER                        :: JLON, JLAT, JC, IFLAG
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_OASIS_PREP',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
#ifdef CPLOASIS
!-------------------------------------------------------------------------------
!
!*       1.     Grid definition :
!               -----------------
!
 CALL GET_TRIP_GRID(TPG%XTRIP_GRID,PRES=ZRES,PLON=ZLON1D,PLAT=ZLAT1D)
!
ZRES = ZRES / 2.0
!
!     4_______3
!     |       |
!     |   .   |
!     |       |
!     |_______|
!     1       2
!
DO JLON=1,KLON
  DO JLAT=1,KLAT
!
!    grid cell center
!
     ZLON(JLON,JLAT)=ZLON1D(JLON)
     ZLAT(JLON,JLAT)=ZLAT1D(JLAT)
!
!    grid cell corner (counterclockwise sense)
!
     ZCORNER_LON(JLON,JLAT,1) = ZLON1D(JLON) - ZRES
     ZCORNER_LAT(JLON,JLAT,1) = ZLAT1D(JLAT) - ZRES
!     
     ZCORNER_LON(JLON,JLAT,3) = ZLON1D(JLON) + ZRES
     ZCORNER_LAT(JLON,JLAT,3) = ZLAT1D(JLAT) + ZRES
!
  ENDDO
ENDDO
!     
ZCORNER_LON(:,:,4) = ZCORNER_LON(:,:,1)
ZCORNER_LAT(:,:,4) = ZCORNER_LAT(:,:,3)
!  
ZCORNER_LON(:,:,2) = ZCORNER_LON(:,:,3)
ZCORNER_LAT(:,:,2) = ZCORNER_LAT(:,:,1)
!
 CALL OASIS_START_GRIDS_WRITING(IFLAG)
!
!
!*       1.1    Grid definition for Land surface :
!               ----------------------------------
!
IF(LCPL_LAND)THEN
!
! 0 = not masked ; 1 = masked
  WHERE(TPG%GMASK(:,:))
    IMASK(:,:) = 0
  ELSEWHERE
    IMASK(:,:) = 1
  ENDWHERE
!
  ZAREA(:,:) = TPG%XAREA(:,:) * (1.0-IMASK(:,:))
!
  CALL OASIS_WRITE_GRID  (YCPL_LAND,KLON,KLAT,ZLON(:,:),ZLAT(:,:))
  CALL OASIS_WRITE_CORNER(YCPL_LAND,KLON,KLAT,INCORNER,ZCORNER_LON(:,:,:),ZCORNER_LAT(:,:,:))
  CALL OASIS_WRITE_AREA  (YCPL_LAND,KLON,KLAT,ZAREA(:,:))
  CALL OASIS_WRITE_MASK  (YCPL_LAND,KLON,KLAT,IMASK(:,:))
!
ENDIF
!
! groundwater surface coupling case
!
IF(LCPL_GW)THEN
!
! 0 = not masked ; 1 = masked
  WHERE(TPG%GMASK_GW(:,:))
    IMASK(:,:) = 0
  ELSEWHERE
    IMASK(:,:) = 1
  ENDWHERE
!
  ZAREA(:,:) = TPG%XAREA(:,:) * (1.0-IMASK(:,:))
!
  CALL OASIS_WRITE_GRID  (YCPL_GW,KLON,KLAT,ZLON(:,:),ZLAT(:,:))
  CALL OASIS_WRITE_CORNER(YCPL_GW,KLON,KLAT,INCORNER,ZCORNER_LON(:,:,:),ZCORNER_LAT(:,:,:))
  CALL OASIS_WRITE_AREA  (YCPL_GW,KLON,KLAT,ZAREA(:,:))
  CALL OASIS_WRITE_MASK  (YCPL_GW,KLON,KLAT,IMASK(:,:))
!
ENDIF
!
! Floodplains surface coupling case
!
IF(LCPL_FLOOD)THEN
!
! 0 = not masked ; 1 = masked
  WHERE(TPG%GMASK_FLD(:,:))
    IMASK(:,:) = 0
  ELSEWHERE
    IMASK(:,:) = 1
  ENDWHERE
!
  ZAREA(:,:) = TPG%XAREA(:,:) * (1.0-IMASK(:,:))
!
  CALL OASIS_WRITE_GRID  (YCPL_FLD,KLON,KLAT,ZLON(:,:),ZLAT(:,:))
  CALL OASIS_WRITE_CORNER(YCPL_FLD,KLON,KLAT,INCORNER,ZCORNER_LON(:,:,:),ZCORNER_LAT(:,:,:))
  CALL OASIS_WRITE_AREA  (YCPL_FLD,KLON,KLAT,ZAREA(:,:))
  CALL OASIS_WRITE_MASK  (YCPL_FLD,KLON,KLAT,IMASK(:,:))
!
ENDIF
!
!*       1.2    Grid definition for sea :
!               -------------------------
!
IF(LCPL_SEA)THEN
!
! 0 = not masked ; 1 = masked
  WHERE(TPG%NGRCN(:,:)==9.OR.TPG%NGRCN(:,:)==12)
    IMASK(:,:) = 0
  ELSEWHERE
    IMASK(:,:) = 1
  ENDWHERE
!
  ZAREA(:,:) = TPG%XAREA(:,:) * (1.0-IMASK(:,:))
!
  CALL OASIS_WRITE_GRID  (YCPL_SEA,KLON,KLAT,ZLON(:,:),ZLAT(:,:))
  CALL OASIS_WRITE_CORNER(YCPL_SEA,KLON,KLAT,INCORNER,ZCORNER_LON(:,:,:),ZCORNER_LAT(:,:,:))
  CALL OASIS_WRITE_AREA  (YCPL_SEA,KLON,KLAT,ZAREA(:,:))
  CALL OASIS_WRITE_MASK  (YCPL_SEA,KLON,KLAT,IMASK(:,:))
!
ENDIF
!
!*       1.3    Grid definition for calving flux :
!               ----------------------------------
!
IF(LCPL_CALVSEA)THEN
!
!* Over Greenland
!
! 0 = not masked ; 1 = masked
  WHERE(TPG%GMASK_GRE(:,:))
    IMASK(:,:) = 0
  ELSEWHERE
    IMASK(:,:) = 1
  ENDWHERE
!
  ZAREA(:,:) = TPG%XAREA(:,:) * (1.0-IMASK(:,:))
!
  CALL OASIS_WRITE_GRID  (YCPL_GRE,KLON,KLAT,ZLON(:,:),ZLAT(:,:))
  CALL OASIS_WRITE_CORNER(YCPL_GRE,KLON,KLAT,INCORNER,ZCORNER_LON(:,:,:),ZCORNER_LAT(:,:,:))
  CALL OASIS_WRITE_AREA  (YCPL_GRE,KLON,KLAT,ZAREA(:,:))
  CALL OASIS_WRITE_MASK  (YCPL_GRE,KLON,KLAT,IMASK(:,:))
!
!* Over Antarctica
!
! 0 = not masked ; 1 = masked
  WHERE(TPG%GMASK_ANT(:,:))
    IMASK(:,:) = 0
  ELSEWHERE
    IMASK(:,:) = 1
  ENDWHERE
!
  ZAREA(:,:) = TPG%XAREA(:,:) * (1.0-IMASK(:,:))
!
  CALL OASIS_WRITE_GRID  (YCPL_ANT,KLON,KLAT,ZLON(:,:),ZLAT(:,:))
  CALL OASIS_WRITE_CORNER(YCPL_ANT,KLON,KLAT,INCORNER,ZCORNER_LON(:,:,:),ZCORNER_LAT(:,:,:))
  CALL OASIS_WRITE_AREA  (YCPL_ANT,KLON,KLAT,ZAREA(:,:))
  CALL OASIS_WRITE_MASK  (YCPL_ANT,KLON,KLAT,IMASK(:,:))
!
ENDIF
!
 CALL OASIS_TERMINATE_GRIDS_WRITING()
!
 CALL OASIS_ENDDEF(IERR)
!
IF(IERR/=OASIS_OK)THEN
   WRITE(KLISTING,*)'TRIP_OASIS_PREP: OASIS enddef problem, err = ',IERR
   CALL ABORT_TRIP('TRIP_OASIS_PREP: OASIS enddef problem')
ENDIF
!
!-------------------------------------------------------------------------------
#endif
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_OASIS_PREP',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE TRIP_OASIS_PREP
