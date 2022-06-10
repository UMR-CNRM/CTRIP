!     #########
SUBROUTINE READ_NAM_TRIP_GRID (TPG, &
                               KLISTING)
!############################
!
!!****  *READ_NAM_TRIP_GRID* - routine to read in namelist the TRIP horizontal grid
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
!!      Original    05/2008 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
USE MODD_TRIP_GRID, ONLY : TRIP_GRID_t
!
USE MODI_TRIP_POSNAM
USE MODI_ABORT_TRIP
USE MODI_OPEN_TRIP_NAMELIST
USE MODI_CLOSE_TRIP_NAMELIST
!
USE MODE_TRIP_GRID
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(TRIP_GRID_t), INTENT(INOUT) :: TPG
!
INTEGER, INTENT(IN) :: KLISTING
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUNAM ! namelist file  logical unit
INTEGER :: I
LOGICAL :: GFOUND
REAL    :: ZWORK
!
!
!*       0.3   Declarations of namelist
!              ------------------------
!
REAL    :: TLONMIN = 0.  ! minimum longitude (degrees)
REAL    :: TLONMAX = 0.  ! maximum longitude (degrees)
REAL    :: TLATMIN = 0.  ! minimum latitude  (degrees)
REAL    :: TLATMAX = 0.  ! maximum latitude  (degrees)
REAL    :: TRES    = 0.  ! 1deg or 0.5deg resolution
!
REAL, DIMENSION(:), ALLOCATABLE ::  ZLON
REAL, DIMENSION(:), ALLOCATABLE ::  ZLAT
!
INTEGER :: ILON,ILAT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_TRIP_GRID/TLONMIN, TLONMAX, TLATMIN, TLATMAX, TRES
!
!------------------------------------------------------------------------------
!
!*       1.    opening of namelist
! 
IF (LHOOK) CALL DR_HOOK('READ_NAM_TRIP_GRID',0,ZHOOK_HANDLE)
!
 CALL OPEN_TRIP_NAMELIST(ILUNAM)
!
!---------------------------------------------------------------------------
!
!*       2.    Reading of projection parameters
!              --------------------------------
!
 CALL TRIP_POSNAM(ILUNAM,'NAM_TRIP_GRID',GFOUND,KLISTING)
IF (GFOUND) THEN
   READ(UNIT=ILUNAM,NML=NAM_TRIP_GRID)
ELSE
   WRITE(KLISTING,*)'READ_NAM_TRIP_GRID: NAM_TRIP_GRID not found in namelist'
   CALL ABORT_TRIP('READ_NAM_TRIP_GRID: NAM_TRIP_GRID not found in namelist')
ENDIF        
!
 CALL CLOSE_TRIP_NAMELIST(ILUNAM)
!
IF(TRES/=0.5.AND.TRES/=1.0)THEN
  IF(TRES<0.08.OR.TRES>0.09)THEN
    WRITE(KLISTING,*)'Error : The resolution of the TRIP grid must be 1deg or 0.5deg '
    WRITE(KLISTING,*)'        or 0.083333deg over France                             '
    WRITE(KLISTING,*)'        In NAM_TRIP_GRID, TRES should be 0.083333 or 0.5 or 1. '
    CALL ABORT_TRIP('READ_NAM_TRIP_GRID: The resolution of the TRIP grid must be 1deg or 0.5deg or 0.083333deg')
  ELSE
    TRES=REAL(NINT(TRES*12.))/12.    
  ENDIF
ENDIF
!
!---------------------------------------------------------------------------
!
!*       3.    Number of lattitude and longitude
!
ILON = INT((TLONMAX-TLONMIN)/TRES)
ILAT = INT((TLATMAX-TLATMIN)/TRES)
!
!---------------------------------------------------------------------------
!
!*       4.    lattitude and longitude values
!
ALLOCATE(ZLON(ILON))
ALLOCATE(ZLAT(ILAT))
!
ZWORK = TLONMIN-(TRES/2.)
DO I=1,ILON
   ZWORK   = ZWORK + TRES
   ZLON(I) = ZWORK
ENDDO
!
ZWORK =TLATMIN-(TRES/2.)
DO I=1,ILAT
   ZWORK   = ZWORK + TRES
   ZLAT(I) = ZWORK
ENDDO
!
!---------------------------------------------------------------------------
!
!*       5.    All this information stored into PTRIP_GRID
!              -------------------------------------------
!
ALLOCATE(TPG%XTRIP_GRID(7+ILON+ILAT))
TPG%XTRIP_GRID(:) = 0.0
!
 CALL PUT_TRIP_GRID(TPG%XTRIP_GRID,TLONMIN,TLONMAX,TLATMIN,TLATMAX,TRES,ILON,ILAT,ZLON,ZLAT)
!
DEALLOCATE(ZLON)
DEALLOCATE(ZLAT)
!
IF (LHOOK) CALL DR_HOOK('READ_NAM_TRIP_GRID',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_TRIP_GRID
