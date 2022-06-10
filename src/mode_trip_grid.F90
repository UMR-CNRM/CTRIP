!##############################
MODULE MODE_TRIP_GRID
!##############################
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!############################################################################
!############################################################################
!############################################################################
 CONTAINS
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE PUT_TRIP_GRID(PTRIP_GRID,PLONMIN,PLONMAX,PLATMIN, &
                                 PLATMAX,PRES,KLON,KLAT,PLON,PLAT    )  
!     ####################################################################
!
!!****  *PUT_TRIP_GRID* - routine to store in PTRIP_GRID the horizontal grid
!!
!!    AUTHOR
!!    ------
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2005 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!
!*       0.1   Declarations of arguments
!              -------------------------
!
REAL,               INTENT(IN)  :: PLONMIN  ! minimum longitude
REAL,               INTENT(IN)  :: PLONMAX  ! maximum longitude
REAL,               INTENT(IN)  :: PLATMIN  ! minimum latitude
REAL,               INTENT(IN)  :: PLATMAX  ! maximum latitude
REAL,               INTENT(IN)  :: PRES     ! 1° or 0.5° resolution
INTEGER,            INTENT(IN)  :: KLON     ! number of points in longitude
INTEGER,            INTENT(IN)  :: KLAT     ! number of points in latitude
REAL, DIMENSION(:), INTENT(IN)  :: PLON     ! longitude
REAL, DIMENSION(:), INTENT(IN)  :: PLAT     ! latitude
REAL, DIMENSION(:), INTENT(OUT) :: PTRIP_GRID! parameters defining this grid
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_GRID:PUT_TRIP_GRID',0,ZHOOK_HANDLE)
PTRIP_GRID(1)             = PLONMIN
PTRIP_GRID(2)             = PLONMAX
PTRIP_GRID(3)             = PLATMIN
PTRIP_GRID(4)             = PLATMAX
PTRIP_GRID(5)             = PRES
PTRIP_GRID(6)             = FLOAT(KLON)
PTRIP_GRID(7)             = FLOAT(KLAT)
PTRIP_GRID(8:7+KLON)      = PLON
PTRIP_GRID(8+KLON:7+KLON+KLAT) = PLAT
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_GRID:PUT_TRIP_GRID',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE PUT_TRIP_GRID
!############################################################################
!############################################################################
!############################################################################
!     ####################################################################
      SUBROUTINE GET_TRIP_GRID(PTRIP_GRID,PLONMIN,PLONMAX,PLATMIN, &
                                 PLATMAX,PRES,KLON,KLAT,PLON,PLAT    )  
!     ####################################################################
!
!!****  *GET_TRIP_GRID* - routine to get from PTRIP_GRID the horizontal grid
!!
!!    AUTHOR
!!    ------
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2005 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!
!*       0.1   Declarations of arguments
!              -------------------------
!
REAL, DIMENSION(:), INTENT(IN)            :: PTRIP_GRID! parameters defining this grid
REAL,               INTENT(OUT), OPTIONAL :: PLONMIN  ! minimum longitude
REAL,               INTENT(OUT), OPTIONAL :: PLONMAX  ! maximum longitude
REAL,               INTENT(OUT), OPTIONAL :: PLATMIN  ! minimum latitude
REAL,               INTENT(OUT), OPTIONAL :: PLATMAX  ! maximum latitude
REAL,               INTENT(OUT), OPTIONAL :: PRES     ! 1° or 0.5° resolution
INTEGER,            INTENT(OUT), OPTIONAL :: KLON     ! number of points in longitude
INTEGER,            INTENT(OUT), OPTIONAL :: KLAT     ! number of points in latitude
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PLON     ! longitude
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PLAT     ! latitude
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILON, ILAT
INTEGER :: IL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_GRID:GET_TRIP_GRID',0,ZHOOK_HANDLE)
ILON = NINT(PTRIP_GRID(6))
ILAT = NINT(PTRIP_GRID(7))
!
IF (PRESENT(PLONMIN))  PLONMIN = PTRIP_GRID(1)
IF (PRESENT(PLONMAX))  PLONMAX = PTRIP_GRID(2)
IF (PRESENT(PLATMIN))  PLATMIN = PTRIP_GRID(3)
IF (PRESENT(PLATMAX))  PLATMAX = PTRIP_GRID(4)
IF (PRESENT(PRES   ))  PRES    = PTRIP_GRID(5)
IF (PRESENT(KLON   ))  KLON    = ILON
IF (PRESENT(KLAT   ))  KLAT    = ILAT
IF (PRESENT(PLON   ))  PLON(:) = PTRIP_GRID(8:7+ILON)
IF (PRESENT(PLAT   ))  PLAT(:) = PTRIP_GRID(8+ILON:7+ILON+ILAT)
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_GRID:GET_TRIP_GRID',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE GET_TRIP_GRID
!---------------------------------------------------------------------------------
!
!############################################################################
!############################################################################
!############################################################################
!############################################################################
!############################################################################
!############################################################################
!
END MODULE MODE_TRIP_GRID
