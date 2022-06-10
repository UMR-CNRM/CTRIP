!###############################################
 SUBROUTINE GET_LAT_GWF (TPG, &
                         KLAT,PRES,PLAT)
!###############################################
!
!!****  *GET_LAT_GWF* - routine to get the TRIP longitude and latitude
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
!!      Original    01/2013 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_TRIP_GRID, ONLY : TRIP_GRID_t
!
USE MODE_TRIP_GRID
!
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
INTEGER,               INTENT(IN ) :: KLAT
REAL,                  INTENT(OUT) :: PRES
REAL, DIMENSION(KLAT), INTENT(OUT) :: PLAT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_LAT_GWF',0,ZHOOK_HANDLE)
 CALL GET_TRIP_GRID(TPG%XTRIP_GRID,PRES=PRES,PLAT=PLAT)
IF (LHOOK) CALL DR_HOOK('GET_LAT_GWF',1,ZHOOK_HANDLE)
!    
!---------------------------------------------------------------------------
!
END SUBROUTINE GET_LAT_GWF
