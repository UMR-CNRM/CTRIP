!###############
MODULE MODN_TRIP_OASIS
!###############
!
!!****  *MODN_TRIP_OASIS - declaration of namelist for TRIP-OASIS coupling
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       10/13
!!      B. Decharme 10/2016  bug surface/groundwater coupling
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
! *  Land surface variables for Trip - Oasis coupling
!
!-------------------------------------------------------------------------------
!
REAL             :: XTSTEP_CPL_LAND = -1.0     ! Coupling time step 
!
! Input variables
!
 CHARACTER(LEN=8) :: CRUNOFF     = '        '   ! Surface runoff 
 CHARACTER(LEN=8) :: CDRAIN      = '        '   ! Deep drainage 
 CHARACTER(LEN=8) :: CCALVING    = '        '   ! Calving flux
 CHARACTER(LEN=8) :: CSRCFLOOD   = '        '   ! Floodplains freshwater flux
!
! Output variables
!
 CHARACTER(LEN=8) :: CFFLOOD     = '        '   ! Floodplains fraction  
 CHARACTER(LEN=8) :: CPIFLOOD    = '        '   ! Flood potential infiltartion  
 CHARACTER(LEN=8) :: CWTD        = '        '   ! Water table depth  
 CHARACTER(LEN=8) :: CFWTD       = '        '   ! Grid-cell fraction of WTD to rise  
!
!-------------------------------------------------------------------------------
!
! * Sea variables for Trip - Oasis coupling
!
!-------------------------------------------------------------------------------
!
REAL             :: XTSTEP_CPL_SEA = -1.0   ! Coupling time step
!
 CHARACTER(LEN=8) :: CRIVDIS  = '        '   ! River discharges to ocean
 CHARACTER(LEN=8) :: CCALVGRE = '        '   ! Calving flux over greenland
 CHARACTER(LEN=8) :: CCALVANT = '        '   ! Calving flux over antarctica
!
!-------------------------------------------------------------------------------
!
!*       1.    NAMELISTS TRIP COUPLED WITH A LAND SURFACE MODEL
!              ------------------------------------------------
!
NAMELIST/NAM_TRIP_LAND_CPL/XTSTEP_CPL_LAND,CRUNOFF,CDRAIN, &
                           CFFLOOD,CPIFLOOD,CWTD,CFWTD,    &
                           CCALVING,CSRCFLOOD
!
!
!*       2.    NAMELISTS TRIP COUPLED WITH A OCEANIC GENERAL CIRCULATION MODEL
!              ---------------------------------------------------------------
!
NAMELIST/NAM_TRIP_SEA_CPL/XTSTEP_CPL_SEA,CRIVDIS,CCALVGRE,CCALVANT
!
!-------------------------------------------------------------------------------
!
END MODULE MODN_TRIP_OASIS
