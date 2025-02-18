!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!#######################
MODULE  MODN_TRIP_RUN
!#######################
!
!!****  *MODN_TRIP_RUN* define the variables and namelist for TRIP
!                       driver programs
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
!!      Original    04/2013
!!      S.Sénési    08/11/16 : interface to XIOS
!!      S.Munier    12/2020 : selection of output variables
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!*    Names of model
!     --------------
!
CHARACTER(LEN=6)                  :: CMODEL_NAME  = 'trip'
!
!*    Names of files
!     --------------
!
CHARACTER(LEN=28), PARAMETER      :: CNAMELIST     = 'TRIP_OPTIONS.nam'
CHARACTER(LEN=50)                 :: CFILE_FRC     = 'TRIP_FORCING.nc'
CHARACTER(LEN=50)                 :: CFILE_ISBAFRC = 'ISBA_DIAG_CUMUL.nc' ! for use in SFX_FORCING
CHARACTER(LEN=50)                 :: CFILE_DRAIN   = 'DRAINC_ISBA.BIN'    ! for use in SFX_FORCING
CHARACTER(LEN=50)                 :: CFILE_RUNOFF  = 'RUNOFFC_ISBA.BIN'   ! for use in SFX_FORCING
!
!
!*    General flags defining forcing options
!     --------------------------------------
!
!
LOGICAL                           :: LCUMFRC  = .FALSE.  ! Cumulated (or not) forcing variables
CHARACTER(LEN=6)                  :: CREADFRC = 'LATLON' ! Forcing file format
                                                         ! VECTOR = vector (normaly ilat*ilon)
                                                         ! LATLON = Regular lat lon grid
LOGICAL                           :: LISBAFRC = .FALSE.  ! True if ISBA_DIAG files used as forcing
!
CHARACTER(LEN=25)                 :: CDRAIN     = 'DRAIN'  ! Drainage name in FORCING.nc file
CHARACTER(LEN=25)                 :: CRUNOFF    = 'RUNOFF' ! Surface runoff name in FORCING.nc file
CHARACTER(LEN=25)                 :: CSRC_FLOOD = '      ' ! Flood source term (P-E-I) name in FORCING.nc file
!
!
!*    General flag
!     ------------
!
LOGICAL                           :: LDIAG_MISC = .FALSE.  ! if true, more diag for model testing
LOGICAL                           :: LRESTART   = .TRUE.   ! write restart file
LOGICAL                           :: LPRINT     = .FALSE.  ! write some information in an ascii file
LOGICAL                           :: LWR_DIAG   = .TRUE.   ! write diag file
!
!*    Time steps
!     ----------
!
REAL                              :: XTSTEP_RUN  = 86400.0
REAL                              :: XTSTEP_DIAG = 86400.0
!
!*    List of variables to be written as diagnostics
!     ----------------------------------------------
!
CHARACTER(LEN=12), DIMENSION(4000) :: CSELECT
!
!-------------------------------------------------------------------------------
!
!*       1.    NAMELISTS
!              ---------
!
NAMELIST/NAM_TRIP_RUN/CREADFRC,CDRAIN,CRUNOFF,LCUMFRC,LISBAFRC, &
                      CFILE_ISBAFRC,CFILE_DRAIN,CFILE_RUNOFF, &
                      LDIAG_MISC,LPRINT,LRESTART,XTSTEP_RUN,XTSTEP_DIAG,LWR_DIAG,CSELECT
!
!-------------------------------------------------------------------------------
!
END MODULE MODN_TRIP_RUN
