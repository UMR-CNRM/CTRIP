!#######################
MODULE  MODN_TRIP_ASSIM
!#######################
!
IMPLICIT NONE
!
!*    Names of model
!     --------------
LOGICAL               :: LASSIM = .FALSE.      ! TRUE if assimilation run
LOGICAL               :: LPARAMENS = .FALSE.   ! TRUE for use of ensemble TRIP_PARAM_???.nc
LOGICAL               :: LINIT_PERT = .TRUE.   ! TRUE for initial perturbation
!CHARACTER(LEN=3)      :: CHQ = 'DIS'           ! Observation variable
!                                               ! 'DIS' : Discharge
!                                               ! 'HST' : Stream elevation
!                                               ! 'HAN' : Stream elevation anomaly
!CHARACTER(LEN=50)     :: CHSTREAM_FILE = 'TRIP_HSTREAM_MEAN.nc' ! Name of averaged water depth file
LOGICAL               :: LOBS_Q = .TRUE.       ! TRUE if discharge assimilation
LOGICAL               :: LOBS_H = .FALSE.      ! TRUE if water depth assimilation
CHARACTER(LEN=3)      :: CLOCAL = 'FUN'        ! Localization based on function (FUN) or covariance (COV)
CHARACTER(LEN=50)     :: CLOCAL_FILE = 'TRIP_LOCAL_VARIOGRAM.nc' ! Name of localization file
INTEGER               :: NLOCAL_LEN = 0        ! Localization length (in pixels)
REAL                  :: XLOCAL_AMP = 1.       ! Multiplicative factor on local covariance
INTEGER               :: NSMOOTH_LEN = 0       ! Smoothing length (in number of forcing time steps)
REAL                  :: XTSTEP_OBS = 86400.   ! Time step of observations (in seconds)
LOGICAL               :: LUSE_OBS_ERR = .FALSE.! If true, sigma_r provided in OBS.nc, else XSGIMA_R
REAL                  :: XSIGMA_R_Q = 0.1      ! Observation error for discharge (multiplicative)
REAL                  :: XSIGMA_R_H = 0.1      ! Observation error for discharge (additive)
REAL                  :: XMEAN_P = 1.0         ! Mean of initial perturbation (multiplicative)
REAL                  :: XAMP_P  = 0.01        ! Amplitude of initial perturbation
CHARACTER(LEN=3)      :: CINFL = 'DEF'         ! Inflation method (DEF, IMP, A09, S21)
REAL                  :: XMEAN_I = 0.0         ! USELESS Mean of inflation (additive)
REAL                  :: XAMP_I  = 1.01        ! Amplitude of inflation (should be >1)
REAL                  :: XINFL_INIT = 1.01     ! Initial value of inflation
REAL                  :: XINFL_VAR_INIT = 0.5  ! Initial value of inflation variance
!
!-------------------------------------------------------------------------------
!
!*       1.    NAMELISTS
!              ---------
!
NAMELIST/NAM_TRIP_ASSIM/LASSIM,LPARAMENS,LINIT_PERT,LOBS_Q,LOBS_H, &
                        CLOCAL,CLOCAL_FILE,NLOCAL_LEN,XLOCAL_AMP,NSMOOTH_LEN,XTSTEP_OBS, &
                        LUSE_OBS_ERR,XSIGMA_R_Q,XSIGMA_R_H,XMEAN_P,XAMP_P,& 
                        XMEAN_I,XAMP_I,CINFL,XINFL_INIT,XINFL_VAR_INIT
!
!-------------------------------------------------------------------------------
END MODULE MODN_TRIP_ASSIM

