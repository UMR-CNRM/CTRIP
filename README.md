# CTRIP

CTRIP code, as described in:

Munier, S. and Decharme, B. (2022). River network and hydro-geomorphological parameters at 1∕12° resolution for global hydrological and climate studies. Earth System Science Data, 14, 2239–2258. https://doi.org/10.5194/essd-14-2239-2022.


# Installation within the SURFEX environment
Assuming that `SURFEXDIR` is the main directory of SURFEX.

1. Get the CTRIP code from the git repository:
```
    cd $SURFEXDIR/src/LIB
    git clone https://github.com/UMR-CNRM/CTRIP.git
```
2. Change the `Makefile.SURFEX.mk` to point to the new CTRIP directory:
```
    cd $SURFEXDIR/src/
    sed -i "s/TRIPv2/CTRIP/" Makefile.SURFEX.mk
```
3. Compile SURFEX, with the following variable to also compile CTRIP and enable the coupling between ISBA and CTRIP via OASIS:
```
    export VER_OASIS="mct"
```


# Main options

### NAM_TRIP
| key | default | description |
| --- | --- | --- |
| LCALCRIVLEN | TRUE | Compute river reach length or read it from TRIP_PGD |
| CVIT | DEF | Type of stream flow velocity (DEF/VAR) |
| XCVEL | 0.5 | Constant velocity value for CVIT=DEF (m/s) |
| XRATMED | 1.1 | Meandering ratio (not used for 1/12° resolution) |
| XTSTEP | 3600 | Computation time step (s) |
| CGROUNDW | DEF | Use groundwater scheme (DEF/CST/DIF) |
| LGWSUBF | TRUE | Use sub-grid fraction to couple with SURFEX |
| XGWSUBD | 0.0 | Sub-grid depth uses to adjust the WTD (m) |
| LCALCNEARAQ | TRUE | Compute nearest aquifer numbering or read it from TRIP_PGD |
| LFLOOD | FALSE | Use floodplain scheme |
| CLAKE | DEF | Use MLAKE lake scheme (DEF/MLK) |

### NAM_TRIP_PREP
| key | default | description |
| --- | --- | --- |
| XTAUG_UNIF | 30.0 | Constant transfert time value (for CGROUNDW=CST) |
| XTAUG_UP |  5.0 | Upstream transfert time value (for CGROUNDW=DIF) |
| XTAUG_DOWN | 30.0 | Downstream transfert time value (for CGROUNDW=DIF) |
| LGWEQ | FALSE | Compute equilibrium water table depth |
| LREAD_FLOOD | FALSE | Read restart flood |

### NAM_TRIP_RUN
| key | default | description |
| --- | --- | --- |
| LCUMFRC | FALSE | Cumulated (or not) forcing variables |
| CREADFRC | LATLON | Forcing file format (VECTOR/LATLON) |
| LISBAFRC | FALSE | True if ISBA_DIAG files used as forcing |
| CDRAIN | DRAIN | Drainage name in FORCING.nc file |
| CRUNOFF | RUNOFF | Surface runoff name in FORCING.nc file |
| CSRC_FLOOD | | Flood source term (P-E-I) name in FORCING.nc file |
| CFILE_ISBAFRC | ISBA_DIAG_CUMUL.nc | Name of forcing file (for use in SFX_FORCING) |
| CFILE_DRAIN | DRAINC_ISBA.BIN | Name of drainage file (for use in SFX_FORCING) |
| CFILE_RUNOFF | RUNOFFC_ISBA.BIN | Name of runoff file (for use in SFX_FORCING) |
| LRESTART | TRUE | Write restart file |
| LPRINT | FALSE | Write some information during simulation |
| LWR_DIAG | TRUE | Write diag file |
| LDIAG_MISC | FALSE | Add diagnostic variables if true (obsolete, use CSELECT) |
| CSELECT | | List of output variables |
| XTSTEP_RUN | 86400 | Time step of the forcing file (s) |
| XTSTEP_DIAG | 86400 | Time step of the output diagnostics (s) |

### NAM_TRIP_ASSIM
| key | default | description |
| --- | --- | --- |
| LASSIM | .FALSE.      | TRUE if assimilation run |
| LPARAMENS | .FALSE.   | TRUE for use of ensemble TRIP_PARAM_???.nc |
| LOBS_Q | .TRUE.       | TRUE if discharge assimilation |
| LOBS_H | .FALSE.      | TRUE if water depth assimilation |
| CLOCAL | 'FUN'        | Localization based on function (FUN) or covariance (COV) |
| CLOCAL_FILE | 'TRIP_LOCAL_VARIOGRAM.nc' | Name of localization file |
| NLOCAL_LEN | 0        | Localization length in number of pixels (for CLOCAL='FUN') |
| XLOCAL_AMP | 1.       | Multiplicative factor on local covariance |
| NSMOOTH_LEN | 0       | Smoothing length (in number of forcing time steps) |
| XTSTEP_OBS | 86400.   | Time step of observations (in seconds) |
| LUSE_OBS_ERR | .FALSE.| If true, XSGIMA_R provided in OBS.nc, else XSGIMA_R |
| XSIGMA_R_Q | 0.1      | Observation error for discharge (multiplicative) |
| XSIGMA_R_H | 0.1      | Observation error for discharge (additive) |
| LINIT_PERT | .TRUE.   | TRUE for initial perturbation |
| XMEAN_P | 1.0         | Mean of initial perturbation (multiplicative) |
| XAMP_P  | 0.01        | Amplitude of initial perturbation |
| CINFL | 'DEF'         | Inflation method (DEF, IMP, A09, S21) |
| XAMP_I  | 1.01        | Amplitude of inflation (should be >1) |
| XINFL_INIT | 1.01     | Initial value of inflation |
| XINFL_VAR_INIT | 0.5  | Initial value of inflation variance |





