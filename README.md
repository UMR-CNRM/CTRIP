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


