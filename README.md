# CTRIP

CTRIP code, as described in:

Munier, S. and Decharme, B. (2022). River network and hydro-geomorphological parameters at 1∕12° resolution for global hydrological and climate studies. Earth System Science Data, 14, 2239–2258. https://doi.org/10.5194/essd-14-2239-2022.


# Installation within the SURFEX environment
Assuming that SURFEXDIR is the main directory of SURFEX.

1. Get the CTRIP code from the git repository:
cd $SURFEXDIR/src/LIB
git clone https://github.com/UMR-CNRM/CTRIP.git

2. Change the Makefile.SURFEX.mk to point to the new CTRIP directory:
cd $SURFEXDIR/src/
sed -i "s/TRIPv2/CTRIP/" Makefile.SURFEX.mk

3. Compile SURFEX, with the following variable to also compile CTRIP and enable the coupling between ISBA and CTRIP via OASIS:
export VER_OASIS="mct"

