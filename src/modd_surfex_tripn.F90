!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
MODULE MODD_SURFEX_TRIP_n
!
USE MODD_TRIP, ONLY : TRIP_t
USE MODD_TRIP_GRID, ONLY : TRIP_GRID_t
USE MODD_TRIP_STATE, ONLY : TRIP_STATE_t
USE MODD_TRIP_DIAG, ONLY : TRIP_DIAG_t
USE MODD_TRIP_LAKE, ONLY : TRIP_LAKE_t
!
!---------------------------------------------------------------------------
!
TYPE TRIP_MODEL_t
!
TYPE(TRIP_t)          :: TP
TYPE(TRIP_GRID_t)     :: TPG
TYPE(TRIP_STATE_t)    :: TPST
TYPE(TRIP_DIAG_t)     :: TPDG
TYPE(TRIP_LAKE_t)     :: TPLK
!
END TYPE TRIP_MODEL_t
!
END MODULE MODD_SURFEX_TRIP_n
