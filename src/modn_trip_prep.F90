!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!#######################
MODULE  MODN_TRIP_PREP
!#######################
!
!!****  *MODN_TRIP_PREP* define the variables for TRIP prep
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
!!      Original    12/2015
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!
!Constant transfert time for CGROUNDW ='CST'
!
REAL             :: XTAUG_UNIF  = 30.0 ! Constant transfert time value
!
!Transfert time for Groundwater diffusive scheme
!
REAL             :: XTAUG_UP    =  5.0 ! Upstream transfert time value
REAL             :: XTAUG_DOWN  = 30.0 ! Downstream transfert time value
!
!Comput equilibrium water table depth
!
LOGICAL          :: LGWEQ = .FALSE.
!
!Read restart flood
!
LOGICAL          :: LREAD_FLOOD = .FALSE.
!
!-------------------------------------------------------------------------------
!
!*       1.    NAMELISTS
!              ---------
!
NAMELIST/NAM_TRIP_PREP/XTAUG_UNIF,XTAUG_UP,XTAUG_DOWN,LGWEQ,LREAD_FLOOD
!
!-------------------------------------------------------------------------------
END MODULE MODN_TRIP_PREP
