!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!#######################
MODULE  MODN_TRIP
!#######################
!
!!****  *MODN_TRIP* define the variables and namelist for TRIP
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
!!      S. Munier   03/2020    CTRIP-12D and parallelization
!!      T. Guinaldo 04/2020    Add MLake
!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
!River reach length
!
LOGICAL          :: LCALCRIVLEN  = .TRUE. !Compute river reach length
!
!Stream flow velocity scheme
!
CHARACTER(LEN=3) :: CVIT  = 'DEF'     !Type of stream flow velocity
                                      !'DEF' = Constant velocit = 0.5m/s
                                      !'VAR' = variable velocity
!
LOGICAL          :: LAPPROXVEL = .FALSE. ! Use approximation for velocity computation
!
REAL             :: XCVEL = 0.5       ! Constant velocity value
!
REAL             :: XBANKSLOPE = 0.0  ! Slope of river banks
!
LOGICAL          :: LBACKWATER = .FALSE. ! Simple representation of backwater effect
                                         ! must be used with CVIT='VAR' and LFLOOD=.TRUE.
!
!Groundwater scheme
!
CHARACTER(LEN=3) :: CGROUNDW = 'DEF'  !Use groundwater scheme
                                      !'DEF' = No groundwater scheme
                                      !'CST' = Constant transfert time
                                      !'DIF' = Groundwater diffusive scheme
!
LOGICAL          :: LGWSUBF  = .TRUE. !Use sub-grid fraction to couple with SURFEX
                                      !as in Verges et al., JGR, 2014
!
REAL             :: XGWSUBD  = 0.0    !Sub-grid depth uses to adjust the WTD
                                      !used to compute the sub-grid fraction
!
LOGICAL          :: LCALCNEARAQ = .TRUE. ! If yes, nearest aquifer numbering is computed
                                         ! Else it is read from TRIP_PGD
!
!Floodplains scheme
!
LOGICAL          :: LFLOOD = .FALSE.  !if true, use TRIP-FLOOD
!
!Lake scheme
!
CHARACTER(LEN=3) :: CLAKE = 'DEF'     ! Use lake scheme MLAKE without reservoir
                                      !'DEF' No lake scheme
                                      !'MLK' Use of the masslake routine  
!
!Other attributes
!
REAL             :: XRATMED     = 1.1  ! Meandering ratio
REAL             :: XTSTEP      = 3600.
!
!-------------------------------------------------------------------------------
!
!*       1.    NAMELISTS
!              ---------
!
NAMELIST/NAM_TRIP/LCALCRIVLEN,CVIT,LAPPROXVEL,XCVEL,XBANKSLOPE, &
                  CGROUNDW,LGWSUBF,XGWSUBD,LCALCNEARAQ, &
                  LBACKWATER,LFLOOD,CLAKE,XRATMED,XTSTEP
!
!-------------------------------------------------------------------------------
END MODULE MODN_TRIP
