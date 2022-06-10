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
