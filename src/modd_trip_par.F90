!######################
MODULE MODD_TRIP_PAR
!######################
!
!!****  *MODD_TRIP_PAR* - declaration of TRIP parameters
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify  the 
!     parameters related to the TRIP RRM. 
!
!!
!!      
!!
!!    AUTHOR
!!    ------
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original  22/05/08
!!      09/16   B. Decharme  limit wtd to -1000m
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
INTEGER, SAVE :: NDIMTAB
INTEGER, SAVE :: NUNDEF
!
REAL, SAVE    :: XM
REAL, SAVE    :: XVELMIN
REAL, SAVE    :: XHSMIN
!
REAL, SAVE    :: XUNDEF
REAL, SAVE    :: XRHOLW
REAL, SAVE    :: XDAY 
REAL, SAVE    :: XSEA 
REAL, SAVE    :: XYEAR 
!
REAL, SAVE    :: XRAD
REAL, SAVE    :: XPI 
!
LOGICAL, SAVE :: LNCPRINT
!
REAL, SAVE    :: XTIME_DIAG
!
REAL, SAVE    :: XGWDZMAX
!
!--------------------------------------------------------------------------------
!
END MODULE MODD_TRIP_PAR












