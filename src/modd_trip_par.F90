!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
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












