!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!     #########
      SUBROUTINE INIT_TRIP_PAR 
!     ########################
!
!
!!****  *INIT_TRIP_PAR* - Initialization of TRIP parameters
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to specify  the 
!     parameters related to the TRIP RRM. 
!
!!
!!      
!!
!!    AUTHOR
!!    ------
!!	B. Decharme   *Meteo France*
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
USE MODD_TRIP_PAR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('INIT_TRIP_PAR',0,ZHOOK_HANDLE)
!
NDIMTAB     = 10
NUNDEF      = 1E+9
!
XVELMIN     = 0.025
XHSMIN      = 0.01
XUNDEF      = 1.0E+20
XM          = 2.0/3.0
XRHOLW      = 1000.0
XDAY        = 86400.0
XSEA        = 135.3E12
XYEAR       = 365.0
XRAD        = 6371229.
XPI         = 2.*ASIN(1.)
XTIME_DIAG  = 0.
!
XGWDZMAX    = 1000.
!
LNCPRINT    = .FALSE.
!
IF (LHOOK) CALL DR_HOOK('INIT_TRIP_PAR',1,ZHOOK_HANDLE)
!
!--------------------------------------------------------------------------------
!
END SUBROUTINE INIT_TRIP_PAR
