!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!     ###################################################################
       SUBROUTINE TRIP_MASSLAKE(PTSTEP, PLAKE_A, PWEIR_Z, PWEIR_W,          &
                                PLAKE_STO, PSIN, PLAKE_STO2, PSOUT, PLAKE_H )
!    ###################################################################
!!
!!-----------------------------------------------------------------------------
!!   *TRIP_MASSLAKE*
!!
!!    PURPOSE
!!    ------
!
!     Calculate the lake storage in the next time step based on the storage of current time
!
!     METHOD
!     ------
!
!     Equation of a thick rectangular threshold for the QOUT
!     Future developments:
!        - add propagation time within lake?
!        - add gaussian shape for lake bathymetry
!
!!    AUTHOR
!!    ------
!!    T.Guinaldo   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     02/19
!
!-------------------------------------------------------------------------------
!*       0.     DECLARATIONS
!-------------------------------------------------------------------------------
!
USE MODD_TRIP_PAR , ONLY: XRHOLW
!
USE YOMHOOK       , ONLY: LHOOK, DR_HOOK
USE PARKIND1      , ONLY: JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)  :: PTSTEP       ! trip timestep                [s]
REAL, INTENT(IN)  :: PLAKE_A      ! lake area                    [m2]
REAL, INTENT(IN)  :: PWEIR_Z      ! weir height                  [m]
REAL, INTENT(IN)  :: PWEIR_W      ! weir width                   [m]
REAL, INTENT(IN)  :: PLAKE_STO    ! lake reservoir storage at t  [kg]
REAL, INTENT(IN)  :: PSIN         ! inflow to the lake reservoir [kg/s]
!
REAL, INTENT(OUT) :: PLAKE_STO2   ! lake reservoir at t+1        [kg]
REAL, INTENT(OUT) :: PSOUT        ! outflow to the river         [kg/s]
REAL, INTENT(OUT) :: PLAKE_H      ! lake height                  [m]
!
!*      0.2    declarations of parameters
REAL, PARAMETER   :: ZG  = 9.81    ! Gravitational acceleration
REAL, PARAMETER   :: ZCd = 0.485   ! Weir coefficient
!
!*      0.3    declarations of local variables
!
REAL :: ZSTOMAX, ZSTOMIN, ZLAKE_H
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_MASSLAKE',0, ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
! * lake storage
!
ZSTOMAX = PLAKE_STO + (PSIN*PTSTEP)
ZSTOMIN = PWEIR_Z*PLAKE_A*XRHOLW
!
!-------------------------------------------------------------------------------
!
! * lake level (constant lake area)
!
ZLAKE_H = ZSTOMAX/(PLAKE_A*XRHOLW)
!
!-------------------------------------------------------------------------------
!
! * outflow (discharge over weir)
!
IF(ZLAKE_H > PWEIR_Z)THEN
  PSOUT = ZCd*XRHOLW*PWEIR_W*SQRT(2*ZG*(ZLAKE_H-PWEIR_Z))*(ZLAKE_H-PWEIR_Z)
  PSOUT = MAX(MIN(PSOUT,(ZSTOMAX-ZSTOMIN)/PTSTEP),0.0)
ELSE
  PSOUT = 0.0
ENDIF
!
!-------------------------------------------------------------------------------
!
! * water budget
!
PLAKE_STO2 = ZSTOMAX - (PSOUT*PTSTEP)
ZLAKE_H = PLAKE_STO2/(PLAKE_A*XRHOLW)-PWEIR_Z
!
!-------------------------------------------------------------------------------
!
! * suppress numerical artifacts
PLAKE_H = ZLAKE_H
! PLAKE_H = MAX(ZLAKE_H,0.0)
! PLAKE_STO2 = PLAKE_STO2 + (PLAKE_H-ZLAKE_H)*(PLAKE_A*XRHOLW)
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_MASSLAKE',1, ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_MASSLAKE
