!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
SUBROUTINE TRIP_DIAG_CPL_ESM (TPST, &
                              PTSTEP_RUN,PDISCHARGE,PCALVING,PWTD,PFWTD)
!     #################################################################
!
!!****  *TRIP_DIAG_CPL_ESM*
!!
!!    PURPOSE
!!    -------
!
!     TRIP cpl diag compuation
!
!!
!!    AUTHOR
!!    ------
!!      B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/12/13 
!!      B. Decharme 10/2016  bug surface/groundwater coupling
!!      S. Munier   28/03/2020 CTRIP-12D and parallelization
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_TRIP_STATE, ONLY : TRIP_STATE_t
!
USE MODD_TRIP_PAR,   ONLY : XUNDEF, XRHOLW
!
USE MODD_TRIP_OASIS, ONLY : LCPL_SEA, LCPL_LAND, LCPL_GW,    &
                            LCPL_FLOOD, LCPL_CALVSEA
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(TRIP_STATE_t), INTENT(INOUT) :: TPST
!
REAL,               INTENT(IN)    :: PTSTEP_RUN !Run  timestep                   [s]
REAL, DIMENSION(:), INTENT(IN)    :: PDISCHARGE !Cumulated river discharges      [kg]
REAL, DIMENSION(:), INTENT(IN)    :: PCALVING   !Input claving flux from glacier [kg/s]
REAL, DIMENSION(:), INTENT(IN)    :: PWTD       !Water table depth               [m]
REAL, DIMENSION(:), INTENT(IN)    :: PFWTD      !Fraction of Water table to rise
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_DIAG_CPL_ESM',0,ZHOOK_HANDLE)
!
!*       1.      Actualisation of sea coupling diagnostic:
!               ------------------------------------------
!
!
! River discharges to ocean [kg/m2]
!
IF(LCPL_SEA)THEN
  WHERE(TPST%NGRCN(:)==9.OR.TPST%NGRCN(:)==12)
    TPST%XCPL_RIVDIS(:) = TPST%XCPL_RIVDIS(:) + PDISCHARGE(:) / TPST%XAREA(:)
  ENDWHERE
ENDIF
!
! Calving flux over greenland and antarctica [kg/m2]
!
IF(LCPL_CALVSEA)THEN
  WHERE(TPST%GMASK_GRE(:))
    TPST%XCPL_CALVGRE(:) = TPST%XCPL_CALVGRE(:) + PCALVING(:) * PTSTEP_RUN / TPST%XAREA(:)
  ENDWHERE
  WHERE(TPST%GMASK_ANT(:))
    TPST%XCPL_CALVANT(:) = TPST%XCPL_CALVANT(:) + PCALVING(:) * PTSTEP_RUN / TPST%XAREA(:)
  ENDWHERE
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       2.      Actualisation of land coupling diagnostic:
!               -------------------------------------------
!
IF(LCPL_LAND)THEN
!
! Water table depth (negative above the surface) and fraction of water table to rise
!
  IF(LCPL_GW)THEN
    WHERE(TPST%GMASK_GW(:))
          TPST%XCPL_WTD (:) = PWTD (:)
          TPST%XCPL_FWTD(:) = PFWTD(:)
    ELSEWHERE(TPST%GMASK(:))
          TPST%XCPL_WTD (:) = XUNDEF
          TPST%XCPL_FWTD(:) = 0.0
    ENDWHERE
  ENDIF
!
! Flood fraction [-] and potential infiltration [kg/m2]
!
  IF(LCPL_FLOOD)THEN
    TPST%XCPL_FFLOOD (:) = TPST%XFFLOOD    (:)
    TPST%XCPL_PIFLOOD(:) = TPST%XFLOOD_STO (:) / TPST%XAREA(:)
  ENDIF
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP_DIAG_CPL_ESM',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_DIAG_CPL_ESM
