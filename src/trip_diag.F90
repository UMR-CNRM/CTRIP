!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
SUBROUTINE TRIP_DIAG(TPDG, TP, TPST,                         &
                     PTSTEP,PSOUT,PSIN,PVEL,PHS,PGOUT,PGNEG, &
                     PWTD,PFWTD,PQGCELL,PHGHS,               &
                     PQFR,PQRF,PVFIN,PVFOUT,PHSF,PSRC_FLOOD, &
                     PDRAIN,PRUNOFF,PDISCHARGE               )
!     #####################################################
!
!!****  *TRIP_DIAG*
!!
!!    PURPOSE
!!    -------
!
!     TRIP diag compuation
!
!!
!!    AUTHOR
!!    ------
!!      B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/12/13
!!      09/16   B. Decharme  limit wtd to -1000m
!!      S. Munier   28/03/2020 CTRIP-12D and parallelization
!!      T. Guinaldo 04/2020    Add MLake
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_TRIP_DIAG, ONLY : TRIP_DIAG_t
USE MODD_TRIP, ONLY : TRIP_t
USE MODD_TRIP_STATE, ONLY : TRIP_STATE_t
!
USE MODD_TRIP_OASIS, ONLY : LCPL_LAND
USE MODD_TRIP_PAR,   ONLY : XRHOLW, XGWDZMAX
!
USE MODN_TRIP_RUN,   ONLY : LDIAG_MISC
USE MODN_TRIP,       ONLY : CGROUNDW, CVIT, LFLOOD, CLAKE
!
USE MODD_TRIP_MPI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(TRIP_DIAG_t),  INTENT(INOUT) :: TPDG
TYPE(TRIP_t),       INTENT(INOUT) :: TP
TYPE(TRIP_STATE_t), INTENT(INOUT) :: TPST
!
REAL,               INTENT(IN)  :: PTSTEP     !Time step                     [s]
!
REAL, DIMENSION(:), INTENT(IN)  :: PSOUT      !streamflow                    [kg/s]
REAL, DIMENSION(:), INTENT(IN)  :: PSIN       !grid-cell input streamflow    [kg/s]
REAL, DIMENSION(:), INTENT(IN)  :: PVEL       !river velocity                [m/s]
REAL, DIMENSION(:), INTENT(IN)  :: PHS        !River heigh                   [m]
REAL, DIMENSION(:), INTENT(IN)  :: PGOUT      !Groundwater outflow           [kg/s]
REAL, DIMENSION(:), INTENT(IN)  :: PGNEG      !Groundwater inflow (neg)      [kg/s]
REAL, DIMENSION(:), INTENT(IN)  :: PWTD       !Water table depth for coupling[m]
REAL, DIMENSION(:), INTENT(IN)  :: PFWTD      !Fraction of water table to rise
REAL, DIMENSION(:), INTENT(IN)  :: PQGCELL    !lateral groundwater exchanges [kg/s]
REAL, DIMENSION(:), INTENT(IN)  :: PHGHS      !groundwater minus river heigh [m]
REAL, DIMENSION(:), INTENT(IN)  :: PQFR       !floodplains to river exchange [kg/s]
REAL, DIMENSION(:), INTENT(IN)  :: PQRF       !river to floodplains exchange [kg/s]
REAL, DIMENSION(:), INTENT(IN)  :: PVFIN      !QRF velocity                  [m/s]
REAL, DIMENSION(:), INTENT(IN)  :: PVFOUT     !QFR velocity                  [m/s]
REAL, DIMENSION(:), INTENT(IN)  :: PHSF       !river minus flodd heigh       [m]
REAL, DIMENSION(:), INTENT(IN)  :: PSRC_FLOOD !P-E-I flood source term       [kg/s]
REAL, DIMENSION(:), INTENT(IN)  :: PDRAIN     !Input drainage or recharge    [kg/s]
REAL, DIMENSION(:), INTENT(IN)  :: PRUNOFF    !Input surface runoff          [kg/s]
!
REAL, DIMENSION(:), INTENT(OUT) :: PDISCHARGE !Cumulated river discharges    [kg]
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSOUT,1))  :: ZGROUND_STO
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_DIAG',0,ZHOOK_HANDLE)
!
! * Actualisation of river discharge diags
!
PDISCHARGE (:) = PDISCHARGE (:) + PSOUT(:) * PTSTEP          ![kg]
TPDG%TDIAG_ST%XQDIS(:) = TPDG%TDIAG_ST%XQDIS(:) + PSOUT(:) * PTSTEP / XRHOLW ![m3]
!
! * Actualisation of input total flux in the river
!
IF(LDIAG_MISC)THEN
  TPDG%TDIAG_ST%XQIN(:) = TPDG%TDIAG_ST%XQIN (:) + PSIN (:) * PTSTEP / XRHOLW
ENDIF
!
! * Actualisation of input surface runoff and drainage (or recharge)
!
IF(LCPL_LAND.AND.LDIAG_MISC)THEN
  TPDG%TDIAG_ST%XRUNOFF(:) = TPDG%TDIAG_ST%XRUNOFF(:) + PRUNOFF(:) * PTSTEP / TPST%XAREA(:)
  TPDG%TDIAG_ST%XDRAIN (:) = TPDG%TDIAG_ST%XDRAIN (:) + PDRAIN (:) * PTSTEP / TPST%XAREA(:)
ENDIF
!
! * Actualisation of stream reservoir
!
TPDG%TDIAG_ST%XSURF_STO(:) = TPDG%TDIAG_ST%XSURF_STO(:) + TPST%XSURF_STO(:) * PTSTEP / TPST%XAREA(:)
!
! * Actualisation of variable velocity diagnostic variables
!
IF(CVIT=='VAR')THEN
  TPDG%TDIAG_ST%XVEL(:) = TPDG%TDIAG_ST%XVEL(:) + PVEL(:) * PTSTEP
ENDIF
IF(CVIT=='VAR'.OR.CLAKE=='MLK')THEN
  TPDG%TDIAG_ST%XHS (:) = TPDG%TDIAG_ST%XHS (:) + PHS (:) * PTSTEP
ENDIF
!
! * Actualisation of groundwater diagnostic variables
!
IF(CGROUNDW/='DEF')THEN
  TPDG%TDIAG_ST%XQGF(:) = TPDG%TDIAG_ST%XQGF(:) + (PGOUT(:)+PGNEG(:)) * PTSTEP / XRHOLW
ENDIF
!
IF(CGROUNDW=='CST')THEN
!
  TPDG%TDIAG_ST%XGROUND_STO(:) = TPDG%TDIAG_ST%XGROUND_STO(:) + TPST%XGROUND_STO(:) * PTSTEP / TPST%XAREA(:)
!
ELSEIF(CGROUNDW=='DIF')THEN
!
  ZGROUND_STO(:) = (XGWDZMAX+PWTD(:)) * TPST%XWEFF(:) * XRHOLW
!
  TPDG%TDIAG_ST%XGROUND_STO(:) = TPDG%TDIAG_ST%XGROUND_STO(:) + ZGROUND_STO  (:) * PTSTEP
  TPDG%TDIAG_ST%XHGROUND   (:) = TPDG%TDIAG_ST%XHGROUND   (:) + TPST%XHGROUND(:) * PTSTEP
  TPDG%TDIAG_ST%XWTD       (:) = TPDG%TDIAG_ST%XWTD       (:) + PWTD         (:) * PTSTEP
  TPDG%TDIAG_ST%XFWTD      (:) = TPDG%TDIAG_ST%XFWTD      (:) + PFWTD        (:) * PTSTEP
  IF(LDIAG_MISC)THEN
    TPDG%TDIAG_ST%XQGCELL  (:) = TPDG%TDIAG_ST%XQGCELL    (:) + PQGCELL      (:) * PTSTEP / XRHOLW
    TPDG%TDIAG_ST%XHGHS    (:) = TPDG%TDIAG_ST%XHGHS      (:) + PHGHS        (:) * PTSTEP
  ENDIF
!
ENDIF
!
! * Actualisation of flooding scheme diagnostic variables
!
IF(LFLOOD)THEN
   TPDG%TDIAG_ST%XFLOOD_STO(:) = TPDG%TDIAG_ST%XFLOOD_STO(:) + TPST%XFLOOD_STO(:) * PTSTEP / TPST%XAREA(:)
   TPDG%TDIAG_ST%XFF       (:) = TPDG%TDIAG_ST%XFF       (:) + TPST%XFFLOOD   (:) * PTSTEP
   TPDG%TDIAG_ST%XHF       (:) = TPDG%TDIAG_ST%XHF       (:) + TPST%XHFLOOD   (:) * PTSTEP
   IF(LDIAG_MISC)THEN
     TPDG%TDIAG_ST%XQFR   (:) = TPDG%TDIAG_ST%XQFR   (:) + PQFR            (:) * PTSTEP / XRHOLW
     TPDG%TDIAG_ST%XQRF   (:) = TPDG%TDIAG_ST%XQRF   (:) + PQRF            (:) * PTSTEP / XRHOLW
     TPDG%TDIAG_ST%XVFIN  (:) = TPDG%TDIAG_ST%XVFIN  (:) + PVFIN           (:) * PTSTEP
     TPDG%TDIAG_ST%XVFOUT (:) = TPDG%TDIAG_ST%XVFOUT (:) + PVFOUT          (:) * PTSTEP
     TPDG%TDIAG_ST%XWF    (:) = TPDG%TDIAG_ST%XWF    (:) + TPST%XWFLOOD    (:) * PTSTEP
     TPDG%TDIAG_ST%XLF    (:) = TPDG%TDIAG_ST%XLF    (:) + TPST%XFLOOD_LEN (:) * PTSTEP
     TPDG%TDIAG_ST%XHSF   (:) = TPDG%TDIAG_ST%XHSF   (:) + PHSF            (:) * PTSTEP
     TPDG%TDIAG_ST%XSOURCE(:) = TPDG%TDIAG_ST%XSOURCE(:) + PSRC_FLOOD      (:) * PTSTEP / TPST%XAREA(:)
   ENDIF
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP_DIAG',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_DIAG
