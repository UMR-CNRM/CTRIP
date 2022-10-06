!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!     #########
      SUBROUTINE FLOOD_UPDATE (TPST,PAREA,PFLOOD_STO)
!     ##########################################################################
!
!!****  *FLOOD_UPDATE*
!!
!!    PURPOSE
!!    -------
!
!     Compute HFLOOD, FFLOOD, LFLOOD, WFLOOD.
!
!!**  METHOD
!!    ------
!
!     Direct calculation
!
!!    EXTERNAL
!!    --------
!
!     None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/11/06
!!      S. Munier   03/2020 CTRIP-12D and parallelization
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_STATE, ONLY : TRIP_STATE_t
!
USE MODN_TRIP,     ONLY : XRATMED
!
USE MODD_TRIP_PAR, ONLY : XUNDEF, XRHOLW
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(TRIP_STATE_t),    INTENT(INOUT) :: TPST
!
REAL,DIMENSION(:),   INTENT(IN)  :: PAREA   ! grid area                 [m²]
REAL,DIMENSION(:),   INTENT(IN)  :: PFLOOD_STO ! Floodplain water mass  [kg]
!
!*      0.2    declarations of local variables
!
REAL,    DIMENSION(SIZE(PAREA,1)) :: ZFLOOD_STO !kg/m2
!
INTEGER, DIMENSION(SIZE(PAREA,1)) :: IUP, IDOWN
!
INTEGER :: ISTATE, JSTATE, IPAS, JPAS
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! * Initialize local variable
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('FLOOD_UPDATE',0,ZHOOK_HANDLE)
!
ISTATE = TPST%NSTATE_LEN_P
IPAS = SIZE(TPST%XTAB_F,2)
!
TPST%XHFLOOD   (:) = 0.0
TPST%XFFLOOD   (:) = 0.0
TPST%XWFLOOD   (:) = 0.0
TPST%XFLOOD_LEN(:) = 0.0
!
ZFLOOD_STO(:) = PFLOOD_STO(:)/PAREA(:)
!
DO JSTATE=1,ISTATE
  IF(ZFLOOD_STO(JSTATE)>0.0)THEN
    DO JPAS=1,IPAS-1
      IF(ZFLOOD_STO(JSTATE)>=TPST%XTAB_VF(JSTATE,JPAS))THEN
        IUP  (JSTATE) = JPAS+1
        IDOWN(JSTATE) = JPAS
      ENDIF
    ENDDO
  ENDIF
ENDDO
!
!-------------------------------------------------------------------------------
! * Calculate new Fflood and Hflood
!-------------------------------------------------------------------------------
!
DO JSTATE=1,ISTATE
  IF(ZFLOOD_STO(JSTATE)>0.0)THEN
    TPST%XFFLOOD(JSTATE) = TPST%XTAB_F(JSTATE,IDOWN(JSTATE))                                  &
                      + (ZFLOOD_STO  (JSTATE)            -TPST%XTAB_VF(JSTATE,IDOWN(JSTATE))) &
                      * (TPST%XTAB_F (JSTATE,IUP(JSTATE))-TPST%XTAB_F (JSTATE,IDOWN(JSTATE))) &
                      / (TPST%XTAB_VF(JSTATE,IUP(JSTATE))-TPST%XTAB_VF(JSTATE,IDOWN(JSTATE)))
    TPST%XHFLOOD(JSTATE) = TPST%XTAB_H(JSTATE,IDOWN(JSTATE))                                  &
                      + (ZFLOOD_STO  (JSTATE)            -TPST%XTAB_VF(JSTATE,IDOWN(JSTATE))) &
                      * (TPST%XTAB_H (JSTATE,IUP(JSTATE))-TPST%XTAB_H (JSTATE,IDOWN(JSTATE))) &
                      / (TPST%XTAB_VF(JSTATE,IUP(JSTATE))-TPST%XTAB_VF(JSTATE,IDOWN(JSTATE)))
  ENDIF
  IF(TPST%XFFLOOD(JSTATE)>=1.0)THEN
    TPST%XFFLOOD(JSTATE) = 1.0
    TPST%XHFLOOD(JSTATE) = TPST%XTAB_H(JSTATE,IUP(JSTATE))                                   &
                      + (ZFLOOD_STO(JSTATE)-TPST%XTAB_VF(JSTATE,IUP(JSTATE))) / XRHOLW
  ENDIF
ENDDO
!
!-------------------------------------------------------------------------------
! * Calculate new Wflood, Lflood
!-------------------------------------------------------------------------------
!
WHERE(ZFLOOD_STO(:)>0.0)
  TPST%XFLOOD_LEN(:) = MIN(TPST%XLEN(:),XRATMED*SQRT(TPST%XFFLOOD(:)*PAREA(:)))
  TPST%XWFLOOD   (:) = PAREA(:)*TPST%XFFLOOD(:)/TPST%XFLOOD_LEN(:)
ENDWHERE
!
IF (LHOOK) CALL DR_HOOK('FLOOD_UPDATE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE FLOOD_UPDATE
