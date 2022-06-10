!     #########
SUBROUTINE TRIP_GW_BUFFER_TANK (TPST, &
                                PTSTEP,OPRINT,PDRAIN,                   &
                                PGROUND_STO2,PGOUT,                     &
                                PGSTO_ALL,PGSTO2_ALL,PGIN_ALL,PGOUT_ALL )
!     #############################################################################
!
!!****  *TRIP_GW_BUFFER_TANK*
!!
!!    PURPOSE
!!    -------
!
!     Calculate the storage in the next time step based on the storage
!     of current time step. The deep drainage is constant during the time step.
!
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
!!      Original    01/02/05
!!      S. Munier   28/03/2020 CTRIP-12D and parallelization
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_STATE, ONLY : TRIP_STATE_t
!
USE MODD_TRIP_PAR, ONLY : XUNDEF
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
REAL, INTENT(IN)                     :: PTSTEP
!                                       KTSTEP = timestep value (=FRC) [s]
!                                              = 10800s
!
LOGICAL, INTENT(IN)                  :: OPRINT   ! Printable budget key 
!
REAL,    DIMENSION(:), INTENT(IN)    :: PDRAIN   ! Surface runoff from ISBA    [kg/s]
!
REAL,    DIMENSION(:), INTENT(INOUT) :: PGROUND_STO2
!                                       PGROUND_STO  = ground water storage at t    [kg]
!                                       PGROUND_STO2 = ground water storage at t+1  [kg]
!
REAL,    DIMENSION(:), INTENT(OUT)   :: PGOUT
!                                       PGOUT = Outflow from the ground reservoir  
!
REAL,                  INTENT(OUT)   :: PGSTO_ALL,PGSTO2_ALL,PGIN_ALL,PGOUT_ALL
!                                       Final budget variable
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(TPST%XGROUND_STO,1)) :: ZGSTOMAX
REAL, DIMENSION(SIZE(TPST%XGROUND_STO,1)) :: ZGOUT
REAL, DIMENSION(SIZE(TPST%XGROUND_STO,1)) :: ZDRAIN
REAL, DIMENSION(SIZE(TPST%XGROUND_STO,1)) :: ZDRAIN_NEG
!
INTEGER :: ILON, ILAT, JLON, JLAT
INTEGER :: ISTATE, JSTATE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_GW_BUFFER_TANK',0,ZHOOK_HANDLE)
!
ISTATE = TPST%NSTATE_LEN_P
!
PGROUND_STO2(:) = 0.0
PGOUT       (:) = 0.0
!
ZGSTOMAX    (:) = 0.0
ZGOUT       (:) = 0.0
!
ZDRAIN    (:) = MAX(0.0,PDRAIN(:))
ZDRAIN_NEG(:) = MIN(0.0,PDRAIN(:))
!
!-------------------------------------------------------------------------------
! * Groundwater case
!-------------------------------------------------------------------------------
!
DO JSTATE = 1,ISTATE
!
  IF(TPST%GMASK_GW(JSTATE))THEN
!
!   ground water storage calculation dG/dt = Drain - G/tau
    PGROUND_STO2(JSTATE) = TPST%XGROUND_STO(JSTATE)*EXP(-PTSTEP/TPST%XTAUG(JSTATE)) &
                           + (1.0-EXP(-PTSTEP/TPST%XTAUG(JSTATE)))*ZDRAIN(JSTATE)&
                           * TPST%XTAUG(JSTATE)
!
!   supress numerical artifacs
    ZGSTOMAX(JSTATE) = TPST%XGROUND_STO(JSTATE) + ZDRAIN(JSTATE)*PTSTEP
    PGROUND_STO2(JSTATE) = MIN(ZGSTOMAX(JSTATE),PGROUND_STO2(JSTATE))
!
!   ground water discharge calculation
    ZGOUT(JSTATE) = (TPST%XGROUND_STO(JSTATE)-PGROUND_STO2(JSTATE))/PTSTEP+ZDRAIN(JSTATE)
!
!   supress numerical artifacs
    PGOUT(JSTATE) = MAX(0.0,ZGOUT(JSTATE))
    PGROUND_STO2(JSTATE) = PGROUND_STO2(JSTATE) + (PGOUT(JSTATE)-ZGOUT(JSTATE))
!
!   account for negative drainage
    PGROUND_STO2(JSTATE) = PGROUND_STO2(JSTATE) + ZDRAIN_NEG(JSTATE)*PTSTEP
!
  ENDIF
!
ENDDO
!
!-------------------------------------------------------------------------------
! * No groundwater case
!-------------------------------------------------------------------------------
!
WHERE(.NOT.TPST%GMASK_GW(:)) PGOUT(:)=ZDRAIN(:)
!
!-------------------------------------------------------------------------------
! * Budget calculation
!-------------------------------------------------------------------------------
!
IF(OPRINT)THEN
!
  PGSTO_ALL  = 0.0
  PGSTO2_ALL = 0.0
  PGIN_ALL   = 0.0
  PGOUT_ALL  = 0.0
!
  WHERE(TPST%GMASK_GW(:)) ZDRAIN(:) = ZDRAIN(:) + ZDRAIN_NEG(:)
!
  DO JSTATE = 1,ISTATE
    IF(TPST%GMASK_GW(JSTATE))THEN
      PGSTO_ALL  = PGSTO_ALL  + TPST%XGROUND_STO(JSTATE) / TPST%XAREA(JSTATE)
      PGSTO2_ALL = PGSTO2_ALL + PGROUND_STO2    (JSTATE) / TPST%XAREA(JSTATE)
      PGIN_ALL   = PGIN_ALL   + ZDRAIN          (JSTATE) / TPST%XAREA(JSTATE)
      PGOUT_ALL  = PGOUT_ALL  + PGOUT           (JSTATE) / TPST%XAREA(JSTATE)
    ENDIF
  ENDDO
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP_GW_BUFFER_TANK',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_GW_BUFFER_TANK
