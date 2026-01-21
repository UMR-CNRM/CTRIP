SUBROUTINE ASSIM_BUILD_YO_HX(TPST, KENS, KOBS, KPNT, KOBS_INDEX, POBS, PXB, PYO, PHX)
!####################################################################
!
!!****  *ASSIM_BUILD_YO_HX*
!!
!!    PURPOSE
!!    -------
!!      Build observation vector
!!
!!    AUTHOR
!!    ------
!!      S. Munier   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!
!-------------------------------------------------------------------------------
!
USE MODD_TRIP_STATE, ONLY : TRIP_STATE_t
USE MODD_TRIP_PAR, ONLY : XUNDEF, XRHOLW
USE MODD_TRIP_LISTING
USE MODD_TRIP_ANALYSIS, ONLY : XHMEAN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!* 0.1    declarations of arguments
!
TYPE(TRIP_STATE_t), INTENT(INOUT) :: TPST
!
INTEGER,                       INTENT(IN)  :: KENS
INTEGER,                       INTENT(IN)  :: KOBS
INTEGER,                       INTENT(IN)  :: KPNT
INTEGER, DIMENSION(KOBS),      INTENT(OUT) :: KOBS_INDEX
REAL,    DIMENSION(2*KPNT),    INTENT(IN)  :: POBS
REAL,    DIMENSION(KPNT,KENS), INTENT(IN)  :: PXB
REAL,    DIMENSION(KOBS),      INTENT(OUT) :: PYO
REAL,    DIMENSION(KOBS,KENS), INTENT(OUT) :: PHX
!
!* 0.2    declarations of local variables
!
INTEGER :: JSTATE, JOBS_INDEX, JOBS, JENS
REAL    :: ZH
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ASSIM_BUILD_YO_HX',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
! Construction of YO
!
JOBS = 0
DO JSTATE = 1,KPNT
  ! Discharge observation
  JOBS_INDEX = 2*(JSTATE-1)+1
  IF (POBS(JOBS_INDEX) /= XUNDEF) THEN
    JOBS = JOBS+1
    KOBS_INDEX(JOBS) = JOBS_INDEX
    PYO(JOBS) = POBS(JOBS_INDEX)
    DO JENS = 1, KENS
      ZH = PXB(JSTATE,JENS)/(XRHOLW*TPST%XLEN(JSTATE)*TPST%XWIDTH(JSTATE))
      PHX(JOBS,JENS) = SQRT(TPST%XSLOPEBED(JSTATE))*TPST%XWIDTH(JSTATE)/TPST%XN(JSTATE)*ZH**(5./3.)
    ENDDO
  ENDIF
  ! WSE observation
  JOBS_INDEX = 2*JSTATE
  IF (POBS(JOBS_INDEX) /= XUNDEF) THEN
    JOBS = JOBS+1
    KOBS_INDEX(JOBS) = JOBS_INDEX
    PYO(JOBS) = POBS(JOBS_INDEX)
    DO JENS = 1, KENS
      PHX(JOBS,JENS) = PXB(JSTATE,JENS)/(XRHOLW*TPST%XLEN(JSTATE)*TPST%XWIDTH(JSTATE))
    ENDDO
  ENDIF
ENDDO
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ASSIM_BUILD_YO_HX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ASSIM_BUILD_YO_HX


