SUBROUTINE ASSIM_BUILD_YO_HX(TPST, KENS, KOBS, KPNT, KOBS_STATE, POBS, PXB, PYO, PHX)
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
USE MODN_TRIP_ASSIM, ONLY : CHQ
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
INTEGER, DIMENSION(KOBS),      INTENT(OUT) :: KOBS_STATE
REAL,    DIMENSION(KPNT),      INTENT(IN)  :: POBS
REAL,    DIMENSION(KPNT,KENS), INTENT(IN)  :: PXB
REAL,    DIMENSION(KOBS),      INTENT(OUT) :: PYO
REAL,    DIMENSION(KOBS,KENS), INTENT(OUT) :: PHX
!
!* 0.2    declarations of local variables
!
INTEGER :: JSTATE, JOBS, JENS
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
  IF (POBS(JSTATE) /= XUNDEF) THEN
    JOBS = JOBS+1
    KOBS_STATE(JOBS) = JSTATE
    PYO(JOBS) = POBS(JSTATE)
  ENDIF
ENDDO
!
IF (CHQ .EQ.'DIS') THEN
!
! Assimilation of discharge observations
!
  DO JENS = 1, KENS
    DO JOBS = 1, KOBS
      JSTATE = KOBS_STATE(JOBS)
      ZH = PXB(JSTATE,JENS)/(XRHOLW*TPST%XLEN(JSTATE)*TPST%XWIDTH(JSTATE))
      PHX(JOBS,JENS) = SQRT(TPST%XSLOPEBED(JSTATE))*TPST%XWIDTH(JSTATE)/TPST%XN(JSTATE)*ZH**(5./3.)
    ENDDO
  ENDDO
!
ELSEIF (CHQ .EQ. 'HST') THEN
!
! Assimilation of river depth observations
!
  DO JENS = 1, KENS
    DO JOBS = 1, KOBS
      JSTATE = KOBS_STATE(JOBS)
      PHX(JOBS,JENS) = PXB(JSTATE,JENS)/(XRHOLW*TPST%XLEN(JSTATE)*TPST%XWIDTH(JSTATE))
    ENDDO
  ENDDO
!
ELSEIF (CHQ .EQ. 'HAN') THEN
!
! Assimilation of river depth anomaly observations
!
  DO JENS = 1, KENS
    DO JOBS = 1, KOBS
      JSTATE = KOBS_STATE(JOBS)
      PHX(JOBS,JENS) = PXB(JSTATE,JENS)/(XRHOLW*TPST%XLEN(JSTATE)*TPST%XWIDTH(JSTATE)) - XHMEAN(JSTATE)
    ENDDO
  ENDDO
!
ELSE
  WRITE(NLISTING,*) 'ASSIM_BUILD_YO_HX : wrong format of observations variables'
  CALL ABORT_TRIP('ASSIM_BUILD_YO_HX: wrong format of observations variables')
ENDIF
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ASSIM_BUILD_YO_HX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ASSIM_BUILD_YO_HX


