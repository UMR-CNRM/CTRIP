SUBROUTINE ASSIM_PERTURBATE_FIELD(KDIM, PAMP, PMEAN, PFIELD, KSEED)
!####################################################################
!
!!****  *ASSIM_PERTURBATE_FIELD*
!!
!!    PURPOSE
!!    -------
!!      Perturbate field with a multiplicative error
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
USE MODE_ASSIM_RANDOM
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!* 0.1    declarations of arguments
!
INTEGER,               INTENT(IN)    :: KDIM
REAL,                  INTENT(IN)    :: PAMP
REAL,                  INTENT(IN)    :: PMEAN
REAL, DIMENSION(KDIM), INTENT(INOUT) :: PFIELD
INTEGER,               INTENT(INOUT) :: KSEED
!
!* 0.2    declarations of local variables
!
REAL, DIMENSION(KDIM) :: ZPERT
INTEGER :: I
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ASSIM_PERTURBATE_FIELD',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
! Perturbation of initial state
!
DO I = 1,KDIM
  CALL GAUSTB(KSEED, PAMP, PMEAN, ZPERT(I))
  KSEED = KSEED + 1000
ENDDO
!
! Multiplicative error
PFIELD(:) = ZPERT(:)*PFIELD(:)
!
! Additive error
!PFIELD(:) = PFIELD(:)+ZPERT(:)
!WHERE (PFIELD(:,:) .LT. 0) THEN
!  PFIELD(:,:) = 0
!ENDWHERE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ASSIM_PERTURBATE_FIELD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ASSIM_PERTURBATE_FIELD

