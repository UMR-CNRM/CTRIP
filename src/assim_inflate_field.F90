SUBROUTINE ASSIM_INFLATE_FIELD(KPNT, KENS, PAMP, PMEAN, PMASK, PFIELD, KSEED)
!####################################################################
!
!!****  *ASSIM_INFLATE_FIELD*
!!
!!    PURPOSE
!!    -------
!!      Inflate ensemble field
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
USE MODD_TRIP_ASSIM
USE MODN_TRIP_ASSIM
USE MODE_ASSIM_RANDOM
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!* 0.1    declarations of arguments
!
INTEGER,                    INTENT(IN)    :: KPNT
INTEGER,                    INTENT(IN)    :: KENS
REAL,                       INTENT(IN)    :: PAMP
REAL,                       INTENT(IN)    :: PMEAN
REAL, DIMENSION(KPNT),      INTENT(IN)    :: PMASK
REAL, DIMENSION(KPNT,KENS), INTENT(INOUT) :: PFIELD
INTEGER,                    INTENT(INOUT) :: KSEED
!
!* 0.2    declarations of local variables
!
INTEGER :: I,J
REAL, DIMENSION(KPNT) :: ZPERT
REAL, DIMENSION(KPNT) :: ZMEAN
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ASSIM_INFLATE_FIELD',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
! Inflation of analyzed state vector
!
DO I = 1,KPNT
  CALL GAUSTB(KSEED, PAMP, PMEAN, ZPERT(I))
  KSEED = KSEED + 1000
ENDDO
!
ZMEAN(:) = 0.
DO J = 1,KENS
  ZMEAN(:) = ZMEAN(:) + PFIELD(:,J)/REAL(KENS)
ENDDO
!
DO J = 1,KENS
  WHERE (PMASK>0)
    PFIELD(:,J) = ZMEAN(:) + (1+ZPERT(:))*(PFIELD(:,J)-ZMEAN(:))
  ENDWHERE
ENDDO
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ASSIM_INFLATE_FIELD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ASSIM_INFLATE_FIELD

