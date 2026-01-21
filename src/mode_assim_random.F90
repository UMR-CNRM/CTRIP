!---------------------------------------------------------------------------------------------
MODULE MODE_ASSIM_RANDOM
!---------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB


IMPLICIT NONE
!* Routine accessibility
!   PRIVATE
!   PUBLIC &
!      & gaustb generate_ens
CONTAINS
!---------------------------------------------------------------------------------------------
!
!Routines
! (*) GAUSS :
!	=> Generate a random variable following a normal distribution with zero mean and a variance of 1
! (*) UNIFVA : Random number generator
!	=> Returns a uniform random deviate between 0.0. and 1.0.
      !              Call with kdum a negative integer to initialize.
      !              Thereafter, do not alter kdum between successive deviates
      !              in sequence.
! (*) GAUSSFA :
!	=> Returns a normally distributed deviate with 0 mean
      !              and unit variance using the unifva(kdum) as the
      !              source of uniform deviates.
! (*) GAUSTB :
!	=> Returns a gaussian randum number with mean and std.
      !              Generate Gaussian random variables.
      !              The standard deviation and mean of the variables are
      !              specified by the variables pamp and pmean.
! (*) GENERATE_ENS :
!	=> Generate an random ensemble of Ne elements following a gaussian distribution
!	of defined mean and standard deviation
!
!---------------------------------------------------------------------------------------------
!
! From M.Rochoux thesis' work on "data assimilation applied to fire spread modeling"
! Author : C.Emery
! Creation : 01/20/2014
! Update :
!	-> 04/03/2014 : Creation of the subroutine GENERATE_ENS
!
!---------------------------------------------------------------------------------------------

SUBROUTINE GENERATE_ENS(Ne,var_ens,mean,std)

!--Arguments
   INTEGER, INTENT(IN) :: Ne
   REAL, INTENT(IN) :: mean, std
   REAL, DIMENSION(Ne), INTENT(INOUT) :: var_ens

!--Local variables
   INTEGER :: i, seed
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_ASSIM_RANDOM:GENERATE_ENS',0,ZHOOK_HANDLE)

!--Routine
  seed = -184557   !A random large number
  DO i = 1, Ne
     CALL gaustb(seed,std,mean,var_ens(i))
     seed=seed+1000
  ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_ASSIM_RANDOM:GENERATE_ENS',1,ZHOOK_HANDLE)

  RETURN
END SUBROUTINE GENERATE_ENS
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!FUNCTION gaustb(kseed,pamp,pmean)
SUBROUTINE GAUSTB(kseed,pamp,pmean,ran_var)
!----------------------------------------------------------------------

!-- Declarations
      INTEGER, INTENT(INOUT)  :: kseed                 !kern for random number generation
      REAL, INTENT(IN)     :: pamp, pmean   !amplitude, mean value
      REAL, INTENT(INOUT)  :: ran_var
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_ASSIM_RANDOM:GAUSTB',0,ZHOOK_HANDLE)

!-- Routine
      ran_var = pamp * gausva( kseed ) + pmean
      !WRITE(*,*) 'gausva:', gausva( kseed ),pamp, pmean, ran_var
IF (LHOOK) CALL DR_HOOK('MODE_ASSIM_RANDOM:GAUSTB',1,ZHOOK_HANDLE)

  RETURN
END SUBROUTINE GAUSTB
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
FUNCTION GAUSVA(kdum)

!-- Declarations
    REAL :: gausva
    INTEGER, INTENT(INOUT) :: kdum !kern for random number generation
    REAL, SAVE :: gset
    INTEGER, SAVE :: niset = 0
    REAL :: zfac, zrsq, zv1, zv2
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_ASSIM_RANDOM:GAUSVA',0,ZHOOK_HANDLE)

!-- Routine
    IF (niset.EQ.0) THEN

         zv1 = 2.0 * unifva(kdum) - 1.0
         zv2 = 2.0 * unifva(kdum) - 1.0
         zrsq = zv1**2 + zv2**2
         DO WHILE ( (zrsq.GE.1.0).OR.(zrsq.EQ.0.0) )
            zv1 = 2.0 * unifva(kdum) - 1.0
            zv2 = 2.0 * unifva(kdum) - 1.0
            zrsq = zv1**2 + zv2**2
         ENDDO
         zfac = SQRT(-2.0*LOG(zrsq)/zrsq)
         gset = zv1*zfac
         gausva = zv2*zfac
         niset = 1

    ELSE
         gausva = gset
         niset  = 0
    ENDIF

IF (LHOOK) CALL DR_HOOK('MODE_ASSIM_RANDOM:GAUSVA',1,ZHOOK_HANDLE)
END FUNCTION GAUSVA
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
FUNCTION UNIFVA(kdum)

!-- Declarations
      REAL :: unifva              !function return
      INTEGER, INTENT(INOUT) :: kdum !seed
      INTEGER, PARAMETER :: &
         & jpia = 16807,      &
         & jpim = 2147483647, &
         & jpiq = 127773,     &
         & jpir = 2836,       &
         & jpntab = 32,       &
         & jpndiv = 1 + (jpim-1)/jpntab
      REAL, PARAMETER :: &
         & ppam = 1.0/jpim, &
         & ppeps = 1.2e-07,   &
         & pprnmx = 1.0-ppeps
      INTEGER, SAVE, DIMENSION(jpntab) :: niv
      INTEGER, SAVE :: niy
      LOGICAL, SAVE :: llinit = .FALSE.
      INTEGER :: ijj, ik, jj
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('MODE_ASSIM_RANDOM:UNIFVA',0,ZHOOK_HANDLE)

!-- Routine

    !initialization
      IF (.NOT.llinit) THEN
         niv(:) = 0
         niy = 0
         llinit = .TRUE.
      ENDIF

    !begin main
      IF ( (kdum.LE.0).OR.(niy.EQ.0) ) THEN

         kdum = MAX(-kdum,1)

         DO jj = jpntab+8,1,-1

            ik   = kdum/jpiq
            kdum = jpia*(kdum-ik*jpiq)-jpir*ik

            IF (kdum.LT.0) kdum = kdum + jpim
            IF (jj.LE.jpntab) niv(jj) = kdum

         ENDDO
         niy = niv(1)

      ENDIF

      ik = kdum/jpiq
      kdum = jpia*(kdum-ik*jpiq) - jpir*ik

      IF (kdum.LT.0) kdum = kdum + jpim

      ijj = 1 + niy/jpndiv
      niy = niv(ijj)
      niv(ijj) = kdum
      unifva = MIN(ppam*niy, pprnmx)
IF (LHOOK) CALL DR_HOOK('MODE_ASSIM_RANDOM:UNIFVA',1,ZHOOK_HANDLE)

END FUNCTION UNIFVA
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------


!FUNCTION gauss()
!    IMPLICIT NONE
!    REAL gauss
!    REAL v1,v2,r
!    REAL ranmar
!    DO
!      v1=2.0*ran()-1.0
!      v2=2.0*ran()-1.0
!      r=v1*v1+v2*v2
!      IF(r .LT. 1.0) EXIT
!    ENDDO
!    gauss=v1*sqrt(-2.0*log(r)/r)
!  RETURN
!END FUNCTION gauss

!---------------------------------------------------------------------------------------------
!---------------------------------------------------------------------------------------------
END MODULE MODE_ASSIM_RANDOM
!---------------------------------------------------------------------------------------------
