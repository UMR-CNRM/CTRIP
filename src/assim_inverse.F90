SUBROUTINE ASSIM_INVERSE(A, C, n)
!####################################################################
!
!!****  *ASSIM_INVERSE*
!!
!!    PURPOSE
!!    -------
!!      Matrix inversion
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
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!* 0.1    declarations of arguments
!
INTEGER,              INTENT(IN)    :: N
REAL, DIMENSION(N,N), INTENT(INOUT) :: A
REAL, DIMENSION(N,N), INTENT(OUT)   :: C
!
!* 0.2    declarations of local variables
!
REAL, DIMENSION(N,N) :: L, U
REAL, DIMENSION(N)   :: B, D, X
REAL :: COEFF
INTEGER :: I, J, K
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ASSIM_INVERSE',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
! step 0: initialization for matrices L and U and b
!-------------------------------------------------------------------------------
!
L(:,:) = 0.0d0
U(:,:) = 0.0d0
B(:) = 0.0d0
!
!-------------------------------------------------------------------------------
! step 1: forward elimination
!-------------------------------------------------------------------------------
!
DO K = 1,N-1
  DO I = K+1,N
    COEFF = A(I,K)/A(K,K)
    L(I,K) = COEFF
    DO J = K+1,N
      A(I,J) = A(I,J)-COEFF*A(K,J)
    ENDDO
  ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
! Step 2: prepare L and U matrices
!-------------------------------------------------------------------------------
!
! L matrix is a matrix of the elimination COEFFicient
! + the diagonal elements are 1.0
DO I=1,N
  L(I,I) = 1.0
ENDDO
!
! U matrix is the upper triangular part of A
DO J = 1,n
  DO I = 1,J
    U(I,J) = A(I,J)
  ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
! Step 3: compute columns of the inverse matrix C
!-------------------------------------------------------------------------------
!
DO K = 1,N
  B(K) = 1.0
  D(1) = B(1)
  ! Step 3a: Solve Ld=b using the forward substitution
  DO I = 2,N
    D(I) = B(I)
    DO J = 1,I-1
      D(I) = D(I) - L(I,J)*D(J)
    ENDDO
  ENDDO
  ! Step 3b: Solve Ux=d using the back substitution
  X(N) = D(N)/U(N,N)
  DO I = N-1,1,-1
    X(I) = D(I)
    DO J = N,I+1,-1
      X(I) = X(I)-U(I,J)*X(J)
    ENDDO
    X(I) = X(I)/U(I,I)
  ENDDO
  ! Step 3c: fill the solutions x(N) into column K of C
  DO I = 1,N
    C(I,K) = X(I)
  ENDDO
  B(K)=0.0
ENDDO
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ASSIM_INVERSE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ASSIM_INVERSE
