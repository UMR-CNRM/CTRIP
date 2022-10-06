!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!     ###############################################################################
      SUBROUTINE GWF_SOLVER (KLON,KLAT,PNPTS,OMASK,PHCOF,PRHS,PCR,PCC,PHGROUND,PEVOL)
!     ###############################################################################
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER, INTENT(IN)                 :: KLON
INTEGER, INTENT(IN)                 :: KLAT
!
REAL, INTENT(IN)                    :: PNPTS
!
LOGICAL, DIMENSION(:,:), INTENT(IN) :: OMASK
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PHCOF
REAL, DIMENSION(:,:), INTENT(IN)    :: PRHS
REAL, DIMENSION(:,:), INTENT(IN)    :: PCR
REAL, DIMENSION(:,:), INTENT(IN)    :: PCC
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PHGROUND
REAL,                 INTENT(OUT)   :: PEVOL
!
!*      0.2    declarations of local variables
!
INTEGER                        :: JLON, JLAT
INTEGER                        :: INBSOU
REAL                           :: ZHOR,ZNOM,ZDENOM,ZDIAG,ZPASD,ZDIAG_HORI
REAL                           :: ZEVOL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
! *      1. INITIALISATION
!           --------------
!
IF (LHOOK) CALL DR_HOOK('GWF_SOLVER',0,ZHOOK_HANDLE)
!
! * error initialization
!
ZEVOL = 0.
!
! *       2. ONE ITERATION LOOP
!            ------------------
!
DO JLAT = 1,KLAT
    DO JLON = 1,KLON
!
        IF (OMASK(JLON,JLAT)) THEN
!
!          skip constant head cells and no-flow
           ZHOR   = 0.
           ZDIAG  = 0.
           ZNOM   = 0.
           ZDENOM = 0.
           ZPASD  = 0.
!
!          RIGHT FLUX
           IF (JLON < KLON) THEN
              ZDIAG_HORI = PCR(JLON,JLAT)
              ZHOR  = ZHOR  + ZDIAG_HORI*PHGROUND(JLON+1,JLAT)
              ZDIAG = ZDIAG + ZDIAG_HORI
           ENDIF
!       
!          LEFT FLUX
           IF (JLON > 1) THEN
              ZDIAG_HORI = PCR(JLON-1,JLAT)
              ZHOR  = ZHOR  + ZDIAG_HORI*PHGROUND(JLON-1,JLAT)
              ZDIAG = ZDIAG + ZDIAG_HORI
           ENDIF
!           
!          BOTTOM FLUX
           IF (JLAT > 1) THEN
              ZDIAG_HORI = PCC(JLON,JLAT-1)
              ZHOR  = ZHOR  + ZDIAG_HORI*PHGROUND(JLON,JLAT-1)
              ZDIAG = ZDIAG + ZDIAG_HORI
           ENDIF
!           
!          TOP FLUX
           IF (JLAT < KLAT) THEN
              ZDIAG_HORI = PCC(JLON,JLAT)
              ZHOR  = ZHOR  + ZDIAG_HORI*PHGROUND(JLON,JLAT+1)
              ZDIAG = ZDIAG + ZDIAG_HORI
           ENDIF
!
!          calcul nominateur
           ZNOM   = ZHOR + PRHS(JLON,JLAT)
!           
!          calcul dénominateur
           ZDENOM = ZDIAG + PHCOF(JLON,JLAT)
!
!          calcul new height
           ZPASD = ZNOM/ZDENOM
!        
!          calcul critère convergence
           ZEVOL = ZEVOL + ABS(ZPASD-PHGROUND(JLON,JLAT))/PNPTS
!
!          update new height
           PHGROUND(JLON,JLAT) = ZPASD
!
        ENDIF
!
    ENDDO
ENDDO
!
PEVOL = ZEVOL
!
IF (LHOOK) CALL DR_HOOK('GWF_SOLVER',1,ZHOOK_HANDLE)
!
END SUBROUTINE GWF_SOLVER
