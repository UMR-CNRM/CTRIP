!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!#########
SUBROUTINE TRIP_OASIS_RECV(TP, TPG,                           &
                           KLISTING,KLON,KLAT,PTIMEC,PRUNOFF, & 
                           PDRAIN,PCALVING,PSRC_FLOOD         )
!#############################################################################
!
!!****  *TRIP_OASIS_RECV* - Receive coupling fields
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2013
!!      B. Decharme 10/2016  bug surface/groundwater coupling
!!      S. Munier   28/03/2020 CTRIP-12D and parallelization
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_TRIP,      ONLY : TRIP_t
USE MODD_TRIP_GRID, ONLY : TRIP_GRID_t
!
USE MODN_TRIP_OASIS, ONLY : XTSTEP_CPL_LAND
!
USE MODD_TRIP_PAR,  ONLY : XUNDEF
!
USE MODD_TRIP_OASIS
!
USE MODI_GW_REDISTRIB
USE MODI_FLOOD_REDISTRIB
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef CPLOASIS
USE MOD_OASIS
#endif
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(TRIP_t), INTENT(INOUT) :: TP
TYPE(TRIP_GRID_t), INTENT(INOUT) :: TPG
!
INTEGER, INTENT(IN)               :: KLISTING
INTEGER, INTENT(IN)               :: KLON
INTEGER, INTENT(IN)               :: KLAT
REAL,    INTENT(IN)               :: PTIMEC        ! Cumulated run time step (s)
!
REAL, DIMENSION(:,:), INTENT(OUT) :: PRUNOFF       ! Surface runoff                  (kg/s)
REAL, DIMENSION(:,:), INTENT(OUT) :: PDRAIN        ! Deep drainage                   (kg/s)
REAL, DIMENSION(:,:), INTENT(OUT) :: PCALVING      ! Calving flux                    (kg/s)
REAL, DIMENSION(:,:), INTENT(OUT) :: PSRC_FLOOD    ! Input P-E-I flood source term   (kg/s)
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL,    DIMENSION(KLON,KLAT) :: ZREAD
REAL,    DIMENSION(KLON,KLAT) :: ZSRC_FLOOD
REAL,    DIMENSION(KLON,KLAT) :: ZWORK
REAL,    DIMENSION(KLON,KLAT) :: ZRESIDU
!
CHARACTER(LEN=50)             :: YCOMMENT
INTEGER                       :: IDATE     ! current coupling time step (s)
INTEGER                       :: IERR      ! Error info
INTEGER                       :: JVAR
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
#ifdef CPLOASIS
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_OASIS_RECV',0,ZHOOK_HANDLE)
!
!*       1.     Initialize :
!               ------------
!
IDATE = INT(PTIMEC)
!
PDRAIN    (:,:) = 0.0
PRUNOFF   (:,:) = 0.0
PCALVING  (:,:) = 0.0
PSRC_FLOOD(:,:) = 0.0
!
!-------------------------------------------------------------------------------
!
!*       2.     Get coupling fields :
!               ---------------------
!
IF(LCPL_LAND.AND.MOD(PTIMEC,XTSTEP_CPL_LAND)==0.0)THEN
!
! * Receive surface runoff input fields
!
  ZREAD(:,:) = XUNDEF  
  YCOMMENT='Surface runoff'
  CALL OASIS_GET(NRUNOFF_ID,IDATE,ZREAD(:,:),IERR)
  CALL CHECK_TRIP_RECV(IERR,YCOMMENT,ZREAD,TPG%GMASK)
  CALL KGM2S_TO_KGS(IERR,ZREAD,PRUNOFF)
!
! * Receive drainage/recharge input fields
!
  ZREAD(:,:) = XUNDEF  
  YCOMMENT='Deep drainage'
  CALL OASIS_GET(NDRAIN_ID,IDATE,ZREAD(:,:),IERR)
!
  CALL CHECK_TRIP_RECV(IERR,YCOMMENT,ZREAD,TPG%GMASK)
!
  IF(LCPL_GW)THEN
!   Redistribute negative recharge flux over groundwater
!   and conserve water mass over each bassin
    ZWORK(:,:) = XUNDEF
    CALL GW_REDISTRIB(TP,TPG, &
                      KLON,KLAT,ZREAD,ZWORK)
  ELSE
    ZWORK(:,:) = ZREAD(:,:)
  ENDIF
!
  CALL KGM2S_TO_KGS(IERR,ZWORK,PDRAIN)
!
! * Receive calving input fields
!
  IF(LCPL_CALVING)THEN
    ZREAD(:,:) = XUNDEF  
    YCOMMENT='calving flux'
    CALL OASIS_GET(NCALVING_ID,IDATE,ZREAD(:,:),IERR)
    CALL CHECK_TRIP_RECV(IERR,YCOMMENT,ZREAD,TPG%GMASK)
    CALL KGM2S_TO_KGS(IERR,ZREAD,PCALVING)
  ENDIF
!
! * Receive floodplains input fields
!
  IF(LCPL_FLOOD)THEN
!
    ZREAD     (:,:) = XUNDEF 
    ZSRC_FLOOD(:,:) = XUNDEF
    ZWORK     (:,:) = XUNDEF
    ZRESIDU   (:,:) = XUNDEF
!
    YCOMMENT='floodplains freshwater flux (P-E-I)'
    CALL OASIS_GET(NSRCFLOOD_ID,IDATE,ZREAD(:,:),IERR)
!
    CALL CHECK_TRIP_RECV(IERR,YCOMMENT,ZREAD,TPG%GMASK_FLD)
!
!   Redistribute freshwater flux over flooded grid-cell
!   and conserve water mass over each bassin
    CALL FLOOD_REDISTRIB(TP,TPG, &
                         KLON,KLAT,XTSTEP_CPL_LAND, &
                         ZREAD,ZSRC_FLOOD,ZWORK     )
!
    CALL KGM2S_TO_KGS(IERR,ZSRC_FLOOD,PSRC_FLOOD)
    CALL KGM2S_TO_KGS(IERR,ZWORK,ZRESIDU)
!
    WHERE(TPG%GMASK_FLD(:,:)) 
      PRUNOFF(:,:)=PRUNOFF(:,:)+ZRESIDU(:,:)
    ENDWHERE
!
  ENDIF
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP_OASIS_RECV',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
 CONTAINS
!-------------------------------------------------------------------------------
!
SUBROUTINE CHECK_TRIP_RECV(KERR,HCOMMENT,PFIELD,OMASK)
!
USE MODI_ABORT_TRIP
!
IMPLICIT NONE
!
INTEGER,                 INTENT(IN   ) :: KERR
CHARACTER(LEN=*),        INTENT(IN   ) :: HCOMMENT
REAL,    DIMENSION(:,:), INTENT(INOUT) :: PFIELD
LOGICAL, DIMENSION(:,:), INTENT(IN   ) :: OMASK
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('TRIP_OASIS_RECV:CHECK_TRIP_RECV',0,ZHOOK_HANDLE)
!
! Check receiving field
!
IF (KERR/=OASIS_OK.AND.KERR<OASIS_RECVD) THEN
   WRITE(KLISTING,'(A,I4)')'Return code from receiving '//TRIM(HCOMMENT)//' : ',KERR
   CALL ABORT_TRIP('TRIP_OASIS_RECV: problem receiving '//TRIM(HCOMMENT))
ENDIF
!
WHERE(.NOT.OMASK(:,:))
      PFIELD(:,:) = 0.0
ENDWHERE
!
IF (LHOOK) CALL DR_HOOK('TRIP_OASIS_RECV:CHECK_TRIP_RECV',1,ZHOOK_HANDLE)
!
END SUBROUTINE CHECK_TRIP_RECV
!
!-------------------------------------------------------------------------------
!
SUBROUTINE KGM2S_TO_KGS(KERR,PIN,POUT)
!
IMPLICIT NONE
!
INTEGER,              INTENT(IN ) :: KERR
REAL, DIMENSION(:,:), INTENT(IN ) :: PIN
REAL, DIMENSION(:,:), INTENT(OUT) :: POUT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('TRIP_OASIS_RECV:KGM2S_TO_KGS',0,ZHOOK_HANDLE)
!
! kg/m2/s -> kg/s
!
IF(KERR>=OASIS_RECVD)THEN
  WHERE(TPG%GMASK(:,:))
        POUT(:,:) = PIN(:,:) * TPG%XAREA(:,:)
  ELSEWHERE
        POUT(:,:) = XUNDEF
  ENDWHERE
ELSE
  POUT(:,:) = 0.0
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP_OASIS_RECV:KGM2S_TO_KGS',1,ZHOOK_HANDLE)
!
END SUBROUTINE KGM2S_TO_KGS
!
!-------------------------------------------------------------------------------
#endif
!-------------------------------------------------------------------------------
!
END SUBROUTINE TRIP_OASIS_RECV
