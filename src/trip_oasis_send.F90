!#########
SUBROUTINE TRIP_OASIS_SEND (TP, TPG, TPST, &
                            KLISTING,KLON,KLAT,KSTATE,PTIMEC)
!############################################
!
!!****  *TRIP_OASIS_SEND* - Send coupling fields
!!
!!    PURPOSE
!!    -------
!!
!!    All fluxes are sent in kg/m2/s
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
!!      S. Munier   28/03/2020 CTRIP-12D and parallelization
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_TRIP, ONLY : TRIP_t
USE MODD_TRIP_GRID, ONLY : TRIP_GRID_t
USE MODD_TRIP_STATE, ONLY : TRIP_STATE_t
!
USE MODD_TRIP_PAR,   ONLY : XUNDEF
!
USE MODN_TRIP_OASIS, ONLY : XTSTEP_CPL_SEA, XTSTEP_CPL_LAND
USE MODD_TRIP_OASIS
!
USE MODD_TRIP_MPI
USE MODE_TRIP_GRID_STATE
!
USE MODI_ABORT_TRIP
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
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(TRIP_t),       INTENT(INOUT) :: TP
TYPE(TRIP_GRID_t),  INTENT(INOUT) :: TPG
TYPE(TRIP_STATE_t), INTENT(INOUT) :: TPST
!
INTEGER, INTENT(IN)               :: KLISTING
INTEGER, INTENT(IN)               :: KLON
INTEGER, INTENT(IN)               :: KLAT
INTEGER, INTENT(IN)               :: KSTATE
REAL,    INTENT(IN)               :: PTIMEC        ! Cumulated run time step (s)
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                    :: IDATE   ! current coupling time step (s)
INTEGER                    :: IERR    ! Error info
INTEGER                    :: JVAR
CHARACTER(LEN=50)          :: YCOMMENT
!
REAL,    DIMENSION(:),   ALLOCATABLE :: ZSTATE
REAL,    DIMENSION(:,:), ALLOCATABLE :: ZWRITE
LOGICAL, DIMENSION(:,:), ALLOCATABLE :: LMASK
REAL,    DIMENSION(0,0)    :: ZGRID_NULL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
#ifdef CPLOASIS
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_OASIS_SEND',0,ZHOOK_HANDLE)
!
!*       1.     Define current coupling time step in second :
!               ---------------------------------------------
!
IDATE = INT(PTIMEC)
!
!-------------------------------------------------------------------------------
!
!*       2.     Send coupling fields to land surface model :
!               -------------------------------------------
!
!
IF (NRANK==NPIO) THEN
  ALLOCATE(ZSTATE(TPST%NNSTATE))
  ALLOCATE(ZWRITE(KLON,KLAT))
  ALLOCATE(LMASK(KLON,KLAT))
ENDIF
!
IF(LCPL_LAND.AND.MOD(PTIMEC,XTSTEP_CPL_LAND)==0.0)THEN
!
  IF(LCPL_GW)THEN
!
    YCOMMENT='Water table depth' !m
    CALL MPI_GATHER(TPST%XCPL_WTD,KSTATE,MPI_DOUBLE,ZSTATE,KSTATE,MPI_DOUBLE,NPIO,NCOMM,IERR)
    IF (NRANK==NPIO) THEN
      CALL TRIP_STATE_TO_GRID(TPST%NSTATE_LON,TPST%NSTATE_LAT,ZSTATE,TP%XCPL_WTD)
      CALL OASIS_PUT(NWTD_ID,IDATE,TP%XCPL_WTD(:,:),IERR)
      CALL CHECK_TRIP_SEND(YCOMMENT)
    ELSE
      CALL OASIS_PUT(NWTD_ID,IDATE,ZGRID_NULL,IERR)
    ENDIF
!
    YCOMMENT='Grid-cell fraction of WTD to rise'
    CALL MPI_GATHER(TPST%XCPL_FWTD,KSTATE,MPI_DOUBLE,ZSTATE,KSTATE,MPI_DOUBLE,NPIO,NCOMM,IERR)
    IF (NRANK==NPIO) THEN
      CALL TRIP_STATE_TO_GRID(TPST%NSTATE_LON,TPST%NSTATE_LAT,ZSTATE,TP%XCPL_FWTD)
      CALL OASIS_PUT(NFWTD_ID,IDATE,TP%XCPL_FWTD(:,:),IERR)
      CALL CHECK_TRIP_SEND(YCOMMENT)
    ELSE
      CALL OASIS_PUT(NFWTD_ID,IDATE,ZGRID_NULL,IERR)
    ENDIF
!
  ENDIF
!
  IF(LCPL_FLOOD)THEN
!
    IF(NRANK==NPIO) LMASK(:,:) = TPG%GMASK(:,:)
!
    YCOMMENT='Flood fraction' !adim
    CALL MPI_GATHER(TPST%XCPL_FFLOOD,KSTATE,MPI_DOUBLE,ZSTATE,KSTATE,MPI_DOUBLE,NPIO,NCOMM,IERR)
    IF (NRANK==NPIO) THEN
      CALL TRIP_STATE_TO_GRID(TPST%NSTATE_LON,TPST%NSTATE_LAT,ZSTATE,TP%XCPL_FFLOOD)
      CALL MASK_TRIP(TP%XCPL_FFLOOD(:,:),ZWRITE(:,:),LMASK(:,:),1.0)
      CALL OASIS_PUT(NFFLOOD_ID,IDATE,ZWRITE(:,:),IERR)
      CALL CHECK_TRIP_SEND(YCOMMENT)
    ELSE
      CALL OASIS_PUT(NFFLOOD_ID,IDATE,ZGRID_NULL,IERR)
    ENDIF
!
    YCOMMENT='Flood potential infiltration' !kg/m2/s
    CALL MPI_GATHER(TPST%XCPL_PIFLOOD,KSTATE,MPI_DOUBLE,ZSTATE,KSTATE,MPI_DOUBLE,NPIO,NCOMM,IERR)
    IF (NRANK==NPIO) THEN
      CALL TRIP_STATE_TO_GRID(TPST%NSTATE_LON,TPST%NSTATE_LAT,ZSTATE,TP%XCPL_PIFLOOD)
      CALL MASK_TRIP(TP%XCPL_PIFLOOD(:,:),ZWRITE(:,:),LMASK(:,:),XTSTEP_CPL_LAND)
      CALL OASIS_PUT(NPIFLOOD_ID,IDATE,ZWRITE(:,:),IERR)
      CALL CHECK_TRIP_SEND(YCOMMENT)
    ELSE
      CALL OASIS_PUT(NPIFLOOD_ID,IDATE,ZGRID_NULL,IERR)
    ENDIF
!
  ENDIF
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       3.     Send coupling fields to ocean :
!               -------------------------------
!
!
IF(LCPL_SEA.AND.MOD(PTIMEC,XTSTEP_CPL_SEA)==0.0)THEN
!
! * Sea output fields
!
  YCOMMENT='Discharge to ocean' !kg/m2/s
  CALL MPI_GATHER(TPST%XCPL_RIVDIS,KSTATE,MPI_DOUBLE,ZSTATE,KSTATE,MPI_DOUBLE,NPIO,NCOMM,IERR)
  IF (NRANK==NPIO) THEN
    CALL TRIP_STATE_TO_GRID(TPST%NSTATE_LON,TPST%NSTATE_LAT,ZSTATE,TP%XCPL_RIVDIS)
    LMASK(:,:) = (TPG%NGRCN(:,:)==9.OR.TPG%NGRCN(:,:)==12)
    CALL MASK_TRIP(TP%XCPL_RIVDIS(:,:),ZWRITE(:,:),LMASK(:,:),XTSTEP_CPL_SEA)
    CALL OASIS_PUT(NRIVDIS_ID,IDATE,ZWRITE(:,:),IERR)
    CALL CHECK_TRIP_SEND(YCOMMENT)
  ELSE
    CALL OASIS_PUT(NRIVDIS_ID,IDATE,ZGRID_NULL,IERR)
  ENDIF
!
! * Calving output fields
!
  IF(LCPL_CALVSEA)THEN
!
    YCOMMENT='Calving flux over greenland' !kg/m2/s
    CALL MPI_GATHER(TPST%XCPL_CALVGRE,KSTATE,MPI_DOUBLE,ZSTATE,KSTATE,MPI_DOUBLE,NPIO,NCOMM,IERR)
    IF (NRANK==NPIO) THEN
      CALL TRIP_STATE_TO_GRID(TPST%NSTATE_LON,TPST%NSTATE_LAT,ZSTATE,TP%XCPL_CALVGRE)
      LMASK(:,:) = TPG%GMASK_GRE(:,:)
      CALL MASK_TRIP(TP%XCPL_CALVGRE(:,:),ZWRITE(:,:),LMASK(:,:),XTSTEP_CPL_SEA)
      CALL OASIS_PUT(NCALVGRE_ID,IDATE,ZWRITE(:,:),IERR)
      CALL CHECK_TRIP_SEND(YCOMMENT)
    ELSE
      CALL OASIS_PUT(NCALVGRE_ID,IDATE,ZGRID_NULL,IERR)
    ENDIF
!
    YCOMMENT='Calving flux over antarctica' !kg/m2/s
    CALL MPI_GATHER(TPST%XCPL_CALVANT,KSTATE,MPI_DOUBLE,ZSTATE,KSTATE,MPI_DOUBLE,NPIO,NCOMM,IERR)
    IF (NRANK==NPIO) THEN
      CALL TRIP_STATE_TO_GRID(TPST%NSTATE_LON,TPST%NSTATE_LAT,ZSTATE,TP%XCPL_CALVANT)
      LMASK(:,:) = TPG%GMASK_ANT(:,:)
      CALL MASK_TRIP(TP%XCPL_CALVANT(:,:),ZWRITE(:,:),LMASK(:,:),XTSTEP_CPL_SEA)
      CALL OASIS_PUT(NCALVANT_ID,IDATE,ZWRITE(:,:),IERR)
      CALL CHECK_TRIP_SEND(YCOMMENT)
    ELSE
      CALL OASIS_PUT(NCALVANT_ID,IDATE,ZGRID_NULL,IERR)
    ENDIF
!
  ENDIF
!
ENDIF
!
IF (NRANK==NPIO) THEN
  DEALLOCATE(ZSTATE)
  DEALLOCATE(ZWRITE)
  DEALLOCATE(LMASK)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP_OASIS_SEND',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
!
SUBROUTINE CHECK_TRIP_SEND(HCOMMENT)
!
USE MODI_ABORT_TRIP
!
IMPLICIT NONE
!
CHARACTER(LEN=*), INTENT(IN) :: HCOMMENT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('TRIP_OASIS_SEND:CHECK_TRIP_SEND',0,ZHOOK_HANDLE)
!
! Check receiving field
!
IF (IERR/=OASIS_OK.AND.IERR<OASIS_SENT) THEN
  WRITE(KLISTING,'(A,I4)')'Return code from sending '//TRIM(HCOMMENT)//' : ',IERR
  CALL ABORT_TRIP('TRIP_OASIS_SEND: problem sending '//TRIM(HCOMMENT))
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP_OASIS_SEND:CHECK_TRIP_SEND',1,ZHOOK_HANDLE)
!
END SUBROUTINE CHECK_TRIP_SEND
!
!-------------------------------------------------------------------------------
!
SUBROUTINE MASK_TRIP(PIN,POUT,OMASK,PDIV)
!
IMPLICIT NONE
!
REAL,    DIMENSION(:,:), INTENT(INOUT) :: PIN
REAL,    DIMENSION(:,:), INTENT(OUT  ) :: POUT
LOGICAL, DIMENSION(:,:), INTENT(IN   ) :: OMASK
REAL                   , INTENT(IN   ) :: PDIV
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('TRIP_OASIS_SEND:MASK_TRIP',0,ZHOOK_HANDLE)
!
POUT(:,:) = XUNDEF
!
WHERE(OMASK(:,:))
  POUT(:,:) = PIN(:,:)/PDIV
  PIN (:,:) = 0.0
ELSEWHERE
  PIN (:,:) = XUNDEF
ENDWHERE
!
IF (LHOOK) CALL DR_HOOK('TRIP_OASIS_SEND:MASK_TRIP',1,ZHOOK_HANDLE)
!
END SUBROUTINE MASK_TRIP
!
!-------------------------------------------------------------------------------
#endif
!-------------------------------------------------------------------------------
!
END SUBROUTINE TRIP_OASIS_SEND
