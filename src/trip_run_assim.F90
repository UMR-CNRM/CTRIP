SUBROUTINE TRIP_RUN_ASSIM (TPDG, TP, TPG, TPLK, TPST, TPA,  &
                           OOASIS, OXIOS, KLISTING,         &
                           KSTATE,KNB_TSTEP_RUN,PRUNTIME,   &
                           KLON,KLAT,KNB,                   &
                           KYEAR,KMONTH,KDAY,PTIME          )
!#############################################
!
!!****  *TRIP_RUN_ASSIM*
!!
!!    PURPOSE
!!    -------
!!      Run trip with assimilation
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      S. Munier
!!
!!    MODIFICATIONS
!!    -------------
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_DIAG,  ONLY : TRIP_DIAG_t
USE MODD_TRIP,       ONLY : TRIP_t
USE MODD_TRIP_GRID,  ONLY : TRIP_GRID_t
USE MODD_TRIP_LAKE,  ONLY : TRIP_LAKE_t
USE MODD_TRIP_STATE, ONLY : TRIP_STATE_t
USE MODD_TRIP_ANALYSIS, ONLY : TRIP_ANALYSIS_t
!
USE MODN_TRIP_RUN, ONLY : LPRINT, XTSTEP_RUN, XTSTEP_DIAG, LWR_DIAG
!
USE MODD_TRIP_PAR, ONLY : XUNDEF, XDAY
!
USE MODD_TRIP_MPI
USE MODD_TRIP_ASSIM
USE MODN_TRIP_ASSIM
!
USE MODE_TRIP_GRID_STATE
USE MODE_TRIP_LAKE
!
USE MODI_TRIP_FORCING_OFFLINE
USE MODI_TRIP_INTERFACE
USE MODI_TRIP_DATE
USE MODI_ASSIM_OBS
USE MODI_ASSIM_PERTURBATE_FIELD
USE MODI_ASSIM_ETKF_LOCAL
USE MODI_TRIP_DIAG_GATHER
USE MODI_TRIP_DIAG_WRITE
USE MODI_TRIP_ANALYSIS_WRITE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
INCLUDE 'mpif.h'
!
!-------------------------------------------------------------------------------
!
!*      0.1    declarations of arguments
!
TYPE(TRIP_DIAG_t),     INTENT(INOUT) :: TPDG
TYPE(TRIP_t),          INTENT(INOUT) :: TP
TYPE(TRIP_GRID_t),     INTENT(INOUT) :: TPG
TYPE(TRIP_LAKE_t),     INTENT(INOUT) :: TPLK
TYPE(TRIP_STATE_t),    INTENT(INOUT) :: TPST
TYPE(TRIP_ANALYSIS_t), INTENT(INOUT) :: TPA
!
LOGICAL, INTENT(IN)  :: OOASIS        ! Oasis coupling or not
LOGICAL, INTENT(IN)  :: OXIOS         ! Do we use XIOS
!
INTEGER, INTENT(IN)  :: KLISTING      ! Listing ID
INTEGER, INTENT(IN)  :: KSTATE        ! Size of state vector
INTEGER, INTENT(IN)  :: KNB_TSTEP_RUN ! number of time step in the run
REAL,    INTENT(IN)  :: PRUNTIME      ! total simulated time
!
INTEGER, INTENT(IN)  :: KLON          ! Number of longitude if forcing offline
INTEGER, INTENT(IN)  :: KLAT          ! Number of latittude if forcing offline
INTEGER, INTENT(IN)  :: KNB           ! number of time step if forcing offline
!
INTEGER, INTENT(OUT) :: KYEAR         ! current year         (UTC)
INTEGER, INTENT(OUT) :: KMONTH        ! current month        (UTC)
INTEGER, INTENT(OUT) :: KDAY          ! current day          (UTC)
REAL,    INTENT(OUT) :: PTIME         ! current time           (s)
!
!-------------------------------------------------------------------------------
!
!*      0.2    declarations of local variables
!
! FORCING VARIABLES
REAL, DIMENSION(:,:), ALLOCATABLE :: ZDRAIN_OL     ! Drainage from the forcing file          (kg)
REAL, DIMENSION(:,:), ALLOCATABLE :: ZRUNOFF_OL    ! Surface runoff from the forcing file    (kg)
REAL, DIMENSION(:,:), ALLOCATABLE :: ZSRC_FLOOD_OL ! Flood source term from the forcing file (kg)
!
REAL, DIMENSION(:),   ALLOCATABLE :: ZRUNOFF       ! Surface runoff               (kg/s)
REAL, DIMENSION(:),   ALLOCATABLE :: ZDRAIN        ! Drainage                     (kg/s)
REAL, DIMENSION(:),   ALLOCATABLE :: ZCALVING      ! Calving flux                 (kg/s)
REAL, DIMENSION(:),   ALLOCATABLE :: ZSRC_FLOOD    ! Input P-E-I flood source term(kg/s)
!
! OBSERVATION VARIABLES
REAL,    DIMENSION(:,:), ALLOCATABLE :: ZOBS
REAL,    DIMENSION(:,:), ALLOCATABLE :: ZOBS_ERR
INTEGER, DIMENSION(:,:), ALLOCATABLE :: IDTOBS
INTEGER, DIMENSION(KNB)              :: INOBS
CHARACTER(LEN=4)                     :: YOBS
!
! ASSIMILATION VARIABLES
REAL,    DIMENSION(:,:), ALLOCATABLE :: ZXB
REAL,    DIMENSION(:,:), ALLOCATABLE :: ZXA
REAL,    DIMENSION(:,:), ALLOCATABLE :: ZHX
REAL,    DIMENSION(:,:), ALLOCATABLE :: ZWORK
REAL,    DIMENSION(:),   ALLOCATABLE :: ZINFL
!
! MISC
REAL    :: ZTIMEC             ! cumulated current time (s)
INTEGER :: JNB_TSTEP_RUN      ! TSTEP_RUN counter
INTEGER :: JNB_TSTEP_DIAG     ! DIAG call counter
INTEGER :: JNB_TSTEP_ANALYSIS ! ANALYSIS call counter
INTEGER :: ICOUNT
INTEGER :: KSEED
INTEGER :: ILAKE_NUM
INTEGER :: I, J, IERR
LOGICAL :: GWRITE_DIAG
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! --------------------------------------------------------------------------------------
! * 0. Initialize
! --------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_RUN_ASSIM',0,ZHOOK_HANDLE)
!
KSEED = -197891-NRANK_ASSIM
IF (NRANK_ASSIM/=NPIO_ASSIM .AND. LINIT_PERT) THEN
  CALL ASSIM_PERTURBATE_FIELD(KSTATE, XAMP_P, XMEAN_P, TPST%XSURF_STO, KSEED)
  LINIT_PERT = .FALSE.
ENDIF
!
IF (NRANK_ASSIM==NPIO_ASSIM) THEN
  ALLOCATE(ZDRAIN_OL(0,0))
  ALLOCATE(ZRUNOFF_OL(0,0))
  ALLOCATE(ZSRC_FLOOD_OL(0,0))
  ALLOCATE(ZRUNOFF(0))
  ALLOCATE(ZDRAIN(0))
  ALLOCATE(ZCALVING(0))
  ALLOCATE(ZSRC_FLOOD(0))
  ALLOCATE(IDTOBS(KSTATE,KNB))
  ALLOCATE(ZOBS(KSTATE,KNB))
  ALLOCATE(ZOBS_ERR(KSTATE,KNB))
  ALLOCATE(ZXB(KSTATE,NENS))
  ALLOCATE(ZXA(KSTATE,NENS))
  ALLOCATE(ZHX(KSTATE,NENS))
  ALLOCATE(ZWORK(KSTATE,NENS+1))
ELSE
  ALLOCATE(ZDRAIN_OL(KSTATE,KNB))
  ALLOCATE(ZRUNOFF_OL(KSTATE,KNB))
  ALLOCATE(ZSRC_FLOOD_OL(KSTATE,KNB))
  ALLOCATE(ZRUNOFF(KSTATE))
  ALLOCATE(ZDRAIN(KSTATE))
  ALLOCATE(ZCALVING(KSTATE))
  ALLOCATE(ZSRC_FLOOD(KSTATE))
  ALLOCATE(IDTOBS(0,KNB))
  ALLOCATE(ZOBS(0,KNB))
  ALLOCATE(ZOBS_ERR(0,KNB))
  ALLOCATE(ZXB(0,0))
  ALLOCATE(ZXA(0,0))
  ALLOCATE(ZHX(0,0))
  ALLOCATE(ZWORK(0,0))
ENDIF
!
! --------------------------------------------------------------------------------------
! * 1. Read and prepare drainage and runoff if offline
! --------------------------------------------------------------------------------------
!
IF (NRANK_ASSIM/=NPIO_ASSIM) THEN
!
  CALL TRIP_FORCING_OFFLINE(TPG, TPLK, TPST,                                  &
                            KLISTING, KSTATE, KNB_TSTEP_RUN, KLON, KLAT, KNB, &
                            ZDRAIN_OL, ZRUNOFF_OL, ZSRC_FLOOD_OL              )
!
ENDIF
!
! --------------------------------------------------------------------------------------
! * 2. Read observations
! --------------------------------------------------------------------------------------
!
IF (NRANK_ASSIM==NPIO_ASSIM) THEN
  CALL ASSIM_OBS(TPST,KLISTING,KSTATE,KLON,KLAT,KNB,INOBS,IDTOBS,ZOBS,ZOBS_ERR)
  WRITE(KLISTING,*) 'Total number of observation time steps: ',NOBS
ENDIF
!
CALL MPI_BCAST(INOBS, KNB, MPI_INTEGER, NPIO_ASSIM, NCOMM_ASSIM, IERR)
!
! --------------------------------------------------------------------------------------
! * 3. Temporal loops
! --------------------------------------------------------------------------------------
!
ZTIMEC         = 0
ICOUNT         = 0
JNB_TSTEP_DIAG = 0
JNB_TSTEP_ANALYSIS = 0
ILAKE_NUM = TPLK%NLAKE_NUM
!
DO JNB_TSTEP_RUN = 1, KNB_TSTEP_RUN
!
! --------------------------------------------------------------------------------------
! * 3.1 Ensemble run of TRIP
! --------------------------------------------------------------------------------------
!
  IF (NRANK_ASSIM/=NPIO_ASSIM) THEN
    ZDRAIN    (:) = ZDRAIN_OL    (:,JNB_TSTEP_RUN) / XTSTEP_RUN
    ZRUNOFF   (:) = ZRUNOFF_OL   (:,JNB_TSTEP_RUN) / XTSTEP_RUN
    ZSRC_FLOOD(:) = ZSRC_FLOOD_OL(:,JNB_TSTEP_RUN) / XTSTEP_RUN
    ZCALVING  (:) = 0.0
!
    GWRITE_DIAG = (INOBS(JNB_TSTEP_RUN)==0)
!
    CALL TRIP_INTERFACE(TPDG, TP, TPG, TPLK, TPST,       &
                        KLISTING,LPRINT,                 &
                        KSTATE,KLON,KLAT,ILAKE_NUM,      &
                        PTIME,ZTIMEC,                    &
                        JNB_TSTEP_RUN,JNB_TSTEP_DIAG,    &
                        XTSTEP_RUN,XTSTEP_DIAG,ZRUNOFF,  &
                        ZDRAIN,ZCALVING,ZSRC_FLOOD,OXIOS,&
                        GWRITE_DIAG )
!
  ELSE
    PTIME = PTIME + XTSTEP_RUN
    ZTIMEC = ZTIMEC + XTSTEP_RUN
  ENDIF
!
! --------------------------------------------------------------------------------------
! * 3.2 Assimilation if obs available
! --------------------------------------------------------------------------------------
!
  IF (INOBS(JNB_TSTEP_RUN)>0) THEN
!
    WRITE(KLISTING,*)'ASSIMILATION ', INOBS(JNB_TSTEP_RUN)
    !IF(NRANK_ASSIM==NPIO_ASSIM)WRITE(*,*)'ASSIMILATION ', JNB_TSTEP_RUN,ZTIMEC,INOBS(JNB_TSTEP_RUN),JNB_TSTEP_ANALYSIS
!
    ! Gather background state vector
    CALL MPI_GATHER(TPST%XSURF_STO,KSTATE,MPI_DOUBLE,ZWORK,KSTATE,MPI_DOUBLE,NPIO_ASSIM,NCOMM_ASSIM,IERR)
    IF (NRANK_ASSIM==NPIO_ASSIM) THEN
      DO J = 1,NENS
        ZXB(:,J) = ZWORK(:,J+1)
      ENDDO
    ENDIF
!
    ! Kalman computation
    CALL ASSIM_ETKF_LOCAL(TPST, TPA, NENS, KSTATE, INOBS(JNB_TSTEP_RUN), ZXB, &
                          ZOBS(:,JNB_TSTEP_RUN), ZOBS_ERR(:,JNB_TSTEP_RUN), IDTOBS(:,JNB_TSTEP_RUN), ZHX, ZXA)
!
    IF (NRANK_ASSIM==NPIO_ASSIM) THEN
!
      JNB_TSTEP_ANALYSIS = JNB_TSTEP_ANALYSIS+1
      CALL TRIP_ANALYSIS_WRITE(TPA, KLISTING, KSTATE, NENS, INOBS(JNB_TSTEP_RUN), JNB_TSTEP_ANALYSIS, ZTIMEC)
!
      DO J = 1,NENS
        ZWORK(:,J+1) = ZXA(:,J)
      ENDDO
!
    ENDIF
!
    CALL MPI_SCATTER(ZWORK, KSTATE, MPI_DOUBLE, &
                     TPST%XSURF_STO, KSTATE, MPI_DOUBLE, NPIO_ASSIM, NCOMM_ASSIM, IERR)
!
    ! Write diagnostic
    IF (NRANK_ASSIM/=NPIO_ASSIM) THEN
      IF (LWR_DIAG.AND.MOD(ZTIMEC,XTSTEP_DIAG)==0.) THEN
        JNB_TSTEP_DIAG = JNB_TSTEP_DIAG+1
        CALL TRIP_DIAG_GATHER(TPLK, TPDG, TPST)
        IF (NRANK==NPIO) THEN
          CALL TRIP_DIAG_WRITE(TPDG, TPG,                                                    &
                               KLISTING,KLON,KLAT,ILAKE_NUM,JNB_TSTEP_DIAG,XTSTEP_DIAG,OXIOS )
        ENDIF
      ENDIF
    ENDIF
  ENDIF
!
  IF (NRANK_ASSIM==NPIO_ASSIM.AND.LPRINT.AND.MOD(ZTIMEC,XDAY)==0.0) THEN
    ICOUNT = ICOUNT +1
    !WRITE(*,'(A28)') '============================'
    WRITE(*,'(A10,I5,A2,I5)') 'TRIP DAY :',ICOUNT,' /',INT(PRUNTIME/XDAY)
    !WRITE(*,'(A28)') '============================'
  ENDIF
!
! * TRIP DATE INCREMENT
!
  CALL TRIP_DATE(KYEAR,KMONTH,KDAY,PTIME)
!
  WRITE(KLISTING,*) '==========================='
  WRITE(KLISTING,*) 'DATE ', INT(ZTIMEC)
  !IF (JOBS.LE.NOBS) WRITE(KLISTING,*) 'NOBS ', INT(ZTIMEC_OBS(JOBS))
!
ENDDO
!
! Copy XINFL for saving into TRIP_RESTART
!
IF(CINFL=='A09'.OR.CINFL=='S21')THEN
  ALLOCATE(ZINFL(KSTATE))
  IF (NRANK_ASSIM==NPIO_ASSIM) ZINFL(:) = TPA%XINFL(:)
  CALL MPI_BCAST(ZINFL,KSTATE,MPI_DOUBLE,NPIO_ASSIM,NCOMM_ASSIM,IERR)
  IF (NRANK==NPIO) CALL TRIP_STATE_TO_GRID(TPST%NSTATE_LON,TPST%NSTATE_LAT,ZINFL,TP%XINFL)
  DEALLOCATE(ZINFL)
ENDIF
!
! --------------------------------------------------------------------------------------
! * 4. End TRIP run
! --------------------------------------------------------------------------------------
!
DEALLOCATE(ZDRAIN_OL)
DEALLOCATE(ZRUNOFF_OL)
DEALLOCATE(ZSRC_FLOOD_OL)
DEALLOCATE(ZRUNOFF)
DEALLOCATE(ZDRAIN)
DEALLOCATE(ZCALVING)
DEALLOCATE(ZSRC_FLOOD)
DEALLOCATE(IDTOBS)
DEALLOCATE(ZOBS)
DEALLOCATE(ZOBS_ERR)
DEALLOCATE(ZXB)
DEALLOCATE(ZXA)
DEALLOCATE(ZHX)
DEALLOCATE(ZWORK)
!
IF (LHOOK) CALL DR_HOOK('TRIP_RUN_ASSIM',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_RUN_ASSIM


