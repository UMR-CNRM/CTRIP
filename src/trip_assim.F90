!###################################################################
PROGRAM TRIP_ASSIM
!###################################################################
!
!
!!****  *TRIP_ASSIM*
!!
!!    PURPOSE
!!    -------
!!
!!    Driver for Assimilation of TRIP variables
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      S. Saysset
!!
!!    MODIFICATIONS
!!    -------------
!!      S. Munier   06/2021 Integration to CTRIP-12D
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURFEX_TRIP_n
USE MODD_OFF_TRIP_n
!
USE MODD_TRIP_LISTING
USE MODD_TRIP_GRID
USE MODD_TRIP_STATE
USE MODD_TRIP_PAR, ONLY : NUNDEF
USE MODD_TRIP_MPI
USE MODD_TRIP_ASSIM
!
USE MODN_TRIP_RUN, ONLY : LRESTART, LPRINT, LWR_DIAG,  &
                          XTSTEP_RUN, XTSTEP_DIAG
USE MODN_TRIP_ASSIM, ONLY : CLOCAL
!
USE MODE_RW_TRIP
!
USE MODI_READ_NAM_TRIP_RUN
USE MODI_READ_NAM_TRIP
USE MODI_READ_NAM_TRIP_GRID
USE MODI_READ_NAM_ASSIM
!
USE MODI_ABORT_TRIP
USE MODI_GET_TRIP_GRID_CONF
!
USE MODI_INIT_TRIP
USE MODI_INIT_TRIP_PAR
USE MODI_TRIP_BASIN_PROC
USE MODI_INIT_TRIP_STATE
USE MODI_TRIP_RUN_CONF
USE MODI_TRIP_RESTART
USE MODI_TRIP_DIAG_RUN
USE MODI_TRIP_RUN
USE MODI_INIT_TRIP_ANALYSIS
USE MODI_INIT_TRIP_LOCAL
USE MODI_TRIP_RUN_ASSIM
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE 'mpif.h'
#endif
!
INTEGER :: IYEAR               ! current year         (UTC)
INTEGER :: IMONTH              ! current month        (UTC)
INTEGER :: IDAY                ! current day          (UTC)
REAL    :: ZTIME               ! current time           (s)
REAL    :: ZRUNTIME            ! total simulated time   (s)
!
INTEGER :: INB_TSTEP_RUN       ! number of time step in the run
INTEGER :: ILON                ! Number of longitude
INTEGER :: ILAT                ! Number of latittude
INTEGER :: ISTATE              ! Number of states per proc
INTEGER :: ILAKE_NUM           ! Number of lakes
!
INTEGER :: INB_OL              ! number of time step if forcing offline
INTEGER :: ILON_OL             ! Number of longitude if forcing offline
INTEGER :: ILAT_OL             ! Number of latittude if forcing offline
!
INTEGER :: IERR                ! Error value
LOGICAL :: GOASIS              ! OASIS used(default=.false.)
LOGICAL :: GXIOS               ! XIOS used(default=.false.)
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! --------------------------------------------------------------------------------------
! * 0. MPI must be initialized before any DR_HOOK call
! --------------------------------------------------------------------------------------
!
#ifndef SFX_MPI
  WRITE(*,'(A)' )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(*,'(2A)')'TRIP_ASSIM must be compiled with MPI'
  WRITE(*,'(A)' )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  CALL ABORT
  STOP
#endif
!
CALL MPI_INIT(IERR)
GOASIS = .FALSE.
GXIOS = .FALSE.
!
! --------------------------------------------------------------------------------------
! * 1. Alloc trip variables and open listing
! --------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_ASSIM',0,ZHOOK_HANDLE)
!
CALL TRIP_ALLOC_LIST(1)
!
CALL INIT_TRIP_PAR
!
! --------------------------------------------------------------------------------------
! * 2. Prepare MPI communicators
! --------------------------------------------------------------------------------------
!
NCOMM_ASSIM = MPI_COMM_WORLD
CALL MPI_COMM_SIZE(NCOMM_ASSIM,NPROC_ASSIM,IERR)
CALL MPI_COMM_RANK(NCOMM_ASSIM,NRANK_ASSIM,IERR)
NENS = NPROC_ASSIM-1
CALL MPI_COMM_SPLIT(NCOMM_ASSIM,NRANK_ASSIM,NRANK_ASSIM,NCOMM,IERR)
CALL MPI_COMM_SIZE(NCOMM,NPROC,IERR)
CALL MPI_COMM_RANK(NCOMM,NRANK,IERR)
!
WRITE(CRANK,'(I0.3)') NRANK_ASSIM
WRITE(CLISTING,*) 'TRIP_RUN_LISTING_'//CRANK//'.txt'
OPEN(UNIT=NLISTING,FILE=ADJUSTL(CLISTING),FORM='FORMATTED',ACTION='WRITE')
!
WRITE(NLISTING,*) '!!!!!!!!!!!!!!!!!!!!!!!'
WRITE(NLISTING,*) '   TRIP assimilation   '
WRITE(NLISTING,*) '!!!!!!!!!!!!!!!!!!!!!!!'
WRITE(NLISTING,*) 'Number of procs       :', NPROC_ASSIM
WRITE(NLISTING,*) 'Ensemble size         :', NENS
WRITE(NLISTING,*) 'Ensemble member       :', NRANK_ASSIM
WRITE(NLISTING,*) 'Global communicator   :', NCOMM_ASSIM
WRITE(NLISTING,*) '!!!!!!!!!!!!!!!!!!!!!!!'
WRITE(NLISTING,*) 'Local number of procs :', NPROC
WRITE(NLISTING,*) 'Local rank            :', NRANK
WRITE(NLISTING,*) 'Local communicator    :', NCOMM
WRITE(NLISTING,*) '!!!!!!!!!!!!!!!!!!!!!!!'
WRITE(NLISTING,*) '                       '
! WRITE(*,*) NRANK_ASSIM,NRANK,NPIO
!
! --------------------------------------------------------------------------------------
! * 3. Read namelists
! --------------------------------------------------------------------------------------
! CALL MPI_BARRIER(NCOMM_ASSIM,IERR)
! IF (NRANK_ASSIM==NPIO_ASSIM) WRITE(*,*) '3. Read namelists'
!
CALL READ_NAM_TRIP_RUN(NLISTING)
!
CALL READ_NAM_TRIP(NLISTING)
!
CALL READ_NAM_ASSIM(NLISTING)
!
! --------------------------------------------------------------------------------------
! * 4. TRIP initializations
! --------------------------------------------------------------------------------------
! CALL MPI_BARRIER(NCOMM_ASSIM,IERR)
! IF (NRANK_ASSIM==NPIO_ASSIM) WRITE(*,*) '4. TRIP initializations'
!
YTRIP_CUR => YTRIP_LIST(1)
!
CALL READ_NAM_TRIP_GRID(YTRIP_CUR%TPG,NLISTING)
!
CALL INIT_TRIP(YTRIP_CUR%TPDG, YTRIP_CUR%TP, YTRIP_CUR%TPG, YTRIP_CUR%TPLK, &
               IYEAR,IMONTH,IDAY,ZTIME,ILON,ILAT,ILAKE_NUM,                 &
               XTSTEP_RUN,XTSTEP_DIAG,LRESTART,GXIOS                        )
!
! --------------------------------------------------------------------------------------
! * 5. Select processor for each basin
! --------------------------------------------------------------------------------------
! CALL MPI_BARRIER(NCOMM_ASSIM,IERR)
! IF (NRANK_ASSIM==NPIO_ASSIM) WRITE(*,*) '5. Select processor for each basin'
!
CALL TRIP_BASIN_PROC(YTRIP_CUR%TPG, YTRIP_CUR%TPLK, YTRIP_CUR%TPST, &
                     NLISTING,ILON,ILAT,ILAKE_NUM                   )
!
! --------------------------------------------------------------------------------------
! * 6. Prepare state vector
! --------------------------------------------------------------------------------------
! CALL MPI_BARRIER(NCOMM_ASSIM,IERR)
! IF (NRANK_ASSIM==NPIO_ASSIM) WRITE(*,*) '6. Prepare state vector'
!
CALL INIT_TRIP_STATE(YTRIP_CUR%TP, YTRIP_CUR%TPG, YTRIP_CUR%TPLK, YTRIP_CUR%TPST, YTRIP_CUR%TPDG)
ISTATE = YTRIP_CUR%TPST%NNSTATE_P
!
! --------------------------------------------------------------------------------------
! * 7. Get run configuration
! --------------------------------------------------------------------------------------
! CALL MPI_BARRIER(NCOMM_ASSIM,IERR)
! IF (NRANK_ASSIM==NPIO_ASSIM) WRITE(*,*) '7. Get run configuration'
!
CALL TRIP_RUN_CONF(NLISTING, GOASIS, IYEAR, IMONTH, IDAY, ZTIME, &
                   ILON , ILAT, INB_TSTEP_RUN, ZRUNTIME )
!
! * Create analysis file
IF (NRANK_ASSIM==NPIO_ASSIM) THEN
  CALL INIT_TRIP_ANALYSIS(YTRIP_CUR%TP, YTRIP_CUR%TPG, YTRIP_CUR%TPST, YTRIP_CUR%TPA, NLISTING, ISTATE, NENS)
!
  IF (CLOCAL=='COV') CALL INIT_TRIP_LOCAL(YTRIP_CUR%TPST, NLISTING, ILON, ILAT, ISTATE)
ENDIF
!
! --------------------------------------------------------------------------------------
! * 8. Run model and assimilation
! --------------------------------------------------------------------------------------
! CALL MPI_BARRIER(NCOMM_ASSIM,IERR)
! IF (NRANK_ASSIM==NPIO_ASSIM) WRITE(*,*) '8. Run model and assimilation'
!
INB_OL  = INB_TSTEP_RUN
ILON_OL = ILON
ILAT_OL = ILAT
!
CALL TRIP_RUN_ASSIM(YTRIP_CUR%TPDG, YTRIP_CUR%TP, YTRIP_CUR%TPG, YTRIP_CUR%TPLK, &
                    YTRIP_CUR%TPST, YTRIP_CUR%TPA,                               &
                    GOASIS, GXIOS, NLISTING, ISTATE, INB_TSTEP_RUN,              &
                    ZRUNTIME, ILON_OL, ILAT_OL, INB_OL, IYEAR, IMONTH, IDAY, ZTIME)
!
!-------------------------------------------------------------------------------
! * 9. Store run mean diagnostic and write restart
!-------------------------------------------------------------------------------
! CALL MPI_BARRIER(NCOMM_ASSIM,IERR)
! IF (NRANK_ASSIM==NPIO_ASSIM) WRITE(*,*) '9. Store run mean diagnostic and write restart'
!
IF(LWR_DIAG.AND.NRANK_ASSIM/=NPIO_ASSIM) THEN
  CALL TRIP_DIAG_RUN(YTRIP_CUR%TPDG, YTRIP_CUR%TPG, &
                     NLISTING,ILON,ILAT,ILAKE_NUM,ZRUNTIME)
ENDIF
!
IF(LRESTART.AND.NRANK_ASSIM/=NPIO_ASSIM) THEN
  CALL TRIP_RESTART(YTRIP_CUR%TP, YTRIP_CUR%TPG, YTRIP_CUR%TPLK, YTRIP_CUR%TPST, &
                    NLISTING,IYEAR,IMONTH,IDAY,ZTIME,ILON,ILAT,ILAKE_NUM         )
ENDIF
!
CALL MPI_BARRIER(NCOMM_ASSIM,IERR)
!
! --------------------------------------------------------------------------------------
! * 10. End of run
! --------------------------------------------------------------------------------------
! CALL MPI_BARRIER(NCOMM_ASSIM,IERR)
! IF (NRANK_ASSIM==NPIO_ASSIM) WRITE(*,*) '10. End of run'
!
WRITE(NLISTING,*) ' '
WRITE(NLISTING,*) '    -----------------------------'
WRITE(NLISTING,*) '    | TRIP ASSIM ENDS CORRECTLY |'
WRITE(NLISTING,*) '    -----------------------------'
WRITE(NLISTING,*) ' '
CLOSE(NLISTING)
!
IF (NRANK_ASSIM==NPIO_ASSIM) THEN
  WRITE(*,*) ' '
  WRITE(*,*) '    -----------------------------'
  WRITE(*,*) '    | TRIP ASSIM ENDS CORRECTLY |'
  WRITE(*,*) '    -----------------------------'
  WRITE(*,*) ' '
ENDIF
!
!CALL TRIP_DEALLO_LIST
!
IF (LHOOK) CALL DR_HOOK('TRIP_ASSIM',1,ZHOOK_HANDLE)
!
! --------------------------------------------------------------------------------------
! * 11. MPI must be finalized
! --------------------------------------------------------------------------------------
!
CALL MPI_FINALIZE(IERR)
!
!-------------------------------------------------------------------------------
END PROGRAM TRIP_ASSIM
