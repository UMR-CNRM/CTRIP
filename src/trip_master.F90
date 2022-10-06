!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!###################################################################
PROGRAM TRIP_MASTER
!###################################################################
!
!
!!****  *TRIP_MASTER*
!!
!!    PURPOSE
!!    -------
!!
!!    Driver for TRIP from CNRM
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/08
!!      S.Sénési    08/11/16 : interface to XIOS
!!      S. Munier   03/2020    CTRIP-12D and parallelization
!!      T. Guinaldo 04/2020    Add MLake
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
!
USE MODN_TRIP_RUN, ONLY : LRESTART, LPRINT, LWR_DIAG,  &
                          XTSTEP_RUN, XTSTEP_DIAG
!
USE MODD_TRIP_PAR, ONLY : XUNDEF, NUNDEF, XDAY
USE MODD_TRIP_MPI
!
USE MODE_RW_TRIP
!
USE MODI_READ_NAM_TRIP_RUN
USE MODI_READ_NAM_TRIP
USE MODI_READ_NAM_TRIP_GRID
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
!
USE MODI_TRIP_OASIS_INIT
USE MODI_TRIP_OASIS_READ_NAM
USE MODI_TRIP_OASIS_DEFINE
USE MODI_TRIP_OASIS_END
!
USE MODI_INIT_TRIP_CPL_ESM
!
USE MODI_TRIP_XIOS_INIT
!
#ifdef SFX_MPI
#ifdef SFX_MPL
USE MPL_DATA_MODULE, ONLY : LMPLUSERCOMM, MPLUSERCOMM
#endif
#endif
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#if defined CPLOASIS || defined WXIOS || defined SFX_MPI
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
! * 0. MPI and OASIS must be initialized before any DR_HOOK call
! --------------------------------------------------------------------------------------
!
CALL TRIP_OASIS_INIT(GOASIS,GXIOS,NCOMM,PRUNTIME=ZRUNTIME)
#ifdef SFX_MPI
IF(.NOT.GOASIS) CALL MPI_INIT(IERR)
#endif
!
#ifdef SFX_MPI
#ifdef SFX_MPL
IF (NCOMM/=0) THEN
  LMPLUSERCOMM = .TRUE.
  MPLUSERCOMM = NCOMM
ENDIF
#endif
#endif
!
! --------------------------------------------------------------------------------------
! * 1. Alloc trip variables and open listing
! --------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_MASTER',0,ZHOOK_HANDLE)
!
CALL TRIP_ALLOC_LIST(1)
!
CALL INIT_TRIP_PAR
!
! --------------------------------------------------------------------------------------
! * 2. Check run attributes
! --------------------------------------------------------------------------------------
!
!Inquire if trip is parallel or not
!
NPROC = NUNDEF
NRANK = NPIO
!
#if defined CPLOASIS || defined SFX_MPI || defined WXIOS
IF (NCOMM==0) NCOMM = MPI_COMM_WORLD
CALL MPI_COMM_SIZE(NCOMM,NPROC,IERR)
CALL MPI_COMM_RANK(NCOMM,NRANK,IERR)
!
IF (NPROC>1) THEN
  WRITE(CRANK,'(I0.3)') NRANK
  WRITE(CLISTING,*) 'TRIP_RUN_LISTING_'//CRANK//'.txt'
ENDIF
#endif
!
OPEN(UNIT=NLISTING,FILE=ADJUSTL(CLISTING),FORM='FORMATTED',ACTION='WRITE')
!
#if defined CPLOASIS || defined SFX_MPI || defined WXIOS
IF(NPROC==NUNDEF.OR.NRANK==NUNDEF)THEN
  WRITE(NLISTING,*)'TRIP_MASTER: PROBLEM WITH MPI, NPROC = ',NPROC
  WRITE(NLISTING,*)'TRIP_MASTER: PROBLEM WITH MPI, NRANK  = ',NRANK
  CALL ABORT_TRIP('TRIP_MASTER: PROBLEM WITH MPI')
ENDIF
#endif
!
#ifndef SFX_MPI
IF(NPROC>1.AND.NPROC<NUNDEF)THEN
  WRITE(NLISTING,*)'TRIP_MASTER: TRIP NOT YET PARALLELIZED, NPROC SHOULD BE 1'
  CALL ABORT_TRIP('TRIP_MASTER: TRIP NOT YET PARALLELIZED')
ENDIF
#endif
!
IF (GXIOS) THEN
  WRITE(NLISTING,*)'TRIP_MASTER: THIS VERSION OF TRIP DOES NOT WORK WITH XIOS'
  CALL ABORT_TRIP('TRIP_MASTER: PROBLEM WITH XIOS')
ENDIF
!
IF(GOASIS)THEN
  WRITE(NLISTING,*) '!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(NLISTING,*) '    OASIS is used      '
  WRITE(NLISTING,*) '!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(NLISTING,*) 'Number of processes   :', NPROC
  WRITE(NLISTING,*) 'Local process number  :', NRANK
  WRITE(NLISTING,*) 'Local communicator    :', NCOMM
  WRITE(NLISTING,*) '!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(NLISTING,*) '                       '
ELSE
  WRITE(NLISTING,*) '!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(NLISTING,*) '   TRIP run offline    '
#ifdef SFX_MPI
  WRITE(NLISTING,*) '!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(NLISTING,*) 'Number of processes   :', NPROC
  WRITE(NLISTING,*) 'Local process number  :', NRANK
  WRITE(NLISTING,*) 'Local communicator    :', NCOMM
#endif
  WRITE(NLISTING,*) '!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(NLISTING,*) '                       '
ENDIF
!
! --------------------------------------------------------------------------------------
! * 3. read namelists
! --------------------------------------------------------------------------------------
!
! IF (NRANK==NPIO) WRITE(*,*) '3. read namelists'
CALL READ_NAM_TRIP_RUN(NLISTING)
!
CALL READ_NAM_TRIP(NLISTING)
!
IF(GOASIS) CALL TRIP_OASIS_READ_NAM(NLISTING,ZRUNTIME)
!
! --------------------------------------------------------------------------------------
! * 4. TRIP initializations
! --------------------------------------------------------------------------------------
!
! IF (NRANK==NPIO) WRITE(*,*) '4. TRIP initializations'
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
!
! IF (NRANK==NPIO) WRITE(*,*) '5. Select processor for each basin'
CALL TRIP_BASIN_PROC(YTRIP_CUR%TPG, YTRIP_CUR%TPLK, YTRIP_CUR%TPST, &
                     NLISTING,ILON,ILAT,ILAKE_NUM                   )
! IF(NRANK==NPIO)WRITE(*,*)YTRIP_CUR%TPST%NSTATE_IND(150,150)
! IF(NRANK/=NPIO)WRITE(*,*)YTRIP_CUR%TPST%NSTATE_IND_P(150,150)
!
! --------------------------------------------------------------------------------------
! * 6. Prepare state vector
! --------------------------------------------------------------------------------------
!
! IF (NRANK==NPIO) WRITE(*,*) '6. Prepare state vector'
CALL INIT_TRIP_STATE(YTRIP_CUR%TP, YTRIP_CUR%TPG, YTRIP_CUR%TPLK, YTRIP_CUR%TPST, YTRIP_CUR%TPDG)
ISTATE = YTRIP_CUR%TPST%NNSTATE_P
!
! --------------------------------------------------------------------------------------
! * 7.1. TRIP - OASIS  grid, partitions and local field definitions
! --------------------------------------------------------------------------------------
!
! IF (NRANK==NPIO) WRITE(*,*) '7.1. TRIP - OASIS  grid, partitions and local field definitions'
CALL INIT_TRIP_CPL_ESM(YTRIP_CUR%TP, YTRIP_CUR%TPG, YTRIP_CUR%TPST, ILON, ILAT, ISTATE)
!
IF(GOASIS)THEN
  CALL TRIP_OASIS_DEFINE(NLISTING,ILON,ILAT)
ENDIF
!
! --------------------------------------------------------------------------------------
! * 7.2. XIOS init
! --------------------------------------------------------------------------------------
!
#ifdef WXIOS
IF (GXIOS) THEN
  ! IF (NRANK==NPIO) WRITE(*,*) '7.2. XIOS init'
  CALL TRIP_XIOS_INIT(YTRIP_CUR%TPG,NCOMM,ILON,ILAT,&
                       IYEAR,IMONTH,IDAY,ZTIME)
ENDIF
#endif
!
! --------------------------------------------------------------------------------------
! * 8. Get run configuration
! --------------------------------------------------------------------------------------
!
! IF (NRANK==NPIO) WRITE(*,*) '8. Get run configuration'
CALL TRIP_RUN_CONF(NLISTING,GOASIS,IYEAR,IMONTH,IDAY,ZTIME, &
                   ILON,ILAT,INB_TSTEP_RUN,ZRUNTIME         )
!
IF(GOASIS)THEN
  INB_OL  = 0
  ILON_OL = ILON
  ILAT_OL = ILAT
ELSE
  INB_OL  = INB_TSTEP_RUN
  ILON_OL = ILON
  ILAT_OL = ILAT
ENDIF
! INB_TSTEP_RUN = 5
!
! --------------------------------------------------------------------------------------
! * 9. Read drainage and runoff and run TRIP
! --------------------------------------------------------------------------------------
!
! IF (NRANK==NPIO) WRITE(*,*) '9. Read drainage and runoff and run TRIP'
CALL TRIP_RUN(YTRIP_CUR%TPDG, YTRIP_CUR%TP, YTRIP_CUR%TPG, YTRIP_CUR%TPLK, YTRIP_CUR%TPST,   &
              GOASIS, GXIOS, NLISTING, ISTATE, INB_TSTEP_RUN,                &
              ZRUNTIME, ILON_OL, ILAT_OL, INB_OL, IYEAR, IMONTH, IDAY, ZTIME )
!
!-------------------------------------------------------------------------------
! * 10. Store run mean diagnostic and write restart
!-------------------------------------------------------------------------------
!
! IF (NRANK==NPIO) WRITE(*,*) '10. Store run mean diagnostic and write restart'
IF (NRANK==NPIO) THEN
  IF (GXIOS) THEN
    CALL WRITE_TRIP(NLISTING,'dummy.nc','areacellr',YTRIP_CUR%TPG%GMASK,YTRIP_CUR%TPG%XAREA,OXIOS=.TRUE.)
  ELSEIF(LWR_DIAG)THEN
    CALL TRIP_DIAG_RUN(YTRIP_CUR%TPDG, YTRIP_CUR%TPG, &
                       NLISTING,ILON,ILAT,ILAKE_NUM,ZRUNTIME)
  ENDIF
ENDIF
!
IF(LRESTART)THEN
  CALL TRIP_RESTART(YTRIP_CUR%TP, YTRIP_CUR%TPG, YTRIP_CUR%TPLK, YTRIP_CUR%TPST, &
                    NLISTING,IYEAR,IMONTH,IDAY,ZTIME,ILON,ILAT,ILAKE_NUM         )
ENDIF
!
! --------------------------------------------------------------------------------------
! * 11. End of run
! --------------------------------------------------------------------------------------
!
CLOSE(NLISTING)
!
IF (NRANK==NPIO) THEN
  WRITE(*,*) ' '
  WRITE(*,*) '    ------------------------------'
  WRITE(*,*) '    | TRIP MASTER ENDS CORRECTLY |'
  WRITE(*,*) '    ------------------------------'
  WRITE(*,*) ' '
ENDIF
!
CALL TRIP_DEALLO_LIST
!
IF (LHOOK) CALL DR_HOOK('TRIP_MASTER',1,ZHOOK_HANDLE)
!
! --------------------------------------------------------------------------------------
! * 12. MPI and OASIS must be finalized after the last DR_HOOK call
! --------------------------------------------------------------------------------------
!
CALL TRIP_OASIS_END(GOASIS,GXIOS)
#ifdef SFX_MPI
IF(.NOT.GOASIS) CALL MPI_FINALIZE(IERR)
#endif
!
!-------------------------------------------------------------------------------
END PROGRAM TRIP_MASTER
