SUBROUTINE TRIP_OASIS_INIT(OOASIS,OXIOS,KLOCAL_COMM,PRUNTIME,LPREP)
!!
!!
!!     PURPOSE
!!     --------
!!
!!     Initialize jointly coupled mode and XIOS communication
!!
!!
!!     METHOD
!!     ------
!!
!!     If XIOS is used, it will init Oasis
!!     OASIS-MCT interface must be initialized before any DR_HOOK call
!!
!!
!!     EXTERNAL
!!     --------
!!
!!     XIOS library
!!
!!     REFERENCE
!!     ---------
!!
!!     S. Valcke et al., 2013: OASIS-MCT User Guide 
!!     CERFACS, Toulouse, France, 50 pp.
!!     https://verc.enes.org/oasis/oasis-dedicated-user-support-1/documentation/oasis3-mct-user-guide
!!
!!     Y.Meurdesoif, 2015 : XIOS ....
!!
!!     AUTHOR
!!     ------
!!
!!     B. Decharme, CNRM
!!
!!     MODIFICATION
!!     --------------
!!
!!     Original    10/2013
!!     09/2016 - S.Senesi - Add XIOS setup
!      S. Munier   28/03/2020 CTRIP-12D and parallelization
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TRIP_MPI, ONLY : NPIO
!
USE MODN_TRIP_RUN, ONLY : CNAMELIST, CMODEL_NAME
!
#ifdef CPLOASIS
USE MOD_OASIS
#endif
#ifdef WXIOS
USE XIOS
#endif
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
LOGICAL, INTENT(OUT)           :: OOASIS      ! key to use OASIS
LOGICAL, INTENT(OUT)           :: OXIOS       ! key to use XIOS
INTEGER, INTENT(OUT)           :: KLOCAL_COMM ! value of local communicator
REAL   , INTENT(OUT), OPTIONAL :: PRUNTIME    ! total simulated time (s)
LOGICAL, INTENT(IN),  OPTIONAL :: LPREP
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
CHARACTER(LEN=9)   :: YWORD, YTIMERUN
CHARACTER(LEN=1000):: YLINE, YFOUND
INTEGER            :: IERR, IRANK
INTEGER            :: ICOMP_ID
INTEGER            :: ITIMERUN
LOGICAL            :: GFOUND
!
!*       0.3   Declarations of namelist variables
!              ----------------------------------
!
LOGICAL               :: GPREP
!
LOGICAL               :: LOASIS      ! key to use OASIS
LOGICAL               :: LXIOS       ! key to use XIOS
!
NAMELIST/NAM_OASIS/LOASIS,LXIOS,CMODEL_NAME
!
!-------------------------------------------------------------------------------
!
! ATTENTION : Do not introduce DR_HOOK in this routine
!
!*       0.     Initialization:
!               ---------------
!
GPREP = .FALSE.
IF(PRESENT(LPREP)) GPREP = LPREP
!
LOASIS = .FALSE.
LXIOS  = .FALSE.
!
IF(PRESENT(PRUNTIME)) PRUNTIME = 0.0
!
!-------------------------------------------------------------------------------
!
!*       1.     Read namelist:
!               --------------
!
OPEN(UNIT=11,FILE=CNAMELIST,ACTION='READ',FORM="FORMATTED",POSITION="REWIND",STATUS='OLD',IOSTAT=IERR)   
!
IF (IERR /= 0) THEN
  WRITE(*,'(A)' )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(*,'(2A)')'TRIP_OASIS_INIT: TRIP NAMELIST NOT FOUND: ',TRIM(CNAMELIST)
  WRITE(*,'(A)' )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  CALL ABORT
  STOP
ENDIF
!
READ (UNIT=11,NML=NAM_OASIS,IOSTAT=IERR)
!
CLOSE(UNIT=11)
!
IF(GPREP) LXIOS = .FALSE.
!
!-------------------------------------------------------------------------------
!
!*       2.     Put specified param for OASIS coupling
!               --------------------------------------
!
OOASIS   = LOASIS
OXIOS    = LXIOS
!
IF(LOASIS)THEN
#ifndef CPLOASIS
  WRITE(*,'(A)')'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(*,'(A)')'TRIP_OASIS_INIT: When OASIS used, TRIP must be compiled with -DCPLOASIS cpp key'
  WRITE(*,'(A)')'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  CALL ABORT
  STOP 
#endif
ENDIF
!
IF(LXIOS)THEN
#ifndef WXIOS
  WRITE(*,'(A)')'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  WRITE(*,'(A)')'TRIP_OASIS_INIT: When XIOS used, TRIP must be compiled with -DWXIOS cpp key'
  WRITE(*,'(A)')'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  CALL ABORT
  STOP 
#endif
ENDIF
!
IF (LXIOS) THEN
#ifdef WXIOS
  CALL XIOS_INITIALIZE(CMODEL_NAME,RETURN_COMM=KLOCAL_COMM)
#endif
ELSE
!
  IF ( .NOT. LOASIS) THEN
    KLOCAL_COMM=0
    RETURN
  ELSE
!
!-------------------------------------------------------------------------------
#ifdef CPLOASIS
!-------------------------------------------------------------------------------
!
!*       3.     Setup OASIS
!               -----------
!
    CALL OASIS_INIT_COMP(ICOMP_ID,CMODEL_NAME,IERR)
!
    IF (IERR/=OASIS_OK) THEN
      WRITE(*,'(A)'   )'TRIP : Error initializing OASIS'
      WRITE(*,'(A,I4)')'TRIP : Return code from oasis_init_comp : ',IERR
      CALL OASIS_ABORT(ICOMP_ID,CMODEL_NAME,'TRIP_OASIS_INIT: Error initializing OASIS')
      CALL ABORT
      STOP
    ENDIF
!
!-------------------------------------------------------------------------------
!
!*       4.     Get local communicator
!               ----------------------
!
    CALL OASIS_GET_LOCALCOMM(KLOCAL_COMM,IERR)
!
    IF (IERR/=OASIS_OK) THEN
      WRITE(*,'(A)'   )'TRIP : Error getting local communicator from OASIS'
      WRITE(*,'(A,I4)')'TRIP : Return code from oasis_get_local_comm : ',IERR
      CALL OASIS_ABORT(ICOMP_ID,CMODEL_NAME,'TRIP_OASIS_INIT: Error getting local communicator')
      CALL ABORT
      STOP
    ENDIF
!
!-------------------------------------------------------------------------------
#endif
!-------------------------------------------------------------------------------
  ENDIF
ENDIF
!
IRANK = 0
#ifdef SFX_MPI
CALL MPI_COMM_RANK(KLOCAL_COMM,IRANK,IERR)
#endif
IF(IRANK==NPIO)THEN
   WRITE(*,'(A)')'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
   IF (LOASIS) WRITE(*,'(A)')'OASIS used for model : '//TRIM(CMODEL_NAME)
   IF (LXIOS)  WRITE(*,'(A)')'XIOS  used for model : '//TRIM(CMODEL_NAME)
   WRITE(*,'(A)')'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
ENDIF
!
IF (LOASIS) THEN
  !
  !*       5.     Read total simulated time in namcouple
  !               --------------------------------------
  !
  IF(.NOT.PRESENT(PRUNTIME))THEN
    RETURN
  ENDIF
  !
  OPEN (UNIT=11,FILE ='namcouple',STATUS='OLD',FORM ='FORMATTED',POSITION="REWIND",IOSTAT=IERR)
  IF (IERR /= 0) THEN
    WRITE(*,'(A)'   )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    WRITE(*,'(A)'   )'TRIP : OASIS namcouple not found'
    WRITE(*,'(A)'   )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    CALL ABORT
    STOP
  ENDIF
!
  YTIMERUN=' $RUNTIME'
  ITIMERUN=-1
!
  DO WHILE (ITIMERUN==-1)
    READ (UNIT = 11,FMT = '(A9)',IOSTAT=IERR) YWORD
    IF(IERR/=0)THEN
      WRITE(*,'(A)'   )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      WRITE(*,'(A)'   )'TRIP : Problem $RUNTIME empty in namcouple'
      WRITE(*,'(A)'   )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!' 
      CALL ABORT
      STOP           
    ENDIF  
    IF (YWORD==YTIMERUN)THEN
      READ (UNIT = 11,FMT = '(A1000)',IOSTAT=IERR) YLINE
      IF(IERR/=0)THEN
        WRITE(*,'(A)'   )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        WRITE(*,'(A)'   )'TRIP : Problem looking for $RUNTIME in namcouple'
        WRITE(*,'(A)'   )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!' 
        CALL ABORT
        STOP           
      ENDIF
      CALL TRIP_FOUND_TIMERUN (YLINE, YFOUND, 1000, GFOUND)
      IF (GFOUND) THEN
        READ (YFOUND,FMT = '(I100)',IOSTAT=IERR) ITIMERUN
        IF(IERR/=0)THEN
          WRITE(*,'(A)'   )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          WRITE(*,'(A)'   )'TRIP : Problem reading $RUNTIME in namcouple'
          WRITE(*,'(2A)'  )'$RUNTIME = ', TRIM(YFOUND)
          WRITE(*,'(A)'   )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!' 
          CALL ABORT
          STOP
        ENDIF
      ENDIF
    ENDIF
  ENDDO
  CLOSE(11)
!
  PRUNTIME = REAL(ITIMERUN)
!
ENDIF
!
!-------------------------------------------------------------------------------
 CONTAINS
!-------------------------------------------------------------------------------
!
SUBROUTINE TRIP_FOUND_TIMERUN(HIN, HOUT, KLEN, OFOUND)
!
IMPLICIT NONE
!
INTEGER ,          INTENT (IN   ) :: KLEN
 CHARACTER (LEN=*), INTENT (INOUT) :: HIN
 CHARACTER (LEN=*), INTENT (INOUT) :: HOUT
LOGICAL,           INTENT (OUT  ) :: OFOUND
!
!* ---------------------------- Local declarations -------------------
!
 CHARACTER(LEN=1), PARAMETER :: YBLANK = ' '
 CHARACTER(LEN=1), PARAMETER :: YNADA  = '#'

 CHARACTER(LEN=KLEN) :: YLINE
 CHARACTER(LEN=KLEN) :: YWORK
!
INTEGER             :: ILEN
INTEGER             :: IERR
!
!
!*    1. Skip line if it is a comment
!        ----------------------------
!
DO WHILE (HIN(1:1)==YNADA)
   READ (UNIT = 11, FMT = '(A9)',IOSTAT=IERR) YLINE 
   IF(IERR/=0)THEN
       WRITE(*,'(A)'   )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
       WRITE(*,'(A)'   )'TRIP : Problem looking for $RUNTINE line in namcouple'
       WRITE(*,'(A)'   )'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
       CALL ABORT
       STOP
   ENDIF
   HIN(1:KLEN) = YLINE(1:KLEN)
ENDDO
!
!* Fill HOUT with blanks
!
HOUT = YBLANK
!
!* Fill temporary string and remove leading blanks
!
YWORK = ADJUSTL(HIN)
!
IF(LEN_TRIM(YWORK)<=0)THEN
   OFOUND = .FALSE.
   RETURN
ENDIF
!
!* Find the length of this set of characters
!
ILEN = INDEX(YWORK,YBLANK) - 1
!
!* Copy to HOUT
!
HOUT(1:ILEN) = YWORK(1:ILEN)
!
OFOUND = .TRUE.
!
END SUBROUTINE TRIP_FOUND_TIMERUN
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE TRIP_OASIS_INIT
