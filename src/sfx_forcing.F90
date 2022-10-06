!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!
PROGRAM SFX_FORCING
!
USE MODD_OFF_SURFEX_n
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE MODD_SURFEX_MPI
USE MODD_SFX_OASIS, ONLY : LOASIS, LCPL_LAND, LCPL_GW, NRUNOFF_ID, NDRAIN_ID
USE MODD_IO_SURF_NC, ONLY : NID_NC
!
USE MODN_ISBA_n, ONLY : NAM_ISBAn, LGLACIER
USE MODN_IO_OFFLINE, ONLY : CNAMELIST, NAM_IO_OFFLINE, CSURF_FILETYPE, &
                            XTSTEP_OUTPUT, XTSTEP_SURF, YALG_MPI
USE MODN_SFX_OASIS
!
USE MODE_POS_SURF, ONLY : POSNAM
!
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_READ_SURF
!
USE MODI_SFX_OASIS_INIT
USE MODI_SFX_OASIS_READ_NAM
USE MODI_SFX_OASIS_DEF_OL
USE MODI_SFX_OASIS_END
!
USE MODI_ABOR1_SFX
!
USE MOD_OASIS
USE NETCDF
!
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE PARKIND1, ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE 'mpif.h'
#endif
!
CHARACTER(LEN=28)    :: YLISTING = 'LISTING_SFX_FORCING         '
!
CHARACTER(LEN=100)   :: YNAME_FILE_DRAIN = 'DRAINC_ISBA.BIN'
CHARACTER(LEN=100)   :: YNAME_FILE_RUNOFF = 'RUNOFFC_ISBA.BIN'
!
REAL,         DIMENSION(:),   ALLOCATABLE :: ZQI, ZQR
REAL(KIND=4), DIMENSION(:),   ALLOCATABLE :: ZQI_PAS, ZQR_PAS
REAL,         DIMENSION(:),   ALLOCATABLE :: ZQI_OLD, ZQR_OLD
REAL,         DIMENSION(:,:), ALLOCATABLE :: ZWRITE
!
INTEGER              :: INB_PNT          ! Number of points
INTEGER              :: INB_TSTEP         ! Number of time steps
!
CHARACTER(LEN=20)    :: YCUMUL = 'CUMUL' ! 'CUMUL' or 'INSTANTANEOUS'
REAL                 :: ZTIMEC
INTEGER              :: JSTEP
INTEGER              :: IDATE
CHARACTER(LEN=100)   :: YLINE
CHARACTER(LEN=50)    :: YCOMMENT
INTEGER              :: ILUOUT = 10
INTEGER              :: ILUNAM, ISIZE, IERR
LOGICAL              :: GFOUND
!
REAL(KIND=JPRB)      :: ZHOOK_HANDLE
!
! --------------------------------------------------------------------------------------
!
!*     0.1.   MPI, OASIS, XIOS and dr_hook initializations
!
LHOOK = .FALSE.
CALL SFX_OASIS_INIT(CNAMELIST,NCOMM)
!
CALL MPI_COMM_SIZE(NCOMM,NPROC,IERR)
CALL MPI_COMM_RANK(NCOMM,NRANK,IERR)
!
IF(.NOT.LOASIS) CALL ABOR1_SFX('SFX_FORCING: LOASIS MUST BE .TRUE. IN OPTIONS.nam')
IF(NPROC>1) CALL ABOR1_SFX('SFX_FORCING: SHOULD USE ONLY ONE PROC')
!
IF (LHOOK) CALL DR_HOOK('SFX_FORCING',0,ZHOOK_HANDLE)
!
OPEN(UNIT=ILUOUT,FILE=ADJUSTL(ADJUSTR(YLISTING)//'.txt'),FORM='FORMATTED',ACTION='WRITE')
!
!-------------------------------------------------------------------------------
! * 1. Read options
!-------------------------------------------------------------------------------
!
! WRITE(*,*) '1. Read options'
!
! Get sfx output options
CALL OPEN_NAMELIST('ASCII ',ILUNAM,CNAMELIST)
CALL POSNAM(ILUNAM,'NAM_IO_OFFLINE',GFOUND,ILUOUT)
IF (GFOUND) READ (UNIT=ILUNAM,NML=NAM_IO_OFFLINE)
CALL POSNAM(ILUNAM,'NAM_ISBAn',GFOUND,ILUOUT)
IF (GFOUND) READ (UNIT=ILUNAM,NML=NAM_ISBAn)
CALL CLOSE_NAMELIST('ASCII ',ILUNAM)
!
! Get sfx coupling options
CALL SFX_OASIS_READ_NAM('ASCII ',XTSTEP_OUTPUT)
!
! Coupling time step must be SURFEX output time step
IF (XTSTEP_CPL_LAND /= XTSTEP_OUTPUT) THEN
  CALL ABOR1_SFX('SFX_FORCING: COUPLING TIME STEP MUST EQUAL OUTPUT TIMESTEP')
ENDIF
!
! --------------------------------------------------------------------------------------
! * 2. SFX_FORCING initializations
! --------------------------------------------------------------------------------------
!
! WRITE(*,*) '2. SFX_FORCING initializations'
!
! Allocations of Surfex Types
CALL SURFEX_ALLOC_LIST(1)
YSC => YSURF_LIST(1)
!
! Get number of points and some SURFEX options
IF (CSURF_FILETYPE=='NC    ') THEN
  IERR = NF90_OPEN('PGD.nc',NF90_NOWRITE,NID_NC)
  CALL READ_SURF(CSURF_FILETYPE,'DIM_FULL',YSC%U%NDIM_FULL,IERR)
  CALL READ_SURF(CSURF_FILETYPE,'ISBA',YSC%IM%O%CISBA,IERR)
  IERR = NF90_CLOSE(NID_NC)
ELSEIF (CSURF_FILETYPE=='ASCII ') THEN
  OPEN(UNIT=20,FILE="PGD.txt",ACTION="READ")
  DO
    READ(20,"(a)") YLINE
    IF (YLINE(10:17)=="DIM_FULL") THEN
      READ(20,"(a)") YLINE
      READ(20,"(I12)") YSC%U%NDIM_FULL
      EXIT
    ENDIF
  ENDDO
  CLOSE(20)
ENDIF
YSC%IM%O%LGLACIER = LGLACIER
YSC%IM%O%LWTD = (LCPL_LAND.AND.LCPL_GW)
INB_PNT = YSC%U%NDIM_FULL
!
! Get number of time steps from file size
INQUIRE(FILE=YNAME_FILE_DRAIN, SIZE=ISIZE)
INB_TSTEP = ISIZE/INB_PNT/4
!
! SURFEX - OASIS  grid, partitions and local field definitions
ALLOCATE(NINDEX(INB_PNT))
NINDEX(:) = NRANK
CALL SFX_OASIS_DEF_OL(YSC%IM%O, YSC%U, 'ASCII ', YALG_MPI)
!
! Initialize runoff and drainage variables
ALLOCATE(ZQI(INB_PNT))
ALLOCATE(ZQR(INB_PNT))
ALLOCATE(ZQI_PAS(INB_PNT))
ALLOCATE(ZQR_PAS(INB_PNT))
ALLOCATE(ZQI_OLD(INB_PNT))
ALLOCATE(ZQR_OLD(INB_PNT))
ALLOCATE(ZWRITE(INB_PNT,1))
ZQI(:) = 0.0
ZQR(:) = 0.0
ZQI_PAS(:) = 0.0
ZQR_PAS(:) = 0.0
ZQI_OLD(:) = 0.0
ZQR_OLD(:) = 0.0
!
! --------------------------------------------------------------------------------------
! * 3. Open forcing files
! --------------------------------------------------------------------------------------
!
! WRITE(*,*) '3. Open forcing files'
!
OPEN(50, FILE=YNAME_FILE_DRAIN, ACCESS='DIRECT',&
    FORM='UNFORMATTED', STATUS='OLD', CONVERT='BIG_ENDIAN', RECL=4*INB_PNT)
OPEN(51, FILE=YNAME_FILE_RUNOFF, ACCESS='DIRECT',&
    FORM='UNFORMATTED', STATUS='OLD', CONVERT='BIG_ENDIAN', RECL=4*INB_PNT)
!
WRITE(ILUOUT,*) 'Total number of time steps:',INB_TSTEP
!
! --------------------------------------------------------------------------------------
! * 4. Temporal loop
! --------------------------------------------------------------------------------------
!
! WRITE(*,*) '4. Temporal loop'
!
ZTIMEC = 0.0
!
DO JSTEP = 1,INB_TSTEP
    !
    ZTIMEC = ZTIMEC+XTSTEP_OUTPUT
    !
    ! Read runoff and drainage from forcing file
    !
    READ(50, REC=JSTEP) ZQI_PAS(:)
    READ(51, REC=JSTEP) ZQR_PAS(:)
    IF (YCUMUL == 'CUMUL') THEN
        ZQI(:) = ZQI_PAS(:) - ZQI_OLD(:)
        ZQR(:) = ZQR_PAS(:) - ZQR_OLD(:)
        ZQI_OLD(:) = ZQI_PAS(:)
        ZQR_OLD(:) = ZQR_PAS(:)
    ELSE
        ZQI(:) = ZQI_PAS(:)
        ZQR(:) = ZQR_PAS(:)
    ENDIF
    !
    ! Send runoff and drainage via OASIS
    !
    IDATE = INT(ZTIMEC-XTSTEP_SURF)
    !
    YCOMMENT='Surface runoff over land'
    ZWRITE(:,:) = XUNDEF
    WHERE(ZQR(:)/=XUNDEF) ZWRITE(:,1) = ZQR(:)/XTSTEP_CPL_LAND
    CALL OASIS_PUT(NRUNOFF_ID,IDATE,ZWRITE(:,:),IERR)
    !
    YCOMMENT='Deep drainage over land'
    ZWRITE(:,:) = XUNDEF
    WHERE(ZQI(:)/=XUNDEF) ZWRITE(:,1) = ZQI(:)/XTSTEP_CPL_LAND
    CALL OASIS_PUT(NDRAIN_ID,IDATE,ZWRITE(:,:),IERR)
    !
ENDDO
!
CLOSE(50)
CLOSE(51)
!
! --------------------------------------------------------------------------------------
! * 5. Finalizing
! --------------------------------------------------------------------------------------
!
DEALLOCATE(ZQI)
DEALLOCATE(ZQR)
DEALLOCATE(ZQI_PAS)
DEALLOCATE(ZQR_PAS)
DEALLOCATE(ZQI_OLD)
DEALLOCATE(ZQR_OLD)
DEALLOCATE(ZWRITE)
DEALLOCATE(NINDEX)
!
WRITE(ILUOUT,*) ' '
WRITE(ILUOUT,*) '    -----------------------------'
WRITE(ILUOUT,*) '    | SFX_FORCING ENDS CORRECTLY |'
WRITE(ILUOUT,*) '    -----------------------------'
WRITE(ILUOUT,*) ' '
CLOSE(ILUOUT)
WRITE(*,*) ' '
WRITE(*,*) '    -----------------------------'
WRITE(*,*) '    | SFX_FORCING ENDS CORRECTLY |'
WRITE(*,*) '    -----------------------------'
WRITE(*,*) ' '
!
IF (LHOOK) CALL DR_HOOK('SFX_FORCING',1,ZHOOK_HANDLE)
!
! * MPI and OASIS must be finalized after the last DR_HOOK call
!
CALL SFX_OASIS_END
!
END PROGRAM SFX_FORCING
