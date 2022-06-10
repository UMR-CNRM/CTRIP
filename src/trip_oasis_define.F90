!#########
SUBROUTINE TRIP_OASIS_DEFINE(KLISTING,KLON,KLAT)
!###############################################
!
!!****  *TRIP_OASIS_DEFINE* - Definitions for exchange of coupling fields
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
USE MODN_TRIP_OASIS
USE MODD_TRIP_OASIS
!
USE MODD_TRIP_MPI
!
USE MODE_TRIP_GRID
!
USE MODI_ABORT_TRIP
USE MODI_GET_LONLAT_TRIP
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
INTEGER, INTENT(IN)    :: KLISTING
INTEGER, INTENT(IN)    :: KLON
INTEGER, INTENT(IN)    :: KLAT
!
!*       0.2   Declarations of local parameter
!              -------------------------------
!
INTEGER,               PARAMETER  :: IG_PARSIZE   = 3       ! Size of array decomposition
INTEGER, DIMENSION(2), PARAMETER  :: IVAR_NODIMS  = (/1,1/) ! rank and number of bundles in coupling field
!
!*       0.3   Declarations of local variables
!              -------------------------------
!
INTEGER, DIMENSION(IG_PARSIZE) :: IPARAL      ! Decomposition for each proc
INTEGER, DIMENSION(2)          :: IVAR_SHAPE  ! indexes for the coupling field local dimension
!
INTEGER                        :: IPART_ID ! Local partition ID
INTEGER                        :: IERR     ! Error info
!
LOGICAL                        :: GFOUND   ! Return code when searching namelist
INTEGER                        :: INAM     ! logical unit of namelist file
!
INTEGER                        :: JLON, JLAT, JC, IFLAG
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_OASIS_DEFINE',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
#ifdef CPLOASIS
!-------------------------------------------------------------------------------
!
!*       1.     Define parallel partitions:
!               ---------------------------
!
! Only proc NPIO sends/receives fields to/from SURFEX
!
IPARAL(:) = 0
!
IF(NRANK==NPIO) THEN
  IPARAL(CLIM_STRATEGY) = CLIM_SERIAL
  IPARAL(CLIM_OFFSET  ) = 0
  IPARAL(CLIM_LENGTH  ) = KLON*KLAT
ENDIF
!
CALL OASIS_DEF_PARTITION(IPART_ID,IPARAL(:),IERR)
!
IF(IERR/=OASIS_OK)THEN
  WRITE(KLISTING,*)'TRIP_OASIS_DEFINE: OASIS def partition problem, err = ',IERR
  CALL ABORT_TRIP('TRIP_OASIS_DEFINE: OASIS def partition problem')
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       2.     Coupling fields shape :
!               -----------------------
!
IVAR_SHAPE = (/1,KLON*KLAT/)
!
!-------------------------------------------------------------------------------
!
!*       3.     Sea variables for Trip - Oasis coupling :
!               -----------------------------------------
!
IF(LCPL_SEA)THEN
!
! Output river discharge to ocean
  CALL OASIS_DEF_VAR(NRIVDIS_ID,CRIVDIS,IPART_ID,IVAR_NODIMS,OASIS_OUT,IVAR_SHAPE,OASIS_DOUBLE,IERR)
  IF(IERR/=OASIS_OK) CALL ABORT_TRIP('TRIP_OASIS_DEFINE: OASIS def var problem for River discharge to ocean')
!
! Particular case for Calving fluxes
!
  IF(LCPL_CALVSEA)THEN
!
!   Output calving flux over Greenland
    CALL OASIS_DEF_VAR(NCALVGRE_ID,CCALVGRE,IPART_ID,IVAR_NODIMS,OASIS_OUT,IVAR_SHAPE,OASIS_DOUBLE,IERR)
    IF(IERR/=OASIS_OK) CALL ABORT_TRIP('TRIP_OASIS_DEFINE: OASIS def var problem for Calving flux over Greenland')
!
!   Output calving flux over Antarctica
    CALL OASIS_DEF_VAR(NCALVANT_ID,CCALVANT,IPART_ID,IVAR_NODIMS,OASIS_OUT,IVAR_SHAPE,OASIS_DOUBLE,IERR)
    IF(IERR/=OASIS_OK) CALL ABORT_TRIP('TRIP_OASIS_DEFINE: OASIS def var problem for Calving flux over Antarctica')
!
  ENDIF
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       4.     Land surface variables for trip - Oasis coupling :
!               ----------------------------------------------------
!
IF(LCPL_LAND)THEN
!
! Input surface runoff
  CALL OASIS_DEF_VAR(NRUNOFF_ID,CRUNOFF,IPART_ID,IVAR_NODIMS,OASIS_IN,IVAR_SHAPE,OASIS_DOUBLE,IERR)
  IF(IERR/=OASIS_OK) CALL ABORT_TRIP('TRIP_OASIS_DEFINE: OASIS def var problem for Surface runoff')
!
! Input calving flux
!
  IF(LCPL_CALVING)THEN
    CALL OASIS_DEF_VAR(NCALVING_ID,CCALVING,IPART_ID,IVAR_NODIMS,OASIS_IN,IVAR_SHAPE,OASIS_DOUBLE,IERR)
    IF(IERR/=OASIS_OK) CALL ABORT_TRIP('TRIP_OASIS_DEFINE: OASIS def var problem for calvin flux')
  ENDIF
!
! Inputd deep drainage
  CALL OASIS_DEF_VAR(NDRAIN_ID,CDRAIN,IPART_ID,IVAR_NODIMS,OASIS_IN,IVAR_SHAPE,OASIS_DOUBLE,IERR)
  IF(IERR/=OASIS_OK) CALL ABORT_TRIP('TRIP_OASIS_DEFINE: OASIS def var problem for Deep drainage')
!
! Particular case due to water table depth / surface coupling
!
  IF(LCPL_GW)THEN
!
!   Output Water table depth
    CALL OASIS_DEF_VAR(NWTD_ID,CWTD,IPART_ID,IVAR_NODIMS,OASIS_OUT,IVAR_SHAPE,OASIS_DOUBLE,IERR)
    IF(IERR/=OASIS_OK) CALL ABORT_TRIP('TRIP_OASIS_DEFINE: OASIS def var problem for Water table depth')
!
!   Output grid-cell fraction of WTD to rise
    CALL OASIS_DEF_VAR(NFWTD_ID,CFWTD,IPART_ID,IVAR_NODIMS,OASIS_OUT,IVAR_SHAPE,OASIS_DOUBLE,IERR)
    IF(IERR/=OASIS_OK) CALL ABORT_TRIP('TRIP_OASIS_DEFINE: OASIS def var problem for fraction of WTD to rise')
!
  ENDIF
!
! Particular case due to floodplains coupling
!
  IF(LCPL_FLOOD)THEN
!
!   Iutput Flood freshwater flux
    CALL OASIS_DEF_VAR(NSRCFLOOD_ID,CSRCFLOOD,IPART_ID,IVAR_NODIMS,OASIS_IN,IVAR_SHAPE,OASIS_DOUBLE,IERR)
    IF(IERR/=OASIS_OK) CALL ABORT_TRIP('TRIP_OASIS_DEFINE: OASIS def var problem for Flood precip interception')
!
!   Output floodplains fraction
    CALL OASIS_DEF_VAR(NFFLOOD_ID,CFFLOOD,IPART_ID,IVAR_NODIMS,OASIS_OUT,IVAR_SHAPE,OASIS_DOUBLE,IERR)
    IF(IERR/=OASIS_OK) CALL ABORT_TRIP('TRIP_OASIS_DEFINE: OASIS def var problem for Floodplains fraction')
!
!   Output floodplains potential infiltration
    CALL OASIS_DEF_VAR(NPIFLOOD_ID,CPIFLOOD,IPART_ID,IVAR_NODIMS,OASIS_OUT,IVAR_SHAPE,OASIS_DOUBLE,IERR)
    IF(IERR/=OASIS_OK) CALL ABORT_TRIP('TRIP_OASIS_DEFINE: OASIS def var problem for Floodplains potential infiltration')
!
  ENDIF
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       5.     End of declaration phase:
!               --------------
!
CALL OASIS_ENDDEF(IERR)
!
IF(IERR/=OASIS_OK)THEN
  WRITE(KLISTING,*)'TRIP_OASIS_DEFINE: OASIS enddef problem, err = ',IERR
  CALL ABORT_TRIP('TRIP_OASIS_DEFINE: OASIS enddef problem')
ENDIF
!
!-------------------------------------------------------------------------------
#endif
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_OASIS_DEFINE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE TRIP_OASIS_DEFINE
