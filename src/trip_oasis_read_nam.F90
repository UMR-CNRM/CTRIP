!#########
SUBROUTINE TRIP_OASIS_READ_NAM(KLISTING,PRUNTIME)
!#######################################################
!
!!****  *TRIP_OASIS_READ_NAM* - routine to read the configuration for TRIP-OASIS coupling
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
!!      Original    05/2008 
!!      B. Decharme 10/2016  bug surface/groundwater coupling
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODN_TRIP_RUN, ONLY : XTSTEP_RUN
!
USE MODN_TRIP, ONLY : LFLOOD, CGROUNDW
!
USE MODN_TRIP_OASIS
USE MODD_TRIP_OASIS, ONLY : LCPL_LAND, LCPL_SEA,     &
                            LCPL_GW, LCPL_CALVING,   &
                            LCPL_FLOOD, LCPL_CALVSEA
!
USE MODI_TRIP_POSNAM
USE MODI_ABORT_TRIP
USE MODI_OPEN_TRIP_NAMELIST
USE MODI_CLOSE_TRIP_NAMELIST
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, INTENT(IN)           :: KLISTING
REAL,    INTENT(IN), OPTIONAL :: PRUNTIME
!
!
!*       0.2   Declarations of local parameter
!              -------------------------------
!
INTEGER,          PARAMETER :: KIN   = 1
INTEGER,          PARAMETER :: KOUT  = 0
 CHARACTER(LEN=5), PARAMETER :: YLAND = 'land'
 CHARACTER(LEN=5), PARAMETER :: YSEA  = 'ocean'
!
!
!*       0.3   Declarations of local variables
!              -------------------------------
!
LOGICAL           :: GFOUND         ! Return code when searching namelist
INTEGER           :: INAM           ! logical unit of namelist file
 CHARACTER(LEN=20) :: YKEY
 CHARACTER(LEN=50) :: YCOMMENT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('TRIP_OASIS_READ_NAM',0,ZHOOK_HANDLE)
!
!
!*       1.     Read namelists and check status :
!               --------------------------------
!
LCPL_LAND    = .FALSE.
LCPL_CALVING = .FALSE.
LCPL_GW      = .FALSE.
LCPL_FLOOD   = .FALSE.
LCPL_SEA     = .FALSE.
LCPL_CALVSEA = .FALSE.
!
 CALL OPEN_TRIP_NAMELIST(INAM)
!
 CALL TRIP_POSNAM(INAM,'NAM_TRIP_LAND_CPL',GFOUND,KLISTING)
!
IF (GFOUND) THEN
   READ(UNIT=INAM,NML=NAM_TRIP_LAND_CPL)
ELSE
   WRITE(KLISTING,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
   WRITE(KLISTING,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
   WRITE(KLISTING,*)'NAM_TRIP_LAND_CPL not found : TRIP not coupled with land surface'
   WRITE(KLISTING,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
   WRITE(KLISTING,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
ENDIF
!
 CALL TRIP_POSNAM(INAM,'NAM_TRIP_SEA_CPL',GFOUND,KLISTING)
!
IF (GFOUND) THEN
   READ(UNIT=INAM,NML=NAM_TRIP_SEA_CPL)
ELSE
   WRITE(KLISTING,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
   WRITE(KLISTING,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
   WRITE(KLISTING,*)'NAM_TRIP_SEA_CPL not found : TRIP not coupled with oceanic model'
   WRITE(KLISTING,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
   WRITE(KLISTING,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
ENDIF
!
 CALL CLOSE_TRIP_NAMELIST(INAM)
!
IF(XTSTEP_CPL_LAND>0.0)LCPL_LAND=.TRUE.
IF(XTSTEP_CPL_SEA >0.0)LCPL_SEA =.TRUE.
!
IF(.NOT.LCPL_LAND.AND..NOT.LCPL_SEA)THEN
  CALL ABORT_TRIP('TRIP_OASIS_READ_NAM: OASIS USED BUT NAMELIST NOT FOUND')
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       2.     Check status for required field and time step consistency
!               ---------------------------------------------------------
!
! * Land surface variables for Trip - Oasis coupling
!
IF(LCPL_LAND)THEN
!
  IF(PRESENT(PRUNTIME))THEN
    IF(XTSTEP_RUN>XTSTEP_CPL_LAND)THEN
       XTSTEP_RUN=XTSTEP_CPL_LAND
       WRITE(KLISTING,*)'! XTSTEP_RUN is superiror to  XTSTEP_CPL_LAND     !'     
       WRITE(KLISTING,*)'! Applied solution : XTSTEP_RUN = XTSTEP_CPL_LAND !'     
    ELSEIF(MOD(XTSTEP_CPL_LAND,XTSTEP_RUN)/=0.)THEN
       WRITE(KLISTING,*)'! XTSTEP_RUN and XTSTEP_CPL_LAND are not good !!!'     
       WRITE(KLISTING,*)'! XTSTEP_RUN =',XTSTEP_RUN,'XTSTEP_CPL_LAND =',XTSTEP_CPL_LAND
       CALL ABORT_TRIP('TRIP_OASIS_READ_NAM: XTSTEP_RUN and XTSTEP_CPL_LAND are not good !!!')
    ENDIF
  ENDIF
!
! Land Input variable
!
  YKEY  ='CRUNOFF'
  YCOMMENT='Surface runoff'
  CALL CHECK_TRIP_FIELD(CRUNOFF,YKEY,YCOMMENT,YLAND,KIN)
!
  YKEY  ='CDRAIN'
  YCOMMENT='Deep drainage'
  CALL CHECK_TRIP_FIELD(CDRAIN,YKEY,YCOMMENT,YLAND,KIN)
!
! Particular case due to calving case
!
  IF(LEN_TRIM(CCALVING)>0)THEN
!
    LCPL_CALVING = .TRUE.
!
    YKEY  ='CCALVING'
    YCOMMENT='Calving flux from land glacier'
    CALL CHECK_TRIP_FIELD(CCALVING,YKEY,YCOMMENT,YLAND,KIN)
!
  ENDIF
!
! Particular case due to groundwater scheme
!          
  IF(LEN_TRIM(CWTD)>0.OR.LEN_TRIM(CFWTD)>0)THEN
    LCPL_GW = .TRUE.
  ENDIF
!
  IF(LCPL_GW)THEN
!
!   Output variable
!
    YKEY  ='CWTD'
    YCOMMENT='Water table depth'
    CALL CHECK_TRIP_FIELD(CWTD,YKEY,YCOMMENT,YLAND,KOUT)
!
    YKEY  ='CFWTD'
    YCOMMENT='Fraction of WTD to rise'
    CALL CHECK_TRIP_FIELD(CFWTD,YKEY,YCOMMENT,YLAND,KOUT)
!
    IF(CGROUNDW/='DIF')THEN
      WRITE(KLISTING,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      WRITE(KLISTING,*)'!!!   Groundwater/surface coupling is asked   !!!'
      WRITE(KLISTING,*)'!!!   But CGROUNDW /= DIF in NAM_TRIP         !!!'
      WRITE(KLISTING,*)'!!!   Please check your TRIP_OPTIONS.nam      !!!'
      WRITE(KLISTING,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      CALL ABORT_TRIP('TRIP_OASIS_READ_NAM: Groundwater/surface coupling but not groundwater scheme')
    ENDIF
!
  ENDIF
!
! Particular case due to floodplains
!          
  IF(LEN_TRIM(CSRCFLOOD)>0.OR.LEN_TRIM(CFFLOOD)>0.OR.LEN_TRIM(CPIFLOOD)>0)THEN
    LCPL_FLOOD = .TRUE.
  ENDIF
!
  IF(LCPL_FLOOD)THEN
!
!   Input variable
!
    YKEY  ='CSRCFLOOD'
    YCOMMENT='flood freshwater flux'
    CALL CHECK_TRIP_FIELD(CSRCFLOOD,YKEY,YCOMMENT,YLAND,KIN)
!
!   Output variable
!
    YKEY  ='CFFLOOD'
    YCOMMENT='Flood fraction'
    CALL CHECK_TRIP_FIELD(CFFLOOD,YKEY,YCOMMENT,YLAND,KOUT)
!
    YKEY  ='CPIFLOOD'
    YCOMMENT='Flood potential infiltration'
    CALL CHECK_TRIP_FIELD(CPIFLOOD,YKEY,YCOMMENT,YLAND,KOUT)
!
    IF(.NOT.LFLOOD)THEN
      WRITE(KLISTING,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      WRITE(KLISTING,*)'!!!   Floodplains coupling with LSM is asked   !!!'
      WRITE(KLISTING,*)'!!!   But LFLOOD = FALSE in NAM_TRIP           !!!'
      WRITE(KLISTING,*)'!!!   Please check your TRIP_OPTIONS.nam       !!!'
      WRITE(KLISTING,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      CALL ABORT_TRIP('TRIP_OASIS_READ_NAM: Floodplains coupling but not Floodplains scheme')
    ENDIF 
!
  ENDIF
!
ENDIF
!
IF(PRESENT(PRUNTIME))THEN
  IF(MOD(PRUNTIME,XTSTEP_RUN)/=0.)THEN
    WRITE(KLISTING,*)'! MOD(PRUNTIME,XTSTEP_RUN)/=0 !!!'     
    WRITE(KLISTING,*)'! XTSTEP_RUN must be a multiple of $RUNTIME in oasis namcouple !!!' 
    WRITE(KLISTING,*)'XTSTEP_RUN =',XTSTEP_RUN,'$RUNTIME =',PRUNTIME
    CALL ABORT_TRIP('TRIP_OASIS_READ_NAM: XTSTEP_RUN must be a multiple of $RUNTIME in oasis namcouple !!!')
  ENDIF
ENDIF
!
! * Sea variables for Trip - Oasis coupling 
!
IF(LCPL_SEA)THEN
!
  IF(LCPL_LAND.AND.MOD(XTSTEP_CPL_SEA,XTSTEP_CPL_LAND)/=0.)THEN
    WRITE(KLISTING,*)'! XTSTEP_CPL_LAND and XTSTEP_CPL_SEA are not good !!!'     
    WRITE(KLISTING,*)'! XTSTEP_CPL_LAND =',XTSTEP_CPL_LAND,'XTSTEP_CPL_SEA =',XTSTEP_CPL_SEA
    IF(XTSTEP_CPL_LAND>XTSTEP_CPL_SEA) &
    WRITE(KLISTING,*)'! XTSTEP_CPL_LAND is superiror to XTSTEP_CPL_SEA     !'     
    CALL ABORT_TRIP('TRIP_OASIS_READ_NAM: XTSTEP_CPL_LAND and XTSTEP_CPL_SEA are not good !!!')
  ENDIF
!
  IF(PRESENT(PRUNTIME))THEN
    IF(MOD(XTSTEP_CPL_SEA,XTSTEP_RUN)/=0.)THEN
      WRITE(KLISTING,*)'! XTSTEP_RUN and XTSTEP_CPL_SEA are not good !!!'     
      WRITE(KLISTING,*)'! XTSTEP_RUN =',XTSTEP_RUN,'XTSTEP_CPL_SEA =',XTSTEP_CPL_SEA
      IF(XTSTEP_RUN>XTSTEP_CPL_SEA) &
      WRITE(KLISTING,*)'! XTSTEP_RUN is superiror to  XTSTEP_CPL_SEA     !'     
      CALL ABORT_TRIP('TRIP_OASIS_READ_NAM: XTSTEP_RUN and XTSTEP_CPL_SEA are not good !!!')
    ENDIF
  ENDIF
!
! Sea Output variables
!
  YKEY  ='CRIVDIS'
  YCOMMENT='River discharge'
  CALL CHECK_TRIP_FIELD(CRIVDIS,YKEY,YCOMMENT,YSEA,KOUT)
!
! Particular case due to calving case
!
  IF(LEN_TRIM(CCALVGRE)>0.OR.LEN_TRIM(CCALVANT)>0)THEN
    LCPL_CALVSEA=.TRUE.
  ENDIF
!
  IF(LCPL_CALVSEA)THEN
!
!   Output variable
!
    YKEY  ='CCALVGRE'
    YCOMMENT='Calving flux over greenland'
    CALL CHECK_TRIP_FIELD(CCALVGRE,YKEY,YCOMMENT,YSEA,KOUT)
!
    YKEY  ='CCALVANT'
    YCOMMENT='Calving flux over antarctica'
    CALL CHECK_TRIP_FIELD(CCALVANT,YKEY,YCOMMENT,YSEA,KOUT)
!
  ENDIF
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP_OASIS_READ_NAM',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
 CONTAINS
!-------------------------------------------------------------------------------
!
SUBROUTINE CHECK_TRIP_FIELD(HFIELD,HKEY,HCOMMENT,HTYP,KID)
!
IMPLICIT NONE
!
 CHARACTER(LEN=*), INTENT(IN) :: HFIELD
 CHARACTER(LEN=*), INTENT(IN) :: HKEY
 CHARACTER(LEN=*), INTENT(IN) :: HCOMMENT
 CHARACTER(LEN=*), INTENT(IN) :: HTYP
INTEGER,          INTENT(IN) :: KID
!
 CHARACTER(LEN=20)  :: YWORK
 CHARACTER(LEN=20)  :: YNAMELIST
 CHARACTER(LEN=128) :: YCOMMENT1
 CHARACTER(LEN=128) :: YCOMMENT2
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('TRIP_OASIS_READ_NAM:CHECK_TRIP_FIELD',0,ZHOOK_HANDLE)
!
IF(LEN_TRIM(HFIELD)==0)THEN
!
  IF(KID==0)THEN
    YWORK=TRIM(HTYP)//' - TRIP'
  ELSE
    YWORK='TRIP - '//TRIM(HTYP)
  ENDIF
!
  SELECT CASE (HTYP)
     CASE('land')
          YNAMELIST='NAM_TRIP_LAND_CPL'
     CASE('ocean')
          YNAMELIST='NAM_TRIP_SEA_CPL'
     CASE DEFAULT
          CALL ABORT_TRIP('TRIP_OASIS_READ_NAM: TYPE NOT SUPPORTED OR IMPLEMENTD : '//TRIM(HTYP))               
  END SELECT
!
  YCOMMENT1= 'TRIP_OASIS_READ_NAM: '//TRIM(HCOMMENT)//' is required by '//TRIM(YWORK)//' coupling'
  YCOMMENT2= 'TRIP_OASIS_READ_NAM: Namelist key '//TRIM(HKEY)//' is not in '//TRIM(YNAMELIST)
!
  WRITE(KLISTING,*)TRIM(YCOMMENT1)
  WRITE(KLISTING,*)TRIM(YCOMMENT2)
  CALL ABORT_TRIP(YCOMMENT1)
!  
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP_OASIS_READ_NAM:CHECK_TRIP_FIELD',1,ZHOOK_HANDLE)
!
END SUBROUTINE CHECK_TRIP_FIELD
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE TRIP_OASIS_READ_NAM
