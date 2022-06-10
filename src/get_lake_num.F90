! ########################################################################################
  SUBROUTINE GET_LAKE_NUM(KLISTING,HFILE, &
                          KLON,KLAT,PLONMIN,PLONMAX,PLATMIN,PLATMAX,KLAKE_NUM,KLAKE_NUM_G)
!
! ########################################################################################
!!
!!----------------------------------------------------------------------------------------
!!   *GET_LAKE_NUM*
!!
!!   PURPOSE
!!   -------
!!
!!   Compute number of lakes within region of interest
!!
!!
!!   METHOD
!!   ------
!!
!!   AUTHOR
!!   ------
!!   T.Guinaldo     *Meteo-France*
!!
!!   MODIFICATIONS
!!   -------------
!!   Original        08/19
!!
!!----------------------------------------------------------------------------------------
!!*   0.        DECLARATIONS
!!----------------------------------------------------------------------------------------
!
USE MODD_TRIP_PAR, ONLY: XUNDEF
USE MODE_RW_TRIP
USE MODI_READ_DIMLEN
!
USE YOMHOOK      , ONLY: LHOOK, DR_HOOK
USE PARKIND1     , ONLY: JPRB
!
IMPLICIT NONE
!
!*    0.1 declaration of arguments
!
INTEGER,          INTENT(IN)  :: KLISTING
CHARACTER(LEN=*), INTENT(IN)  :: HFILE
INTEGER,          INTENT(IN)  :: KLON
INTEGER,          INTENT(IN)  :: KLAT
REAL,             INTENT(IN)  :: PLONMIN
REAL,             INTENT(IN)  :: PLONMAX
REAL,             INTENT(IN)  :: PLATMIN
REAL,             INTENT(IN)  :: PLATMAX
!
INTEGER,          INTENT(OUT) :: KLAKE_NUM
INTEGER,          INTENT(OUT) :: KLAKE_NUM_G
!
!*    0.2 declaration of local variables
!
CHARACTER(LEN=20) :: YVAR
REAL, DIMENSION(KLON) :: ZLON
REAL, DIMENSION(KLAT) :: ZLAT
REAL, DIMENSION(KLON,KLAT) :: ZLAKE_ID
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASK
INTEGER :: JLON, JLAT, JNUM, IDIMLEN(1)
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_LAKE_NUM',0,ZHOOK_HANDLE)
!
YVAR = 'longitude'
CALL READ_TRIP(KLISTING,HFILE,YVAR,ZLON)
YVAR = 'latitude'
CALL READ_TRIP(KLISTING,HFILE,YVAR,ZLAT)
YVAR = 'LAKE_ID_IN'
CALL READ_TRIP(KLISTING,HFILE,YVAR,ZLAKE_ID)
!
YVAR = 'LAKE_Z'
CALL READ_DIMLEN(KLISTING,HFILE,YVAR,1,IDIMLEN)
KLAKE_NUM_G = IDIMLEN(1)
!
ALLOCATE(IMASK(KLAKE_NUM_G))
IMASK = 0
!
DO JLAT = 1,KLAT
  IF(ZLAT(JLAT)<PLATMIN.OR.ZLAT(JLAT)>PLATMAX) CYCLE
  DO JLON = 1,KLON
    IF(ZLON(JLON)<PLONMIN.OR.ZLON(JLON)>PLONMAX) CYCLE
    IF(ZLAKE_ID(JLON,JLAT)>0.0 .AND. ZLAKE_ID(JLON,JLAT)/=XUNDEF)THEN
      JNUM = INT(ZLAKE_ID(JLON,JLAT))
      IMASK(JNUM) = 1
    ENDIF
  ENDDO
ENDDO
!
KLAKE_NUM = SUM(IMASK)
!
DEALLOCATE(IMASK)
!
IF (LHOOK) CALL DR_HOOK('GET_LAKE_NUM',1,ZHOOK_HANDLE)
!
ENDSUBROUTINE GET_LAKE_NUM
