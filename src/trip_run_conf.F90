!######
SUBROUTINE TRIP_RUN_CONF(KLISTING,OOASIS,KYEAR,KMONTH,KDAY,PTIME,  &
                         KLON,KLAT,KNB_TSTEP_RUN,PRUNTIME         )
!####################################################################
!
!!****  *TRIP_RUN_CONF* - prepare the dimenssions (xt or xyt) of a run
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
!!      B. decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2008
!!      S. Munier   03/2020    CTRIP-12D and parallelization
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODN_TRIP_RUN, ONLY : CFILE_FRC,CREADFRC,CDRAIN, &
                          CRUNOFF,XTSTEP_RUN
!
USE MODN_TRIP,     ONLY : XTSTEP
!
USE MODD_TRIP_MPI
!
USE MODI_ABORT_TRIP
USE MODI_TRIP_FORCING_CONF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,                INTENT(IN)  :: KLISTING
LOGICAL,                INTENT(IN)  :: OOASIS
INTEGER,                INTENT(IN)  :: KYEAR
INTEGER,                INTENT(IN)  :: KMONTH
INTEGER,                INTENT(IN)  :: KDAY
REAL,                   INTENT(IN)  :: PTIME
INTEGER,                INTENT(IN)  :: KLON
INTEGER,                INTENT(IN)  :: KLAT
!
INTEGER,                INTENT(OUT)   :: KNB_TSTEP_RUN
REAL,                   INTENT(INOUT) :: PRUNTIME
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! Read the configuration of the run
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_RUN_CONF',0,ZHOOK_HANDLE)
!
IF(OOASIS)THEN
!
  KNB_TSTEP_RUN = INT(PRUNTIME/XTSTEP_RUN)
!
ELSE
!
  ! Forcing file
  WRITE(KLISTING,*) 'TRIP forcing file: ',CFILE_FRC
  CALL TRIP_FORCING_CONF(KLISTING,KYEAR,KMONTH,KDAY,PTIME,       &
                         CFILE_FRC,CREADFRC,CDRAIN,CRUNOFF,KLON, &
                         KLAT,XTSTEP_RUN,KNB_TSTEP_RUN,PRUNTIME )
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP_RUN_CONF',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE TRIP_RUN_CONF
