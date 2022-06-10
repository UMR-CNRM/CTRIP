!!    ##############################################
      SUBROUTINE TRIP_POSNAM(KULNAM,HDNAML,OFOUND,KLISTING)
!!    ##############################################
!!
!!*** *TRIP_POSNAM*
!!
!!    PURPOSE
!!    -------
!     To position namelist file at correct place for reading
!     namelist CDNAML.
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENT
!!    -----------------
!!
!!    REFERENCE
!!    ----------
!!       ECMWF Research Department documentation of the IFS (Hamrud)
!!
!!    AUTHOR
!!    -------
!!    I. Mallet  15/10/01 
!!
!!    MODIFICATIONS
!!    --------------
!!       I. Mallet  15/10/01     adaptation to MesoNH (F90 norm)
!------------------------------------------------------------------------------
!
USE MODI_ABORT_TRIP
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.    DECLARATIONS
!              ------------
!
!*       0.1   Declarations of arguments
!
INTEGER,          INTENT(IN) :: KULNAM
 CHARACTER(LEN=*), INTENT(IN) :: HDNAML
LOGICAL,          INTENT(OUT):: OFOUND
INTEGER, OPTIONAL,INTENT(IN) :: KLISTING
!
!*       0.2   Declarations of local variables
!
 CHARACTER(LEN=120) :: YLINE
 CHARACTER(LEN=1)   :: YLTEST
INTEGER            :: ILEN,ILEY,INDL,IND1,IRET
INTEGER            :: J,JA, JFILE
LOGICAL            :: LLOPENED
!
 CHARACTER(LEN=1),DIMENSION(26) :: YLO=(/'a','b','c','d','e','f','g','h', &
     'i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'/)
 CHARACTER(LEN=1),DIMENSION(26) :: YUP=(/'A','B','C','D','E','F','G','H', &
     'I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'/)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       1.    POSITION FILE
!              -------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_POSNAM',0,ZHOOK_HANDLE)
OFOUND=.FALSE.
ILEN=LEN(HDNAML)
!
!      CONTINUE READING THE FILE, THEN REWIND IF NOT FOUND
DO JFILE=1,2
  search_nam : DO
    YLINE=' '
    READ(UNIT=KULNAM,FMT='(A)',IOSTAT=IRET,END=100) YLINE
!   If file does not exist, most compilers would just create it and jump 
!   to the END label ; but a few of them would report an error:         
    IF (IRET /=0 ) THEN                 
      INQUIRE(KULNAM,OPENED=LLOPENED)
      IF (LLOPENED) THEN
        IF (PRESENT(KLISTING)) &
          WRITE(KLISTING,FMT=*) 'MODE_POS_TRIP : error reading from unit ',&
                               KULNAM,' file ',HDNAML,' line ',YLINE
        CALL ABORT_TRIP('MODE_POS_TRIP: read error in namelist file') 
      ELSE
        EXIT search_nam
      END IF
    ELSE   
!     FIRST SEARCH for "&" IN THE LINE, THEN CORRECT LINE AND TEST :
      INDL=INDEX(YLINE,'&')
      IF (INDL .NE. 0 ) THEN
        ILEY=LEN(YLINE)
        DO J=1,ILEY
          DO JA=1,26
            IF (YLINE(J:J)==YLO(JA)) YLINE(J:J)=YUP(JA) 
          END DO
        END DO
        IND1=INDEX(YLINE,'&'//HDNAML)
        IF(IND1.NE.0) THEN
          YLTEST=YLINE(IND1+ILEN+1:IND1+ILEN+1)
          IF(YLTEST == ' ') THEN
!           NAMELIST FOUND : RETURN
            BACKSPACE(KULNAM)
            OFOUND=.TRUE.
            IF (PRESENT(KLISTING)) WRITE(KLISTING,FMT=*) '-- namelist ',HDNAML,' read'
            IF (LHOOK) CALL DR_HOOK('TRIP_POSNAM',1,ZHOOK_HANDLE)
            RETURN
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDDO search_nam
  100  CONTINUE
  IF(JFILE == 1) REWIND(KULNAM)
ENDDO

BACKSPACE(KULNAM)
! end of file: namelist name not found
IF (PRESENT(KLISTING)) &
WRITE(KLISTING,FMT=*)  &
'-- namelist ',HDNAML,' not found: default values used if required'
IF (LHOOK) CALL DR_HOOK('TRIP_POSNAM',1,ZHOOK_HANDLE)
!------------------------------------------------------------------
END SUBROUTINE TRIP_POSNAM
