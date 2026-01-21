MODULE MODD_TRIP_ASSIM
!
IMPLICIT NONE
!
INTEGER :: NPROC_ASSIM = 1
INTEGER :: NRANK_ASSIM = 0
INTEGER :: NCOMM_ASSIM = -1
INTEGER :: NPIO_ASSIM  = 0
!
CHARACTER(LEN=256)                  :: COBS_FILE = 'OBS.nc' ! Name of observation file
INTEGER                             :: NOBS                 ! Number of obervation times
INTEGER                             :: NENS                 ! Number of ensemble members
CHARACTER(LEN=256)                  :: COBS_Q_NAME = 'QDIS'
CHARACTER(LEN=256)                  :: COBS_Q_ERR_NAME = 'QDIS_ERR'
CHARACTER(LEN=256)                  :: COBS_H_NAME = 'HSTREAM'
CHARACTER(LEN=256)                  :: COBS_H_ERR_NAME = 'HSTREAM_ERR'
!
END MODULE MODD_TRIP_ASSIM

