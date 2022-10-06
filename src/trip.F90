!TRP_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!TRP_LIC This is part of the CTRIP software governed by the CeCILL-C licence
!TRP_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!TRP_LIC for details. version 1.
!     #########
      SUBROUTINE TRIP(TPST,KLISTING,                                 &
                      OPRINT,KTRIP,KTSTEP,KTSTEP_END,PTSTEP,         &
                      PDRAIN,PRUNOFF,PSOURCE,PHS,PVEL,               &
                      PSOUT,PSIN,PGOUT,PQFR,PQRF,PVFIN,PVFOUT,PHSF,  &
                      PGSTO_ALL,PGSTO2_ALL,PGIN_ALL,PGOUT_ALL        )
!     ###################################################################
!
!****  *TRIP*
!
!    PURPOSE
!    -------
!
!     TRIP river routing and Floodplains schemes.
!
!
!    REFERENCE
!    ---------
!
!    AUTHOR
!    ------
!      B. Decharme
!
!    MODIFICATIONS
!    -------------
!      Original    01/02/05
!      Modif.      28/05/08
!      S. Munier   28/03/2020 CTRIP-12D and parallelization
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_STATE, ONLY : TRIP_STATE_t
!
USE MODD_TRIP_PAR,   ONLY : XSEA,XYEAR
!
USE MODN_TRIP,       ONLY : CGROUNDW,LFLOOD
!
USE MODI_TRIP_GW_BUFFER_TANK
USE MODI_TRIP_SURFACE_WATER
USE MODI_TRIP_SURFACE_FLOOD
!
USE MODI_TRIP_UPDATE_AND_CONSERV
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
! !*      0.1    declarations of arguments
!
TYPE(TRIP_STATE_t),      INTENT(INOUT) :: TPST
!
INTEGER,                 INTENT(IN)    :: KLISTING
!
LOGICAL,                 INTENT(IN)    :: OPRINT   ! Printable budget key
INTEGER,                 INTENT(IN)    :: KTRIP
INTEGER,                 INTENT(IN)    :: KTSTEP
INTEGER,                 INTENT(IN)    :: KTSTEP_END
!
REAL,                    INTENT(IN)    :: PTSTEP   ! Trip timestep [s]
!
REAL,    DIMENSION(:),   INTENT(IN)    :: PDRAIN       ! Subsurface runoff to river [kg/s]
REAL,    DIMENSION(:),   INTENT(IN)    :: PRUNOFF      ! Surface runoff to river    [kg/s]
REAL,    DIMENSION(:),   INTENT(IN)    :: PSOURCE      ! precip-infiltration-evaporation [kg/s]
!
REAL,    DIMENSION(:),   INTENT(INOUT) :: PHS        ! River or lake height [m]
REAL,    DIMENSION(:),   INTENT(IN)    :: PVEL       ! River channel velocity  [m/s]
!
REAL,    DIMENSION(:),   INTENT(OUT)   :: PSOUT ! Outflow from the surface river or lake reservoir [kg/s]
REAL,    DIMENSION(:),   INTENT(OUT)   :: PSIN  ! Inflow to the surface river or lake reservoir [kg/s]
REAL,    DIMENSION(:),   INTENT(INOUT) :: PGOUT ! ground water outflow        [kg/s]
REAL,    DIMENSION(:),   INTENT(OUT)   :: PQFR  ! Flood flow to river
REAL,    DIMENSION(:),   INTENT(OUT)   :: PQRF  ! River flow to floodplain
!
REAL,    DIMENSION(:),   INTENT(OUT)   :: PVFIN ! River flow to flood velocity [m/s]
REAL,    DIMENSION(:),   INTENT(OUT)   :: PVFOUT! Flood flow to river velocity [m/s]
REAL,    DIMENSION(:),   INTENT(OUT)   :: PHSF  ! River-Floodplain depth comparison [m]
!
REAL,                    INTENT(INOUT) :: PGSTO_ALL  !Global groundwater storage at t    [kg]
REAL,                    INTENT(INOUT) :: PGSTO2_ALL !Global groundwater storage at t-1  [kg]
REAL,                    INTENT(INOUT) :: PGIN_ALL   !Global gw recharge + lateral input [kg/m2/s]
REAL,                    INTENT(INOUT) :: PGOUT_ALL  !Global gw outflow                  [kg/m2/s]
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PDRAIN,1)) :: ZSURF_STO2  ! river channel storage at t+1     [kg]
REAL, DIMENSION(SIZE(PDRAIN,1)) :: ZGROUND_STO2! Groundwater storage at t+1       [kg]
REAL, DIMENSION(SIZE(PDRAIN,1)) :: ZFLOOD_STO2 ! Floodplain water storage at t+1  [kg]
REAL, DIMENSION(SIZE(PDRAIN,1)) :: ZQFR
REAL, DIMENSION(SIZE(PDRAIN,1)) :: ZQRF
!
REAL    :: ZFSTO_ALL, ZFSTO2_ALL, ZSOURCE_ALL, ZSSTO_ALL, ZSSTO2_ALL, &
           ZSIN_ALL, ZDRUN_ALL, ZSOUT_ALL, ZVEL_ALL, ZFIN_ALL,        &
           ZFOUT_ALL, ZHS_ALL, ZHF_ALL, ZFF_ALL, ZRECUP_ALL
!
INTEGER :: IERR
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! * Init
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP',0,ZHOOK_HANDLE)
!
ZFSTO_ALL   = 0.0
ZFSTO2_ALL  = 0.0
ZSOURCE_ALL = 0.0
ZSSTO_ALL   = 0.0
ZSSTO2_ALL  = 0.0
ZSIN_ALL    = 0.0
ZDRUN_ALL   = 0.0
ZSOUT_ALL   = 0.0
ZVEL_ALL    = 0.0
ZFIN_ALL    = 0.0
ZFOUT_ALL   = 0.0
ZHS_ALL     = 0.0
ZHF_ALL     = 0.0
ZFF_ALL     = 0.0
ZRECUP_ALL  = 0.0
!
ZSURF_STO2  (:) = 0.0
ZGROUND_STO2(:) = 0.0
ZFLOOD_STO2 (:) = 0.0
!
ZQFR(:) = 0.0
ZQRF(:) = 0.0
!
!-------------------------------------------------------------------------------
! *  Ground water storage
!-------------------------------------------------------------------------------
!
IF(CGROUNDW=='DEF')THEN
!
  PGOUT(:) = MAX(PDRAIN(:),0.0)
!
ELSEIF(CGROUNDW=='CST')THEN
!
  CALL TRIP_GW_BUFFER_TANK(TPST, &
                           PTSTEP,OPRINT,PDRAIN,ZGROUND_STO2,PGOUT, &
                           PGSTO_ALL,PGSTO2_ALL,PGIN_ALL,PGOUT_ALL  )
!
ENDIF
!
!-------------------------------------------------------------------------------
! * Floodplains storage
!-------------------------------------------------------------------------------
!
IF(LFLOOD)THEN
!
  CALL TRIP_SURFACE_FLOOD(TPST, &
                          KLISTING,PTSTEP,OPRINT,PVEL,PHS,PSOURCE, &
                          ZFLOOD_STO2,PQFR,PQRF,PVFIN,PVFOUT,PHSF, &
                          ZFSTO_ALL,ZFSTO2_ALL,ZSOURCE_ALL,        &
                          ZFIN_ALL,ZFOUT_ALL,ZHF_ALL,ZFF_ALL       )
!
  ZQFR(:) = PQFR(:)
  ZQRF(:) = PQRF(:)
!
ENDIF
!
!-------------------------------------------------------------------------------
! * Surface water storage
!-------------------------------------------------------------------------------
!
CALL TRIP_SURFACE_WATER(TPST, &
                        KLISTING,OPRINT,PTSTEP,                  &
                        PRUNOFF,PGOUT,ZQFR,ZQRF,PHS,PVEL,        &
                        ZSURF_STO2,PSIN,PSOUT,                   &
                        ZSSTO_ALL,ZSSTO2_ALL,ZSIN_ALL,ZDRUN_ALL, &
                        ZSOUT_ALL,ZVEL_ALL,ZHS_ALL               )
!
!-------------------------------------------------------------------------------
! * Update all reservoir and conserve water mass as possible
!-------------------------------------------------------------------------------
!
CALL TRIP_UPDATE_AND_CONSERV(TPST, &
                             OPRINT,ZSURF_STO2,ZFLOOD_STO2,ZGROUND_STO2,ZRECUP_ALL)
!
!-------------------------------------------------------------------------------
! * Writting the budget
!-------------------------------------------------------------------------------
!
IF(OPRINT)THEN
!
  IF(KTRIP==1.AND.KTSTEP==1)THEN
    WRITE(KLISTING,*)''
    WRITE(KLISTING,*)'        START RUN ISBA-TRIP-FP     '
    WRITE(KLISTING,*)'          Budget en  kg/m2         '
    WRITE(KLISTING,*)''
    WRITE(KLISTING,'(a90)')'  RUN TSTEP   S_err    F_err  G_err    MR(mm/y) Vel(m/s)     Hs       Hf        Ff      UNCSV'
    WRITE(KLISTING,*)''
  ENDIF
!
  WRITE(KLISTING,'(i3,1x,i3,3(2x,g8.2),f8.2,f8.2,8(2x,f8.3))')         &
  KTRIP,KTSTEP,                                                        &
!   surface budget S_err
  (ZSSTO_ALL-ZSSTO2_ALL)+PTSTEP*(ZSIN_ALL-ZSOUT_ALL),                  &
!   floodplains budget F_err
  (ZFSTO_ALL-ZFSTO2_ALL)+PTSTEP*(ZSOURCE_ALL+ZFIN_ALL-ZFOUT_ALL),      &
!   ground budget G_err
  (PGSTO_ALL-PGSTO2_ALL)+PTSTEP*(PGIN_ALL-PGOUT_ALL),                  &
!   output flow to the sea, MR(mm/y) 
  (ZSSTO_ALL-ZSSTO2_ALL +PTSTEP*ZDRUN_ALL)/XSEA*XYEAR*REAL(KTSTEP_END),&
!   mean flow velocity, Vel(m/s), and mean stream depth, Hs (m) 
!   mean floddplains depth, Hf (m), and mean flood fraction, Ff (%) 
  ZVEL_ALL,ZHS_ALL,ZHF_ALL,100.*ZFF_ALL,ZRECUP_ALL
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP
