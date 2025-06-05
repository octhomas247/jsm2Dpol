PROGRAM jsm2Dpol

!      Outputs the dust cross-sections, intensity and polarised emission
!      for the three-component model by Siebenmorgen R., 2023, A&A, 670A,
!      115. The relative masses of the components are derived from input
!      dust abundances and size distributions. The relative mass of
!      submicron-sized grains and the cross-sections
!      (cm^2/gramm-ISM-dust) are computed in the subroutine
!      sigtDark_EbvAvPol
! 
!      Dust Model:
!      1. Cross-sections:
!       Based on Siebenmorgen R. (2023, A&A, 670A, 115).
!      2. Column density estimation:
!         Follows Eq. (2) and Eq. (3) from Siebenmorgen & Chini (2023, 10.48550/arXiv.2311.03310).
! 
!      Dust Components:
!      1) Nano-particles (VSG):
!         - Graphite + PAH (2175 AA bump, far-UV reddening).
!         - Nano-silicates (far-UV contribution).
!      2) Large grains:
!         - Prolate grains with a/b = 2, aligned (IDG alignment).
!         - Optical constants: Demyk et al. (2023) for aSi; Zubko (1996) for aC.
!         - Radii: 6 nm to ~260 nm (Mathis et al., 1977).
!      3) Submicron-sized grains (Dark Dust):
!         - Fluffy spheres (20% aC, 30% Si, 50% vacuum).
!         - Radii: 260 nm to ~3 microns.
! 
!      WARNING: parameter for dust porosity 'por' is hardcoded in
!      jsm2Dpol.f and needs to be adjusted to match the files d.Q* which
!      include the grain efficiency computed using the Bruggeman mixing
!      rule for porosity treated as vaccuum inclusion ('por').
   
!      Input (./Input/):
!        - jsm12fit.inp:
!          Dust parameters for fitting reddening curves:
!            - Abundances: abuc, abusi, abuvsi, abucvgr, abucpahs.
!            - Sizes: qmrn, alec, alesi, arad_polmin_aC, arad_polmin_Si, arad_polmax, aled.
!        - w12_vv2_Ralf_283wave.dat:
!          Wavelength grid for the dust model.
!        - jsmTaufit.inp:
!          Includes tauV and Ebv_obs for equations (2) and (3)  and P_serk polarisation of Serkowski fit.
!        - d.Q* files:
!          Grain efficiencies Q for various components.
! 
!      Output (./Output/):
!        - Kappa.out:
!          Optical properties for absorption (sa_*) and scattering (ss_*).
!        - Kappa4fit.out:
!          Optical properties for observed wavelength ranges.
!        - PolKappa.out:
!          Polarization cross-sections (sigp_*). Dark dust: here: sigp_dark = 0.
!        - tau4fit.out:
!          Extinction and reddening curves (normalized and absolute).
! 
!      Cross-Section Notation:
!      - Cross-sections (K) are in cm^2/g-dust and computed for each component.
!      - Converted to optical depth by multiplying by Nl, Nd.
! 
!      Version History
!        - 26.11.2024: this version      
!        - 24.04.2023: Unified SpT (luminosity) with GAIA (parallax) distances.
!        - 20.09.2022: Updated optics (n, k) using Demyk+22 for X50A+E20R silicates.
!        - 04.12.2021: Included dark dust model.
!        - 1991: Original version (Siebenmorgen, PhD; Siebenmorgen & Kruegel, 1992, A&A).
!       For issues please contact me as author
!       Greetings, Ralf Siebenmorgen
!      ------------------------------------------------------------------

USE constants

! USE test
USE tempmrn_mod
USE effi_mod
USE vsg_mod
USE vec_lr_mod
USE vec_nf_mod
USE vec_sig_mod
USE fest_mod
USE paheva_mod
USE paraDark_mod
USE abu_mod
USE rdark_mod

USE const_mod
USE functions
!   USE jsm2Dpolcom
USE JSM_UTILS
USE SUBROUTINES

IMPLICIT NONE
INTEGER, PARAMETER :: nfo = 283
REAL(KIND=8), DIMENSION(nfo) :: welo, qoabspahs, qoabspahb

REAL(KIND=8), DIMENSION(lr, nfo) :: qoac, qosc, goc, qopc, qocpc
REAL(KIND=8), DIMENSION(lrsi, nfo) :: qoasi, qossi, gosi, qopsi, qocpsi
REAL(KIND=8), DIMENSION(lrd, nfo) :: qoad, qosd, god, qopd, qocpd
REAL(KIND=8), DIMENSION(lrv, nfo) :: qoavsi, qosvsi, govsi, qoagr, qosgr, gogr

REAL(KIND=8), DIMENSION(nf) :: welv, weld, welov
REAL(KIND=8), DIMENSION(nf) :: emi_pahs, emi_pahb, emi_vgr, emi_vsi, emi_d
REAL(KIND=8), DIMENSION(nf) :: emip_c, emip_si, emip_d, emip_t
REAL(KIND=8), DIMENSION(nf) :: dummy, seVolt

INTEGER, DIMENSION(20) :: idummy

CHARACTER(LEN=80) :: cdumSi, cdumaC, cdumD, CDUM, CDUMAD
CHARACTER(LEN=10) :: comp

REAL(KIND=8) :: Rv, OMEGA, CMEXT, CMPOL, CMCPL, CMSCA
REAL(KIND=8) :: Ebv, Rv_mod, Rv_obs
REAL(KIND=8) :: md_aCSi, md_aC, md_Si, abud_acSi
REAL(KIND=8) :: p_serk

REAL(KIND=8) :: AB, DEL0, ADUM, ABUCPAHSB, POR
REAL(KIND=8) :: VSI, VC, VVAC, VEIS, VD
INTEGER :: KPROL, J, IDG, IDGSI
LOGICAL :: openedq

CHARACTER(LEN=*), PARAMETER :: INPUTS_DIR = './Input/'

LOGICAL :: STATUS


! c ----------------------------------------------------------------------------
! c   Universelle Konstanten und Parameter wmolsi_EK= 168




! Define all physical constants as parameters
! REAL(KIND=8), PARAMETER :: &
! hwirk  = 6.6262D-27,  & ! Planck constant (erg s)
! clicht = 2.997925D10, & ! Speed of light (cm/s)
! protm  = 1.672D-24,   & ! Proton mass (g)
! boltz  = 1.38062D-16, & ! Boltzmann constant (erg/K)
! sigma  = 5.669563D-5, & ! Stefan-Boltzmann constant (erg/cm^2/s/K^4)
! eVolt  = 1.602D-12,   & ! Energy of 1 eV (erg)
! tbb    = 2.9D0,       & ! Some temperature or constant (dimensionless?)
! Grav   = 6.673D-8,    & ! Gravitational constant (cm^3/g/s^2)
! Lsun   = 3.846D33,    & ! Solar luminosity (erg/s)
! Msun   = 1.989D33     ! Solar mass (g)

! REAL(KIND=8), PARAMETER :: fevap = 1.0D-8

! Material densities and molecular weights
! REAL(KIND=8), PARAMETER :: &
! rhsi    = 3.4D0,      &
! rhvsi   = 3.5D0,      &
! wmolsi  = 134.5D0,    &
! wmolvsi = 134.5D0

REAL(KIND=8), PARAMETER :: &
rhsiE10 = 2.8D0,      &
rhsiE20 = 2.9D0,      &
rhsiE30 = 3.0D0,      &
rhsiE40 = 3.1D0,      &
rhSiX   = 2.7D0

REAL(KIND=8), PARAMETER :: &
wmolSiE10 = 99.86D0,  &
wmolSiE20 = 99.33D0,  &
wmolSiE30 = 99.79D0,  &
wmolSiE40 = 99.26D0

REAL(KIND=8), PARAMETER :: &
wmolSiX35 = 141.0D0,  &
wmolSiX40 = 121.0D0,  &
wmolSiX50 = 100.0D0

! REAL(KIND=8), PARAMETER :: &
! rhc    = 1.6D0,       &
! rhgr   = 2.24D0,      &
! wmolc  = 12.0D0

! c
! c ----------------------------------------------------------------------------
! Default parameters

INTEGER :: impfit, IEVAPVSG

REAL(KIND=8) :: alac, alec, alasi, alesi

REAL(KIND=8) :: hydpahs, hydpahb
REAL(KIND=8) :: rdust, totlum, tstar, powl
      
REAL(KIND=8) :: ALAD, FAK, FAK1, E_MIE, EMISMAX, WMIE, PV, PVMAX, RDUST0
REAL(KIND=8) :: TABSPAH, SUM, TNEW, TOTEMIS, W_MIE, WDUM, X, XM1, ZEITMRN

! c ------------------------------------------------------------------       
! c   For  mpfit: read input parameters from ./Input/jsm12fit.inp and ./Input/jsmTaufit.inp
! c

INTEGER :: i, IX, II, K, K1P25, K1P65, K2P3, k850, KJOT, KUV, KVISD, KVISV, L




! MODELFOLDER     = .FALSE.
! DARKFORSFOLDER  = .FALSE.

! Variables computed at runtime
! REAL(KIND=8) :: Eb, pi, pi4, au

! Assign runtime values
Eb   = 5.0D0                 ! Binding energy of PAH (eV)
pi   = 4.0D0 * ATAN(1.0D0)   ! Pi constant
pi4  = 4.0D0 * pi            ! 4 * Pi
au   = 1.496D13              ! Astronomical Unit (cm)


print*, ' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
print*, '  --> crossec_dd              START                            ++++++++++'

amrn      = 0.0D0
adark     = 0.0D0
ahsi      = 0.0D0
ahc       = 0.0D0
ahd       = 0.0D0

qasi      = 0.0D0
qssi      = 0.0D0
qpsi      = 0.0D0
qcpsi     = 0.0D0

qac       = 0.0D0
qsc       = 0.0D0
qpc       = 0.0D0
qcpc      = 0.0D0

qad       = 0.0D0
qsd       = 0.0D0         
qpd       = 0.0D0
qcpd      = 0.0D0
        
xnl       = 0.0D0
xnd       = 0.0D0
qabspahs  = 0.0D0
qabspahb  = 0.0D0

emis      = 0.0D0
emi_d     = 0.0D0
emi_c     = 0.0D0
emi_si    = 0.0D0
emi_vgr   = 0.0D0
emi_vsi   = 0.0D0
emi_pahs  = 0.0D0
emi_pahb  = 0.0D0

emip_c    = 0.0D0
emip_si   = 0.0D0
emip_d    = 0.0D0
emip_t    = 0.0D0      

sigt      = 0.0D0
sigt_l    = 0.0D0

sigt_d    = 0.0D0
siga_d    = 0.0D0
sigs_d    = 0.0D0
sigp_d    = 0.0D0
sigcp_d   = 0.0D0

siga_ac   = 0.0D0
siga_si   = 0.0D0
sigp_ac   = 0.0D0
sigp_si   = 0.0D0
sigcp_ac  = 0.0D0
sigcp_si  = 0.0D0
sigs_ac   = 0.0D0
sigs_si   = 0.0D0

siga_vgr  = 0.0D0
sigs_vgr  = 0.0D0
siga_vsi  = 0.0D0
sigs_vsi  = 0.0D0
siga_pah  = 0.0D0
siga_pahs = 0.0D0
siga_pahb = 0.0D0
      


! Initialization
impfit   = 1            ! default: 0
ibug     = 1
iblack   = 0
igasabs  = 0
nnTvsg   = 200

fakisrf  = 1.0D0
ispecvsg = 1
ispecpah = 1
lav      = 1
lev      = 4

alac   = 6.1D-7
alec   = 2.6D-5
alasi  = alac
alesi  = 2.6D-5
aled   = 3.1D-4
qmrn   = 3.0D0
qvsg   = 3.0D0

arad_polmin_aC = 1.2356D-5
arad_polmin_Si = 7.5857D-6
arad_polmax    = aled

abuc     = 86.23D0
abusi    = 15.0D0
abucvgr  = 6.4D0
abuvsi   = 13.19D0
abucpahs = 10.0D0
abucpahb = 0.0D0

fDark    = 0.5D0        ! fraction of dust mass in Dark Dust

zcpahs   = 30
zcpahb   = 0
hydpahs  = 0.4D0
hydpahb  = 0.2D0

tauV     = 1.72D0       ! depends on Star see ./Input/jsmTaufit.inp
ebv_obs  = 0.32D0
rdust    = 1.5D12
totlum   = 3.85D33
tstar    = 6000.0D0
powl     = 0.0D0




! Check the condition and stop if necessary
IF (impfit .EQ. 1 .AND. abucpahb .NE. 0.0D0) THEN
    STOP ' Enter in jsmDD.inp for mpfit=1:  abupahb = 0 '
ENDIF


! Open the input file and read the data
IF (impfit .NE. 0) THEN
    OPEN(UNIT=20, FILE='./Input/jsm12fit.inp', FORM='FORMATTED')
    REWIND 20
    READ(20,500) (idummy(i), i = 1, 20)
    READ(20,*) abuc, abusi, abuvsi, abucvgr, abucpahs, qmrn, alec, alesi, &
              arad_polmin_aC, arad_polmin_Si, arad_polmax, aled
    CLOSE(20)

500 FORMAT(20A4)

    OPEN(UNIT=20, FILE='./Input/jsmTaufit.inp', FORM='FORMATTED')
    REWIND 20
    READ(20,500) (idummy(i), i = 1, 20)
    READ(20,*) tauV, Ebv_obs, P_serk
    CLOSE(20)

    ! Update qvsg based on qmrn
    qvsg = qmrn

    ! Check if alesi exceeds the maximum allowed value and issue a warning
    IF (alesi .GT. 3.442E-5) THEN
        WRITE(6,*) ' *** Warning: set to max radius alesi = 3.442E-5'
        alesi = 3.442E-5
    ENDIF
ENDIF
      

WRITE(6,*) ' '
WRITE(6,*) '** Reading 8 input parameters from file jsm12fit.inp'
WRITE(6,*) '** Reading 2 input parameters from file jsmTaufit.inp'

! Calculate Rv based on the input parameters
Rv = 1.0857 * tauV / ebv_obs

! Output the values to the screen with appropriate formatting
WRITE(6, '(A41, 1X, 3F8.2)') ' ** Rv(Av), tauV, ebv_obs= ', Rv, tauV, ebv_obs
WRITE(6, '(A41, 1X, F10.2)') ' ** abuc                 = ', abuc
WRITE(6, '(A41, 1X, F10.2)') ' ** abusi                = ', abusi
WRITE(6, '(A41, 1X, F10.2)') ' ** abuvsi               = ', abuvsi
WRITE(6, '(A41, 1X, F10.2)') ' ** abucvgr              = ', abucvgr
WRITE(6, '(A41, 1X, F10.2)') ' ** abucpah              = ', abucpahs
WRITE(6, '(A41, 1X, F10.2)') ' ** fDark                = ', fDark
WRITE(6, '(A41, 1X, F10.2)') ' ** qmrn                 = ', qmrn

! Optional formatting for scientific notation if needed
! WRITE(6, '(A41, 1X, 1P1E10.2)') ' ** alec                 = ', alec
! WRITE(6, '(A41, 1X, 1P1E10.2)') ' ** alesi                = ', alesi
! WRITE(6, '(A41, 1X, 1P1E10.2)') ' ** arad_polmin_aC       = ', arad_polmin_aC
! WRITE(6, '(A41, 1X, 1P1E10.2)') ' ** arad_polmin_Si       = ', arad_polmin_Si       
! WRITE(6, '(A41, 1X, 1P1E10.2)') ' ** arad_polmax  (na)   = ', arad_polmax
! WRITE(6, '(A41, 1X, 1P1E10.2)') ' ** arad_Darkmax         = ', aled

! Check if min values exceed max values and stop if true
IF (arad_polmin_aC .GE. arad_polmax .OR. arad_polmin_Si .GE. arad_polmax) THEN
    STOP 'Check arad_polmin/max'
ENDIF


qmrn = qmrn - 1d0
qvsg = qvsg - 1d0
zhpahs = hydpahs * zcpahs
zhpahb = hydpahb * zcpahb


IF (ispecvsg .EQ. 0.0) THEN
    abucvgr = 0.0
    abuvsi = 0.0
ENDIF


IF (ispecpah .EQ. 0.0) THEN
    abucpahs = 0.0
    abucpahb = 0.0
ENDIF


IF (zcpahb .EQ. 0.0) THEN
    abucpahb = 0.0
ENDIF



! Print current dust parameters vvac, por, del0, ab
CALL PRINT_DUST_PARAMS(VVAC, POR, DEL0, AB)

! Try to read vvac, por, del0, ab from d.Q* files
! If files are missing and if in known file structure, 
! try to fetch d.Q* files from library /Qfile_tv/
CALL INITIALIZE_DUST_PARAMETERS(VVAC, POR, DEL0, AB, STATUS)

! Print current dust parameters vvac, por, del0, ab
CALL PRINT_DUST_PARAMS(VVAC, POR, DEL0, AB)

WRITE(6, '(a40, 1p4e10.2)') ' *** rhc, rhgr, rhsi, wmolsi        = ', rhc, rhgr, rhsi, wmolsi

OPEN(UNIT=3, FILE=INPUTS_DIR // 'd.QellipSi', FORM='formatted')
OPEN(UNIT=4, FILE=INPUTS_DIR // 'd.QellipaC', FORM='formatted')
OPEN(UNIT=7, FILE=INPUTS_DIR // 'd.QellipDark', FORM='formatted')

REWIND(UNIT=3)
REWIND(UNIT=4)
REWIND(UNIT=7)


! por = 0.10
rhsi = rhsiX * 0.97 / (1.0 + por) + rhsiE20 * 0.03 / (1.0 + por)
wmolsi = wmolsiX50 * 0.97 + wmolsiE20 * 0.03
rhc = rhc / (1.0 + por)


! -------------------------------------------
!    DARK dust : relative weights  in 1g Staub: aC, Si, Eis'
!    geweis = Masse(Eis) / Masse(aC+Si)  = 1d-6 (hier ohne Eis)
!    volvac = Volumenanteil des Vakuum bezogen auf gesamt Volumen (from input) 
!    Volume of porous Dark dust grain: Vd = Vsi+Vc+Vvac. 
!    Mean density of porous Dark dust grain in 1 gramm of mass is : 
!    rhd    = total mass porous grain / Vd
! -------------------------------------------

! Set Vsi, Vc according to Vvac (previously hardcoded)
CALL SET_DARK_DUST_VOLUMES(VVAC, VSI, VC)
! Vsi  = 0.5
! Vc   = 0.3
! Vvac = 0.2
Veis = 0.0

WRITE(6,*) ' Relative volume in Dark dust  :'
WRITE(6,*) '      aC        Si        Eis       Vac       total'
WRITE(6,'(1x,5f10.2)') Vc, Vsi, Veis, Vvac, (Vc + Vsi + Veis + Vvac)

! mass in 1 gram of dust in aC and Si sub-particles:
md_ac     = (Vc * rhc)   / (Vc * rhc + Vsi * rhsi)
md_Si     = (Vsi * rhsi) / (Vc * rhc + Vsi * rhsi)
md_aCSi   = md_ac / md_Si
abud_acSi = md_aCSi * wmolsi / wmolc

Vd   = Vc + Vsi + Vvac
rhd  = (Vc * rhc + Vsi * rhsi) / Vd ! density of porous grain
WRITE(6,'(a35, f7.2)') '   Density of porous Dark dust = ', rhd
       
!
! -------------------------------------------
!   Optics of dust - read Q's and interpolate to frequency grid of MRN, dark dust and vsgr, vsi
! -------------------------------------------
!   MRN - particles: wavelengths, absorption, and scattering efficiencies
!   from file d.QellipaC, d.QellipSi, d.QellipDark of the ellipsoid
! -------------------------------------------
PRINT*, '    ***     open:   d.QellipSi'
PRINT*, '    ***     open:   d.QellipaC'
PRINT*, '    ***     open:   d.QellipDark'

DO j = 1, 12
    READ(3,'(a80)') cdumSi
    READ(4,'(a80)') cdumaC
    READ(7,'(a80)') cdumD
ENDDO
       
! Read if grains are prolate/oblate, their ab, optical constants used, alignment idg, del0
! Could include a check that indeed those are the same for all d.Q files (not yet done)
WRITE(6,*) 'Reading kprol, ab, comp, idgSi, del0 from d.QellipSi'
READ(3, '(I4, F4.2, A10, I2, F7.1)') kprol, ab, comp, idg, del0
WRITE(6,*) kprol, ab, comp, idgSi, del0

CALL PRINT_DUST_PARAMS(VVAC, POR, DEL0, AB)

WRITE(6, '(A80)') cdumSI
READ(4, '(A80)') cdumaC
WRITE(6, '(A80)') cdumaC
READ(7, '(A80)') cdumD
WRITE(6, '(A80)') cdumD
READ(3, '(A80)') cdumSi
READ(4, '(A80)') cdumaC
READ(7, '(A80)') cdumaD

! WRITE(6,*) 'k=0: Prolate'
! WRITE(6,*) 'k=1: Oblate'

DO k = 1, nfo
    ! Prolate silicates
    DO l = 1, lrsi
       READ(3,*) welo(k), amrn(l), OMEGA, qoasi(l,k), qopsi(l,k), qocpsi(l,k), qossi(l,k)
       qoasi(l,k) = qoasi(l,k) - qossi(l,k)  ! In d.q ist extinction gespeichert
 
        IF (qoasi(l,k) .LE. 0) THEN
            PRINT*, 'l,k =', l, k
            PRINT*, "welo(k), amrn(l), OMEGA, qoasi(l,k), qopsi(l,k), qocpsi(l,k), qossi(l,k)"
            WRITE(6, '(1P7E10.2, A5)') welo(k), amrn(l), OMEGA, qoasi(l,k), qopsi(l,k), qocpsi(l,k), qossi(l,k), ' Si'
            STOP " *** Check d.QellipSi Q < 0"
        END IF
    END DO
 
    ! Prolate aC: number of aC grain radii
    DO l = 1, lr
       READ(4,*) wdum, amrn(l), OMEGA, qoac(l,k), qopc(l,k), qocpc(l,k), qosc(l,k)
       ! WRITE(6, '(1P7E10.2, A5)') wdum, amrn(l), OMEGA, qoac(l,k), qopc(l,k), qocpc(l,k), qosc(l,k), ' aC'
       
       qoac(l,k) = qoac(l,k) - qosc(l,k)  ! Extinction is stored in d.q
 
       IF (qoac(l,k) .LE. 0) THEN
            WRITE(6,*) "l,k = ", l, k
            WRITE(6,*) "wdum, amrn(l), OMEGA, qoac(l,k), qopc(l,k), qocpc(l,k), qosc(l,k), aC"
            WRITE(6, '(1P7E10.2, A5)') wdum, amrn(l), OMEGA, qoac(l,k), qopc(l,k), qocpc(l,k), qosc(l,k), ' aC'
            STOP "** qoac = 0 in d.Qellip* "
        END IF
    END DO
 

! Dark dust: r < 1 mu prolate; r > 1 mu spherical
    DO l = 1, lrd  ! Number of dark dust grain radii
        READ(7,*) weld(k), adark(l), OMEGA, qoad(l,k), qopd(l,k), qocpd(l,k), qosd(l,k)
        qoad(l,k) = qoad(l,k) - qosd(l,k)  ! In d.q ist extinction gespeichert
     
        IF (qoad(l,k) .LE. 0) THEN
           WRITE(6,*) "l,k = ", l, k
           WRITE(6,*) ' weld, adark OMEGA qoad qopd qocpd qosd : dark'
           WRITE(6, '(1P7E10.2, A5)') weld(k), adark(l), OMEGA, qoad(l,k), qopd(l,k), qocpd(l,k), qosd(l,k), ' dark'
           ! STOP '** qoad = 0 in d.Qellip* '
        END IF
    END DO
     
    IF (welo(k) .NE. weld(k)) THEN
        PRINT*, "k, welo(k), weld(k) "
        PRINT*, k, welo(k), weld(k), amrn, "  "
        STOP "** wrong wavelengths in files: d.Qellip* "
    END IF
     
END DO

! Set polarisation efficiencies q = abs(q)
qopc  = ABS(qopc)
qopsi = ABS(qopsi)
qopd  = ABS(qopd)

qocpc  = ABS(qocpc)
qocpsi = ABS(qocpsi)
qocpd  = ABS(qocpd)
     
! Test:
!         CALL locat(weld, nfo, 0.55d-4, kvisd)
!         WRITE(6, '(A20, F12.3, A30)') '    weld(kvisd) (mu) = ', weld(kvisd)*1e4, ' in d.Qmie_dark'

CLOSE(3)
CLOSE(4)
CLOSE(7)
     

! -------------------------------------------
! Wavelength grid of dust model use file: ./Input/w12_vv2_Ralf_283wave.dat
! Until here old wavelengths as in d.q files and now interpolate to
! wavelength (cm) grid of dust model
! -------------------------------------------
MM      = NF
MM1     = MM - 1
IF (MM .NE. 283) STOP ' mm ne 283'

OPEN(UNIT=3, FILE='./Input/w12_vv2_Ralf_283wave.dat', FORM='formatted')
REWIND 3
READ(3, '(A80)') CDUM
READ(3, *) (WEL(K), K = 1, MM)
CLOSE(UNIT=3)


! Interpolate to new wavelength grid for ellipsoids
DO K = 1, MM
    IF (WELO(K) .NE. WEL(K) .AND. K .NE. 242) THEN
        PRINT*, 'WEL(K), WELO(K), K'
        PRINT*, K, WEL(K), WELO(K)
        STOP 'Shall be the same MRN:'
    END IF
    
    CALL LOCAT(WELO, NFO, WEL(K), J)
    IF (J .LT. 1) STOP 'MRN: WEL-Gitter LOCAT: check J<1 ?'
    IF (J .GE. NFO) J = NFO - 1
    IF (J .GE. NFO .AND. (WEL(K) .NE. WELO(J))) THEN
        PRINT*, WELO(1), WELO(NFO), WEL(K), K
        STOP 'WEL Gitter LOCAT J=MM: check ?'
    END IF
    
    FAK = (WEL(K) - WELO(J)) / (WELO(J+1) - WELO(J))
        
    DO L = 1, LRSI ! Number of MRN Si grain radii: LRSI = 84
        QASI(L,K)  = QOASI(L,J)  + (QOASI(L,J+1)  -  QOASI(L,J)) * FAK
        QPSI(L,K)  = QOPSI(L,J)  + (QOPSI(L,J+1)  -  QOPSI(L,J)) * FAK
        QCPSI(L,K) = QOCPSI(L,J) + (QOCPSI(L,J+1) - QOCPSI(L,J)) * FAK
        QSSI(L,K)  = QOSSI(L,J)  + (QOSSI(L,J+1)  -  QOSSI(L,J)) * FAK
    ENDDO
    
    DO L = 1, LR ! Number of MRN aC grain radii: LR = 100
        QAC(L,K)  = QOAC(L,J)  + (QOAC(L,J+1)  -  QOAC(L,J)) * FAK
        QPC(L,K)  = QOPC(L,J)  + (QOPC(L,J+1)  -  QOPC(L,J)) * FAK
        QCPC(L,K) = QOCPC(L,J) + (QOCPC(L,J+1) - QOCPC(L,J)) * FAK
        QSC(L,K)  = QOSC(L,J)  + (QOSC(L,J+1)  -  QOSC(L,J)) * FAK
    ENDDO
ENDDO


DO K = 1, NFO
    DO L = 1, LRSI
        IF (IBUG .GT. 2 .AND. WEL(K) .LE. 1.06D-5 .AND. WEL(K) .GE. 1.04D-5 .AND. &
            AMRN(L) .LE. 3.30E-5 .AND. AMRN(L) .GE. 3.70E-5) THEN
            WRITE(6, '(1P7E10.2, A5)') WEL(K), AMRN(L), OMEGA, QASI(L,K), QPSI(L,K), &
                QCPSI(L,K), QSSI(L,K), ' Si'
        END IF
    ENDDO
ENDDO


! Interpolate to wavelength grid for Dark dust
IF ((ABS(WELD(1)/WEL(1) - 1.0) .GE. 1.E-3) .OR. (ABS(WELD(NF)/WEL(NF) - 1.0) .GE. 1.E-3)) THEN
    PRINT*, '  WEL(1), WELD(1), WEL(NF), WELD(NF)', WEL(1), WELD(1), WEL(NF), WELD(NF)
    STOP 'Check wavelength grids:'
ENDIF
    

DO K = 1, MM
    IF (ABS(WELD(K) - WEL(K)) .GT. 1.E-4 .AND. K .NE. 242) THEN
        PRINT*, WEL(K), WELD(K), K
        STOP 'Shall be the same DARK'
    ENDIF
        
    CALL LOCAT(WELD, NFO, WEL(K), J)
    
    IF (J .GE. NFO) J = NFO - 1
    IF (J .LE. 1) J = 1

    ! check WELD(J) after LOCAT:
    IF (WEL(K) .GE. WELD(J) * (1.0 + 1.D-7)) THEN
        PRINT*, ' K, J, WELD(J), WEL(K), WELD(J+1)'
        STOP 'Check wavelength grid: 1 term WELD(J+1) < WEL(K) <= WELD(J)'
    ENDIF
    
    IF (WEL(K) .LT. WELD(J+1) / (1.0 + 1.D-7)) THEN
        PRINT*, ' K, J, WELD(J), WEL(K), WELD(J+1)'
        PRINT*, K, J, WELD(J), WEL(K), WELD(J+1)
        STOP 'Check wavelength grid: 2 term WEL(K) <= WELD(J)'
    ENDIF
        
    IF (IBUG .GE. 2 .AND. WELD(J+1) .GT. WEL(K) + 1E-4) THEN
        WRITE(6, '(A28, 2I5, 1P3E9.2)') ' WELD(J) <= WEL(K) < WELD(J+1)', K, J, WELD(J), WEL(K), WELD(J+1)
    ENDIF
    
        ! Ready to interpolate Dark dust q's:        
    FAK = (WEL(K) - WELD(J)) / (WELD(J+1) - WELD(J))
    
    DO L = 1, LRD  ! Number of dark dust grain radii
        QAD(L, K) = QOAD(L, J) + (QOAD(L, J+1) - QOAD(L, J)) * FAK
        QSD(L, K) = QOSD(L, J) + (QOSD(L, J+1) - QOSD(L, J)) * FAK
        QPD(L, K) = QOPD(L, J) + (QOPD(L, J+1) - QOPD(L, J)) * FAK
        QCPD(L, K) = QOCPD(L, J) + (QOCPD(L, J+1) - QOCPD(L, J)) * FAK
        GD(L, K) = GOD(L, J) + (GOD(L, J+1) - GOD(L, J)) * FAK
    ENDDO
ENDDO


! For testing:
IF (IBUG .GT. 2) THEN
    PRINT*, ARAD_POLMIN_AC, ARAD_POLMIN_SI, ARAD_POLMAX, ADARK(80), WELD(KVISD)
    
    DO K = 1, NF
        DO L = 1, LRD       
            IF (ADARK(L) .GE. ARAD_POLMIN_AC .AND. ADARK(L) .LE. ARAD_POLMAX .AND. WELD(K) .GE. 0.3D-4) THEN
                WRITE(16, '(1P5E10.2, 2I4)') WELD(K), ADARK(L), QOAD(L, K), QSD(L, K), QPD(L, K), L, K
                WRITE(17, '(1P5E10.2, 2I4)') WELD(K), ADARK(L), QOAD(L, K), QOSD(L, K), QOPD(L, K), L, K
                
                IF (WELD(K) .EQ. WELD(KVISD)) THEN
                    WRITE(6, '(1P5E10.2, 2I4)') WELD(K), ADARK(L), QOAD(L, K), QSD(L, K), QPD(L, K), L, K
                ENDIF
            ENDIF
        ENDDO
    ENDDO
END IF  ! end testing


    
! -------------------------------------------
!   Nano grains: Graphite + silicates - wavelengths, absorption and scattering efficiencies from file d.Qmie_*
!      print*, ' vsg:  Graphite'
! -------------------------------------------
OPEN(unit=3, FILE='./Input/d.Qmie_vGr', FORM='formatted')
READ(3, '(A80)') CDUMD
WRITE(6, '(A80)') CDUMD
READ(3, '(A80)') CDUMD

DO K = 1, NFO
    DO L = 1, LRV
        READ(3, *) WELV(K), AVSG(L), QOAGR(L, K), QOSGR(L, K), GOGR(L, K), X, XM1
    ENDDO
ENDDO

CALL LOCAT(WELV, NFO, 0.55D-4, KVISV)

CLOSE(3)

! print*, ' vsg: Silicates'
OPEN(unit=3, FILE='./Input/d.Qmie_vSi', FORM='formatted')
    READ(3, '(A80)') CDUMD
    WRITE(6, '(A80)') CDUMD
    READ(3, '(A80)') CDUMD

    DO K = 1, NFO
        DO L = 1, LRV
            READ(3, *) WDUM, ADUM, QOAVSI(L, K), QOSVSI(L, K), GOVSI(L, K), X, XM1

            IF (ABS(WDUM - WELV(K)) .GE. 1E-3) THEN
                WRITE(6, '(2I4, 1P2E10.2)') K, L, WDUM, WELV(K)
                STOP ' WELV(K) NE WDUM: CHECK VSG WEL'
            ENDIF

            IF (ABS(ADUM - AVSG(L)) .GE. 1E-3) THEN
                WRITE(6, '(2I4, 1P2E10.2)') K, L, WDUM, WELV(K)
                STOP ' AVSG(L) NE ADUM: CHECK VSG RADII'
            ENDIF
        ENDDO
    ENDDO

CLOSE(3)

! test if wavelength grid is the same for MRN and VSG:
DO K = 1, NFO
    IF (ABS((WELV(K) - WELO(K)) / WELO(K)) .GE. 0.001) THEN
        PRINT*, K, WELO(K), WELV(K), ABS((WELV(K) - WELO(K)) / WELO(K))
        STOP ' WRONG VSG WAVELENGTHS'
    ENDIF
ENDDO
! test: ok

! WRITE(6, '(A18, F6.3, A30)') '  WELV(KVISV) (MU) = ', WELD(KVISV)*1E4, ' IN D.QMIE_VGR'
      

! Interpolate to new wavelength grid
DO K = 1, MM
    CALL LOCAT(WELV, NFO, WEL(K), J)
    IF (J .LT. 1) THEN
        STOP 'WEL-GITTER LOCAT: CHECK J < 1 ?'
    ENDIF
    
    IF (J .GE. NFO) THEN
        J = NFO - 1
    ENDIF
    
    IF (J .GE. NFO .AND. (WEL(K) .NE. WELV(J))) THEN
        PRINT*, WELV(1), WELV(NFO), WEL(K), K
        STOP 'WEL GITTER LOCAT J = MM: CHECK ?'
    ENDIF
    
    FAK = (WEL(K) - WELV(J)) / (WELV(J+1) - WELV(J))
    
    DO L = 1, LRV ! Number of VSG radii
        QAGR(L, K) = QOAGR(L, J) + (QOAGR(L, J+1) - QOAGR(L, J)) * FAK
        QSGR(L, K) = QOSGR(L, J) + (QOSGR(L, J+1) - QOSGR(L, J)) * FAK
        QAVSI(L, K) = QOAVSI(L, J) + (QOAVSI(L, J+1) - QOAVSI(L, J)) * FAK
        QSVSI(L, K) = QOSVSI(L, J) + (QOSVSI(L, J+1) - QOSVSI(L, J)) * FAK
    ENDDO
ENDDO


! -------------------------------------------
! X-rays : For nano-grains reduction of grain absorption efficiencies
! as calculated by Mie, similar to Fig.5 of Smith and Dwek (97). Grain of
! radius ARAD and photon energy E > E_MIE (keV) => QABS reduced ~ 1/FR.
! -------------------------------------------

IF (WEL(MM) .LE. 100D-8) THEN
    PRINT*, '*** Reduction of abs efficiencies ~1/nu for wel < 136AA'
END IF

DO L = 1, LRV
    ARAD = AVSG(L)
    
    DO K = 1, MM
        IF (ARAD .LE. 10D-8) THEN
            E_MIE = 0.1
        ELSE IF (ARAD .LE. 50D-8 .AND. ARAD .GT. 10D-8) THEN
            E_MIE = 0.4
        ELSE IF (ARAD .LE. 300D-8 .AND. ARAD .GT. 50D-8) THEN
            E_MIE = 1.0
        ELSE IF (ARAD .LE. 1D-5 .AND. ARAD .GT. 300D-8) THEN
            E_MIE = 2.0
        ELSE IF (ARAD .LE. 2D-5 .AND. ARAD .GT. 1D-5) THEN
            E_MIE = 4.0
        ELSE IF (ARAD .LE. 1D-4 .AND. ARAD .GT. 2D-5) THEN
            E_MIE = 7.0
        ELSE IF (ARAD .GT. 1D-4) THEN
            E_MIE = 10.0
        END IF

        W_MIE = CLICHT / (1D3 * E_MIE * EVOLT / HWIRK)

        CALL LOCAT(WEL, MM, W_MIE, IX)
        
        IF (WEL(K) .LT. WEL(IX)) THEN
            QAGR(L, K) = QAGR(L, K) * WEL(K) / WEL(IX)
            QAVSI(L, K) = QAVSI(L, K) * WEL(K) / WEL(IX)
            PRINT*, WEL(K), W_MIE, WEL(IX), ARAD
            STOP 'Verify setting MIE reduction for X-rays (was checked OK)'
        END IF
    END DO
END DO

! Check wavelength settings for KJIVBU bands
CALL LOCAT(WEL, MM, 2.159D-4, K2P3)
CALL LOCAT(WEL, MM, 1.662D-4, K1P65)
CALL LOCAT(WEL, MM, 1.235D-4, K1P25)
CALL LOCAT(WEL, MM, 5.477D-5, KVIS)
CALL LOCAT(WEL, MM, 4.440D-5, KBLUE)
CALL LOCAT(WEL, MM, 3.656D-5, KUV)

IF (ABS(1.0D0 - 2.159D-4 / WEL(K2P3)) .GT. 1.D-3 .OR. &
    ABS(1.0D0 - 1.662D-4 / WEL(K1P65)) .GT. 1.D-3 .OR. &
    ABS(1.0D0 - 1.235D-4 / WEL(K1P25)) .GT. 1.D-3 .OR. &
    ABS(1.0D0 - 5.477D-5 / WEL(KVIS)) .GT. 1.D-3 .OR. &
    ABS(1.0D0 - 4.440D-5 / WEL(KBLUE)) .GT. 1.D-3) THEN

    PRINT*, WEL(K2P3), 2.159D-4, ABS(1.0D0 - 2.159D-4 / WEL(K2P3))
    PRINT*, WEL(K1P65), 1.662D-4, ABS(1.0D0 - 1.662D-4 / WEL(K1P65))
    PRINT*, WEL(K1P25), 1.235D-4, ABS(1.0D0 - 1.235D-4 / WEL(K1P25))
    PRINT*, WEL(KVIS), 5.477D-5, ABS(1.0D0 - 5.477D-5 / WEL(KVIS))
    PRINT*, WEL(KBLUE), 4.440D-5, ABS(1.0D0 - 4.440D-5 / WEL(KBLUE))

    STOP 'Check wavelength settings for KJIVBU bands: still OK?'
END IF

IF (ABS(1.0D0 - 3.656D-5 / WEL(KUV)) .GT. 1.D-3) THEN
    WRITE(6, '(A20, 1P3E10.2)') 'U band not ok: ', WEL(KUV), 3.656D-5, ABS(1.0D0 - 3.656D-5 / WEL(KUV))
END IF

! Calculate frequency and frequency differences
DO 10 K = 1, MM
    FR(K) = CLICHT / WEL(K)
    FR3(K) = FR(K)**3
    IF (K .LE. 2) GO TO 10
    DFR(K-1) = 0.5D0 * (FR(K) - FR(K-2))
    10 CONTINUE
! END DO

DFR(1) = 0.5D0 * (FR(2) - FR(1))
DFR(MM) = 0.5D0 * (FR(MM) - FR(MM-1))


! -------------------------------------------
! Interpolation of Qs for MRN, VSG, Ellipsoids, Dark dust done'
! Optics of dust done - Q's are stored 
! =======================================================================
! Check grain radii request in input and available in d.q files
! -------------------------------------------
CALL LOCAT(AMRN, LR, ALAC, LAC)
CALL LOCAT(AMRN, LR, ALEC, LEC)
CALL LOCAT(AMRN, LRSI, ALASI, LASI)
CALL LOCAT(AMRN, LRSI, ALESI, LESI)

! Check and adjust lec for MRN grains
FAK = ABS(AMRN(LEC) / ALEC - 1.0D0)
FAK1 = ABS(AMRN(LEC + 1) / ALEC - 1.0D0)
IF (FAK1 .LT. FAK .AND. LEC .LT. LR - 1) THEN
    LEC = LEC + 1
END IF

! Check and adjust lesi for VSG grains
FAK = ABS(AMRN(LESI) / ALESI - 1.0D0)
FAK1 = ABS(AMRN(LESI + 1) / ALESI - 1.0D0)
IF (FAK1 .LT. FAK .AND. LESI .LT. LR - 1) THEN
    LESI = LESI + 1
END IF


! write(6,'(a40, i5, 1p2e10.2)') ' Radii aC   asked: ALAC, ALEC LEC=', LEC, ALAC, ALEC
! write(6,'(a40, i5, 1p2e10.2)') ' Radii Si   asked: ALASI, LESI=', LESI, ALASI, ALESI   


! Radii assignments for MRN and VSG grains
ALAC = AMRN(LAC)
ALEC = AMRN(LEC)
ALASI = AMRN(LASI)
ALESI = AMRN(LESI)

! Write the radii for carbon and silicate grains
WRITE(6, '(A20, 1P2E10.2, 2I5)') ' Radii aC  :  ', AMRN(LAC), AMRN(LEC), LAC, LEC
WRITE(6, '(A20, 1P2E10.2, 2I5)') ' Radii Si  :  ', AMRN(LASI), AMRN(LESI), LASI, LESI

! Calculate and check Dark Dust grain radii
! alad = 1.d-7 for testing, verify correctness
ALAD = MAX(AMRN(LESI+1), AMRN(LEC+1)) 
CALL LOCAT(ADARK, LRD, ALAD, LAD)
CALL LOCAT(ADARK, LRD, ALED, LED)

FAK = ABS(ADARK(LED) / ALED - 1.0D0)
FAK1 = ABS(ADARK(LED+1) / ALED - 1.0D0)
IF (FAK1 .LT. FAK .AND. LED .LT. LRD - 1) THEN
    LED = LED + 1
END IF
ALED = ADARK(LED)

! Write Dark Dust radii
WRITE(6, '(A20, 1P3E10.2, 2I5)') '  Radii Dark : ', ADARK(LAD), ARAD_POLMAX, ADARK(LED), LAD, LED

IF (LED .GE. LRD) PRINT*, ' *** WARNING: check max grain size in d.q* for Dark Dust'

! ====================================================================
! Dark Dust calculations based on Siebenmorgen & Chini (2023)
! Ref: https://ui.adsabs.harvard.edu/abs/2023arXiv231103310S/abstract
! doi = 10.48550/arXiv.2311.03310
! Two dust types:
! a) Large + nano grains in the ISM with optical depth: tau_l and cross section sigt_l
! b) Dark dust as a separate component with optical depth: tau_d and cross section sigt_d
! -------------------------------------------
CALL SIGTDARK_EBVAVPOL
! =================================================================================

IF (IGASABS .EQ. 0) GOTO 401
FAK = 200.0D0 / SIGT(KVIS)

WRITE(6,*) '   sigt in [cm^2/gram gas+dust ISM]'
WRITE(6, '(A30, 1P1E9.3)') ' Norm. sigt(V)=200cm^2/g  = ', FAK
SIGT = SIGT * FAK

401  CONTINUE

! Writing output to file
OPEN(UNIT=8, FILE='./Output/Kappa.out', FORM='FORMATTED')
REWIND 8
WRITE(8,*) '# Optical depth (Siebenmorgen 2023, A&A 670A,115; SC 2023 doi: 10.48550/arXiv.2311.03310)'
WRITE(8,*) '# wel(cm) sa_aC    ss_aC    sa_Si    ss_Si    sa_vgr   ss_vgr   sa_vsi   ss_vsi   s_pahS+B sa_Dark  ss_Dark sigt '
WRITE(8,*) '# ------------------------------------------------------------------------------------------------------------- '
WRITE(8, '(1P1E9.3, 11E9.2, 1E10.3)') (WEL(K), &
   SIGA_AC(K), SIGS_AC(K), SIGA_SI(K), SIGS_SI(K), &
   SIGA_VGR(K), SIGS_VGR(K), SIGA_VSI(K), SIGS_VSI(K), &
   SIGA_PAHS(K) + SIGA_PAHB(K), SIGA_D(K), SIGS_D(K), SIGT(K), K=1,MM)
CLOSE(8)


! -------------------------------------------
! Normalization:
! A(l) = 1.086 * tau 
! A(l) = 1.086 N_dust * Cext 
! P(l) = 1.086 N_dust * Cpol
! => P(l)/A(l) = Cpol(l)/Cext(l) = sigpol(l)/sigt(l)
! -------------------------------------------


! Open file for polarisation optical depth output
OPEN(UNIT=18, FILE='./Output/PolKappa.out', FORM='FORMATTED')
REWIND 18
WRITE(18,*) '# Polarisation optical depth (Siebenmorgen 2023, A&A 670A,115; SC 2023 doi: 10.48550/arXiv.2311.03310)'
WRITE(18,*) '# wel         sigp_aC     sigp_Si      sigp_dark    sigt'
WRITE(18,*) '#----------------------------------------------------------------------------------------'
WRITE(18,'(1P5E12.3)') (WEL(K), SIGP_AC(K), SIGP_SI(K), SIGP_D(K), SIGT(K), K=1, MM)
CLOSE(18)

! Calculate Rv_mod and Ebv
RV_MOD = SIGT(KVIS) / (SIGT(KBLUE) - SIGT(KVIS))
EBV = 2.5 / LOG(10.0D0) * (SIGT(KBLUE) - SIGT(KVIS))

! Open file for tau4fit output
OPEN(UNIT=16, FILE='./Output/tau4fit.out', FORM='FORMATTED')
REWIND(16)
WRITE(16,'(A60, 2F9.3)') '#  wel (mu)   tau/tauV  E(w-V)/E(B-V)  E(w-V)| Rv_mod, Ebv =', RV_MOD, EBV
WRITE(16,'(1X, 1P4E11.3)') WEL(1) * 1E4, SIGT(1) / SIGT(KVIS), &
        (SIGT(1) / SIGT(KVIS) - 1.0D0) * RV_MOD, 2.5 / LOG(10.0D0) * (SIGT(1) / SIGT(KVIS) - 1.0D0) * SIGT(KVIS)
WRITE(16,'(1X, 1P4E11.3)') WEL(K2P3) * 1E4, SIGT(K2P3) / SIGT(KVIS), &
        (SIGT(K2P3) / SIGT(KVIS) - 1.0D0) * RV_MOD, 2.5 / LOG(10.0D0) * (SIGT(K2P3) / SIGT(KVIS) - 1.0D0) * SIGT(KVIS)
WRITE(16,'(1X, 1P4E11.3)') WEL(K1P65) * 1E4, SIGT(K1P65) / SIGT(KVIS), &
        (SIGT(K1P65) / SIGT(KVIS) - 1.0D0) * RV_MOD, 2.5 / LOG(10.0D0) * (SIGT(K1P65) / SIGT(KVIS) - 1.0D0) * SIGT(KVIS)
WRITE(16,'(1X, 1P4E11.3)') WEL(K1P25) * 1E4, SIGT(K1P25) / SIGT(KVIS), &
        (SIGT(K1P25) / SIGT(KVIS) - 1.0D0) * RV_MOD, 2.5 / LOG(10.0D0) * (SIGT(K1P25) / SIGT(KVIS) - 1.0D0) * SIGT(KVIS)
WRITE(16,'(1X, 1P4E11.3)') WEL(KVIS) * 1E4, SIGT(KVIS) / SIGT(KVIS), &
        (SIGT(KVIS) / SIGT(KVIS) - 1.0D0) * RV_MOD, 2.5 / LOG(10.0D0) * (SIGT(KVIS) / SIGT(KVIS) - 1.0D0) * SIGT(KVIS)
WRITE(16,'(1X, 1P4E11.3)') WEL(KBLUE) * 1E4, SIGT(KBLUE) / SIGT(KVIS), &
        (SIGT(KBLUE) / SIGT(KVIS) - 1.0D0) * RV_MOD, 2.5 / LOG(10.0D0) * (SIGT(KBLUE) / SIGT(KVIS) - 1.0D0) * SIGT(KVIS)
WRITE(16,'(1X, 1P4E11.3)') WEL(KUV) * 1E4, SIGT(KUV) / SIGT(KVIS), &
        (SIGT(KUV) / SIGT(KVIS) - 1.0D0) * RV_MOD, 2.5 / LOG(10.0D0) * (SIGT(KUV) / SIGT(KVIS) - 1.0D0) * SIGT(KVIS)

! -------------------------------------------
! ' Optical+FUSE range: 1. > wel > 0.0909, x < 11.0 in tau4fit and Kapp4fit'
! ' Optical+IUE range: 1. > wel > 0.1148, x < 8.7 in tau4fit and Kapp4fit'
! -------------------------------------------

! Loop over wavelengths (k = 1, mm)
    DO k = 1, mm  
    IF (WEL(k) .LE. 0.281D-4 .AND. WEL(k) .GE. 0.1249D-4) THEN
        WRITE(16, '(1X,1P4E11.3)') WEL(k) * 1E4, &
            SIGT(k) / SIGT(KVIS), (SIGT(k) / SIGT(KVIS) - 1.0D0) * RV_MOD, &
            2.5 / LOG(10.0D0) * (SIGT(k) / SIGT(KVIS) - 1.0D0) * SIGT(KVIS)
    END IF

    IF (WEL(k) .LE. 0.119D-4 .AND. WEL(k) .GE. 0.09D-4) THEN
        WRITE(16, '(1X,1P4E11.3)') WEL(k) * 1E4, &
            SIGT(k) / SIGT(KVIS), (SIGT(k) / SIGT(KVIS) - 1.0D0) * RV_MOD, &
            2.5 / LOG(10.0D0) * (SIGT(k) / SIGT(KVIS) - 1.0D0) * SIGT(KVIS)
    END IF
END DO
CLOSE(16)
    
! Open file for Kappa4fit output
OPEN(UNIT=18, FILE='./Output/Kappa4fit.out', FORM='FORMATTED')
REWIND 18

WRITE(18,*) '# Optical depth (Siebenmorgen 2023, A&A 670A,115; SC 2023 doi: 10.48550/arXiv.2311.03310)'
WRITE(18,*) "# wel(cm) sa_aC    ss_aC    sa_Si    ss_Si    sa_vgr   ss_vgr   sa_vsi   ss_vsi   s_pahS+B sa_Dark  ss_Dark sigt"
WRITE(18,*) "# --------------------------------------------------------------------------------------------------------------"

! Output for longest wavelength (k = 1)
k = 1
WRITE(18, '(1P1E10.3, 11E9.2, 1E10.3)') WEL(k), SIGA_AC(k), SIGS_AC(k), SIGA_SI(k), &
    SIGS_SI(k), SIGA_VGR(k), SIGS_VGR(k), SIGA_VSI(k), SIGS_VSI(k), &
    SIGA_PAHS(k) + SIGA_PAHB(k), SIGA_D(k), SIGS_D(k), SIGT(k)

! Output for other wavelengths
k = K2P3
WRITE(18, '(1P1E10.3, 11E9.2, 1E10.3)') WEL(k), SIGA_AC(k), SIGS_AC(k), SIGA_SI(k), &
    SIGS_SI(k), SIGA_VGR(k), SIGS_VGR(k), SIGA_VSI(k), SIGS_VSI(k), &
    SIGA_PAHS(k) + SIGA_PAHB(k), SIGA_D(k), SIGS_D(k), SIGT(k)

k = K1P65
WRITE(18, '(1P1E10.3, 11E9.2, 1E10.3)') WEL(k), SIGA_AC(k), SIGS_AC(k), SIGA_SI(k), &
    SIGS_SI(k), SIGA_VGR(k), SIGS_VGR(k), SIGA_VSI(k), SIGS_VSI(k), &
    SIGA_PAHS(k) + SIGA_PAHB(k), SIGA_D(k), SIGS_D(k), SIGT(k)

k = K1P25
WRITE(18, '(1P1E10.3, 11E9.2, 1E10.3)') WEL(k), SIGA_AC(k), SIGS_AC(k), SIGA_SI(k), &
    SIGS_SI(k), SIGA_VGR(k), SIGS_VGR(k), SIGA_VSI(k), SIGS_VSI(k), &
    SIGA_PAHS(k) + SIGA_PAHB(k), SIGA_D(k), SIGS_D(k), SIGT(k)

k = KVIS
WRITE(18, '(1P1E10.3, 11E9.2, 1E10.3)') WEL(k), SIGA_AC(k), SIGS_AC(k), SIGA_SI(k), &
    SIGS_SI(k), SIGA_VGR(k), SIGS_VGR(k), SIGA_VSI(k), SIGS_VSI(k), &
    SIGA_PAHS(k) + SIGA_PAHB(k), SIGA_D(k), SIGS_D(k), SIGT(k)

k = KBLUE
WRITE(18, '(1P1E10.3, 11E9.2, 1E10.3)') WEL(k), SIGA_AC(k), SIGS_AC(k), SIGA_SI(k), &
    SIGS_SI(k), SIGA_VGR(k), SIGS_VGR(k), SIGA_VSI(k), SIGS_VSI(k), &
    SIGA_PAHS(k) + SIGA_PAHB(k), SIGA_D(k), SIGS_D(k), SIGT(k)

k = KUV
WRITE(18, '(1P1E10.3, 11E9.2, 1E10.3)') WEL(k), SIGA_AC(k), SIGS_AC(k), SIGA_SI(k), &
    SIGS_SI(k), SIGA_VGR(k), SIGS_VGR(k), SIGA_VSI(k), SIGS_VSI(k), &
    SIGA_PAHS(k) + SIGA_PAHB(k), SIGA_D(k), SIGS_D(k), SIGT(k)
    
! Loop to check wavelength ranges and write corresponding values
DO k = 1, mm  
    IF (WEL(k) .LE. 0.281D-4 .AND. WEL(k) .GE. 0.1249D-4) THEN
        WRITE(18, '(1P1E10.3, 11E9.2, 1E10.3)') WEL(k), SIGA_AC(k), SIGS_AC(k), SIGA_SI(k), &
            SIGS_SI(k), SIGA_VGR(k), SIGS_VGR(k), SIGA_VSI(k), SIGS_VSI(k), &
            SIGA_PAHS(k) + SIGA_PAHB(k), SIGA_D(k), SIGS_D(k), SIGT(k)
    END IF

    IF (WEL(k) .LE. 0.119D-4 .AND. WEL(k) .GE. 0.09D-4) THEN
        WRITE(18, '(1P1E10.3, 11E9.2, 1E10.3)') WEL(k), SIGA_AC(k), SIGS_AC(k), SIGA_SI(k), &
            SIGS_SI(k), SIGA_VGR(k), SIGS_VGR(k), SIGA_VSI(k), SIGS_VSI(k), &
            SIGA_PAHS(k) + SIGA_PAHB(k), SIGA_D(k), SIGS_D(k), SIGT(k)
    END IF
END DO
    
CLOSE(18)


! Close previously opened units
CLOSE(UNIT=2)
CLOSE(UNIT=11)
CLOSE(UNIT=24)
CLOSE(UNIT=26)

PRINT*, ' +++                crossec_dd              DONE                     ++++++++++'
PRINT*, ' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
PRINT*, '        '

! ----------------------------------------------------------------------------
! c Strahlungsfeld ISRF  oder vom Stern oder AGN
! ----------------------------------------------------------------------------
! The following block of code is for calculating the radiation field:
! Uncomment and modify if necessary to handle the dissociation table file.
! ----------------------------------------------------------------------------
! IF (FAKISRF .LE. 1D-5 .AND. FAKISRF .GE. -999.0) THEN
!     OPEN(UNIT=24, FILE='dissociation.tab', FORM='FORMATTED')
!     WRITE(24,*) ' Rdust [AU]  vper [m/s]  sumevap'
!     PRINT*, ' Rdust [AU]  vper [m/s]  sumevap'
! END IF

! Initialize dust radius and loop to update
RDUST0 = RDUST
DO II = 1, 1
    print*,II
    ! Loop may be modified to iterate over a range
    RDUST = RDUST0 * 1.6**(II - 1)
    RR = RDUST
    SUMEVAP = 0D0
    TOTABS = 0D0
    TABSPAH = 0D0

    ! If ISRF factor is within a certain threshold, print and compute values
    IF (FAKISRF .LE. 1D-5 .AND. FAKISRF .GE. -999.0) THEN
        PRINT*, ' -------------------'
        PRINT*, ' '
        PRINT*, ' Compute for Rdust =', RDUST
        PRINT*, ' '
    END IF

    ! Call the ISRF subroutine for calculations
    CALL ISRF(RDUST, TOTLUM, TSTAR, POWL)



! ----------------------------------------------------------------------------
!  Berechnung der Emission pro g IM [erg/s/Hz/ster] of  MRN + Dark dust grains
! ----------------------------------------------------------------------------

    PRINT*, 'Type:  Radius    rho       en1csid   Mass      ah_csid   Temp'
    ! *** MRN aC - particles ***
    WRITE(6,110)
    110  FORMAT(' ***  MRN aC - Teilchen   ***')
    
    ! Set initial temperature
    TNEW = 150.0
    
    ! Loop over the range from Lac to Lec
    DO L = Lac, Lec
        SUMJV = 0.0
    
        ! Loop over the range for mm
        DO K = 1, MM
            QABS(K) = QAC(L, K)
    
            ! If blackbody, set QABS to 1
            IF (IBLACK .EQ. 1) THEN
                QABS(K) = 1D0
            END IF
    
            SUMJV = SUMJV + QABS(K) * J_V(K) * DFR(K)
        END DO
    
        ARAD = AMRN(L)
    
        ! Call tempmrn subroutine to update temperature
        CALL TEMPMRN(TNEW)
    
        ! Loop over mm for emission calculation
        DO K = 1, MM
            EMI_C(K) = EMI_C(K) + AHC(L) * QABS(K) * BPL(FR(K), FR3(K), TNEW)
    
            ! aC pol emission calculation
            IF (AMRN(L) .GE. ARAD_POLMIN_AC .AND. AMRN(L) .LE. ARAD_POLMAX) THEN
                EMIP_C(K) = EMIP_C(K) + AHC(L) * QPC(L, K) * BPL(FR(K), FR3(K), TNEW)
            END IF
        END DO
    
        ! Print results for the last iteration in the loop
        IF (L .EQ. LEC) THEN
            WRITE(6, '(A6, 1P6E10.2)') '  aC : ', AMRN(L), RHC, EN1C, ACmass, AHC(L), TNEW
        END IF
    END DO

! ----------------------------------------------------------------------------
! c              ***  MRN silicate - particles   ***
! ----------------------------------------------------------------------------
    WRITE(6,111)
    111 FORMAT(' ***  MRN silicate - particles   ***')

    ! Set initial temperature
    TNEW = 150.0

    ! Loop over the range from lasi to lesi
    DO L = LASI, LESI
        SUMJV = 0.0

        ! Loop over the range of mm
        DO K = 1, MM
            QABS(K) = QASI(L, K)

            ! If blackbody, set QABS to 1
            IF (IBLACK .EQ. 1) THEN
                QABS(K) = 1.0
            END IF

            SUMJV = SUMJV + QABS(K) * J_V(K) * DFR(K)
        END DO

        ARAD = AMRN(L)

        ! Call tempmrn subroutine to update temperature
        CALL TEMPMRN(TNEW)

        ! Loop over mm for emission calculation
        DO K = 1, MM
            EMI_SI(K) = EMI_SI(K) + AHSI(L) * QABS(K) * BPL(FR(K), FR3(K), TNEW)

            ! Si pol emission calculation
            IF (AMRN(L) .GE. ARAD_POLMIN_SI .AND. AMRN(L) .LE. ARAD_POLMAX) THEN
                EMIP_SI(K) = EMIP_SI(K) + AHSI(L) * QPSI(L, K) * BPL(FR(K), FR3(K), TNEW)
            END IF
        END DO

        ! Print results for the last iteration in the loop
        IF (L .EQ. LESI) THEN
            WRITE(6, '(A6, 1P6E10.2)') '  Si: ', AMRN(L), RHSI, EN1Si, SIMass, AHSI(L), TNEW
        END IF
    END DO


! ----------------------------------------------------------------------------
!               ***  Dark Dust   ***
! ----------------------------------------------------------------------------
    WRITE(6,112)
    112 FORMAT(' ***  Dark Dust   ***')

    ! Set initial temperature
    TNEW = 150.0

    ! Loop over the range from Lad to Led
    DO L = LAD, LED
        SUMJV = 0.0

        ! Loop over the range for mm
        DO K = 1, MM
            QABS(K) = QAD(L, K)

            ! If blackbody, set QABS to 1
            IF (IBLACK .EQ. 1) THEN
                QABS(K) = 1.0
            END IF

            SUMJV = SUMJV + QABS(K) * J_V(K) * DFR(K)
        END DO

        ARAD = ADARK(L)

        ! Call tempmrn subroutine to update temperature
        CALL TEMPMRN(TNEW)

        ! Loop over mm for emission calculation
        DO K = 1, MM
            EMI_D(K) = EMI_D(K) + AHD(L) * QABS(K) * BPL(FR(K), FR3(K), TNEW)

            ! Dark pol emission calculation
            IF (AMRN(L) .GE. ARAD_POLMIN_aC .AND. AMRN(L) .LE. ARAD_POLMAX) THEN
                EMIP_D(K) = EMIP_D(K) + AHD(L) * QPD(L, K) * BPL(FR(K), FR3(K), TNEW)
            END IF
        END DO

        ! Print results for the first and last iterations in the loop
        IF (L .EQ. LAD .OR. L .EQ. LED) THEN
            WRITE(6, '(A6, 1P5E10.2)') ' Dark:', ADARK(L), RHD, EN1D, AHD(L), TNEW
        END IF
    END DO


! ----------------------------------------------------------------------------
!             Emission of  Very Small Graphites (VSG)
!    VSGs consist only of Carbon, hence material = 0
! ----------------------------------------------------------------------------

! Check if processing should continue based on ispecvsg and abucvgr
    IF (ISPECVSG .EQ. 0) THEN
        GO TO 502
    END IF
    IF (ABUCVGR .EQ. 0) THEN
        GO TO 501
    END IF
    
    ! *** VSG graphite emission calculation ***
    WRITE(6, 113)
    113 FORMAT(' ***  VSG graphite  ***')
    
    MATERIAL = 0
    
    ! Loop over the range from lav to lev for VSG calculations
    DO L = LAV, LEV
        ARAD = AVSG(L)
        XATOM = PI4 / 3.D0 * RHGR / PROTM / WMOLC * ARAD**3
        SUMJV = 0.D0
        SUM = 0.D0
    
        ! Loop over the range of mm for the absorption and scattering
        DO K = 1, MM
            QABS(K) = QAGR(L, K)
            QSCA(K) = QSGR(L, K)
            SUMJV = SUMJV + QABS(K) * J_V(K) * DFR(K)
    
            ! If k >= kjot, accumulate the sum based on specific conditions
            IF (K .GE. KJOT) THEN
                SUM = SUM + QABS(K) * J_V(K) / HWIRK / FR(K) * DFR(K)
            END IF
        END DO
    

! ----------------------------------------------------------------------------
!   Prüfe, ob Fluktuations-Rechnung überhaupt nötig
! ----------------------------------------------------------------------------

! Calculate the fluctuation timescales and conditions for emission
        ZEITPHOT = 1.D0 / (PI4 * PI * ARAD**2 * SUM)
        TNEW = 150.D0
        CALL TEMPMRN(TNEW)
        TMRN = TNEW
        ZEITMRN = 2.D0 * (ARAD / 4.D-7)**6
        ZEITFEIN = 3.D0 * ZEITMRN
        
        ! Debug print if ibug >= 3
        IF (IBUG .GE. 3) THEN
            ! PRINT 240, ZEITPHOT, ZEITFEIN, ZEITMRN, TMRN
            240 FORMAT(' dt_UV_Abs [s] =', 1PE10.2, ' t_fein=', E10.2, &
            ' t_mrn=', E10.2, ' T_mrn =', 12E10.2)
        END IF
    
        ! Set flag for VSG evaporation
        IEVAPVSG = 0
    
        ! Check if fluctuation time is smaller than the MRN time
        print*,zeitphot,zeitmrn, arad
        IF (ZEITPHOT .LT. ZEITMRN) THEN
            ! Calculate emission for each k if fluctuation time is smaller
            DO K = 1, MM
                EMIS(K) = PI * ARAD**2 * QABS(K) * BPL(FR(K), FR3(K), TMRN)
                ! PRINT*,EMIS(K)
            END DO
        ELSE
            ! Call the VSG function if fluctuation time is greater
            ! print*,'here', l
            CALL VSG
        END IF

    ! -------------------------------------------
    !    Emission pro g MRN-Staub [erg/s/Hz/ster].  Extinction der vsg.
    !    ievapvsg = 1:  Verdampfung.
    ! ----------------------------------------------------------------------------

        IF (IEVAPVSG .EQ. 1) THEN
            ! If VSG is evaporating, print the values for arad and sumevap
            WRITE(6, 200) ARAD, SUMEVAP
            WRITE(3, 200) ARAD, SUMEVAP
            200 FORMAT('vGr evaporate   arad, sumevap =', 1P2E9.2)
        ELSE
            ! If VSG is not evaporating, update the emission, absorption, and scattering values
            DO K = 1, MM
                ! PRINT*, EMI_VGR(K), EMIS(K), PI, ARAD, AHVGR(L)
                EMI_VGR(K) = EMI_VGR(K) + EMIS(K) / PI / ARAD**2 * AHVGR(L)
                SIGA_VGR(K) = SIGA_VGR(K) + AHVGR(L) * QABS(K)
                SIGS_VGR(K) = SIGS_VGR(K) + AHVGR(L) * QSCA(K)
            END DO

            ! Initialize totals
            TOTABS = 0.D0
            TOTEMIS = 0.D0
        
            ! Calculate the total absorption and emission
            DO K = 1, MM
                TOTABS = TOTABS + PI4 * PI * ARAD**2 * QABS(K) * J_V(K) * DFR(K)
                ! The line below is commented out, but it would add the emission calculation
                ! TOTEMIS = TOTEMIS + PI4 * PI * ARAD**2 * QABS(K) * BPL(FR(K), FR3(K), TMRN) * DFR(K)
                TOTEMIS = TOTEMIS + PI4 * EMIS(K) * DFR(K)
            END DO
    
            ! Calculate the absorption-to-emission ratio
            RATIO = TOTABS / TOTEMIS
    
            ! Print the results for total absorption, ratio, and arad
            WRITE(6, 936) TOTABS, RATIO, ARAD
        END IF
    END DO

! end loop over all vsg radii
! ----------------------------------------------------------------------------
!   Emission of  Very Small Silicates
!   vsg bestehen nur aus Si-grains, daher material = 1
! ----------------------------------------------------------------------------

501 CONTINUE

IF (ABUVSI .EQ. 0) THEN
    GO TO 502
END IF
    
! ----------------------------------------------------------------------------
!               ***  VSG silicates  ***
! ----------------------------------------------------------------------------
WRITE(6, 114)
114 FORMAT(' ***  VSG silicates  ***')

MATERIAL = 1

DO 101 L = LAV, LEV
    ARAD = AVSG(L)
    SUMJV = 0.D0
    SUM = 0.D0

    DO 16 K = 1, MM
        QABS(K) = QAVSI(L, K)
        QSCA(K) = QSVSI(L, K)
        SUMJV = SUMJV + QABS(K) * J_V(K) * DFR(K)
        
        IF (K .GE. KJOT) THEN
            SUM = SUM + QABS(K) * J_V(K) / HWIRK / FR(K) * DFR(K)
        END IF
    16 CONTINUE



! ----------------------------------------------------------------------------
!   Prüfe, ob Fluktuations-Rechnung überhaupt nötig
! ----------------------------------------------------------------------------

    ZEITPHOT  = 1D0 / (PI4 * PI * ARAD**2 * SUM)
    TNEW = 150.D0
    CALL TEMPMRN(TNEW)
    TMRN = TNEW
    ZEITMRN  = 2D0 * (ARAD / 4D-7)**6
    ZEITFEIN = 3D1 * ZEITMRN
    
    IF (IBUG .GE. 3) THEN
        PRINT 240, ZEITPHOT, ZEITFEIN, ZEITMRN, TMRN
    ! 240 FORMAT(' dt_UV_Abs [s] =', 1PE10.2, ' t_fein=', E10.2, ' t_mrn=', E10.2, ' T_mrn =', 12E10.2)
    END IF
    
    IEVAPVSG = 0
    
    IF (ZEITPHOT .LT. ZEITMRN .OR. ARAD .GE. 50D-8) THEN
        DO  K = 1, MM
            EMIS(K) = PI * ARAD**2 * QABS(K) * BPL(FR(K), FR3(K), TMRN)
        END DO
    ELSE
        CALL VSG
    END IF
    
    ! ----------------------------------------------------------------------------
    !   Emission pro g MRN-Staub [erg/s/Hz/ster]. Extinktion der vsg.
    !   IEVAPVSG = 1: Verdampfung.
    ! ----------------------------------------------------------------------------
    
    IF (IEVAPVSG .EQ. 1) THEN
        WRITE(3, 201) ARAD, SUMEVAP
        WRITE(6, 201) ARAD, SUMEVAP
    201 FORMAT('vSi evaporate   ARAD, SUMEVAP =', 1P2E9.2)
    ELSE
        DO K = 1, MM
            EMI_VSI(K)  = EMI_VSI(K)  + EMIS(K) / PI / ARAD**2 * AHVSI(L)
            SIGA_VSI(K) = SIGA_VSI(K) + AHVSI(L) * QABS(K)
            SIGS_VSI(K) = SIGS_VSI(K) + AHVSI(L) * QSCA(K)
        END DO
        
        TOTABS = 0.D0
        TOTEMIS = 0.D0
        DO K = 1, MM
            TOTABS = TOTABS + PI4 * PI * ARAD**2 * QABS(K) * J_V(K) * DFR(K)
            ! TOTEMIS = TOTEMIS + PI4 * PI * ARAD**2 * QABS(K) * BPL(FR(K), FR3(K), TMRN) * DFR(K)
            TOTEMIS = TOTEMIS + PI4 * EMIS(K) * DFR(K)
        END DO
        RATIO = TOTABS / TOTEMIS
        WRITE(6, 937) TOTABS, RATIO, ARAD
    ! 937 FORMAT('Total Absorption: ', E10.2, ' Ratio: ', E10.2, ' ARAD: ', E10.2)
    END IF
    
101 CONTINUE
! End loop over all VSG radii



936 FORMAT(" absEnergy=", 1PE9.2, ' abs/emis= ', E9.2, " vGr a=  ", E9.2)
937 FORMAT(" absEnergy=", 1PE9.2, ' abs/emis= ', E9.2, " vSi a=  ", E9.2)

502 CONTINUE

! ----------------------------------------------------------------------------
!      Emission of P A Hs         (PAHs bestehen aus C ==> material = 0)
! ----------------------------------------------------------------------------
! Hier:   xatom = No of C + H atoms
! ----------------------------------------------------------------------------
    
IF (ISPECPAH .EQ. 0) GO TO 503
    
! *** small PAH ***
WRITE(6, 115)
115 FORMAT(' *** small PAH  ***')
    
WRITE(6, '(A30, 2F7.1)') '     small PAH: ZCPAH, ZHPah = ', ZCPAH, ZHPah
    
TOTABS = 0.D0
SUMEVAP = 0.D0
MATERIAL = 0
ZCPAH = ZCPAHS
ZHPah = ZHPAHS
XATOM = ZCPAH + ZHPah
ABUCPAH = ABUCPAHS
ARAD = SQRT(ZCPAH / 1.2) * 1D-8
    
! Loop over wavelengths (mm) and assign absorption values
DO K = 1, MM
    QABS(K) = QABSPAHS(K)
END DO
    
! Call subroutine to compute emission for small PAHs
CALL PAH_EMIS

! Debugging: Output values for verification
IF (IBUG .GE. 3) THEN
    WRITE(22, '(1P1E10.2)') ARAD
    WRITE(22, '(1P2E10.2)') (TEM(I), PW(I), I = 1, NNTVSG)
END IF
    
! ----------------------------------------------------------------------------
!   Umrechnung auf Emission pro g IM [erg/s/Hz/ster]. 
!   FAK = Zahl der PAHs pro g IM
! ----------------------------------------------------------------------------

pahmass = abucpah * wmolc  / (abuc_tot*wmolc + abusi *wmolsi  + &
                                               abuvsi*wmolvsi)
 fak = pahmass/(wmolc*zcpah*protm)  
 do  77  k = 1, mm
 emi_pahs(k) = emis(k) * fak
!  print*,emi_pahs(k)
77  continue

! ----------------------------------------------------------------------------
!   Emission of Big PAHs
!   ========================
!   Big PAHs are treated similarly to small PAHs but with different parameters.
! ----------------------------------------------------------------------------

! Update zcpah for big PAHs
ZCPAH = ZCPAHB

! If zcpah is less than 1, skip the calculation for big PAHs
IF (ZCPAH .LT. 1.0) GO TO 503

! *** Big PAH ***
WRITE(6, 116)
116 FORMAT(' *** Big PAH ***')

WRITE(6, *) '     Big PAH: ZCPAH, ZHPah'
WRITE(6, '(16X, 2F7.1)') ZCPAH, ZHPah

TOTABS = 0.D0
SUMEVAP = 0.D0
    
! Update ZHPah for big PAHs
ZHPah = ZHPahB

! Calculate total number of atoms in the big PAH
XATOM = ZCPAH + ZHPah

! Update abucpah for big PAHs
ABUCPAH = ABUCPAHSB

! Calculate radius of the big PAH
ARAD = SQRT(ZCPAH / 1.2) * 1D-8

! Loop over wavelengths and assign absorption values for big PAHs
DO K = 1, MM
    QABS(K) = QABSPAHB(K)
END DO
    
! Call subroutine to compute emission for big PAHs
CALL PAH_EMIS

! Debugging: Output values for verification if needed
IF (IBUG .GE. 3) THEN
    WRITE(22, '(1P1E10.2)') ARAD
    WRITE(22, '(1P2E10.2)') (TEM(I), PW(I), I = 1, NNTVSG)
END IF

! ----------------------------------------------------------------------------
!   Convert emission to per gram IM [erg/s/Hz/ster] for Big PAHs
!   FAK = Number of Big PAHs per gram of interstellar medium (IM)
! ----------------------------------------------------------------------------

! Calculate the mass of PAHs
PAHMASS = ABUCPAH * WMOLC / (ABUC_TOT * WMOLC + ABUSI * WMOLSI + ABUVSI * WMOLVSI)

! Calculate the factor for PAH number per gram IM
FAK = PAHMASS / (WMOLC * ZCPAH * PROTM)

! Loop to apply the factor and compute emission for big PAHs
DO K = 1, MM
    EMI_PAHB(K) = EMIS(K) * FAK
END DO


503 CONTINUE

! ----------------------------------------------------------------------------
!   Store total absorption in tabspah for future reference
! ----------------------------------------------------------------------------
TABSPAH = TOTABS

! ----------------------------------------------------------------------------
!    Compute mean photon energy
! ----------------------------------------------------------------------------
! Uncomment the following block if you want to compute and print the mean photon energy

! sumz = 0.D0
! sumn = 0.D0
! DO K = 1, MM-1
!     sumz = sumz + J_V(K) * DFR(K)
!     sumn = sumn + J_V(K) / HWIRK / FR(K) * DFR(K)
! END DO

! Write out the mean photon energy in eV
! WRITE(6,*) ' <hv> (eV)', sumz / sumn / EVOLT

1000 CONTINUE   ! End loop over radii


! ----------------------------------------------------------------------------
!    Total Dust Emission [erg/s/Hz/ster] per g IM
! ----------------------------------------------------------------------------

! Calculate maximum emission from MRN
FAK = MAXVAL(EMI_C + EMI_SI)
! DO K = 1, MM
    ! print*, emi_pahs(k), emi_pahb(k)
! end do
! Loop over wavelengths (k) and set small emissions to zero
DO K = 1, MM
    IF (EMI_C(K) .LT. FAK * 1.D-20) EMI_C(K) = 0.D0
    IF (EMI_SI(K) .LT. FAK * 1.D-20) EMI_SI(K) = 0.D0
    IF (EMI_VGR(K) .LT. FAK * 1.D-20) EMI_VGR(K) = 0.D0
    IF (EMI_VSI(K) .LT. FAK * 1.D-20) EMI_VSI(K) = 0.D0
    IF (EMI_PAHS(K) .LT. FAK * 1.D-20) EMI_PAHS(K) = 0.D0
    IF (EMI_PAHB(K) .LT. FAK * 1.D-20) EMI_PAHB(K) = 0.D0
    IF (EMI_D(K) .LT. FAK * 1.D-20) EMI_D(K) = 0.D0

    ! Compute total emission for each wavelength
    EMIS(K) = EMI_C(K) + EMI_SI(K) + EMI_VGR(K) + EMI_VSI(K) + EMI_PAHS(K) + EMI_PAHB(K) + EMI_D(K)
END DO
    
! Calculate and print the maximum emission
EMISMAX = MAXVAL(EMIS)
PRINT *, ' '
WRITE(6, '(A42, 1P1E12.3)') 'Peak total dust emission max(EMIS) =', EMISMAX

! Calculate total emission for each component and filter small values
EMIP_T = EMIP_C + EMIP_SI + EMIP_D
DO K = 1, MM
    IF (EMIS(K) .LE. 1.E-40) EMI_C(K) = 0.D0
    IF (EMI_D(K) .LE. 1.E-40) EMI_D(K) = 0.D0
    IF (EMI_C(K) .LE. 1.E-40) EMI_C(K) = 0.D0
    IF (EMI_SI(K) .LE. 1.E-40) EMI_SI(K) = 0.D0
    IF (EMIP_C(K) .LE. 1.E-40) EMIP_C(K) = 0.D0
    IF (EMIP_SI(K) .LE. 1.E-40) EMIP_SI(K) = 0.D0
    IF (EMIP_D(K) .LE. 1.E-40) EMIP_D(K) = 0.D0
    IF (EMIP_T(K) .LE. 1.E-40) EMIP_T(K) = 0.D0
    IF (EMI_VGR(K) .LE. 1.E-40) EMI_VGR(K) = 0.D0
    IF (EMI_VSI(K) .LE. 1.E-40) EMI_VSI(K) = 0.D0
    IF (EMI_PAHS(K) .LE. 1.E-40) EMI_PAHS(K) = 0.D0
    IF (EMI_PAHB(K) .LE. 1.E-40) EMI_PAHB(K) = 0.D0
    IF (J_V(K) .LE. 1.E-40) J_V(K) = 0.D0
END DO

! ----------------------------------------------------------------------------
!   Polarisation in the optical and polarized intensity at 850mu
! ----------------------------------------------------------------------------

WRITE(6,*) '# ----------- Polarisation p and polarisation efficiency p/tauV '
pV = (sigp_aC(kvis) + sigp_Si(kvis) + sigp_d(kvis)) / sigt(kvis)
pVmax = 0.D0

! Loop to calculate maximum polarization in the range [-10, 20]
DO k = -10, 20
    pVmax = MAX(pVmax, (sigp_aC(kvis + k) + sigp_Si(kvis + k) + sigp_d(kvis + k)) / sigt(kvis + k))
END DO

CALL locat(wel, mm, 850.D-4, k850)

! Writing polarization data to standard output
WRITE(6, '(A40, 2F7.4)') ' *** Omega=60deg:     pV, pVmax = ', 100.D0 * pV, 100.D0 * pVmax
WRITE(6, '(A40, F6.1, 1P2E12.3)') ' *** at 850: w (mu) Ipol, Itot  = ', 1E4 * wel(k850), emip_t(k850), emis(k850)
WRITE(6,*) ' '
WRITE(6,*) '                      tauV    P_Serk  pV      p850/pV     p850/pV/tauV'
WRITE(6, '(A20, 4F8.3, 9X, 1F7.3)') '  Pol (Omega=60deg) ', tauV, P_Serk, 100.D0 * pV, 100.D0 * emip_t(k850) / emis(k850) / P_Serk, 100.D0 * (emip_t(k850) / emis(k850)) / (P_Serk / tauV)

! Outputting emission data to file
OPEN(UNIT=4, FILE='./Output/emis.out', FORM='FORMATTED')
REWIND 4
WRITE(4,*) '#   Flux in [erg/s/Hz/ster] per g IM'
WRITE(4,*) '# wel(mu)  emis_tot  emis_c    emis_Si   emis_pol  emi_Dark  emi_vgr   emi_vsi   epah_s+b  ISRF j_v'
WRITE(4, '(1P10E10.2)') (wel(k) * 1D4, emis(k), emi_c(k), emi_si(k), emip_t(k), emi_d(k), emi_vgr(k), emi_vsi(k), emi_pahs(k) + emi_pahb(k), MAX(j_v(k), 1D-40), k = 1, mm)
CLOSE(4)
    
! Outputting polarized emission data to file
OPEN(UNIT=4, FILE='./Output/emipol.out', FORM='FORMATTED')
REWIND 4
WRITE(4,*) '#   Polarized Flux in [erg/s/Hz/ster] per g IM'
WRITE(4,*) '# wel(mu)  emis      emip_t    emip_c    emip_Si   emip_d'
WRITE(4, '(1P6E10.2)') (wel(k) * 1D4, emis(k), emip_t(k), emip_c(k), emip_si(k), emip_d(k), k = 1, mm)
CLOSE(4)

! Print final message about the dark dust model
PRINT *, ' ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
PRINT *, ' +++ Dark dust model (Siebennmorgen 2023 A&A, 670A, 115                     +++'
PRINT *, ' +++ Amount of dark dust: Eq.2,3 in SC 2023 doi: 10.48550/arXiv.2311.03310) +++'
PRINT *, ' +++               jsmD2              DONE                     ++++++++++'
PRINT *, '        '


END DO
END PROGRAM jsm2Dpol
