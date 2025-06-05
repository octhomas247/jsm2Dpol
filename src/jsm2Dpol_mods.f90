MODULE constants
    IMPLICIT NONE
    INTEGER, PARAMETER :: nf = 283
    INTEGER, PARAMETER :: nnn = 500
    INTEGER, PARAMETER :: nres = 20
    INTEGER, PARAMETER :: lrv = 5
    INTEGER, PARAMETER :: lr = 100
    INTEGER, PARAMETER :: lrsi = 84
    INTEGER, PARAMETER :: lrd = 201
END MODULE constants


MODULE tempmrn_mod
    USE constants
    IMPLICIT NONE
    REAL(KIND=8), DIMENSION(nf) :: fr, fr3, dfr, qabs, qsca
    REAL(KIND=8), DIMENSION(nf) :: emis, j_v
END MODULE tempmrn_mod


MODULE effi_mod
    USE constants
    IMPLICIT NONE
    REAL(KIND=8), DIMENSION(lr,nf) :: qac, qsc, gc, qavsi, qpc, qpsi, qcpc, qcpsi
    REAL(KIND=8), DIMENSION(lrsi,nf) :: qasi, qssi, gsi
    REAL(KIND=8), DIMENSION(lrv,nf) :: qagr, qsgr, ggr, qsvsi, gvsi
    REAL(KIND=8), DIMENSION(lrd,nf) :: qpd, qcpd, qad, qsd, gd
    REAL(KIND=8), DIMENSION(nf) :: qabspahs, qabspahb
END MODULE effi_mod


MODULE vsg_mod
    USE constants
    IMPLICIT NONE
    REAL(KIND=8), DIMENSION(lrv) :: avsg, ahvgr, ahvsi
    REAL(KIND=8) :: ahpah, ahpahs, ahpahb, xatom
    REAL(KIND=8), DIMENSION(nnn,nnn+1) :: at
    REAL(KIND=8), DIMENSION(nnn,nnn) :: bt
    REAL(KIND=8), DIMENSION(nnn) :: un, dun, dtun, aconti, tem, pw
END MODULE vsg_mod


MODULE vec_lr_mod
    USE constants
    IMPLICIT NONE
    REAL(KIND=8), DIMENSION(lr) :: ahc, amrn, ahsi
    REAL(KIND=8), DIMENSION(lrd) :: adark, ahd
    REAL(KIND=8) :: en1c, en1si, en1d, fakgr, faksi
END MODULE vec_lr_mod


MODULE vec_nf_mod
    USE constants
    IMPLICIT NONE
    REAL(KIND=8), DIMENSION(nf) :: wel, emi_c, emi_si
END MODULE vec_nf_mod


MODULE vec_sig_mod
    USE constants
    IMPLICIT NONE
    REAL(KIND=8), DIMENSION(nf) :: sigt, sigs
    REAL(KIND=8), DIMENSION(nf) :: siga_vgr, sigs_vgr, siga_vsi, sigs_vsi
    ! REAL(KIND=8), DIMENSION(nf) :: siga_pah, siga_pahs, siga_pahb, sigas
    REAL(KIND=8), DIMENSION(nf) :: siga_pah, siga_pahs, siga_paha, siga_pahb, sigas
    REAL(KIND=8), DIMENSION(nf) :: siga_aC, sigs_aC, siga_Si, sigs_Si
    REAL(KIND=8), DIMENSION(nf) :: sigp_aC, sigcp_aC, sigp_Si, sigcp_Si
    REAL(KIND=8), DIMENSION(nf) :: siga_d, sigs_d, sigt_d, sigp_d, sigcp_d
    REAL(KIND=8), DIMENSION(nf) :: sigt_l
END MODULE vec_sig_mod


MODULE fest_mod
    USE constants
    IMPLICIT NONE
    INTEGER :: igasabs, ibug, iblack
    INTEGER :: mm, mm1, mmold, mm1old
    INTEGER :: kvis, kblue, klyc
    INTEGER :: lac, lec, lasi, lesi, lav, lev, lad, led
    INTEGER :: ispecvsg, ispecpah, material, ievap
    INTEGER :: nnTvsg
    INTEGER, DIMENSION(nres) :: ir_res
END MODULE fest_mod


MODULE paheva_mod
    IMPLICIT NONE
    REAL(KIND=8) :: rr, tauV
END MODULE paheva_mod


MODULE const_mod
    IMPLICIT NONE
    REAL(KIND=8) :: pi, pi4
    REAL(KIND=8) :: AU
    REAL(KIND=8) :: rhd
    REAL(KIND=8) :: qmrn, qvsg, sumjv, arad
    REAL(KIND=8) :: zcpahs, zhpahs, zcpahb, zhpahb, zcpah, zhpah
    REAL(KIND=8) :: fakisrf, Tmrn, Tevap, zeitphot, zeitfein
    REAL(KIND=8) :: sumevap, ratio, totabs, Eb, vper

    REAL(KIND=8), PARAMETER :: &
    hwirk  = 6.6262D-27,  & ! Planck constant (erg s)
    clicht = 2.997925D10, & ! Speed of light (cm/s)
    protm  = 1.672D-24,   & ! Proton mass (g)
    boltz  = 1.38062D-16, & ! Boltzmann constant (erg/K)
    sigma  = 5.669563D-5, & ! Stefan-Boltzmann constant (erg/cm^2/s/K^4)
    eVolt  = 1.602D-12,   & ! Energy of 1 eV (erg)
    tbb    = 2.9D0,       & ! Some temperature or constant (dimensionless?)
    Grav   = 6.673D-8,    & ! Gravitational constant (cm^3/g/s^2)
    Lsun   = 3.846D33,    & ! Solar luminosity (erg/s)
    Msun   = 1.989D33     ! Solar mass (g)

    REAL(KIND=8), PARAMETER :: fevap = 1.0D-8

    ! REAL(KIND=8), PARAMETER :: &
    REAL(KIND=8) :: &
    rhsi    = 3.4D0,      &
    rhvsi   = 3.5D0,      &
    wmolsi  = 134.5D0,    &
    wmolvsi = 134.5D0

    ! REAL(KIND=8), PARAMETER :: &
    REAL(KIND=8) :: &
    rhc    = 1.6D0,       &
    rhgr   = 2.24D0,      &
    wmolc  = 12.0D0

END MODULE const_mod


MODULE paraDark_mod
    IMPLICIT NONE
    REAL(KIND=8) :: fdark, Ebv_obs, xnd, xnl
END MODULE paraDark_mod


MODULE abu_mod
    IMPLICIT NONE
    REAL(KIND=8) :: abuc, abuc_tot, abusi, abuvsi, abucvgr
    REAL(KIND=8) :: abucpah, abucpahs, abucpahb
    REAL(KIND=8) :: aCmass, Simass, vsimass
    REAL(KIND=8) :: pahmass, pahmasss, pahmassb
    REAL(KIND=8) :: vgrmass, tmassMRN, tmassDark
END MODULE abu_mod



! USE tempmrn_mod
! USE effi_mod
! USE vsg_mod
! USE vec_lr_mod
! USE vec_nf_mod
! USE vec_sig_mod
! USE fest_mod
! USE paheva_mod
! USE paraDark_mod
! USE abu_mod
! USE rdark_mod

MODULE test
    USE constants
    IMPLICIT NONE

    REAL(KIND=8), DIMENSION(nf) :: fr, fr3, dfr, qabs, qsca
    REAL(KIND=8), DIMENSION(nf) :: emis, j_v

    REAL(KIND=8), DIMENSION(lr,nf) :: qac, qsc, gc, qavsi, qpc, qpsi, qcpc, qcpsi
    REAL(KIND=8), DIMENSION(lrsi,nf) :: qasi, qssi, gsi
    REAL(KIND=8), DIMENSION(lrv,nf) :: qagr, qsgr, ggr, qsvsi, gvsi
    REAL(KIND=8), DIMENSION(lrd,nf) :: qpd, qcpd, qad, qsd, gd
    REAL(KIND=8), DIMENSION(nf) :: qabspahs, qabspahb

    REAL(KIND=8), DIMENSION(lrv) :: avsg, ahvgr, ahvsi
    REAL(KIND=8) :: ahpah, ahpahs, ahpahb, xatom
    REAL(KIND=8), DIMENSION(nnn,nnn+1) :: at
    REAL(KIND=8), DIMENSION(nnn,nnn) :: bt
    REAL(KIND=8), DIMENSION(nnn) :: un, dun, dtun, aconti, tem, pw

    REAL(KIND=8), DIMENSION(lr) :: ahc, amrn, ahsi
    REAL(KIND=8), DIMENSION(lrd) :: adark, ahd
    REAL(KIND=8) :: en1c, en1si, en1d, fakgr, faksi

    REAL(KIND=8), DIMENSION(nf) :: wel, emi_c, emi_si

    REAL(KIND=8), DIMENSION(nf) :: sigt, sigs
    REAL(KIND=8), DIMENSION(nf) :: siga_vgr, sigs_vgr, siga_vsi, sigs_vsi
    REAL(KIND=8), DIMENSION(nf) :: siga_pah, siga_pahs, siga_paha, siga_pahb, sigas
    REAL(KIND=8), DIMENSION(nf) :: siga_aC, sigs_aC, siga_Si, sigs_Si
    REAL(KIND=8), DIMENSION(nf) :: sigp_aC, sigcp_aC, sigp_Si, sigcp_Si
    REAL(KIND=8), DIMENSION(nf) :: siga_d, sigs_d, sigt_d, sigp_d, sigcp_d
    REAL(KIND=8), DIMENSION(nf) :: sigt_l

    INTEGER :: igasabs, ibug, iblack
    INTEGER :: mm, mm1, mmold, mm1old
    INTEGER :: kvis, kblue, klyc
    INTEGER :: lac, lec, lasi, lesi, lav, lev, lad, led
    INTEGER :: ispecvsg, ispecpah, material, ievap
    INTEGER :: nnTvsg
    INTEGER, DIMENSION(nres) :: ir_res

    REAL(KIND=8) :: rr, tauV

    REAL(KIND=8) :: fdark, Ebv_obs, xnd, xnl

    REAL(KIND=8) :: abuc, abuc_tot, abusi, abuvsi, abucvgr
    REAL(KIND=8) :: abucpah, abucpahs, abucpahb
    REAL(KIND=8) :: aCmass, Simass, vsimass
    REAL(KIND=8) :: pahmass, pahmasss, pahmassb
    REAL(KIND=8) :: vgrmass, tmassMRN, tmassDark

    REAL(KIND=8) :: arad_polmin_aC, arad_polmin_Si
    REAL(KIND=8) :: arad_polmax, aled

END MODULE test

MODULE rdark_mod
    IMPLICIT NONE
    REAL(KIND=8) :: arad_polmin_aC, arad_polmin_Si
    REAL(KIND=8) :: arad_polmax, aled
END MODULE rdark_mod

MODULE functions
    IMPLICIT NONE
    CONTAINS
    FUNCTION BPL(FR, FR3, T) RESULT(BPL_OUT)
        REAL(KIND=8), INTENT(IN) :: FR, FR3, T
        REAL(KIND=8)             :: C1, C2, X, BPL_OUT
        
        C1 = 1.4745D-47
        C2 = 4.7994D-11
        
        X = C2 * FR / T
    
        IF (X > 4.0D2) THEN
            BPL_OUT = 0.0D0
        ELSE
            BPL_OUT = C1 * FR3 / (EXP(X) - 1.0D0)
        END IF
    END FUNCTION BPL
END MODULE functions



