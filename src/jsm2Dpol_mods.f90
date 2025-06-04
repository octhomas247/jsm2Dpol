MODULE config

    IMPLICIT NONE
    INTEGER, PARAMETER :: nf = 283
    INTEGER, PARAMETER :: nnn = 500
    INTEGER, PARAMETER :: nres = 20
    INTEGER, PARAMETER :: lrv = 5
    INTEGER, PARAMETER :: lr = 100
    INTEGER, PARAMETER :: lrsi = 84
    INTEGER, PARAMETER :: lrd = 201

END MODULE config


MODULE constants

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
    Msun   = 1.989D33       ! Solar mass (g)

    REAL(KIND=8), PARAMETER :: fevap = 1.0D-8

    REAL(KIND=8) :: &
    rhsi    = 3.4D0,      &
    rhvsi   = 3.5D0,      &
    wmolsi  = 134.5D0,    &
    wmolvsi = 134.5D0

    REAL(KIND=8) :: &
    rhc    = 1.6D0,       &
    rhgr   = 2.24D0,      &
    wmolc  = 12.0D0

END MODULE constants


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
