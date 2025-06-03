# JSM2DPOL

Test Outputs the dust cross-sections, intensity and polarised emission for the three-component model by Siebenmorgen R., 2023, A&A, 670A, 115. The relative masses of the components are derived from input dust abundances and size distributions. The relative mass of submicron-sized grains and the cross-sections ($cm^2/g$ ISM dust) are computed in the subroutine `sigtDark_EbvAvPol`.


## Dust Model
1. Cross-sections: Based on Siebenmorgen R. (2023, A&A, 670A, 115).
2. Column density estimation: Follows Eq. (2) and Eq. (3) from Siebenmorgen & Chini (2023, 10.48550/arXiv.2311.03310).
        
## Dust Components:
1) Nano-particles (VSG):
    - Graphite + PAH (2175 AA bump, far-UV reddening).
    - Nano-silicates (far-UV contribution).
2) Large grains:
    - Prolate grains with $a/b = 2$, aligned (IDG alignment).
    - Optical constants: Demyk et al. (2023) for aSi; Zubko (1996) for aC.
    - Radii: 6 nm to ~260 nm (Mathis et al., 1977).
3) Submicron-sized grains (Dark Dust):
    - Fluffy spheres (20% aC, 30% Si, 50% vacuum).
    - Radii: 260 nm to ~3 microns.


         

##     Input (./Input/)
- `d.Q* files` : Grain efficiencies Q for various components.
They include the grain efficiency computed using the Bruggeman mixing rule for porosity treated as vaccuum inclusion ('por'). E.g.:
    - `d.QellipaC`
    - `d.QellipSi`
    - `d.QellipDark`

- `jsmTaufit.inp`: Includes tauV and Ebv_obs for equations (2) and (3)  and P_serk polarisation of Serkowski fit.
- `w12_vv2_Ralf_283wave.dat`: Wavelength grid for the dust model.
- `jsm12fit.inp`: Dust parameters for fitting reddening curves:
    - Abundances: `abuc`, `abusi`, `abuvsi`, `abucvgr`, `abucpahs`.
    - Sizes: `qmrn`, `alec`, `alesi`, `arad_polmin_aC`, `arad_polmin_Si`, `arad_polmax`, `aled`.



##     Output (./Output/)
   - `Kappa.out`: Optical properties for absorption (`sa_*`) and scattering (`ss_*`).
   - `Kappa4fit.out`: Optical properties for observed wavelength ranges.
   - `PolKappa.out`: Polarization cross-sections (`sigp_*`). Dark dust: here: `sigp_dark = 0`.
   - `tau4fit.out`: Extinction and reddening curves (normalized and absolute).

##     Cross-Section Notation
Cross-sections ($K$) are in $cm^2/g$-dust, computed for each component and converted to optical depth by multiplying by $Nl$, $Nd$.

## 📁 Project Structure
    /jsm2Dpol/
    ├── Makefile            # Makefile for compilation           
    ├── jsm2Dpol_sub.f90    # Subroutines module
    ├── jsm2Dpol.f90        # Main program
    ├── jsm2Dpolcom.f90     # Common Definitions
    └── README.md           # This file

## Compilation Instructions
Create the `af90.j` executable with

        make

or

        make jsm
    
and move it next to your ./Input/ and ./Output/ folders before execution:

        ./af90.j

##     Version History
- 26.11.2024: this version      
- 24.04.2023: Unified SpT (luminosity) with GAIA (parallax) distances.
- 20.09.2022: Updated optics (n, k) using Demyk+22 for X50A+E20R silicates.
- 04.12.2021: Included dark dust model.
- 1991: Original version (Siebenmorgen, PhD; Siebenmorgen & Kruegel, 1992, A&A).
## Credits
For issues please contact me as author 

Greetings, Ralf Siebenmorgen

