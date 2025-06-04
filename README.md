# JSM2DPOL

Computes dust cross-sections, extinction, and polarised emission based on the three-component model by  
Siebenmorgen R. (2023, A&A, 670A, 115). Relative masses are derived from dust abundances and size distributions.  
The subroutine `sigtDark_EbvAvPol` computes the cross-sections ($cm^2/g$ ISM dust) and the relative mass of submicron-sized grains.

---

## 🌌 Dust Model

- **Cross-sections**: Based on Siebenmorgen (2023, A&A, 670A, 115)  
- **Column density**: Based on Eq. (2) and Eq. (3) from  
  Siebenmorgen & Chini (2023, arXiv:2311.03310)

---

## 🪶 Dust Components

1. **Nano-particles (VSG)**  
   - Graphite + PAHs (2175 Å bump, far-UV reddening)  
   - Nano-silicates (far-UV)

2. **Large grains**  
   - Prolate ($a/b = 2$), aligned (IDG model)  
   - Optical constants:  
     - aSi: Demyk et al. (2023)  
     - aC: Zubko (1996)  
   - Radii: 6–260 nm (Mathis et al., 1977)

3. **Submicron-sized grains (Dark Dust)**  
   - Fluffy spheres: 20% aC, 30% Si, 50% vacuum  
   - Radii: 260 nm – 3 µm

---

## 📥 Input Files (in `./Input/`)

- `d.Q*` files: Efficiency Q data for grain types (with porosity via Bruggeman rule), e.g.:  
  - `d.QellipaC`  
  - `d.QellipSi`  
  - `d.QellipDark`

- `jsmTaufit.inp`:  
  Contains `tauV`, `Ebv_obs`, and `P_serk` (Serkowski polarisation fit)

- `w12_vv2_Ralf_283wave.dat`:  
  Wavelength grid

- `jsm12fit.inp`:  
  Dust parameters for fitting reddening:
  - Abundances: `abuc`, `abusi`, `abuvsi`, `abucvgr`, `abucpahs`
  - Sizes: `qmrn`, `alec`, `alesi`, `arad_polmin_aC`, `arad_polmin_Si`, `arad_polmax`, `aled`

---

## 📤 Output Files (in `./Output/`)

- `Kappa.out`: Absorption (`sa_*`) and scattering (`ss_*`)  
- `Kappa4fit.out`: Observational wavelength range  
- `PolKappa.out`: Polarised cross-sections (`sigp_*`, dark dust set to zero)  
- `tau4fit.out`: Extinction/reddening curves (absolute and normalized)

---

## 📐 Cross-Section Notation

All cross-sections ($K$) are in units of $cm^2/g$-dust, scaled to optical depth via `Nl`, `Nd`.

---

## 📁 Project Structure

    jsm2Dpol/
    ├── src/
    │   ├── jsm2Dpol_main.f90         # Main program
    │   ├── jsm2Dpol_mods.f90         # Config/constants/functions modules
    │   ├── jsm2Dpol_utils.f90        # General-purpose utilities
    │   └── jsm2Dpol_subroutines.f90  # Physics subroutines and shared variables
    ├── examples/
    │   └── test_case_1/              # Sample inputs and outputs
    ├── Makefile                      # Build script
    ├── LICENSE
    └── README.md   

---

## ⚙️ Compilation

Build the executable `af90.j`:

```bash
make
# or:
make jsm
```

Move the executable next to your `./Input/` and `./Output/` folders:

```bash
./af90.j
```

---

## 🧾 Version History

- **26.11.2024** — Current version  
- **24.04.2023** — Unified SpT with GAIA parallax distances  
- **20.09.2022** — Updated optical constants (Demyk+22)  
- **04.12.2021** — Introduced dark dust  
- **1991** — Original release (Siebenmorgen PhD; Siebenmorgen & Kruegel 1992, A&A)

---

## 👤 Credits

Please contact Ralf Siebenmorgen for any issues.  
