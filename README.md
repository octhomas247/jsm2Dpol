# JISMO: Joint Interstellar Sightline Model
<!-- > A Fortran90 code for modeling polarised extinction and emission in the diffuse ISM using a three-component grain model. -->
## Overview

JISMO is a Fortran90 program for modeling polarized dust extinction, scattering and thermal emission in the interstellar medium (ISM). It is based on the three-component dust grain model from [Siebenmorgen R. (2023, A&A, 670A, 115)](https://arxiv.org/abs/2211.10146). It includes support for:

- Multiple grain types (nano, amorphous, and large submicron grains)
- Polarization-dependent optical cross-sections
- Alignment efficiency modeling
- Comparison with observed extinction and polarization

![](./header.jpeg?raw=true "JISMO Header")

## Scientific Context – LIPS III

This repository accompanies the modeling work presented in:

**LIPS III: The Large Interstellar Polarisation Survey – Observational constraints on grain structures and alignment efficiencies**  
Siebenmorgen et al. (2025), *A&A, XXX, XXX*

The LIPS III study aims to constrain the physical properties of interstellar dust — including grain size, structure, composition, and alignment — by jointly analyzing:

- Reddening curves
- Optical linear polarization spectra from VLT/FORS
- Planck 850 μm polarized dust emission (for a subset of stars)
- Gaia distance measurements

A three-component grain model is fitted to individual sightlines using a multi-step approach. For three representative stars, the model is constrained with both optical and submillimeter polarization. For 24 additional sightlines, the model is applied using only optical data. This repository includes the code (JISMO) and the data used in those applications.

## Repository Structure

The repository is organized into three main components:

1. **Standalone JISMO Program** (`src/`): Contains the main JISMO Fortran90 source code. Can be used independently of the LIPS III study.
2. **With Rsv constraints** (`models/por*/` folders): Fits reddening + FORS + Planck data for 3 stars using Eq. 18 (iteratively matching Rsv and Rpp).
3. **Without Rsv** (`models/Model24stars/`): Applies the dust model to 24 additional sightlines using only reddening and FORS polarization (no Rsv), using a Dark Dust model with:

    - Porosity = 10%
    - Vacuum content = same as aC + aSi
    - δ₀ = 10 μm
    - Axis ratio a/b = 2.5


---

    jismo/
    ├── src/                         # Standalone JISMO program
    │   ├── jsm_main.f90
    │   ├── jsm_mods.f90
    │   ├── jsm_utils.f90
    │   └── jsm_subroutines.f90
    │    
    ├── example/                    # Minimal technical example
    │   ├── Input/                  # Pre-generated inputs
    │   ├── Output/                 # Expected result
    │   └── af90.j                  # JISMO Output binary
    │    
    ├── models/                     # Scientific models
    │   ├── Model24stars/
    │   │   ├── Run_all.pro         # Model-specific driver
    │   │   └── ...
    │   │
    │   ├── por05DarkForsRsv/
    │   │   ├── V00d00.2ab1.5/
    │   │   │   ├── Run_all.pro
    │   │   │   └── ...          
    │   │   └── ...
    │   ├── por10DarkForsRsv/
    │   └── por20DarkForsRsv/
    │    
    ├── IDL_shared/                 # Shared IDL helper routines
    │   ├── istart_all.pro
    │   └── ...
    │    
    ├── Qfile/                      # Library of d.Q files
    │   ├── d.aC00ab2.0d00          # d.QellipaC for por=0.0, ab=2.0, del0 = 0.02
    │   └── ...
    │
    ├── scripts/                    # Python helpers, IDL-to-input wrappers
    │   └── run_model.py            # Optional wrapper: runs IDL, then JISMO
    │    
    ├── Makefile                    
    ├── LICENSE
    └── README.md

---

## Getting Started

Build the executable `af90.j`:

```bash
make
# or:
make jsm
```

### Individual Execution

Move the executable from `build/` next to the `Input/` and `Output/` folders of your project and run (see `example/`):

```bash
./af90.j
```

### Input Files (`./Input/`)

These files are required to run a JISMO model for a given sightline:

- d.Q* files — Dust efficiencies for different grain populations, computed using Mie theory or spheroidal models with the Bruggeman mixing rule (porosity as vacuum inclusion). Typical files include:
  - d.QellipaC        – Elongated amorphous carbon grains
  - d.QellipSi        – Elongated silicate grains
  - d.QellipDark      – Elongated dark dust grains
  - d.Qmie_vGr        – Spherical graphite grains
  - d.Qmie_vSi        – Spherical silicate grains

- jsmTaufit.inp — Observational constraints for the current sightline:
  - tauV       – Optical depth at V band
  - Ebv_obs    – Observed colour excess
  - P_serk     – Serkowski fit to optical polarisation

- w12_vv2_Ralf_283wave.dat — Wavelength grid used for computing cross-sections and comparing to observations.

- jsm12fit.inp — Main configuration file for dust composition and size distribution:
  - Abundances:
      abuc, abusi, abuvsi, abucvgr, abucpahs
  - Sizes and distribution:
      qmrn, alec, alesi, arad_polmin_aC, arad_polmin_Si, arad_polmax, aled

- PAH2170.wq — Cross-section data for PAH emission near 2170 Å (UV bump).

---

### Output Files (`./Output/`)

The model produces the following output for each run:

- Kappa.out — Total extinction, absorption (sa_\*) and scattering (ss_\*) cross-sections.

- Kappa4fit.out — Same as above, but restricted to the observational wavelength grid.

- PolKappa.out — Polarised absorption and scattering cross-sections (sigp_*).
  (Note: for the dark dust component, polarisation is assumed to be zero.)

- tau4fit.out — Modeled extinction/reddening curves (absolute and normalized).

- emis.out — Dust emission spectrum from all components.

- emipol.out — Polarised dust emission spectrum.

---

## Running the Model

Each model run fits a **three-component dust grain model** to observations of an individual sightline. The fitting is controlled by the model-specific IDL driver (`Run_all.pro`), which:

- Prepares input data and folders
- Performs the model fitting (iterative only for the 3 representative stars)

The actual computation of extinction, polarization, and emission is done by the Fortran program (`af90.j`), which is called separately by a Python wrapper.

This process is automated with the `run_model.py` script:

```bash
./scripts/run_model.py models/Model24stars/
```

This script:

- Runs the IDL fitting script (`Run_all.pro`)
- Executes the Fortran binary (`af90.j`) from the `build/` directory

There is no need to move `af90.j` into individual model folders.

### Optional: View IDL Output

To print IDL output to the terminal and save it to `idl_run.log` inside the model folder:

```bash
./scripts/run_model.py models/Model24stars/ --log-idl
```

### Notes

- The specified model folder must include:
  - A `Run_all.pro` IDL driver
  - An `Input/` subfolder with required configuration and Q files
- Output is written to `Output/` within the model directory.

The workflow supports two modes:
- **3 representative stars:** Fitting includes both **optical and submillimeter polarization** (Planck 850 µm), with **iterative adjustment** of the outer radius to match Rsv and Rpp (see Eq. 18).
- **24 additional stars:** Fitting uses only **optical polarization and reddening**, with a **single-pass fit**.

---

## Version History

- **2025-05** — Modularized codebase and enabled reading of dust parameters from d.Q\* input files  
- **2025-04** — Ported from FORTRAN77 to structured FORTRAN90  
- **2024-11-26** — Code update for LIPS III analysis  
- **2023-04-24** — Stellar types unified with GAIA parallax distances  
- **2022-09-20** — Adopted updated optical constants from [Demyk et al. (2022)](https://arxiv.org/abs/2209.06513)  
- **2021-12-04** — Dark dust component introduced  
- **1991** — Original JSM model developed ([Siebenmorgen & Kruegel 1992, A&A, 259, 614](https://adsabs.harvard.edu/full/1992A%26A...259..614S))

## Contact and Acknowledgements

For questions or support, please contact  
📧 [Ralf Siebenmorgen](mailto:Ralf.Siebenmorgen@eso.org)  

If you use this code in your research, please cite the relevant publications as listed in the [LIPS III abstract section](#lips-iii-context).  