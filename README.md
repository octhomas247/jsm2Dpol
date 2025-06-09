# JISMO: Joint IDL and Scattering Modeling of Dust Sightlines/Joint Interstellar Sightline Model

**JISMO** is a dual-pipeline codebase designed for modeling dust properties in individual interstellar sightlines. It was developed and applied in the context of the [LIPS III](#applications-and-citation) study, where it was used to model ISM dust properties based on reddening, polarization, and dust emission for a sample of stars based on FORS spectra and other observations.

![](./HD027778.png?raw=true "JISMO Header")

---

## Overview

JISMO combines two core components:

1. **IDL Fitting Pipeline**

   * fits optical polarization (from VLT/FORS), reddening curves, and optionally submillimeter Planck data to optimize the dust grain model parameters.
   * Iteratively determines the best-fit physical parameters by calling the Fortran simulation.

2. **Fortran90 Dust Modeling (JSM)**

   * Models polarized dust cross-sections, extinction, and emission.
   * Takes input parameters from the fitting pipeline and computes detailed outputs across wavelength grids for a three-component dust grain mixture (see [Siebenmorgen R. (2023, A&A, 670A, 115)](https://arxiv.org/abs/2211.10146)).

The pipeline supports iterative fitting: the IDL routines call the Fortran90 JSM model repeatedly to update the dusts key parameters until convergence.

---

## Repository Structure

```
jismo/
├── Data/            # Global observational data (FORS spectra, reddening curves, etc.)
│   └── Init/        # Initialization input files for modeling
├── IDL/             # Shared IDL procedures and fitting scripts
├── models/          # All model runs, one folder per target sightline
├── Qfile/           # d.Q input library (cross section data) for different dust types
├── scripts/         # Shell and Python helper scripts
├── src/             # Fortran90 JSM source code
└── build/           # Compiled binary (af90.j)
```

---

## Output Files

The Fortran90 code produces the following output files per model:

* `Kappa.out` – Total extinction, absorption (sa\_\*) and scattering (ss\_\*) cross-sections
* `Kappa4fit.out` – As above, but matched to observational wavelength grid
* `PolKappa.out` – Polarised cross-sections (sigp\_\*); zero for dark dust
* `tau4fit.out` – Modeled reddening and extinction curves
* `emis.out` – Dust emission spectrum
* `emipol.out` – Polarised dust emission spectrum

Fitting results and diagnostic plots are stored in `Result/` and `ResultRsv/`.

---

## Model Fitting

### Input Observables

* **Reddening curves** (e.g. E(B-V), A(V), A($\lambda$))
* **Optical polarization** (Q, U)
* **Planck 850 μm polarization** (optional)

### Fitted Parameters

* Grain abundances for carbon, silicates, and PAHs
* Size distribution parameters ($q_{mrn}$, $r^{-}_{p}$ for aC, Si)
* Dark dust contribution

---

## Quick Start

To use JISMO, first compile JSM:

```bash
# Recompile the Fortran90 JSM code
make clean
make jsm
```

Create a new model folder and start fitting your specific target (e.g. HD02778) with:

```bash
# Create a new model
./scripts/create_model.py MyStar123

# Run the pipeline (IDL + Fortran)
cd models/MyStar123 
./run_jismo.sh HD027778 noscreen
```

with the optional `noscreen` flag to supress `X` display output of IDL for e.g. remotely working with JISMO.

This will:

* Copy default input templates, Q-files and the JSM binary
* Prompt you to select porosity, vacuum fraction, axis ratio, and alignment efficiency of your dust components
* Fit the model iteratively using observational data in `./Data/`

To clean/reset your model folder, type:

```bash
./scripts/clean_model.py models/MyStar123/
```

To fetch a new set of cross section files for the dust components type:

```bash
./scripts/copy_qfiles.py models/MyStar123/
```

---

## Dependencies and Included Tools

* **IDL** (tested with versions supporting Coyote graphics)
* **gfortran** or Intel Fortran for JSM compilation
* **Python 3** for utility scripts

The repository includes (or expects access to):

* **MPFIT** – IDL nonlinear least-squares fitting library by C. Markwardt
* **Coyote Library** – IDL visualization and utility tools by David W. Fanning
* **COSMOS** – IDL astronomy routines
* **AASTRON** – Astronomical utilities from NASA's GSFC

Please refer to the respective licenses of these packages. If you use JISMO in a publication, also cite the original sources of these tools.

Included within this repository is a library of cross section data for different types of aC, Si and dark dust, and comes preloaded with the FORS spectrum and reddening curves used in the accompanying publication (see below). 

---

## Applications and Citation

JISMO was developed for and used in the study:

> **LIPS III: Large Interstellar Polarization Survey**
> Siebenmorgen et al. (2025) *(in prep.)*

In this paper, the pipeline was applied to:

* **3 stars** with full polarization and dust emission constraints (incl. Planck 850 μm)
* **24 additional targets** with limited polarization data and fixed dark dust parameters

If you use JISMO for scientific work, please cite the paper above and mention the use of the JISMO pipeline.

<!-- ## License

JISMO is distributed under the MIT License (see `LICENSE`). The included data and third-party code components may have their own licenses.

--- -->

## Contact

For questions, issues, or collaborations, please open an issue or contact

📧 [Ralf Siebenmorgen](mailto:Ralf.Siebenmorgen@eso.org) or 📧 [Thomas Vannieuwenhuyse](mailto:thomas.vannieuwenhuyse@eso.org)
