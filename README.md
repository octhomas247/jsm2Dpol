# JISMO: Joint IDL and Scattering Modeling of Dust Sightlines/Joint Interstellar Sightline Model

**JISMO** is a joint IDL–Fortran toolkit for modeling polarized dust properties along individual interstellar sightlines. It was developed and applied in the context of the [LIPS III](#applications-and-citation) study, where it was used to model interstellar dust based on reddening, polarization, and dust emission, using FORS spectra and other observations.

![](./HD027778.png?raw=true "JISMO Header")

## Overview

**JISMO** integrates two core components:

1. **IDL Fitting Pipeline**
    * Fits optical polarization curves, reddening curves, and optionally submillimeter Planck data to optimize dust grain model parameters.
    * Iteratively determines the best-fit physical properties by calling the Fortran-based dust simulation.

2. **Fortran90 Dust Modeling (JSM)**
    * Computes polarized dust cross-sections, extinction, and emission.
    * Takes input parameters from the fitting pipeline and produces detailed wavelength-dependent outputs for a three-component dust mixture
(see [Siebenmorgen R. (2023, A&A, 670A, 115)](https://arxiv.org/abs/2211.10146)).
    * Can also be used independently of the fitting routine (see [Dust Modeling: JSM](#dust-modeling-jsm)).

Together, these components form an iterative pipeline: the IDL layer repeatedly calls the JSM Fortran model to refine dust parameters until convergence.

## Repository Structure

```
jismo/
├── Data/           # Global observational data (e.g., FORS spectra, reddening curves)
│   └── Init/       # Initialization input files for modeling
├── IDL/            # IDL routines and fitting scripts
├── models/         # Output folders for individual JISMO model runs (one per target)
├── Qfile/          # d.Q input library (dust cross section data by type)
├── scripts/        # Shell and Python helper scripts
├── src/            # Fortran90 source code for JSM
├── example/        # Example standalone JSM model
└── build/          # Compiled binary (e.g., af90.j)
```

## Dust Modeling: JSM

The **JSM** (Joint Dust Simulation Model) Fortran90 component can be run independently of the IDL fitting pipeline to compute extinction, polarization, and emission based on specified dust parameters.

### Input Files

The JSM model expects the following inputs:

#### 1. `d.Q*` **Files** (Dust Cross Sections)

<details>
<summary>Show Input File Description</summary>

---

These files provide grain-specific optical properties (cross-sections) as a function of **wavelength** and **grain radius**, and are required for calculating extinction, scattering, and polarization.

Two types of Q-files are needed:

• **Ellipsoidal grains (d.Qellip\*)**

Used for aligned, non-spherical dust grains (e.g. carbon, silicate, or “dark dust”). These files are computed using the **Imperfect Davis-Greenstein (IDG)** alignment model and include:

• `por`, `vvac`: Porosity (vacuum fraction) of the grain mixture

• `a/b`: Axis ratio of the ellipsoids

• `IDG`, `del0`: Alignment model parameters

• Tabulated data:

* `LAMBDA`: Wavelength (μm)
* `Rv`: Volume-equivalent grain radius (cm)
* `OMEGA`: Grain alignment angle
* `Qmean EXT`: Mean extinction efficiency
* `Qmean POL`: Mean polarization efficiency
* `Qmean CPL`: Circular polarization
* `Qmean SCA`: Mean scattering efficiency

> A library of *d.Qellip\** files for different dust parameters is included within this repository within the *./Qfile/* folder.

• **Spherical grains (d.Qmie\*)**

Used for unaligned spherical grains (e.g. PAHs or graphite). Calculated using **Mie theory**, with each row providing:

• `w`: Wavelength (μm)

• `r`: Grain radius (cm)

• `Qabs`: Absorption efficiency

• `Qsca`: Scattering efficiency

• `g`: Asymmetry factor

• `x`, `max(|m-1|*x)`: Mie parameters related to grain optics


> These files are typically sourced from precomputed libraries and should match the physical dust mixture used in your model.

</details>

#### 2. `jsm12fit.inp` (Dust Size and Abundance Parameters)

<details>
<summary>Show Input File Description</summary>

---

Defines the dust grain **composition**, **abundances**, and **size distribution** for each dust component used in the model. The file contains a single line of whitespace-separated parameters:

| Parameter       | Description                                                                 |
|----------------|-----------------------------------------------------------------------------|
| `abuc`         | Abundance of carbonaceous grains (e.g., amorphous carbon)                   |
| `abusi`        | Abundance of silicate grains                                                |
| `abuvsi`       | Vacuum fraction within silicates (porosity parameter)                       |
| `abucvgr`      | Abundance of very small graphite grains                                     |
| `abucpahs`     | Abundance of PAH grains                                                     |
| `qmrn`         | Power-law index of the grain size distribution (typically ~3.5 for MRN)     |
| `alec`         | Maximum size (in cm) of carbonaceous grains                                 |
| `alesi`        | Maximum size (in cm) of silicate grains                                     |
| `apolmin_aC`   | Minimum size (in cm) of aligned carbon grains contributing to polarization  |
| `apolmin_Si`   | Minimum size (in cm) of aligned silicate grains                             |
| `apolmax`      | Maximum size (in cm) of aligned grains (common to both aC and Si)           |
| `aDarkmax`     | Maximum grain size (in cm) for the dark dust component                      |

> ⚠️ The polarization sizes (`apolmin_*`, `apolmax`) define the grain size range over which grains are assumed to be aligned and contribute to the modeled linear polarization.

</details>

#### 3. `jsmTaufit.inp` (Observed Constraints)

<details>
<summary>Show Input File Description</summary>

---

Provides observed reference values for extinction and polarization used to evaluate the model performance. Each line contains three values:

| Parameter   | Description                                                              |
|-------------|--------------------------------------------------------------------------|
| `tauV`      | Visual optical depth (A(V) / 1.086)                                       |
| `Ebv_obs`   | Observed color excess E(B–V), typically derived from reddening curves     |
| `P_serk`    | Observed peak linear polarization from a Serkowski-law fit               |

> These values are used to compare the modeled extinction and polarization outputs against observations during fitting.

</details>

#### 4. `w12_vv2_Ralf_283wave.dat` (Wavelength Grid)

<details>
<summary>Show Input File Description</summary>

---

Defines the wavelength sampling (in cm) used in the model calculations. The file contains 283 values, listed in descending order from 1 cm down to 9.08 × 10⁻⁶ cm, with one wavelength per line.

> All extinction, polarization, and emission quantities are computed across this grid.

</details>

#### 5. `PAH2170.wq` (PAH Absorption Template)

<details>
<summary>Show Input File Description</summary>

---

Contains the **Drude profile parameters** used to model the PAH absorption bump near 2175 Å. Each line defines:

| Parameter | Description                                  |
|-----------|----------------------------------------------|
| `x0`      | Central frequency (in μm⁻¹), e.g. 4.6 μm⁻¹ (~2175 Å) |
| `γ`       | Width (FWHM) of the absorption feature        |

> These parameters define the PAH absorption using a Drude profile and are used internally when computing total extinction.

</details>

### Output Files

#### 1. `Kappa.out` and `Kappa4fit.out` (Cross-Sections)

<details>
<summary>Show Output File Description</summary>

---

These files list modeled cross-sections (in cm²/g) per dust component.

- **`Kappa.out`**: Full-resolution version based on the internal wavelength grid (from `w12_vv2_Ralf_283wave.dat`)
- **`Kappa4fit.out`**: Interpolated version matched to the observational grid used in fitting

Each row corresponds to a wavelength, and columns represent:

| Column       | Description                                      |
|--------------|--------------------------------------------------|
| `wel`        | Wavelength (in cm)                               |
| `sa_*`       | Absorption cross-section for component `*`       |
| `ss_*`       | Scattering cross-section for component `*`       |
| `sigt`       | Total extinction cross-section |

Dust components:

- `aC` – Amorphous Carbon
- `Si` – Silicates
- `vgr`, `vsi` – Vacuum-mixed (porous) grains
- `Dark` – Dark dust
- `pahS+B` – PAH small and big grains

</details>

#### 2. `PolKappa.out` (Polarized Cross-Sections)

<details>
<summary>Show Output File Description</summary>

---

Lists the modeled **polarization cross-sections** (`sigp_*`) for aligned dust grains across the wavelength grid. These values determine the polarization efficiency of each dust component.

| Column       | Description                                 |
|--------------|---------------------------------------------|
| `wel`        | Wavelength (in cm)                          |
| `sigp_aC`    | Polarized absorption for Amorphous Carbon   |
| `sigp_Si`    | Polarized absorption for Silicates          |
| `sigp_dark`  | Polarized absorption for Dark dust          |
| `sigt`       | Total extinction cross-section (from `Kappa.out`) |

> Units are in cm²/g. Only aligned grains (aC, Si, and dark dust) contribute to polarization.

</details>

#### 3. `tau4fit.out` (Modeled Reddening & Extinction Curves)

<details>
<summary>Show Output File Description</summary>

---

Contains modeled extinction and reddening information, matched to observational wavelengths.

| Column         | Description                                                  |
|----------------|--------------------------------------------------------------|
| `wel`          | Wavelength (in microns)                                      |
| `tau/tauV`     | Optical depth relative to τ(V), i.e., τ(λ)/τ(V)              |
| `E(w–V)/E(B–V)`| Modeled reddening curve normalized to E(B–V)                 |
| `E(w–V)`       | Absolute reddening E(λ–V), using model values for R_V and E(B–V) |

> The header also includes the model-inferred values for R_V and E(B–V).

</details>

#### 4. `emis.out` and `emipol.out` (Dust Emission Spectra)

<details>
<summary>Show Output File Description</summary>

---

Contain modeled dust emission per unit mass, across wavelengths.

| Column       | Description                                                           |
|--------------|------------------------------------------------------------------------|
| `wel`        | Wavelength (in microns)                                                |
| `emis_tot`   | Total dust emission (or polarized emission in `emipol.out`)            |
| `emis_c`     | Emission from carbonaceous grains                                      |
| `emis_Si`    | Emission from silicate grains                                          |
| `emis_pol`   | Emission from aligned polarizing grains                                |
| `emi_Dark`   | Emission from dark dust component                                      |
| `emi_vgr`    | Emission from very small graphite grains                               |
| `emi_vsi`    | Emission from very small silicate grains                               |
| `epah_s+b`   | Emission from PAHs (small and big)                                     |
| `ISRF j_v`   | Interstellar Radiation Field at the given wavelength (j_ν)             |

> All values are in [erg/s/Hz/steradian] per gram of interstellar material (IM).  
> `emipol.out` contains the **polarized** component of the emission, with the same structure.

</details>

### Running a Standalone Model

The `./example/` folder contains a ready-to-run setup using predefined dust parameters and Q-files.

To run the model:

```bash
# Recompile the Fortran90 JSM code
make clean
make jsm
```

Then, replace the default executable in the example folder:

```
cp build/af90.j example/
cd example/
./af90.j
```

The output files will be written into the same directory.

---

## Model Fitting with **JISMO**

### Input Observables

* **Reddening curves**: e.g. E(B-V), A(V), A($\lambda$)
* **Optical polarization**: Stokes Q, U
* **Planck 850 μm polarization** (optional)

### Fitted Parameters

* Grain abundances for carbon, silicates, and PAHs
* Size distribution parameters (e.g., $q_{mrn}$, $r^{-}_{p}$ for aC, Si)
* Fraction of dark dust

### Quick Start

To use **JISMO**, first compile JSM:

```bash
# Recompile the Fortran90 JSM code
make clean
make jsm
```

Then create a model folder and fit a target star (e.g., HD027778):

```bash
# Create a new model
./scripts/create_model.py MyStar123

# Run the pipeline (IDL + Fortran)
cd models/MyStar123 
./run_jismo.sh HD027778 noscreen
```

Use the optional noscreen flag to suppress graphical output (helpful for remote sessions).

This will:

* Copy input templates, Q-files, and the JSM binary
* Prompt for dust parameters: porosity, vacuum fraction, axis ratio, and alignment efficiency
* Fit the model using data from `./Data/`
* Save results and diagnostic plots to the model’s `Result/` and `ResultRsv/` directories

### Maintenance Utilities

To clean or reset a model folder:

```bash
./scripts/clean_model.py models/MyStar123/
```

To fetch new cross section (d.Q) files for the dust components:

```bash
./scripts/copy_qfiles.py models/MyStar123/
```

## Dependencies and Included Tools

* **IDL** (tested with versions supporting Coyote graphics)
* **gfortran** or Intel Fortran (for compiling the JSM component)
* **Python 3** (for utility scripts)

This repository includes or expects access to:

* **MPFIT** – IDL nonlinear least-squares fitting library by C. Markwardt
* **Coyote Library** – IDL visualization and utility tools by David W. Fanning
* **COSMOS** – IDL astronomy routines
* **AASTRON** – Astronomical utilities from NASA's GSFC

Please refer to the respective licenses of these packages. If you use **JISMO** in a publication, also cite the original sources of these tools.

The repository also includes:

* A library of dust **cross section files** (d.Q) for various aC, Si, and dark dust components
* Preloaded **FORS spectra** and **reddening curves** used in the associated publication (see below)

## Applications and Citation

**JISMO** was developed for and used in the study:

> **LIPS III: Large Interstellar Polarization Survey**
> Siebenmorgen et al. (2025) *(in prep.)*

In this paper, JISMO was applied to:

* **3 stars** with full polarization and dust emission constraints (incl. Planck 850 μm)
* **24 additional targets** with limited polarization data and fixed dark dust parameters

If you use **JISMO** for scientific research, please cite the above publication and mention your use of the pipeline.

<!-- ## License

JISMO is distributed under the MIT License (see `LICENSE`). The included data and third-party code components may have their own licenses.

--- -->

## Contact

For questions, issues, or collaboration inquiries, feel free to contact:

📧 [Ralf Siebenmorgen](mailto:Ralf.Siebenmorgen@eso.org) or  
📧 [Thomas Vannieuwenhuyse](mailto:thomas.vannieuwenhuyse@eso.org)