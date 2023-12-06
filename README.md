# Garcia-2024-CRDS-Interference
Data and analysis on investigatation of interference on CRDS instruments.

# Maintainer
Jesper Nørlem Kamp.
Contact information here: <https://au.dk/jk@bce>.

# Published paper
The contents of this repo are presented in the following paper, which is open-access:

xx

# Overview
This repo contains all the data and data processing scripts needed to produce the results presented in the paper listed above.
The scripts run in R (<https://www.r-project.org/>) and require several add-on packages.

# Directory structure

## `H2O Interferences`
Measurement data and scripts on H2O interference.
Use scripts `H2O_Test1&2.R` and `H2O_Test3.R`.

## `NH3 Interferences`
Measurement data and scripts on NH3 interference.
Use scripts `Calibration_NH3.R` and `NH3_Interferences.R`.

## `VOC Interferences`
Measurement data and scripts on VOC interference.
Use scripts `PTRdata_Picarrodata_Timeallign_Example.R` and `VOC_Interferences.R`.

# Links to paper
This section give the sources of tables and figures presented in the paper.

| Paper component          |  Repo source                             |  Repo scripts             |
|-----------------         |-----------------                         |---------------            |
|    Figure 3              | `plots-meas/01_flux_wind_meas.pdf`       | `scripts-meas/plot_big.R` |
|    Figure 4              | `plots-meas/30_emis_vs_AER.pdf`          | `scripts-meas/plot.R`     |
|    Figure 5              | `plots-ALFAM2/54_flux_comp_sel_zoom.pdf` | `scripts-ALFAM2/plot.R`   |
|    Figure S1             | `plots-pH/40_surface_pH.pdf`             | `scripts-pH/plot.R`       |
|    Figure S2             | `plots-pH/40_surface_pH.pdf`             | `scripts-pH/plot.R`       |
|    Figure S3             | `plots-pH/40_surface_pH.pdf`             | `scripts-pH/plot.R`       |
|    Figure S4             | `plots-ALFAM2/60_error_comp.pdf`         | `scripts-ALFAM2/plot.R`   |
|    Figure S5             | `plots-ALFAM2/53_flux_comp_sel.pdf`      | `scripts-ALFAM2/plot.R`   |
|    Figure S6             | `plots-ALFAM2/80_r1.pdf`                 | `scripts-ALFAM2/plot.R`   |

