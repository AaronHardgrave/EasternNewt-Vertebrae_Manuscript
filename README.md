# Eastern Newt Vertebral Morphology

Code and data to accompany the manuscript:

**"Terrestriality as a Constraint on Vertebral Shape in the Eastern Newt"**

Aaron J. Hardgrave, Sandy M. Kawano, & Richard T. Carter

Submitted to the *Journal of Anatomy*

## Contents

- `EasternNewt_Vert_041226.R` — Full analysis script
- `Oriented Fixed Landmarks/` — 3D landmark data (zipped) in SlicerMorph JSON format, organized by vertebral position

## Overview

This study uses 3D geometric morphometrics to test whether terrestrial life stages impose morphological constraints on vertebral shape in the Eastern Newt (*Notophthalmus viridescens*). Landmarks were collected from micro-CT scanned specimens across four life stages (terrestrial juveniles, aquatic juveniles, semi-aquatic adults, and aquatic paedomorphs) at ten vertebral positions (Atlas through Caudal 3).

The analysis script includes:

- Generalized Procrustes Analysis (GPA) and bilateral symmetry correction
- Principal Component Analysis (PCA) on symmetrized shape coordinates
- Procrustes ANOVA/ANCOVA with pairwise comparisons of least-squares means and allometric slopes
- Canonical Variate Analysis (CVA) with leave-one-out cross-validation (LOOCV)
- Morphological disparity analysis (total and partial)
- PCA mesh warping landmark export for 3D visualization in Slicer

## Setup

**Software:**
- R 4.4.1
- geomorph 4.0.8
- RRPP 2.0.3
- Additional packages are installed automatically by the script if not already present
- SlicerMorphR is installed from GitHub via `devtools::install_github('SlicerMorph/SlicerMorphR')`

**Data:**
1. Download and unzip the landmark data folder
2. Open `EasternNewt_Vert_041226.R`
3. Update all `setwd()` calls to reflect your local file paths

## Notes

- Permutation-based analyses (RRPP with 9,999 or 10,000 iterations) were run without a set seed. Exact p-values will vary slightly between runs but conclusions are robust.
- Caudal 2 analyses use a reduced sample (n = 41) due to two specimens with missing haemal arches.
- Bonferroni-corrected significance threshold: α = 0.0083 (0.05 / 6 pairwise comparisons per region).

## Contact

Aaron J. Hardgrave — East Tennessee State University
