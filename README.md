# NestArchOrg compendium

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/Gchism94/NestArchOrg/main?urlpath=rstudio)
[![.github/workflows/run-on-docker.yml](https://github.com/Gchism94/NestArchOrg/actions/workflows/run-on-docker.yml/badge.svg)](https://github.com/Gchism94/NestArchOrg/actions/workflows/run-on-docker.yml)
[![DOI](https://zenodo.org/badge/511707834.svg)](https://zenodo.org/badge/latestdoi/511707834)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## A compendium of code, data, and author's manuscript accompanying the preprint:

#### Greg Chism, [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-5478-2445). William Nichols, Anna Dornhaus, *Cavity geometry shapes overall ant colony organization through spatial limits but workers maintain fidelity zones*. Preprint on *bioRxiv*, 30 Oct 2023 <https://doi.org/10.1101/2022.06.30.498314>

This compendium includes data found on the Zenodo repository: 
[![DOI](https://zenodo.org/badge/94135460.svg)](https://doi.org/10.5281/zenodo.6784395)

## To cite this repository use the following: 

> Chism, G., Nichols, W., & Dornhaus, A. (2022). NestArchOrg (Version 1.0.0) [Computer software]. https://doi.org/10.5281/zenodo.6828919

## Overview
This repository is organized as a reproducible research compendium. 
Click the [![Binder](http://mybinder.org/badge.svg)](http://beta.mybinder.org/v2/gh/Gchism94/NestArchOrg/main?urlpath=rstudio) button above to explore in an interactive RStudio session.  Binder uses [rocker-project.org](https://rocker-project.org) Docker images to ensure a consistent and reproducible computational environment.  These Docker images can also be used locally.  

## File Organization

    analysis/
    |
    ├── paper/
    │   ├── paper.Rmd       # this is the main document to edit
    │   └── paper.pdf       # this is an elsevier .pdf written from paper.Rmd
    |
    ├── figures/            # location of the figures produced by the scripts in R
    ├── tables/             # location of the tables produced by the scripts in R
    ├── data/
    │   ├── rawData/        # data obtained from elsewhere
    |   ├── processed/      # processed output data from R scripts
    │   └── refData/        # data used to obtain final data and during the analysis
    |   
    ├── supplementaryMaterials/
    │   ├── supplementaryFigures/     
    |   |                   # supplementary figures for the main manuscript
    │   └── supplementaryTables/      
    |                       # supplementary tables for the main manuscript 
    |
    └── R                   # Run in the following order (also see associated README.md)
        ├── Stat_boxplot_custom.R
        |
        ├── binsWorking.R   # R script used to bin raw x, y coordinates into nest sections
        ├── distanceFunctions.R        
        |                   # R script used to calculate distances in the nest from binned x, y coordinates
        ├── fidelityZonesFunctions.R  # R script used to calculate site fidelity for binned marked worker x, y coordinates
        └── nestArchFunctsAnalyses.R
                            # R script used to produce all statistical analyses and figures
        

An `Rmd` notebook and associated pdf for the manuscript can be found in [analysis/paper](https://github.com/Gchism94/NestArchOrg/tree/main/analysis). This notebook produces a .pdf document in elsevier format.  

README.md files are included in all subdirectories with explanations or contents related to the paper. It should also provide a useful starting point for extending and exploring these materials for other projects.

Or to explore the code locally, clone or download this repository into RStudio or your preferred environment and install the compendium by running `devtools::install()`.  To install additional dependencies used only in formatting the figures, use `devtools::install(dep=TRUE)`.  

