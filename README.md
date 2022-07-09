# NestArchOrg compendium

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/Gchism94/NestArchOrg/HEAD)
[![Build Status](https://travis-ci.org/cboettig/noise-phenomena.svg?branch=master)](https://travis-ci.org/cboettig/noise-phenomena)
[![DOI](https://zenodo.org/badge/94135460.svg)](https://doi.org/10.5281/zenodo.6784395)

A compendium of code, data, and author's manuscript accompanying the publication:

#### Greg Chism, [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-5478-2445). *Nest shape influences colony organization in ants: colony spatial distribution differs from random movement*. Preprint on *bioRxiv*, 02 July 2022 <https://doi.org/10.1101/2022.06.30.498314>

## Overview

This repository is organized as a reproducible research compendium. 
Click the [![Binder](http://mybinder.org/badge.svg)](http://beta.mybinder.org/v2/gh/cboettig/noise-phenomena/master?urlpath=rstudio) button above to explore in an interactive RStudio session.   Binder uses [rocker-project.org](https://rocker-project.org) Docker images to ensure a consistent and reproducible computational environment.  These Docker images can also be used locally.  

An `Rmd` notebook and associated pdf for Appendix A can be found in [appendixA](/appendixA).  This notebook includes explanations and code necessary for all of the numerical examples discussed in the paper.  It should also provide a useful starting point for extending and exploring these models with other parameters.

Or to explore the code locally, clone or download this repository into RStudio or your preferred environment and install the compendium by running `devtools::install()`.  To install additional dependencies used only in formatting the figures, use `devtools::install(dep=TRUE)`.  


This compendium is checked by Travis-CI continuous integration.  Click the [![Build Status](https://travis-ci.org/cboettig/noise-phenomena.svg?branch=master)](https://travis-ci.org/cboettig/noise-phenomena) button for details.
