[![](https://img.shields.io/badge/devel%20version-0.99.4-black.svg)](https://github.com/neurogenomics/PeakyFinders)
[![R build
status](https://github.com/neurogenomics/PeakyFinders/workflows/rworkflows/badge.svg)](https://github.com/neurogenomics/PeakyFinders/actions)
[![](https://img.shields.io/github/last-commit/neurogenomics/PeakyFinders.svg)](https://github.com/neurogenomics/PeakyFinders/commits/master)
[![](https://img.shields.io/github/languages/code-size/neurogenomics/PeakyFinders.svg)](https://github.com/neurogenomics/PeakyFinders)
[![](https://codecov.io/gh/neurogenomics/PeakyFinders/branch/master/graph/badge.svg)](https://codecov.io/gh/neurogenomics/PeakyFinders)
[![License:
GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)  
<h4>  
Authors: <i>Brian Schilder</i>  
</h4>
<h4>  
README updated: <i>Feb-28-2023</i>  
</h4>

<!-- To modify Package/Title/Description/Authors fields, edit the DESCRIPTION file -->

## `PeakyFinders`: Mining, Calling, and Importing Epigenomic Peaks in R

Contains flexible and user-friendly functions to find, import, and
harmonise epigenomic peaks data from **GEO**, **ENCODE**, **ROADMAP**,
and **AnnotationHub**. Efficiently imports multiple peak files in one
function call (either genome-wide or specific regions) using
parallelisation. When peaks are not available, automatically calls peaks
from *bedGraph* or *bigWig* files.

If you use `PeakyFinders`, please cite:

> author1, author2, author3 (publicationYear) articleTitle,
> *journalName*; volumeNumber, [linkToPublication](linkToPublication)

## Installation

``` r
if(!require("remotes")) install.packages("remotes")

remotes::install_github("https://github.com/neurogenomics/PeakyFinders")
library(PeakyFinders)
```

## Documentation

### [Website](https://neurogenomics.github.io/PeakyFinders)

### [Getting started](https://neurogenomics.github.io/PeakyFinders/articles/PeakyFinders)

<hr>

## Contact

### [Neurogenomics Lab](https://www.neurogenomics.co.uk/)

UK Dementia Research Institute  
Department of Brain Sciences  
Faculty of Medicine  
Imperial College London  
[GitHub](https://github.com/neurogenomics)  
[DockerHub](https://hub.docker.com/orgs/neurogenomicslab)

<br>
