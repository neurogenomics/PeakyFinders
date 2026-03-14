<img src='https://github.com/neurogenomics/PeakyFinders/raw/master/inst/hex/hex.png' title='Hex sticker for PeakyFinders' height='300'><br>
[![License:
GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)
[![](https://img.shields.io/badge/devel%20version-0.99.4-black.svg)](https://github.com/neurogenomics/PeakyFinders)
[![](https://img.shields.io/github/languages/code-size/neurogenomics/PeakyFinders.svg)](https://github.com/neurogenomics/PeakyFinders)
[![](https://img.shields.io/github/last-commit/neurogenomics/PeakyFinders.svg)](https://github.com/neurogenomics/PeakyFinders/commits/master)
<br> [![R build
status](https://github.com/neurogenomics/PeakyFinders/workflows/rworkflows/badge.svg)](https://github.com/neurogenomics/PeakyFinders/actions)
[![](https://codecov.io/gh/neurogenomics/PeakyFinders/branch/master/graph/badge.svg)](https://app.codecov.io/gh/neurogenomics/PeakyFinders)
<br>
<a href='https://app.codecov.io/gh/neurogenomics/PeakyFinders/tree/master' target='_blank'><img src='https://codecov.io/gh/neurogenomics/PeakyFinders/branch/master/graphs/icicle.svg' title='Codecov icicle graph' width='200' height='50' style='vertical-align: top;'></a>  
<h4>  
Authors: <i>Brian Schilder</i>  
</h4>
<h4>  
README updated: <i>Mar-13-2026</i>  
</h4>

## PeakyFinders: Mining, Calling, and Importing Epigenomic Peaks in R

Contains flexible and user-friendly functions to find, import, and
harmonise epigenomic peaks data from **GEO**, **ENCODE**, **ROADMAP**,
and **AnnotationHub**. Efficiently imports multiple peak files in one
function call (either genome-wide or specific regions) using
parallelisation. When peaks are not available, automatically calls peaks
from *bedGraph* or *bigWig* files.

### Key Features

- **Multi-source import**: Fetch peaks from GEO, ENCODE, ROADMAP, and
  AnnotationHub with a single function
- **Automatic peak calling**: When peak files aren’t available,
  automatically calls peaks from bedGraph or bigWig files using MACS3 or
  SEACR
- **Region filtering**: Import only peaks overlapping regions of
  interest
- **Genome build handling**: Automatic liftOver between genome builds
  (hg19/hg38)
- **Parallelisation**: Efficiently import multiple peak files in
  parallel

## Installation

``` r
if(!require("BiocManager")) install.packages("BiocManager")

BiocManager::install("neurogenomics/PeakyFinders")
library(PeakyFinders)
```

## Quick Start

``` r
library(PeakyFinders)

# Import peaks from multiple sources at once
ids <- c(
    "GSM945244",    # GEO
    "ENCSR000AHD",  # ENCODE
    "AH32001"       # AnnotationHub
)

peaks <- import_peaks(
    ids = ids,
    builds = "hg38",
    searches = list(narrowPeak = "narrowpeak")
)

# Filter to a specific region
query_region <- GenomicRanges::GRanges("chr22:1-50000000")
peaks_filtered <- import_peaks(
    ids = ids,
    builds = "hg38",
    query_granges = query_region,
    query_granges_build = "hg38"
)

# Search for available datasets
encode_metadata <- search_encode(
    target = "H3K27ac",
    biosample = "K562"
)
```

## Documentation

### [Website](https://neurogenomics.github.io/PeakyFinders)

### [Getting started](https://neurogenomics.github.io/PeakyFinders/articles/peakyfinders)

## Citation

If you use PeakyFinders, please cite:

> Brian M. Schilder, Nathan G. Skene (2024). PeakyFinders: Mining,
> Calling, and Importing Epigenomic Peaks in R. R package version 0.99.4

<hr>

## Contact

### [Neurogenomics Lab](https://www.neurogenomics.co.uk/)

UK Dementia Research Institute Department of Brain Sciences Faculty of
Medicine Imperial College London
[GitHub](https://github.com/neurogenomics)
[DockerHub](https://hub.docker.com/orgs/neurogenomicslab)

<br>
