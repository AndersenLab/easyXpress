# easyXpress <img src="man/figures/logo.png" alt="hex" align = "right" width="130" />

## Overview 
This package is designed for reading, processing, and visualizing of nematode morphology data extracted from images using CellProfiler's WormToolbox.

## Installation
`easyXpress` is specialized for use with image data produced by the [`cellprofiler-nf` nextflow pipeline](https://github.com/AndersenLab/cellprofiler-nf). To install `easyXpress` you will need the [`devtools`](https://github.com/hadley/devtools) package. You can install `devtools` and `easyXpress` using the commands below:

```r
install.packages("devtools")
devtools::install_github("AndersenLab/easyXpress")
```
OS X installations of `easyXpress` require `XQuartz` to be installed. Follow the instructions [here to install XQuartz](https://www.xquartz.org/).

The functionality of the package can be broken down into three main goals:

+ Reading data generated from CellProfiler pipelines alongside information about experimental design.

+ Flagging and pruning anomalous data points.

+ Generating diagnostic images.

For more information about implementing `cellprofiler-nf` to generate data used by the `easyXpress` package, see [`AndersenLab/cellprofiler-nf`](https://github.com/AndersenLab/cellprofiler-nf).

## Directory structure

The directory structure holding data is critically important. Below is an example of a correct project directory structure. 
The `cp_data` directory contains an `.RData` file output by `cellprofiler-nf`. The `processed_images` directory contains `_overlay.png` files output by `cellprofiler-nf`. There should be one `.png` file for each well included in your analysis. The `design` directory contains a `.csv` with all the variables necessary to describe your experiment (i.e. experiment names, drug names, drug concentrations, strain names, food types, etc.).    
  
If you do not have condition information you do not need the `design` directory.

```
/projects/20200812_example
├── cp_data
│   ├── CellProfiler-Analysis_20191119_example_data.RData
└── processed_images
│   ├── 20191119-growth-p01-m2x_A01_overlay.png
│   ├── 20191119-growth-p01-m2x_A02_overlay.png
│   ├── 20191119-growth-p01-m2x_A03_overlay.png
│   ├── ...    
├── design
    └── 20191119_design.csv
```
This directory exhibits the minimal file content and naming for the easyXpress package to work.

### Project directory

The project directory contains all of the files attached to a specific experiment conducted on a specific date. 
The naming convention for these folders should include the date in the format 4-digit year::2-digit month::2-digit day 
and experiment name separated by underscores. 

```
# Example directory name
# Date is January 1st, 2020
# Experiment name is "ExperimentName"

20200101_ExperimentName/
```

### File naming

The processed image files should be formatted with the experiment data, name of the experiment, the plate number, the magnification used for imaging, and the well name. All processed image files must be saved as `.png` files. In the file named `20191119-growth-p01-m2x_A01_overlay.png` the first section `20191119` is the experiment date, `growth` is the name of the experiment, `p01` is the plate number, `m2x` is the magnification used for imaging, and `A01` is the well name.

## Package Overview 
The `easyXpress` package consists of six function classes that work together to clean and process experimental data. The `tidy` functions will help pre-process raw images to get them ready for submission to the `cellprofiler-nf` pipeline. The `ObjectFlag` or `OF` functions help to flag problematic data output from `cellprofiler-nf`. The `WellFlag` or `WF` functions work to flag anomalous summary statistics for micro-plate wells. Throughout the data cleaning workflow, the `check` and `view` function classes are used to validate whether the flag functions are properly applied. All other functions serve to facilitate the cleaning process and do not have a standardized naming convention.

For more detailed information regarding use of these functions, see the article: [**Dose Response Processing**](https://andersenlab.org/easyXpress/articles/Dose_Response.html).

### Citation

Please cite the following in publications that use `easyXpress`:
#### easyXpress: An R package to analyze and visualize high-throughput C. elegans microscopy data generated using CellProfiler
#### Joy Nyaanga, Timothy A. Crombie, Samuel J. Widmayer, Erik C. Andersen
#### (2021 August 12) _PLoS ONE_
#### [PLoS ONE](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0252000) [PubMed](https://pubmed.ncbi.nlm.nih.gov/34383778/)


