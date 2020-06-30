# easyxpress

easyxpress is specialized for use with worms and is specific to use in the Andersen Lab and, therefore, is not available from CRAN. To install easyxpress you will need the [`devtools`](https://github.com/hadley/devtools) package. You can install `devtools` and `easyXpress` using the commands below:

```r
install.packages("devtools")
devtools::install_github("AndersenLab/easyxpress")
```

The functionality of the package can be broken down into three main goals:

+ Reading data generated from CellProfiler pipelines alongside information about strains, conditions, controls, and experimental design.

+ Flagging and pruning anomalous data points

+ Generating diagnositic images

## Directory structure

Because so much information must be transferred alongside the plate data, the directory structure from which you are reading is critically important. Below is an example of a correct project directory structure. The `cp_data` directory contains`.csv` files from the CellProfiler pipeline copied directly from the default output folder for CellProfiler. Depending on the CellProfiler pipeline you run you may have multiple NonOverlappingWorms files. In the example below there are two such files. The `processed_images` directory contains `.png` files from the CellProfiler pipeline. There should be one `.png` file for each well included in your analysis. The `design` directory contains the `.csv` file having all the variables necessary to describe your experiment, i.e. drug names, drug concentrations, strain names, food types, etc. The `.cpproj` file is not used by easyxpress, but is advised to reproduce the original analysis.

```
/projects/20190920_15hHB101foodpreps_RUN1
├── cp_data
│   ├── dualmodel_NonOverlappingWorms_control.csv
│   └── dualmodel_NonOverlappingWorms_full.csv
└── processed_images
    └── 20190913_15hHB101foodpreps_01_2x_A01_w1_overlay.png
│   ├── 20190913_15hHB101foodpreps_01_2x_A02_w1_overlay.png
│   ├── 20190913_15hHB101foodpreps_01_2x_A03_w1_overlay.png
│   ├── ...    
├── design
    └── design.csv
├── 20190920_15hHB101foodpreps_RUN1.cpproj
```

### Experiment directory

The experiment directory contains all of the files attached to a specific experiment conducted on a specific date. The naming convention for these folders should include the date in the format 4-digit year::2-digit month::2-digit day and experiment name separated by underscores. 

```
# Example directory name
# Date is January 1st, 2020
# Experiment name is "ExperimentName"

20200101_ExperimentName/
```

### File naming

The processed image files should be formatted with the experiment data, name of the experiment, the plate number, the magnification used for imaging, and the well name all separated by underscores. All processed image files must be saved as `.png` files. In the file named `20190913_15hHB101foodpreps_01_2x_A01_overlay.png` the first section `20190913` is the experiment date, `15hHB101preps` is the name of the experiment, `01` is the plate number, `2x` is the magnification used for imaging, and `A01` is the well name.

## Pipeline

The complete easyxpress package consists of only seven functions: `readXpress`, `modelSelection`, `edgeFlag`, `setFlags`, `process`, `Xpress`, and `viewWell`.

### `read_data()`

`read_data()` can take as an argument a path to an experimental directory with CellProfiler data files. This directory should have CellProfiler data in a sub-folder named `cp_data`. The data can be in .Rda, .Rds, or .csv formats. If logical parameter, design is TRUE, a design file will be joined to data. The design file should be located in a sub-folder of the experimental directory named design. If FALSE no design file will be joined. This function will output a single data frame containing all CellProfiler model outputs as well as experimental treatments if a design file is used.

For further information use the command `?readXpress` to access the documentation.

### `modelSelection()`

### `edgeFlag()`

### `setFlags()`

### `process()`

### `Xpress()`

### `viewWell()`

If the `remote_server` parameter for this function is set to `TRUE` the `RCurl` package with SFTP protocol support is required. To ensure SFTP protocol support follow these instructions. 

1. In terminal install home brew if necessary https://brew.sh/
    + `/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"`
2. In terminal install curl with SFTP support https://github.com/marcelomazza/homebrew-curl-libssh2
    + `brew install marcelomazza/homebrew-curl-libssh2/curl`
3. In R, update PATH before installing RCurl. This only effects R session.
    + `Sys.setenv(PATH=paste('/usr/local/opt/curl/bin', Sys.getenv('PATH'), sep=":"))`
4. In R, confirm that new PATH looks for curl in /usr/local/opt/curl/bin first.
    + `Sys.getenv("PATH")`
5. In R, install RCurl from source
    + `install.packages("RCurl", type = "source")`
6.  In R, load RCurl package and check for sftp protocol support
```
library(RCurl)
RCurl::curlVersion()
```

### Overview

![Overview](./READMEfiles/Overview.png)

### Example
```r
library(easyxpress)
library(dplyr)

# Define your experimental directory
dirs <- "your_directory1"

# Read in the data
raw <- readXpress(dirs)

# Select appropriate generalized model dataframe
model_selected <- modelSelection(raw)

# Assign edge flags
edge_flagged <- edgeFlag(model_select)

# Flag all suspect data points within wells
raw_flagged <- setFlags(edge_flag)

# Process and summarize data
proc_raw <- process(raw_flagged, Metadata_Well)

# Review suspect data with image overlay
p1 <- viewWell(proc_raw, directory = "your_directory", plate = "your_plate", well = "your_well")

...
# Using the wrapper function
final_df <- Xpress(dirs, design = FALSE, radius = 825, cluster_flag = TRUE, well_edge_flag = TRUE, Metadata_Well)

# Save the final data frame
saveRDS(final_df, "~/cp_finaldf.rds")

```
