## ---- include = FALSE----------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  fig.align = "center",
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------------------------------------------------------------------------------------------
library(easyXpress)

## ----message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------
## In this example, there is no design file. As such, the argument design = FALSE

# Define experimental directory and file name
dirs <- rprojroot::find_package_root_file("vignettes", "example_data")
datafile <- "CellProfiler-Analysis_20191119_example_data.RData"

# Read in the data
raw <- easyXpress::readXpress(filedir = dirs, rdafile = datafile, design = FALSE)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------------------------------------------
library(tidyverse)
subset <- raw %>%
  dplyr::filter(model == "L1") %>%
  dplyr::select(Metadata_Experiment,Metadata_Plate,Metadata_Well,Image_FileName_RawBF,model, worm_length_um) 
knitr::kable(subset[1:4,], caption = "Subset of data frame")

## ----message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------
model_selected <- easyXpress::modelSelection(raw)

## ----echo=FALSE----------------------------------------------------------------------------------------------------------------------------------------------
subset <- model_selected
knitr::kable(subset[1:4,1:9], caption = "Subset of data frame")

## ------------------------------------------------------------------------------------------------------------------------------------------------------------
edge_flagged <- easyXpress::edgeFlag(model_selected, radius=825, center_x=1024, center_y=1024)

## ----message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------
raw_flagged <- easyXpress::setFlags(edge_flagged, cluster_flag = TRUE, well_edge_flag = TRUE)

## ----message=FALSE, paged.print=TRUE-------------------------------------------------------------------------------------------------------------------------
processed <- easyXpress::process(raw_flagged, Metadata_Plate, Metadata_Well)

## ----echo=FALSE----------------------------------------------------------------------------------------------------------------------------------------------
knitr::kable(summary(processed), caption = "Processed list items")

## ----message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------
final_df <- easyXpress::Xpress(filedir = dirs, rdafile = datafile, Metadata_Plate, Metadata_Well)

## ----eval=FALSE, include=TRUE--------------------------------------------------------------------------------------------------------------------------------
#  # Saving summarized_processed_data list element to new variable
#  sumproc_data <- processed[[4]]
#  
#  easyXpress::viewPlate(sumproc_data, "p05")
#  
#  ## Not run ##

## ----fig.height=5, fig.width=7-------------------------------------------------------------------------------------------------------------------------------
## This example shows the processed data

# Saving processed_data list element to new variable
proc_data <- processed[[2]]

# Define processed image directory
proc_img_dir <- rprojroot::find_package_root_file("vignettes", "example_data", "ProcessedImages")

easyXpress::viewWell(proc_data, proc_img_dir, "p57", "C07", boxplot = TRUE) 

