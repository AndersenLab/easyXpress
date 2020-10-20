#' readXpress
#'
#' This is the primary function for reading CellProfiler data
#' into R with this package.
#' It is built exclusively for use with worm image data saved as a .RData file
#'
#' @param filedir The directory with CellProfiler data.
#' This directory should have CellProfiler .Rda output in a sub-folder named cp_data.
#' @param rdafile The file name of a particular .rda file
#' to load from the cp_data sub-folder.
#' @param design Logical parameter, if TRUE then a design file
#' will be joined to data.
#' The design file should be located in a sub-folder
#' of the filedir named design.
#' If FALSE no design file will be joined.
#' @return A single data frame named raw_data that contains
#' all CellProfiler model outputs as well as experimental treatments
#' if a design file is used.
#' @importFrom dplyr %>%
#' @export

readXpress <- function(filedir, rdafile, design = FALSE) {
  #loading specified .rda file
  print(glue::glue("loading from specified .rda: {filedir}/cp_data/{rdafile}"))# laod rda file
  #open data from .rda file
  load(glue::glue("{filedir}/cp_data/{rdafile}"))
  #extract names of data objects from .RData file
  data_names <- grep("model.outputs", ls(), value = TRUE)
  #join data objects and convert worm_length to microns
  # dynGet might not be what we want here look for alternatives
  raw_data_read <- purrr::map(data_names, dynGet) %>%
    purrr::reduce(dplyr::full_join) %>%
    dplyr::mutate(worm_length_um = 3.2937 * Worm_Length)

  if (!design) { #if you are not using a design file
    print("NO DESIGN FILE LOADED. SET DESIGN = TRUE TO LOAD A DESIGN FILE")
    raw_data <- raw_data_read
    # Return raw_data_read as raw_data
    return(raw_data)
  } else {
    #if you are using a design file
    #make a design file list
    design_file_list <- list.files(glue::glue("{filedir}/design"))
    #join design file to raw_data.
    design_file <- readr::read_csv(glue::glue("{filedir}/design/{design_file_list[1]}"), guess_max = 50000)

    #join to raw data
    raw_data <- dplyr::left_join(raw_data_read, design_file)
    return(raw_data)
  }
}
