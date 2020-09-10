#' readXpress
#'
#' This is the primary fuction for reading CellProfiler data
#' into R with this package.
#' It is built exclusively for use with worm image data.
#'
#' @param filedir The directory with CellProfiler data.
#' This directory should have CellProfiler data in a sub-folder named cp_data.
#' The data can be in .Rda, .Rds, or .csv formats.
#' @param design Logical parameter, if TRUE then a design file
#' will be joined to data.
#' The design file should be located in a sub-folder
#' of the filedir named design.
#' If FALSE no design file will be joined.
#' @param rdafile The file name of a particular .rda file
#' to load from the cp_data sub-folder.
#' @return A single data frame named raw_data that contains
#' all CellProfiler model outputs as well as experimental treatments
#' if a design file is used.
#' @importFrom dplyr %>%
#' @export
readXpress <- function(filedir, design = FALSE, rdafile = NULL) {
  if (is.null(rdafile)) {
    #create data file list
    data_file_list <- list.files(glue::glue("{filedir}/cp_data"))
    data_file_path_list <- list.files(glue::glue("{filedir}/cp_data"),
                                      full.names = T)
    #Identify file types for CellProfiler output data
    file_rda <- stringr::str_detect(data_file_path_list, c(".Rda|.RData|.rda"))
    file_rds <- stringr::str_detect(data_file_path_list, c(".RDS|.Rds|.rds"))
    file_csv <- stringr::str_detect(data_file_list, ".csv")
    if (any(file_rda == TRUE)) {
      #Read data from .RData file
      print("Reading CellProfiler output from .RData files")
      #open data from .RData file
      load(glue::glue("{filedir}/cp_data/{data_file_list[1]}"))
      #extract names of data objects from .RData file
      data_names <- grep("model.outputs", ls(), value = TRUE)
      #join data objects and convert worm_length to microns
      # dynGet might not be what we want here look for alternatives
      raw_data_read <- purrr::map(data_names, dynGet) %>%
        purrr::reduce(dplyr::full_join) %>%
        dplyr::mutate(worm_length_um = 3.2937 * Worm_Length)
    }
    if (any(file_rds == TRUE)) {
      #Read data from .Rds files
      print("Reading CellProfiler output from .Rds files")
      #extract model names from .csv file(s)
      model_names <- stringr::str_replace(
        stringr::str_replace(data_file_list, "[:punct:]", ""),
        "RDS|Rds|rds", "")
      #open data from .Rds file(s), name, join, and convert worm_length to microns
      raw_data_read <- purrr::map2_dfr(data_file_path_list, model_names,
                                       ~readr::read_rds(.x) %>%
                                         dplyr::mutate(model = .y)) %>%
        dplyr::mutate(worm_length_um = 3.2937 * Worm_Length)
    }
    if (any(file_csv == TRUE)) {
      #Read data from .csv files
      print("Reading CellProfiler output from .csv files")
      #extract model names from .csv file(s)
      model_names <- stringr::str_replace(
        stringr::str_replace(data_file_list, "[:punct:]", ""),
        "csv", "")
      #open data from .csv file(s), name, join, and convert worm_length to microns
      raw_data_read <- purrr::map2_dfr(data_file_path_list, model_names,
                                       ~readr::read_csv(.x) %>%
                                         dplyr::mutate(model = .y)) %>%
        dplyr::mutate(worm_length_um = 3.2937 * Worm_Length)
    }
  } else { #if you are specifying a particular .rda file
    message(glue::glue("loading from specified .rda: {filedir}/cp_data/{rdafile}"))# laod rda file
    #open data from .rda file
    load(glue::glue("{filedir}/cp_data/{rdafile}"))
    #extract names of data objects from .RData file
    data_names <- grep("model.outputs", ls(), value = TRUE)
    #join data objects and convert worm_length to microns
    # dynGet might not be what we want here look for alternatives
    raw_data_read <- purrr::map(data_names, dynGet) %>%
      purrr::reduce(dplyr::full_join) %>%
      dplyr::mutate(worm_length_um = 3.2937 * Worm_Length)
  }
  if (!design) { #if you are not using a design file
    print("NO DESIGN FILE LOADED. SET DESIGN = TRUE TO LOAD A DESIGN FILE")
    raw_data <- raw_data_read
    # Return raw_data_read as raw_data
    return(raw_data)
  } else { #if you are using a design file
    #make a design file list
    design_file_list <- list.files(glue::glue("{filedir}/design"))
    #join design file to raw_data. na.omit will remove rows with any NAs
    design_file <- readr::read_csv(glue::glue("{filedir}/design/{design_file_list[1]}"), guess_max = 50000) %>%
      dplyr::mutate(Metadata_Plate = dplyr::case_when(
        Metadata_Plate == 1 ~ "01",
        Metadata_Plate == 2 ~ "02",
        Metadata_Plate == 3 ~ "03",
        Metadata_Plate == 4 ~ "04",
        Metadata_Plate == 5 ~ "05",
        Metadata_Plate == 6 ~ "06",
        Metadata_Plate == 7 ~ "07",
        Metadata_Plate == 8 ~ "08",
        Metadata_Plate == 9 ~ "09",
        Metadata_Plate >= 10 ~ as.character(Metadata_Plate)))
    if (class(raw_data_read$Metadata_Plate) == "character") {
      #join to raw data
      raw_data <- dplyr::left_join(raw_data_read, design_file)
      return(raw_data)
    } else {
      #Set Metadata_Plate to character
      raw_data <- raw_data_read %>%
        dplyr::mutate(Metadata_Plate = dplyr::case_when(
          Metadata_Plate == 1 ~ "01",
          Metadata_Plate == 2 ~ "02",
          Metadata_Plate == 3 ~ "03",
          Metadata_Plate == 4 ~ "04",
          Metadata_Plate == 5 ~ "05",
          Metadata_Plate == 6 ~ "06",
          Metadata_Plate == 7 ~ "07",
          Metadata_Plate == 8 ~ "08",
          Metadata_Plate == 9 ~ "09",
          Metadata_Plate >= 10 ~ as.character(Metadata_Plate))) %>%
        dplyr::left_join(design_file)
      return(raw_data)
    }
  }
}
