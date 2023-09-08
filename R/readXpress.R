#' readXpress
#'
#' This is the primary function for reading CellProfiler data
#' into R with this package.
#' It is built exclusively for use with worm image data saved as a .rda file.
#'
#' @param filedir The directory with CellProfiler data.
#' This directory should have CellProfiler .rda output in a sub-folder named cp_data.
#' @param rdafile The file name of a particular .rda file
#' to load from the cp_data sub-folder.
#' @param design Logical parameter, if TRUE then a design file
#' will be joined to data.
#' The design file should be located in a sub-folder
#' of the filedir named design.
#' If FALSE no design file will be joined.
#' @param px_per_um The number of pixels per micron (um) for the images.
#' This conversion factor will vary for different objectives or microscopes. The default is set for the AndersenLab
#' imageXpress nano 2X objective at \code{3.2937} pixels per micron (um). Please enter another conversion factor if necessary.
#' @param px_thresh A pixel threshold used to filter small objects from the data. The default setting is \code{30} pixels.
#' This is the standard threshold used for the AndersenLab images taken with the imageXpress nano using the 2X objective.
#' Please adjust if necessary.
#' @return A single data frame that contains
#' all CellProfiler model outputs as well as experimental treatments
#' if a design file is used. Several messages are also output to describe how objects have been filtered.
#' @importFrom dplyr %>%
#' @importFrom utils capture.output
#' @export

readXpress <- function(filedir, rdafile, design = FALSE, px_per_um = 3.2937, px_thresh = 30) {
  #loading specified .rda file
  message(glue::glue("loading from specified .rda:\n{filedir}/cp_data/{rdafile}"))# laod rda file
  #open data from .rda file
  load(glue::glue("{filedir}/cp_data/{rdafile}"))
  #extract names of data objects from .RData file
  data_names <- grep("model.outputs", ls(), value = TRUE)
  #join data objects and convert worm_length to microns
  # dynGet might not be what we want here look for alternatives
  suppressMessages(raw <- purrr::map(data_names, dynGet) %>%
    purrr::reduce(suppressMessages(dplyr::full_join)))

  # find the number of rows cut per model
  n.size.cut <- raw %>%
    dplyr::mutate(small = ifelse(Worm_Length < px_thresh, 1, 0)) %>%
    dplyr::group_by(model) %>%
    dplyr::mutate(filtered = sum(small, na.rm = T),
                  total_rows = dplyr::n()) %>%
    dplyr::distinct(model, total_rows, filtered) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(model = sub(model, pattern = ".model.outputs", replacement = ""))
  message(glue::glue("\nApplying pixel threshold of {px_thresh}px.\nThe number of filtered rows for each model are displayed below."))
  message(message(paste0(capture.output(knitr::kable(n.size.cut)), collapse = "\n")))

  # find the number of rows without parent objects
  n.parent.cut <- raw %>%
    dplyr::mutate(no.parent = ifelse(Parent_WormObjects == 0, 1, 0)) %>%
    dplyr::group_by(model) %>%
    dplyr::mutate(filtered = sum(no.parent, na.rm = T),
                  total_rows = dplyr::n()) %>%
    dplyr::distinct(model, total_rows, filtered) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(model = sub(model, pattern = ".model.outputs", replacement = ""))
  message(glue::glue("Applying missing parent filter.\nThe number of filtered rows for each model are displayed below."))
  message(message(paste0(capture.output(knitr::kable(n.parent.cut)), collapse = "\n")))

  # check whether primary object data are present and preform useful calculations.
  if ("po_AreaShape_Area" %in% names(raw)) {
    message("Primary object attributes detected.\nCalculating `wo_po_area_frac`.\n")
    raw_data_read <- raw %>%
      dplyr::filter(Worm_Length > px_thresh) %>% # filter objects smaller than threshold
      dplyr::filter(Parent_WormObjects != 0) %>% # Remove objects without a parent object.
      dplyr::mutate(worm_length_um = px_per_um * Worm_Length,
                    wo_po_area_frac = AreaShape_Area / po_AreaShape_Area)
  } else {
    message("Primary object attributes NOT detected in data.\nConsider running updated version of cellprofiler-nf if desired.\n")
    raw_data_read <- raw %>%
      dplyr::filter(Worm_Length > px_thresh) %>% # filter objects smaller than threshold
      dplyr::filter(Parent_WormObjects != 0) %>% # Remove objects without a parent object.
      dplyr::mutate(worm_length_um = px_per_um * Worm_Length)
  }

  # Check if you are not using a design file
  if (!design) {
    message("Design file not joined.\nPlease use 'design = TRUE' if you would like to join a design file.\n")
    raw_data <- raw_data_read
    # Return raw_data_read as raw_data
    message("DONE")
    return(raw_data)
  } else {
    #if you are using a design file
    #make a design file list
    design_file_list <- list.files(glue::glue("{filedir}/design"))
    #join design file to raw_data.
    message(glue::glue("Joining design file:\n{design_file_list}\n"))
    design_file <- readr::read_csv(glue::glue("{filedir}/design/{design_file_list[1]}"),
                                   guess_max = 50000,
                                   show_col_types = FALSE)

    #join to raw data
    suppressMessages(raw_data <- dplyr::left_join(raw_data_read, design_file))
    message("DONE")
    return(raw_data)
  }
}
