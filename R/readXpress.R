#' readXpress
#'
#' This function reads CellProfiler data into R. It is built exclusively for use with worm image data saved as a .rda file.
#'
#' @param filedir The project directory or directories with CellProfiler data.
#' Provide a full path to the directory or a vector of project directory paths.
#' Each directory must have a \code{cellprofiler-nf} output .rda file in a sub-folder named \code{cp_data}.
#' @param rdafile The specific .rda file name in the \code{cp_data} directory to read.
#' If multiple project directories are supplied to \code{filedir},
#' then include the .rda files for each project in the same order of the directories given in \code{filedir}.
#' @param design Logical parameter, if TRUE then a design file
#' will be joined to data.
#' The design file should be located in a sub-folder
#' of the filedir named design.
#' If FALSE no design file will be joined.
#' @param px_per_um The number of pixels per micron (um) for the images.
#' This conversion factor will vary for different objectives or microscopes. The default is set for the AndersenLab
#' imageXpress nano 2X objective at \code{3.2937} pixels per micron (um). Please enter another conversion factor if necessary.
#' @param length_thresh An object length threshold in um used to filter objects from the data. The default setting is \code{98.811} um.
#' This is the standard threshold used for the AndersenLab images taken with the imageXpress nano. Please adjust only if necessary.
#' @param doseR Logical, is this dose response data? The default, \code{doseR = FALSE},
#' expects control data to be recorded in the design file a particular way. Specifically, the drug and diluent variables should be identical for controls,
#' e.g, \code{drug = DMSO, diluent = DMSO, concentration_um = 0}. If \code{doseR = TRUE}, the controls are expected to be coded differently,
#' .e.g, \code{drug = ABZ, diluent = DMSO, concentration_um = 0}. Warning messages are produced if the controls do not fit expectations, but try to ensure the
#' controls are coded properly without relying this function to catch all edge cases.
#' @return A list including two elements
#' 1) A data frame that contains all CellProfiler model outputs as well as experimental treatments
#' if a design file is used. If multiple project directories and .rda files are supplied, they will be joined together.
#' 2) A data frame the contains the complete design file read from the project directory or directories.
#' This data frame is useful for checking the completeness of the data after filtering steps are completed.
#' @importFrom dplyr %>%
#' @importFrom utils capture.output
#' @export

readXpress <- function(filedir, rdafile, design = FALSE, px_per_um = 3.2937, length_thresh = 98.811, doseR = F) {
  # expecting message
  if(doseR == T){
  message("You set doseR = TRUE. Reading data as a dose response.")
  }
  if(doseR == F){
    message("You set doseR = FALSE. Not reading data as a dose reponse.")
  }
  #check for one project directory
  if(length(filedir) == 1 & length(rdafile) == 1) {
    # tell use about it
    message("1 project detected:")
    #loading specified .rda file
    message(glue::glue("loading data from .rda:\n{filedir}/cp_data/{rdafile}"))# laod rda file
    #open data from .rda file
    load(glue::glue("{filedir}/cp_data/{rdafile}"))
    #extract names of data objects from .RData file
    data_names <- grep("model.outputs", ls(), value = TRUE)
    # dynGet might not be what we want here look for alternatives
    suppressMessages(raw <- purrr::map(data_names, dynGet) %>%
                       purrr::reduce(suppressMessages(dplyr::full_join)) %>%
                       dplyr::mutate(worm_length_um = Worm_Length * px_per_um))
  }
  # check on multiple project directories
  if(length(filedir) > 1 &
     length(rdafile) > 1 &
     length(filedir) == length(rdafile)) {
    # tell use about it
    message(glue::glue("{length(filedir)} projects detected:"))
    #loading specified .rda file
    message(glue::glue("loading data from {length(filedir)} .rda files:"))
    # make a list to hold project data
    proj_list <- NULL
    for( i in 1:length(filedir)) {
      message(glue::glue("loading {filedir[i]}/cp_data/{rdafile[i]}"))
      #open data from .rda file
      load(glue::glue("{filedir[i]}/cp_data/{rdafile[i]}"))
      #extract names of data objects from .RData file
      data_names <- grep("model.outputs", ls(), value = TRUE)
      # dynGet might not be what we want here look for alternatives
      suppressMessages(raw <- purrr::map(data_names, dynGet) %>% #get vs dynGet
                         purrr::reduce(suppressMessages(dplyr::full_join)))
      proj_list[[i]] <- raw
    }
    # rbind the list
    raw <- data.table::rbindlist(proj_list) %>%
      dplyr::mutate(worm_length_um = Worm_Length * px_per_um)

  }
  if(length(filedir) > 1 &
     length(rdafile) > 1 &
     length(filedir) != length(rdafile)) {
    message(glue::glue("The number of file directories does not match the number of .rda files.\n
                       Please check that the filedir and rdafile arguments are specified correctly."))
    return()
  }
  # find the number of rows cut per model
  n.size.cut <- raw %>%
    dplyr::mutate(small = ifelse(worm_length_um < length_thresh, 1, 0)) %>%
    dplyr::group_by(model) %>%
    dplyr::mutate(filtered = sum(small, na.rm = T),
                  total_rows = dplyr::n()) %>%
    dplyr::distinct(model, total_rows, filtered) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(model = sub(model, pattern = ".model.outputs", replacement = ""))
  message(glue::glue("\nApplying length threshold of {length_thresh} um.\nThe number of filtered rows for each model are displayed below."))
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
      dplyr::filter(!is.na(Worm_Length)) %>% # filter non-length objects
      dplyr::filter(worm_length_um > length_thresh) %>% # filter objects smaller than threshold
      dplyr::filter(Parent_WormObjects != 0) %>% # Remove objects without a parent object.
      dplyr::mutate(wo_po_area_frac = AreaShape_Area / po_AreaShape_Area)
  } else {
    message("Primary object attributes NOT detected in data.\nConsider running updated version of cellprofiler-nf if desired.\n")
    raw_data_read <- raw %>%
      dplyr::filter(!is.na(Worm_Length)) %>% # filter non-length objects
      dplyr::filter(worm_length_um > length_thresh) %>% # filter objects smaller than threshold
      dplyr::filter(Parent_WormObjects != 0) # Remove objects without a parent object.
  }

  # Check if you are not using a design file
  if (!design) {
    message("Design file not joined.\nPlease use 'design = TRUE' if you would like to join a design file.\n")
    raw_data <- raw_data_read %>%
      dplyr::mutate(well.id = paste(Metadata_Experiment, Metadata_Plate, Metadata_Well, sep = "_")) %>%
      dplyr::select(well.id, everything())
    # Return raw_data_read as raw_data
    message("DONE")
    return(raw_data)
  } else {
    #make a design file list
    design_file_list <- list.files(glue::glue("{filedir}/design"))
    #if you are using a single design file
    if(length(filedir) == 1 & length(rdafile) == 1) {
      # report which design file is being joined
      message(glue::glue("Joining design file:\n{filedir}/design/{design_file_list[1]}\n"))
      design_file <- readr::read_csv(glue::glue("{filedir}/design/{design_file_list[1]}"),
                                     guess_max = 50000,
                                     show_col_types = FALSE)

      # check on control coding
      controls <- unique(design_file$diluent)
      drugs <- unique(design_file$drug)
      # warn about controls
      if(doseR == F & F %in% (controls %in% drugs)) {
        # send a warning.
        message(glue::glue("WARNING: the controls are not configured as expected in the design file for doseR = FALSE. Do you want doseR = TRUE? If not, Control conditions should have the same value for drug and diluent and a 0 for concentration_um. Please correct the control condition coding before using easyXpress well flag (WF) functions.
                           For example:"))
        example <- tibble::tibble(drug = c("DMSO", "Water", "death juice", "seizure sauce"),
                                  concentration_um = c(0, 0, 10, 100),
                                  diluent = c("DMSO", "water", "DMSO", "water"))
        message(message(paste0(capture.output(knitr::kable(example)), collapse = "\n")))
      }
      # warn about doseR=T
      if(doseR == T & T %in% (controls %in% drugs)){
        # send a warning.
        message(glue::glue("WARNING: the controls are not configured as expected in the design file for doseR = TRUE. Do you want doseR = FALSE? If not, control conditions should have 0 for concentration_um and be named for the drug not the diluent. Please correct the control condition coding before using easyXpress well flag (WF) functions.
                           For example:"))
        example <- tibble::tibble(drug = c("death juice", "death juice", "seizure sauce", "seizure sauce"),
                                  concentration_um = c(0, 10, 0, 100),
                                  diluent = c("DMSO", "DMSO", "water", "water"))
        message(message(paste0(capture.output(knitr::kable(example)), collapse = "\n")))
      }
      #join to raw data
      suppressMessages(raw_data <- dplyr::left_join(raw_data_read, design_file) %>%
                         dplyr::mutate(well.id = paste(Metadata_Experiment, Metadata_Plate, Metadata_Well, sep = "_")) %>%
                         dplyr::select(well.id, everything()))
      message("DONE")
      return(list(raw_data = raw_data, design = design_file))
    }
    # if you are processing multiple projects
    if(length(filedir) > 1 & length(rdafile) > 1) {
      # check that design files have Metadata Experiment variable in them.
      design_data_list <- NULL
      for( i in 1:length(design_file_list)){
        design_file <- readr::read_csv(glue::glue("{filedir[i]}/design/{design_file_list[i]}"),
                                       guess_max = 50000,
                                       show_col_types = FALSE)
        if("Metadata_Experiment" %in% names(design_file)) {
          # report which design file is being joined
          message(glue::glue("joining design file:\n{filedir[i]}/design/{design_file_list[i]}\n"))
        } else {
          stop(glue::glue("The following design file is missing a Metadata_Experiment variable.
                          Please ensure each design file has a Metadata_Experiment variable if multiple projects are being processed.
                          {filedir[i]}/design/{design_file_list[i]}"))
        }
        design_data_list[[i]] <- design_file
      }
      # bind the list
      design_file <- data.table::rbindlist(design_data_list)

      # check on control coding
      controls <- unique(design_file$diluent)
      drugs <- unique(design_file$drug)
      if(doseR == F & F %in% (controls %in% drugs)) {
        # send a warning.
        message(glue::glue("WARNING: the controls are not configured as expected in the design file doseR = FALSE. Do you want doseR = TRUE? If not, control conditions should have the same value for drug and diluent and a 0 for concentration_um. Please correct the control condition coding before using easyXpress well flag (WF) functions.
                           For example:"))
        example <- tibble::tibble(drug = c("DMSO", "water", "death juice", "seizure sauce"),
                                  concentration_um = c(0, 0, 10, 100),
                                  diluent = c("DMSO", "water", "DMSO", "water"))
        message(message(paste0(capture.output(knitr::kable(example)), collapse = "\n")))
      }
      # warn about doseR=T
      if(doseR == T & T %in% (controls %in% drugs)){
        # send a warning.
        message(glue::glue("WARNING: the controls are not configured as expected in the design file for doseR = TRUE. Do you want doseR = FALSE? If not, control conditions should have 0 for concentration_um and be named for the drug not the diluent. Please correct the control condition coding before using easyXpress well flag (WF) functions.
                           For example:"))
        example <- tibble::tibble(drug = c("death juice", "death juice", "seizure sauce", "seizure sauce"),
                                  concentration_um = c(0, 10, 0, 100),
                                  diluent = c("DMSO", "DMSO", "water", "water"))
        message(message(paste0(capture.output(knitr::kable(example)), collapse = "\n")))
      }

      #ADD A CHECK ON VARIABLE CLASS! BLEACH e.g.

      #join to raw data
      suppressMessages(raw_data <- dplyr::left_join(raw_data_read, design_file, by = c("Metadata_Experiment", "Metadata_Plate", "Metadata_Well")) %>%
                         dplyr::mutate(well.id = paste(Metadata_Experiment, Metadata_Plate, Metadata_Well, sep = "_")) %>%
                         dplyr::select(well.id, everything()))
      message("DONE")
      return(list(raw_data = raw_data, design = design_file))
    }
  }
}

