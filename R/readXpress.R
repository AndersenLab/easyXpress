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
#' @return A single data frame that contains
#' all CellProfiler model outputs as well as experimental treatments
#' if a design file is used. If multiple project directories and .rda files are supplied,
#' they will be joined together. Several messages are also output to describe how objects have been filtered.
#' @importFrom dplyr %>%
#' @importFrom utils capture.output
#' @export

readXpress <- function(filedir, rdafile, design = FALSE, px_per_um = 3.2937, length_thresh = 98.811) {
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
    raw_data <- raw_data_read
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

      #join to raw data
      suppressMessages(raw_data <- dplyr::left_join(raw_data_read, design_file))
      message("DONE")
      return(raw_data)
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
      #join to raw data
      suppressMessages(raw_data <- dplyr::left_join(raw_data_read, design_file, by = c("Metadata_Experiment", "Metadata_Plate", "Metadata_Well")))
      message("DONE")
      return(raw_data)
    }
  }
}

