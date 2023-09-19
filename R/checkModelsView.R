#' checkModelsView
#'
#' This function will create an one or more arrays of image overlays to help users evaluate cellprofiler model performance and object length thresholds.
#'
#' @param data A data frame output from the \code{modelSelection} function or any \code{OF} function from easyXpress.
#' @param ... Variable(s) used to group data. Variable names in data are supplied separated by commas and without quotes. For example \code{drug, concentration_um}.
#' @param modelName The name of the cellprofiler model to check in the diagnostic plots. Typically this is the smallest model in the dataset.
#' The default is set to \code{"MDHD"}.
#' @param OF Select one of \code{"filter", "ignore"}. The default is \code{"filter"}, which will filter out all objects flagged by OFs.
#' \code{"ignore"} will include all objects in the diagnostic plots.
#' @param length_thresh An object length threshold in um used to label objects from the data. The default setting is \code{164.685} um.
#' This is the standard threshold used for the AndersenLab 2X objective images. Please adjust if necessary.
#' @param strainN The maximum number of strains to include in a overlay. This function will return at maximum \code{strainN} strains per overlay.
#' These are strains are choosen because they have the lowest average object length in the group.
#' @param wellN The maximum number of wells to include in a overlay. This function will return at maximum \code{wellN} wells per strain within an overlay.
#' These wells are choosen because they have the lowest average object length within the strain.
#' @param proc.img.dir Supply one of two options. 1 - The full path to a directory holding all processed images. For example, "~/proc_images/".
#' 2 - A variable name in \code{data} that holds the full path to the directory holding processed images matching the data.
#' Users will need to add this variable to the data frame themselves. For example, "image_path".
#' For either option to work the processed images must have the standard \code{_overlay.png} suffix and file name output from CellProfiler.
#' @param well.label A variable name in \code{data} to display as a well label. For example, "Metadata_Well".
#' @param out.dir The full path to an existing directory that will be used to hold the overlays.
#' @return A dataframe nested by the grouping variables with or without flagged objects removed.
#' @importFrom dplyr contains
#' @export

checkModelsView <- function(data, ..., modelName = "MDHD", OF = "filter", length_thresh = 164.685, strainN = 4,
                            wellN = 4, proc.img.dir, well.label, out.dir) {
  # check OF argument and stop if invalid
  if(OF %in% c("filter", "ignore") == F) {
    stop(message(glue::glue("The OF argument is not recognized. Please set it to 'ignore' or 'filter'.")))
  }
  # make color and shape palettes for plotting
  my.col.pal <- c("#009200","#009200", "#990000", "#990000")

  names(my.col.pal) <- c(paste0(modelName, " > ", round(length_thresh, digits = 0), "um"),
                         paste0("other > ", round(length_thresh, digits = 0), "um"),
                         paste0(modelName, " <= ", round(length_thresh, digits = 0), "um"),
                         paste0("other <=", round(length_thresh, digits = 0), "um"))

  my.shape.pal <- c(22, 21, 22, 21)

  names(my.shape.pal) <- c(paste0(modelName, " > ", round(length_thresh, digits = 0), "um"),
                    paste0("other > ", round(length_thresh, digits = 0), "um"),
                    paste0(modelName, " <= ", round(length_thresh, digits = 0), "um"),
                    paste0("other <=", round(length_thresh, digits = 0), "um"))


  # set the obj.label
  o.data <- data %>%
    dplyr::mutate(obj.label = dplyr::case_when(model == modelName & worm_length_um <= length_thresh ~ paste0(modelName, " <= ", round(length_thresh, digits = 0), "um"),
                                        model == modelName & worm_length_um > length_thresh ~ paste0(modelName, " > ", round(length_thresh, digits = 0), "um"),
                                        model != modelName & worm_length_um <= length_thresh ~ paste0("other <=", round(length_thresh, digits = 0), "um"),
                                        model != modelName & worm_length_um > length_thresh ~ paste0("other > ", round(length_thresh, digits = 0), "um"),
                                        TRUE ~ "unknown"))

  if(OF == "filter"){
    message("Filtering flagged objects from data.")
    # filter all object flags
    of.data <- easyXpress::filterOF(data = o.data, rmVars = T)
  }
  if(OF =="ignore"){
    of.data <- o.data
  }

  # nest the data
  nest <- of.data %>%
    dplyr::group_by(drug, concentration_um) %>% #... drug, concentration_um
    tidyr::nest() %>%
    tidyr::unite(col = group, -data)

  # work through the groups
  for(i in unique(nest$group)) {
    # setup data
    d <- nest %>%
      dplyr::filter(group == i) %>%
      tidyr::unnest(cols = data) %>%
      dplyr::group_by(Metadata_Experiment, Metadata_Plate, Metadata_Well) %>%
      dplyr::mutate(mean_wormlength_um = mean(worm_length_um, na.rm = TRUE)) %>%
      dplyr::group_by(strain) %>%
      dplyr::mutate(mean_strain_length = mean(mean_wormlength_um, na.rm = TRUE)) %>%
      dplyr::ungroup()

    # get lowest strainN strains
    strains <- d %>%
      dplyr::distinct(strain, mean_strain_length) %>%
      dplyr::arrange(mean_strain_length) %>%
      dplyr::mutate(slice_index = 1:dplyr::n()) %>%
      dplyr::filter(slice_index <= strainN) %>%
      dplyr::pull(strain)

    # get lowest wellN wells for those strains
    wells <- d %>%
      dplyr::distinct(Metadata_Experiment, Metadata_Plate, Metadata_Well, strain, mean_wormlength_um) %>%
      dplyr::filter(strain %in% strains) %>%
      dplyr::group_by(strain) %>%
      dplyr::arrange(mean_wormlength_um) %>%
      dplyr::mutate(slice_index = 1:dplyr::n()) %>%
      dplyr::filter(slice_index <= wellN) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(well_id = paste(Metadata_Experiment, Metadata_Plate, Metadata_Well, sep = "_")) %>%
      dplyr::pull(well_id)

    # filter down d
    f.d <- d %>%
      dplyr::filter(strain %in% strains) %>%
      dplyr::mutate(checkMV_well_id = paste(Metadata_Experiment, Metadata_Plate, Metadata_Well, sep = "_")) %>%
      dplyr::filter(checkMV_well_id %in% wells) %>%
      dplyr::arrange(strain)

    # setup arg list
    argsList = list(data = f.d, proc.img.dir = proc.img.dir, well.label = well.label,
                    obj.label = "obj.label", obj.col.pal = my.col.pal, obj.shape.pal = my.shape.pal, file = paste0(out.dir, "/", i, ".png"))
    # plot it
    p = do.call('viewOverlay', args = argsList)

  }
  # return the plot
  message("DONE")
  return(nest)
}
