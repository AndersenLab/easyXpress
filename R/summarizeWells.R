#' summarizeWells
#'
#' Summarize the objects within wells by calculating various population statistics, including the median, mean, standard deviation, and coefficient of variation (CV) for \code{worm_length_um}.
#' The number of objects in the wells are also calculated. The output can be used with the various group flag (GF) functions if desired.
#'
#'
#' @param data A data frame output from the \code{model_selection} or \code{OF} functions.
#' @param OF Specify either \code{"filter"} or \code{"ignore"}. The default is \code{"filter"}, which will filter out flagged objects.
#' \code{"ignore"} will ignore flagged data and include those objects in the summary statistics for a well. Setting OF to \code{"ignore"} is NOT recommended.
#' @param drop Logical, if \code{TRUE}, the default. The standard variables holding object data are dropped.
#' Be careful using \code{FALSE}. If you plan to return to using the object level data, only the variables will be retained but all rows (objects) will not be present.
#' Therefore, the unsummarized object data will need to be rejoined with the summarized data to have both present in a single dataframe. The best practice is to retain the object level data in one data frame
#' and summarized well data in another, which can be used with the group flag (GF) functions to complete the processing.
#' @return A data frame with summary statistics for wells to be used with easyXpress group flag (GF) functions.
#' @export

summarizeWells <- function(data, OF = "filter", drop = T){
  if(!(OF %in% c("ignore", "filter"))) {
    stop('Invalid OF argument. Please set the OF argument to either "ignore" or "filter"')
  }
  if(OF == "filter") {
    # filter the flagged objects
    data.of <- easyXpress::filterOF(data = data, rmVars = T)
    message("All flagged objects are filtered prior to summarizing wells.")

    if(drop == T){
      #Find model names to cull data
      model_names <- levels(data$model)

      # drop object data
      to.summarize <- data.of %>%
        dplyr::select(-contains("wellmask"),
                      -contains("PathName_"),
                      -contains("Intensity_"),
                      -contains("Location_"),
                      -contains("AreaShape_"),
                      -contains("Distance_"),
                      -contains("Image"),
                      -contains("flag"),
                      -contains("Number"),
                      -dplyr::one_of(model_names),
                      -contains("model"),
                      -contains("Parent_"))
      message("The standard object variables are dropped from the summarized data.")
    } else {
      to.summarize <- data.of
    }
  }
  if(OF == "ignore") {
    if(drop == T){
      #Find model names to cull data
      model_names <- levels(data$model)

      # drop object data
      to.summarize <- data %>%
        dplyr::select(-contains("wellmask"),
                      -contains("PathName_"),
                      -contains("Distance_"),
                      -contains("Intensity_"),
                      -contains("Location_"),
                      -contains("AreaShape_"),
                      -contains("Distance_"),
                      -contains("Image"),
                      -contains("flag"),
                      -contains("Number"),
                      -dplyr::one_of(model_names),
                      -contains("model"),
                      -contains("Parent_"))
      message("The standard object variables are dropped from the summarized data.")
    } else {
      to.summarize <- data
    }
  }
  # do the summary
  out <- to.summarize %>%
    dplyr::group_by(Metadata_Experiment, Metadata_Plate, Metadata_Well) %>%
    dplyr::mutate(mean_wormlength_um = mean(worm_length_um, na.rm = TRUE),
                     min_wormlength_um = as.numeric(quantile(worm_length_um, na.rm = TRUE)[1]),
                     q10_wormlength_um = as.numeric(quantile(worm_length_um, probs = 0.1, na.rm = TRUE)[1]),
                     q25_wormlength_um = as.numeric(quantile(worm_length_um, probs = 0.25, na.rm = TRUE)[1]),
                     median_wormlength_um = median(worm_length_um, na.rm = T),
                     sd_wormlength_um = sd(worm_length_um, na.rm = T),
                     q75_wormlength_um = as.numeric(quantile(worm_length_um, probs = 0.75, na.rm = TRUE)[1]),
                     q90_wormlength_um = as.numeric(quantile(worm_length_um, probs = 0.90, na.rm = TRUE)[1]),
                     max_wormlength_um = as.numeric(quantile(worm_length_um, na.rm = TRUE)[5]),
                     cv_wormlength_um = (sd_wormlength_um/mean_wormlength_um),
                     n = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-contains("Worm_")) %>%
    dplyr::distinct(Metadata_Experiment, Metadata_Plate, Metadata_Well, .keep_all = T) %>%
    dplyr::mutate(well.id = paste(Metadata_Experiment, Metadata_Plate, Metadata_Well, sep = "_")) %>%
    dplyr::select(well.id, dplyr::everything())
  # message
  message("The well.id variable is added to the output data using paste(Metadata_Experiment, Metadata_Plate, Metadata_Well, sep = '_')")

  # return it
  return(out)
}
