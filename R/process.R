#' process
#'
#' @param flag_data Flagged data to be summarized
#' @param ... specify variable used to summarize data
#' @return List with four elements: raw data, processed data, and summaries for both datasets.
#' @importFrom stats quantile sd median
#' @importFrom dplyr contains
#' @export

process <- function(flag_data, ...) {

  #Find model names
  model_names <- levels(flag_data$model)

  # 1 -- data as is, after above steps. Nothing removed
  raw_data <- flag_data

  # 2 -- removing rows where flag = TRUE
  processed_data <- flag_data %>%
    dplyr::filter(flag_removed == TRUE) %>% # filters to worm objects for which flagged data are appropriately removed. See "set_flags()" function
    dplyr::filter(well_outlier_flag == FALSE) # filters to worm objects that are NOT within well outliers

  # 3 -- summarizing #1 by "..."
  summarized_raw <- raw_data %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(mean_wormlength_um = mean(worm_length_um, na.rm = TRUE),
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
    dplyr::left_join(., raw_data) %>%
    dplyr::select(-contains("Worm_"),
                  -contains("Distance_"),
                  -contains("Intensity_"),
                  -contains("Location_"),
                  -contains("AreaShape_"),-contains("Distance_"),
                  -contains("Image"),
                  -contains("flag"),
                  -contains("Number"),
                  -dplyr::one_of(model_names),
                  -contains("model"),
                  -contains("Parent_")) %>%
    dplyr::distinct(..., .keep_all=T)

  # 4 -- summarizing #2 by "..."
  summarized_processed <- processed_data %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(mean_wormlength_um = mean(worm_length_um, na.rm = TRUE),
                     min_wormlength_um = as.numeric(quantile(worm_length_um, na.rm = TRUE)[1]),
                     q10_wormlength_um = as.numeric(quantile(worm_length_um, probs = 0.1, na.rm = TRUE)[1]),
                     q25_wormlength_um = as.numeric(quantile(worm_length_um, probs = 0.25, na.rm = TRUE)[1]),
                     median_wormlength_um = median(worm_length_um, na.rm = T),
                     sd_wormlength_um = sd(worm_length_um, na.rm = T),
                     q75_wormlength_um = as.numeric(quantile(worm_length_um, probs = 0.75, na.rm = TRUE)[1]),
                     q90_wormlength_umF = as.numeric(quantile(worm_length_um, probs = 0.90, na.rm = TRUE)[1]),
                     max_wormlength_um = as.numeric(quantile(worm_length_um, na.rm = TRUE)[5]),
                     cv_wormlength_um = (sd_wormlength_um/mean_wormlength_um),
                     n = dplyr::n()) %>%
    dplyr::left_join(., processed_data) %>%
    dplyr::select(-contains("Worm_"),
                  -contains("Distance_"),
                  -contains("Intensity_"),
                  -contains("Location_"),
                  -contains("AreaShape_"),-contains("Distance_"),
                  -contains("Image"),
                  -contains("flag"),
                  -contains("Number"),
                  -dplyr::one_of(model_names),
                  -contains("model"),
                  -contains("Parent_")) %>%
    dplyr::distinct(..., .keep_all=T)

  sum_by <- raw_data %>%
    dplyr::select(...)
  print(paste("SUMMARIZED BY", names(sum_by))) #what is bring summarized

  return(list("raw_data" = raw_data,
              "processed_data" = processed_data,
              "summarized_raw" = summarized_raw,
              "summarized_processed" = summarized_processed))
}
