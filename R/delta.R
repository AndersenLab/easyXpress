#' delta
#'
#' This function will calculate the difference in well summary statistics between the experimental condition and the median control condition within a group.
#' The proper grouping variables are supplied with \code{...}.
#'
#' @param data A data frame output from any \code{WF} function.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Variable(s) used to group data. It is recommended to group data to independent bleaches for all strains. Variable names in data are supplied separated by commas and without quotes.
#' For example, the typical variables for grouping are \code{Metadata_Experiment, bleach, strain}.
#' @param WF Select \code{"filter"} or \code{"ignore"}. The default, \code{"filter"}, will filter out all flagged wells before calculating the delta from control.
#' \code{"ignore"} will calculate the delta including all flagged data. Be careful using \code{"ignore"}, it is only included for diagnostic purposes.
#' @param vars The well summary statistics to perform the delta calculation on. These are supplied in a character vector. For example, the default is set to \code{c("median_wormlength_um", "cv_wormlength_um")}.
#' @return A data frame identical to the input but with control delta variables added, i.e., \code{median_wormlength_um_delta} and \code{cv_wormlength_um_delta}.
#' The median values for the control conditions are also added as \code{control_median_wormlength_um} and \code{control_cv_wormlength_um}.
#' @export

delta <- function(data, ..., WF = "filter", vars = c("median_wormlength_um", "cv_wormlength_um")) {
  # check on control coding
  controls <- unique(data$diluent)
  drugs <- unique(data$drug)

  if(F %in% (controls %in% drugs)) {
    # send a stop message
    message(glue::glue("ERROR: the controls are not configured as expected. Control conditions should have the same value for drug and diluent and a 0 for concentration_um. Please correct the control condition coding before using easyXpress well flag functions.
                           For example:"))
    example <- tibble::tibble(drug = c("DMSO", "Water", "death juice", "seizure sauce"),
                              concentration_um = c(0, 0, 10, 100),
                              diluent = c("DMSO", "Water", "DMSO", "Water"))
    stop(message(message(paste0(capture.output(knitr::kable(example)), collapse = "\n"))))
  }

  # filter wells if needed
  if(WF == "filter") {
    d <- easyXpress::filterWF(data = data, rmVars = T)
    message(glue::glue("Flagged wells are filtered from the data."))
  } else {
    d <- data
    message(glue::glue('Flagged wells are NOT being filtered from the data. This is NOT the recommended approach. Please consider WF = "filter".'))
  }

  # grouped by message
  group_by <- paste(names(d %>% dplyr::select(...)), collapse = ", ")
  message(glue::glue("The data are grouped by, {group_by}."))

  # setup means
  # get control values for each group
  control_values <- d %>%
    dplyr::filter(drug %in% controls) %>% # filter to control wells
    dplyr::group_by(...) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(vars), ~mean(.), .names = "control_{.col}")) %>% # get control values
    dplyr::distinct(dplyr::across(dplyr::matches("_delta|control_"))) %>%
    dplyr::ungroup()

  # calculate the delta
  suppressMessages(delta <- d %>%
    dplyr::left_join(., control_values) %>%
    dplyr::group_by(...) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(vars), .names = "{.col}_delta") - dplyr::across(dplyr::starts_with("control_"))) %>% # get the delta values
    dplyr::ungroup())

  # message
  message("The mean control value within groups has been subtracted from the well summary statstics:")

  message(glue::glue("{paste(vars, collapse = '\n')}"))
  # return the data
  return(delta)
}
