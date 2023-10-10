#' delta
#'
#' This function will calculate the difference in well summary statistics between the experimental condition and the median control condition within a group.
#' The proper grouping variables are supplied with \code{...}.
#'
#' @param data A data frame output from any \code{WF} function.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Variable(s) used to group data. It is recommended to group data to independent bleaches for all strains. Variable names in data are supplied separated by commas and without quotes.
#' For example, the typical variables for grouping are \code{Metadata_Experiment, bleach, strain}.
#' @param WF Select \code{"filter"} or \code{"ignore"}. The default, \code{"filter"}, will filter out all flagged wells before calculating the delta from control, if present.
#' \code{"ignore"} will calculate the delta including all flagged data. Be careful using \code{"ignore"}, it is only included for diagnostic purposes.
#' @param vars The well summary statistics to perform the delta calculation on. These are supplied in a character vector. For example, the default is set to \code{c("median_wormlength_um", "cv_wormlength_um")}.
#' @param doseR Logical, is this dose response data? The default, \code{doseR = FALSE},
#' expects control data to be recorded in the design file a particular way. Specifically, the drug and diluent variables should be identical for controls,
#' e.g, \code{drug = DMSO, diluent = DMSO, concentration_um = 0}. If \code{doseR = TRUE}, the controls are expected to be coded differently,
#' .e.g, \code{drug = ABZ, diluent = DMSO, concentration_um = 0}. Warning messages are produced if the controls do not fit expectations, but try to ensure the
#' controls are coded properly without relying this function to catch all edge cases.
#' @return A data frame identical to the input but with control delta variables added, i.e., \code{median_wormlength_um_delta} and \code{cv_wormlength_um_delta}.
#' The median values for the control conditions are also added as \code{control_median_wormlength_um} and \code{control_cv_wormlength_um}.
#' @export

delta <- function(data, ..., WF = "filter", vars = c("median_wormlength_um", "cv_wormlength_um"), doseR = F) {
  # expecting message
  if(doseR == T){
    message("You set doseR = TRUE. Expecting controls to be coded as for a dose response.")
  }
  if(doseR == F){
    message("You set doseR = FALSE. Not expecting controls to be coded for a dose reponse.")
  }
  # check on control coding
  controls <- unique(data$diluent)
  drugs <- unique(data$drug)

  # check on expected dcontrols
  if(doseR == F & F %in% (controls %in% drugs)) {
    # send a warning.
    message(glue::glue("ERROR: the controls are not configured as expected in the design file doseR = FALSE. Do you want doseR = TRUE? If not, control conditions should have the same value for drug and diluent and a 0 for concentration_um. Please correct the control condition coding before using easyXpress well flag (WF) functions.
                           For example:"))
    example <- tibble::tibble(drug = c("DMSO", "water", "death juice", "seizure sauce"),
                              concentration_um = c(0, 0, 10, 100),
                              diluent = c("DMSO", "water", "DMSO", "water"))
    stop(message(message(paste0(capture.output(knitr::kable(example)), collapse = "\n"))))
  }
  # warn about doseR=T
  if(doseR == T & T %in% (controls %in% drugs)){
    # send a warning.
    message(glue::glue("ERROR: the controls are not configured as expected in the design file for doseR = TRUE. Do you want doseR = FALSE? If not, control conditions should have 0 for concentration_um and be named for the drug not the diluent. Please correct the control condition coding before using easyXpress well flag (WF) functions.
                           For example:"))
    example <- tibble::tibble(drug = c("death juice", "death juice", "seizure sauce", "seizure sauce"),
                              concentration_um = c(0, 10, 0, 100),
                              diluent = c("DMSO", "DMSO", "water", "water"))
    stop(message(message(paste0(capture.output(knitr::kable(example)), collapse = "\n"))))
  }

  # get any user flags from data
  uf1 <- names(data %>% dplyr::select(contains("_WellFlag")))

  # filter wells if needed
  if(WF == "filter") {
    if(length(uf1) == 0) {
      message(glue::glue("No flagged wells detected."))
      d <- data
    } else {
    d <- easyXpress::filterWF(data = data, rmVars = T)
    message(glue::glue("Flagged wells are filtered from the data."))
    }
  } else {
    d <- data
    message(glue::glue('Flagged wells are NOT being filtered from the data. This is NOT the recommended approach. Please consider WF = "filter".'))
  }

  # grouped by message
  group_by <- paste(names(d %>% dplyr::select(...)), collapse = ", ")
  message(glue::glue("The data are grouped by, {group_by}."))

  # setup means
  if(doseR == F) {
  # get control values for each group
  control_values <- d %>%
    dplyr::filter(drug %in% controls) %>% # filter to control wells
    dplyr::group_by(...) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(vars), ~mean(.), .names = "control_{.col}")) %>% # get control values
    dplyr::distinct(dplyr::across(dplyr::matches("_delta|control_"))) %>%
    dplyr::ungroup()
  }
  if(doseR == T) {
    # get control values for each group
    control_values <- d %>%
      dplyr::filter(concentration_um == 0) %>% # filter to control wells
      dplyr::group_by(...) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(vars), ~mean(.), .names = "control_{.col}")) %>% # get control values
      dplyr::distinct(dplyr::across(dplyr::matches("_delta|control_"))) %>%
      dplyr::ungroup()
  }
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
