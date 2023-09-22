#' titerWF
#'
#' This function will flag wells that belong to bleaches with highly variable worm titers.
#'
#' @param data A data frame output from the \code{summarizeWells} function.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Variable(s) used to group data into independent bleaches for strains. Variable names in data are supplied separated by commas and without quotes.
#' Typically the grouping variables will be: \code{Metadata_Experiemnt, strain, bleach}.
#' @param thresh A numeric value used as a threshold to flag bleaches with high coefficient of variation in well n (cv.n). Bleaches with high cv.n are often either over-bleached or titered incorrectly.
#' @param plot Logical, if \code{TRUE}, the default. A ggplot2 object will be returned showing the distribution of CVs in worm number across the wells in control conditions for all independent bleaches.
#' The \code{thresh} value is plotted as a red vertical line.
#' @return Either a single data frame identical to the input data with the \code{titer_WellFlag} variable added.
#' The \code{titer_WellFlag} variable is coded as \code{"titer"} for all wells belonging to a strain with a coefficient of variation in worm number that is greater than \code{thresh}.
#' Or, if \code{plot = T} a list with two elements, the first element is the data frame, the other is the diagnostic plot.
#' @export

titerWF <- function(data, ..., thresh = 0.68, plot = T) {
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

  # set the flags for all independent bleaches.
  d <- data %>%
    dplyr::select(well.id, ..., n, drug, concentration_um, diluent) %>% # Metadata_Experiment, strain, bleach # ...
    dplyr::filter(drug == diluent & concentration_um == 0) %>%
    dplyr::group_by(...) %>% # ...
    dplyr::mutate(num.wells = n(),
                  cv.n = sd(n)/mean(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(quant.95.cv.n = quantile(cv.n, .95, na.rm = T),
                  quant.97.5.cv.n = quantile(cv.n, .975, na.rm = T),
                  titer_WellFlag = ifelse(cv.n > thresh, "titer", NA_character_))

  # get the wells to flag with titer
  t.wells <- d %>%
    dplyr::filter(titer_WellFlag == "titer") %>%
    dplyr::pull(well.id)

  # set the flag by well.id
  t.d <- data %>%
    dplyr::mutate(titer_WellFlag = dplyr::case_when(well.id %in% t.wells ~ "titer",
                                             TRUE ~ NA_character_))

  # get distinct bleaches for ploting and reporting
  d.p <- d %>%
    dplyr::distinct(..., .keep_all = T)

  # message
  message(glue::glue("{length(d.p)} independent bleaches detected. The titer_WellFlag is set in the output data."))

  if(plot == T) {
    # plot it with thresh
    p <- ggplot2::ggplot(d.p) +
      ggplot2::aes(x = cv.n, fill = strain) +
      ggplot2::geom_histogram(bins = 30) +
      ggplot2::geom_vline(xintercept = thresh, linetype = 2, color = "red") +
      ggplot2::theme_bw() +
      ggplot2::labs(title = glue::glue("Titer filter cv.n > {thresh}"),
                    y = "bleach count")

    # return
    message(glue::glue("A diagnostic plot for checking cv.n threshold is returned. See ?titerWF() for more details."))
    out <- list(titerWF.d = t.d, titerWF.p = p)
    return(out)
  } else {
    # return data only
    return(t.d)
  }
}
