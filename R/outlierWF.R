#' outlierWF
#'
#' This function will flag outlier wells based the median_wormlength_um for wells.
#'
#' @param data A data frame output from the \code{summarizeWells}function or any of the \code{WF} functions.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Variable(s) used to group the well summarized data. Variable names in data are supplied separated by commas and without quotes.
#' The outliers will be detected across the wells within the group specified.
#' @param iqr Logical, if \code{TRUE}, wells in a group are called outliers if the well median_wormlength_um is outside the range \code{median(median_wormlength_um) +/- (thresh * IQR)}.
#' If \code{FALSE}, wells in a group are called outliers if the well median_wormlength_um is outside the range \code{mean(median_wormlength_um) +/- (thresh * SD)}.
#' The default is \code{TRUE}. NOTE: The Interquartile Range (IQR) method is useful for most distributions. The Standard Deviation (SD) method should only
#' be applied to normal distributions.
#' @param thresh A numeric value used to calculate outliers. By default this parameter is set to \code{NULL}.
#' If \code{thresh = NULL} then it is set to the recommended values within the function:
#' (\code{1.5} if \code{iqr = TRUE}) and (\code{3} if \code{iqr = FALSE}).
#' @param filterWF Logical, if \code{TRUE} any flagged wells detected in data are not considered for the calculation of outliers.
#' @return A single data frame identical to the input data with the \code{outlier_WellFlag} variable added.
#' The \code{outlier_WellFlag} variable is coded as \code{"outlier"} for objects deemed outliers and \code{NA_character_} for non-outliers.
#' @export

outlierWF <- function(data, ..., iqr = TRUE, thresh = NULL, filterWF = TRUE) {
  #set the threshold
  if(iqr == T & is.null(thresh)) {
    thresh <- 1.5
  }
  if(iqr == F & is.null(thresh)) {
    thresh <- 3
  }

  # Decide how to handle flagged wells and warn
  if(filterWF == T) {
    message("Previously flagged wells will not be used when calcualting outliers within the group. This is the recommended approach.")
    data.in <- easyXpress::filterWF(data = data, rmVars = T)
  } else {
    message("WARNING: Previously flagged wells will be used when calculating outliers within the group. This is NOT the recommended approach. Please consider whether filterWF = T is better.")
    # set input
    data.in <- data
  }

  # remove the outliers
  if(iqr == T) {
    # use IQR method
    message(glue::glue("Flagging outlier wells in group if median_wormlength_um is outside the range: median +/- ({thresh}*IQR)"))
    out <- data.in %>%
      dplyr::group_by(...) %>%
      dplyr::mutate(outlier_WellFlag = (easyXpress::iqrOutlier(median_wormlength_um, thresh = thresh)),
                    outlier_WellFlag = ifelse(is.na(outlier_WellFlag), "outlier", NA_character_)) %>%
      dplyr::ungroup()
  }

  if(iqr == F) {
    # use SD method
    message(glue::glue("Flagging outlier wells in group with median_wormlength_um {thresh} SDs away from mean"))
    out <- data.in %>%
      dplyr::group_by(...) %>%
      dplyr::mutate(outlier_WellFlag = (easyXpress::sdOutlier(median_wormlength_um, thresh = thresh)),
                    outlier_WellFlag = ifelse(is.na(outlier_WellFlag), "outlier", NA_character_)) %>%
      dplyr::ungroup()
  }

  # Add back any filtered data
  suppressMessages(out <- data %>%
                     dplyr::full_join(., out))
  # Return it
  return(out)
}
