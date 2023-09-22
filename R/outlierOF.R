#' outlierOF
#'
#' This function will flag outlier objects based the Worm_Legnth variable.
#'
#' @param data A data frame output from the \code{modelSelection} function.
#' @param iqr Logical, if \code{TRUE}, objects in a well are called outliers if their worm_length_um is outside the range \code{median(worm_length_um) +/- (thresh * IQR)}.
#' If \code{FALSE}, objects in a well are called outliers if their worm_length_um is outside the range \code{mean(worm_length_um) +/- (thresh * SD)}.
#' The default is \code{TRUE}. NOTE: The Interquartile Range (IQR) method is useful for most distributions. The Standard Deviation (SD) method should only
#' be applied to normal distributions.
#' @param thresh A numeric value used to calculate outliers. By default this parameter is set to \code{NULL}.
#' If \code{thresh = NULL} then it is set to the recommended values within the function:
#' (\code{1.5} if \code{iqr = TRUE}) and (\code{3} if \code{iqr = FALSE}).
#' @param filterOF Logical, if \code{TRUE} any flagged objects detected in data are not considered for the calculation of outliers.
#' @return A single data frame identical to the input data with the \code{outlier_ObjectFlag} variable added.
#' The \code{outlier_ObjectFlag} variable is coded as \code{"outlier"} for objects deemed outliers and \code{NA_character} for non-outliers.
#' @export

outlierOF <- function(data, iqr = TRUE, thresh = NULL, filterOF = TRUE) {
  #set the threshold
  if(iqr == T & is.null(thresh)) {
    thresh <- 1.5
  }
  if(iqr == F & is.null(thresh)) {
    thresh <- 3
  }

  # Decide how to handle flagged objects and warn
  if(filterOF == T) {
    message("Previously flagged objects will not be used when calcualting outliers. This is the recommended approach.")
    data.in <- easyXpress::filterOF(data = data, rmVars = T)
  } else {
    message("WARNING: Previously flagged objects will be used when calculating outliers. This is NOT the recommended approach. Please consider whether filterOF = T is better.")
    # set input
    data.in <- data
  }

  # remove the outliers
  if(iqr == T) {
    # use IQR method
    message(glue::glue("Flagging outlier objects in each well if worm_length_um is outside the range: median +/- ({thresh}*IQR)"))
    out <- data.in %>%
      dplyr::group_by(Metadata_Experiment, Metadata_Plate, Metadata_Well) %>%
      dplyr::mutate(outlier_ObjectFlag = (easyXpress::iqrOutlier(worm_length_um, thresh = thresh)),
                    outlier_ObjectFlag = ifelse(is.na(outlier_ObjectFlag), "outlier", NA_character_)) %>%
      dplyr::ungroup()
  }

  if(iqr == F) {
    # use SD method
    message(glue::glue("Flagging outlier objects in each well with worm_length_um {thresh} SDs away from mean"))
    out <- data.in %>%
      dplyr::group_by(Metadata_Experiment, Metadata_Plate, Metadata_Well) %>%
      dplyr::mutate(outlier_ObjectFlag = (easyXpress::sdOutlier(worm_length_um, thresh = thresh)),
                    outlier_ObjectFlag = ifelse(is.na(outlier_ObjectFlag), "outlier", NA_character_)) %>%
      dplyr::ungroup()
  }

  # Add back any filtered data
  suppressMessages(out <- data %>%
    dplyr::full_join(., out))
  # Return it
  return(out)
}
