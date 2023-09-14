#' outlierOF
#'
#' This function will flag outlier objects based the Worm_Legnth variable.
#'
#' @param data A data frame output from the \code{modelSelection} function.
#' @param iqr Logical, if \code{TRUE}, objects in a well are called outliers if their Worm_Length is outside the range \code{median(Worm_Length) +/- (thresh * IQR)}.
#' If \code{FALSE}, objects in a well are called outliers if their Worm_Length is outside the range \code{mean(Worm_Length) +/- (thresh * SD)}.
#' The default is \code{TRUE}. NOTE: The Interquartile Range (IQR) method is useful for most distributions. The Standard Deviation (SD) method should only
#' be applied to normal distributions.
#' @param thresh OPTIONAL: A numeric value used to calculate outliers. By default this parameter is set to \code{NULL}.
#' If \code{thresh = NULL} then it is set to the recommended values within the function:
#' (\code{1.5} if \code{iqr = TRUE}) and (\code{3} if \code{iqr = FALSE}).
#' @return A single data frame identical to the input data with the \code{outlier_ObjectFlag} variable added.
#' The \code{outlier_ObjectFlag} variable is coded as \code{"outlier"} for objects deemed outliers and \code{NA_character} for non-outliers.
#' @export

outlierOF <- function(data, iqr = TRUE, thresh = NULL) {
  #set the threshold
  if(iqr == T & is.null(thresh)) {
    thresh <- 1.5
  }
  if(iqr == F & is.null(thresh)) {
    thresh <- 3
  }

  if(iqr == T) {
    # use IQR method
    message(glue::glue("Flagging outlier objects in each well if Worm_Length is outside the range: median +/- ({thresh}*IQR)"))
    out <- data %>%
      dplyr::group_by(Metadata_Experiment, Metadata_Plate, Metadata_Well) %>%
      dplyr::mutate(outlier_ObjectFlag = (easyXpress::iqrOutlier(Worm_Length, thresh = thresh)),
                    outlier_ObjectFlag = ifelse(is.na(outlier_ObjectFlag), "outlier", NA_character_)) %>%
      dplyr::ungroup()
  }

  if(iqr == F) {
    # use SD method
    message("barf")
    message(glue::glue("Flagging outlier objects in each well with Worm_length {thresh} SDs away from mean"))
    out <- data %>%
      dplyr::group_by(Metadata_Experiment, Metadata_Plate, Metadata_Well) %>%
      dplyr::mutate(outlier_ObjectFlag = (easyXpress::sdOutlier(Worm_Length, thresh = thresh)),
                    outlier_ObjectFlag = ifelse(is.na(outlier_ObjectFlag), "outlier", NA_character_)) %>%
      dplyr::ungroup()
  }

  # retun it
  return(out)
}
