#' setWF
#'
#' This function will assign all flagged wells a single value returned as the \code{wellFlag} variable.
#' It is useful for collapsing many easyXpress WFs into a single flag label based on the order in which the flags were
#' added to the data frame.
#'
#' @param data A data frame with \code{_WellFlag} variables. Typically the data is generated with easyXpress WF functions but any variable
#' that contains \code{_WellFlag} in its name will be considered.
#' @return A single data frame with all flagged wells labelled in the \code{wellFlag} variable. All objects that are not flagged are
#' labelled \code{noFlag}.
#' @export

setWF <- function(data) {
  # Find the WellFlags in data in the order they appear
  uf <- names(data %>% dplyr::select(contains("_WellFlag")))

  message(glue::glue("{length(uf)} WellFlags detected in data. They were applied in the following order:"))
  for(i in 1:length(uf)) {
    message(glue::glue("{uf[i]}"))
  }

  # Get a single WellFlag vector based on the order in which the flags were run.
  wellFlag <- data %>%
    dplyr::select(contains("_WellFlag")) %>%
    tidyr::unite(wellFlag, sep = "_", na.rm = T) %>%
    tidyr::separate(wellFlag, into = "wellFlag", sep = "_", extra = "drop", remove = T) %>%
    dplyr::pull(wellFlag)

  if("wellFlag" %in% names(data)) {
    message("WARNING: The wellFlag variable already exists in the data. It will be overwritten in the output.")
    # Add it to data and summarize by grouping variables if provided.
    data.OF <- data %>%
      dplyr::select(-wellFlag) %>%
      dplyr::bind_cols(wellFlag = wellFlag)

  } else {
    # Add it to data and summarize by grouping variables if provided.
    data.OF <- data %>%
      dplyr::bind_cols(wellFlag = wellFlag)
  }

  # label the flagged objects
  message("The flagged wells are labelled in the wellFlag variable.")
  out <- data.OF %>%
    dplyr::mutate(wellFlag = ifelse(wellFlag == "" | is.na(wellFlag), "noFlag", wellFlag))

  # return it
  return(out)
}
