#' filterWF
#'
#' This function will filter out all flagged wells identified by \code{_WellFlag} variables.
#'
#' @param data A data frame with \code{_WellFlag} variables. Typically the data is generated with easyXpress WF functions.
#' @param rmVars Logical, if \code{TRUE} the \code{_WellFlag} variables are removed from the data frame after flagged objects are removed.
#' If \code{FALSE}, the variables are retained. This argument can be useful for joining data frames. The default is \code{TRUE}.
#' @return A single data frame with all flagged wells removed.
#' @export

filterWF <- function(data, rmVars = TRUE) {
  # Find the WellFlags in data in the order they appear
  uf <- names(data %>% dplyr::select(contains("_WellFlag")))

  message(glue::glue("{length(uf)} WellFlags detected in data. They were applied in the following order:"))
  for(i in 1:length(uf)) {
    message(glue::glue("{uf[i]}"))
  }

  # Get a single WellFlag vector based on the order in which the flags were run
  wellFlag <- data %>%
    dplyr::select(contains("_WellFlag")) %>%
    tidyr::unite(wellFlag, sep = "_", na.rm = T) %>%
    tidyr::separate(wellFlag, into = "wellFlag", sep = "_", extra = "drop", remove = T) %>%
    dplyr::pull(wellFlag)

  if("wellFlag" %in% names(data)) {
    message("WARNING: The wellFlag variable already exists in the data. It will be overwritten in the output.")
    # Add it to data and summarize by grouping variables if provided.
    data.WF <- data %>%
      dplyr::select(-wellFlag) %>%
      dplyr::bind_cols(wellFlag = wellFlag)

  } else {
    # Add it to data.
    data.WF <- data %>%
      dplyr::bind_cols(wellFlag = wellFlag)
  }

  # Filter the flagged wells
  data.filterWF <- data.WF %>%
    dplyr::filter(wellFlag == "" | is.na(wellFlag))

  # catch full filter
  if(nrow(data.filterWF) == 0){
    stop("All data filtered due to flags. Please review any WF function settings to retain more data.")
  }

  if(rmVars == FALSE) {
    # return
    return(data.filterWF)
  }
  if(rmVars == TRUE) {
    data.filterWF.rm <- data.filterWF %>%
      dplyr::select(-dplyr::contains("_WellFlag"),
                    -dplyr::contains("wellFlag"))

    # return
    return(data.filterWF.rm)
  }
}
