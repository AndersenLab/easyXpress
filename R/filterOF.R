#' filterOF
#'
#' This function will filter out all flagged objects identified by \code{_ObjectFlag} variables.
#'
#' @param data A data frame with \code{_ObjectFlag} variables. Typically the data is generated with easyXpress OF functions.
#' @param rmVars Logical, if \code{TRUE} the \code{_ObjectFlag} variables are removed from the data frame after flagged objects are removed.
#' If \code{FALSE}, the variables are retained. This argument can be useful for joining data frames. The default is \code{TRUE}.
#' @return A single data frame with all flagged objects removed.
#' @export

filterOF <- function(data, rmVars = TRUE) {
  # Find the ObjectFlags in data in the order they appear
  uf <- names(data %>% dplyr::select(contains("_ObjectFlag")))

  message(glue::glue("{length(uf)} ObjectFlags detected in data. They were applied in the following order:"))
  for(i in 1:length(uf)) {
    message(glue::glue("{uf[i]}"))
  }

  # Get a single ObjectFlag vector based on the order in which the flags were run
  objectFlag <- data %>%
    dplyr::select(contains("_ObjectFlag")) %>%
    tidyr::unite(objectFlag, sep = "_", na.rm = T) %>%
    tidyr::separate(objectFlag, into = "objectFlag", sep = "_", extra = "drop", remove = T) %>%
    dplyr::pull(objectFlag)

  if("objectFlag" %in% names(data)) {
    message("WARNING: The objectFlag variable already exists in the data. It will be overwritten in the output.")
    # Add it to data and summarize by grouping variables if provided.
    data.OF <- data %>%
      dplyr::select(-objectFlag) %>%
      dplyr::bind_cols(objectFlag = objectFlag)

  } else {
    # Add it to data and summarize by grouping variables if provided.
    data.OF <- data %>%
      dplyr::bind_cols(objectFlag = objectFlag)
  }

  # Filter the flagged objects
  data.filterOF <- data.OF %>%
    dplyr::filter(objectFlag == "" | is.na(objectFlag))

  # catch full filter
  if(nrow(data.filterOF) == 0){
    stop("All data filtered due to flags. Please review any OF function settings to retain more data.")
  }

  if(rmVars == FALSE) {
  # return
  return(data.filterOF)
  }
  if(rmVars == TRUE) {
    data.filterOF.rm <- data.filterOF %>%
      dplyr::select(-dplyr::contains("_ObjectFlag"),
                    -dplyr::contains("objectFlag"))

    # return
    return(data.filterOF.rm)
  }
}
