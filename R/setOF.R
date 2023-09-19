#' setOF
#'
#' This function will assign all flagged objects a single value returned as the \code{objectFlag} variable.
#' It is useful for collapsing many easyXpress OFs into a single flag label based on the order in which the flags were
#' added to the data frame.
#'
#'
#' @param data A data frame with \code{_ObjectFlag} variables. Typically the data is generated with easyXpress OF functions but any variable
#' that contains \code{_ObjectFlag} in its name will be considered.
#' @return A single data frame with all flagged objects labelled in the \code{objectFlag} variable. All objects that are not flagged are
#' labelled \code{noFlag}.
#' @export

setOF <- function(data){
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

  # label the flagged objects
  message("The flagged objects are labelled in the objectFlag variable.")
  out <- data.OF %>%
    dplyr::mutate(objectFlag = ifelse(objectFlag == "" | is.na(objectFlag), "noFlag", objectFlag))

  # return it
  return(out)
}
