#' clusterOF
#'
#' This function will flag objects that belong to the same \code{Parent_WormObject} identified by cellprofiler.
#'
#' @param data A data frame output from the \code{modelSelection} and \code{edgeOF} functions.
#' @return A single data frame identical to the input data with the \code{cluster_ObjectFlag} variable added.
#' The \code{cluster_ObjectFlag} variable is coded as \code{"cluster"} for objects that are clustered,
#' all other objects are coded as \code{NA_character}.
#' @export

clusterOF <- function(data) {
  cluster_flagged <- data %>%
    dplyr::mutate(cluster_ObjectFlag = ifelse(cluster_flag == T, "cluster", NA_character_))
  return(cluster_flagged)
}
