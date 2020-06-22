#' Xpress
#'
#' This is a wrapper function that will run all functions in the package
#' and output a processed dataframe.
#'
#' @inherit readXpress
#' @inherit modelSelection
#' @inherit edgeFlag
#' @inherit setFlags
#' @inherit process
#' @return List with four elements: raw data, processed data, and summaries for both datasets.
#' @export
#'

Xpress <- function(filedir, design = FALSE, radius = 825, cluster_flag = TRUE, well_edge_flag = TRUE, ...) {

  output <- readXpress(filedir) %>%
    modelSelection(df = .) %>%
    edgeFlag(model_selected_data = .) %>%
    setFlags(data = .) %>%
    process(flag_data = ., ...)

  return(output)
}
