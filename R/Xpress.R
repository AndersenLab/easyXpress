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

Xpress <- function(filedir, rdafile, ..., design = FALSE, radius = 825, center_x = 1024, center_y = 1024, cluster_flag = TRUE, well_edge_flag = TRUE) {

  output <- readXpress(filedir = filedir, rdafile = rdafile, design = design) %>%
    modelSelection(df = .) %>%
    edgeFlag(model_selected_data = ., radius = radius, center_x = center_x, center_y = center_y) %>%
    setFlags(data = ., cluster_flag = cluster_flag, well_edge_flag = well_edge_flag) %>%
    process(flag_data = ., ...)

  return(output)
}
