#' edgeFlag
#'
#' This is the primary function for flagging well edge worms
#' in CellProfiler data
#'
#' @param model_selected_data A data frame where selected worm model
#' denotations have been joined and assigned to each worm object
#' @param radius Radius in pixels away from image center with even illumination.
#' @return A single data frame with worm objects on the edge
#' of the well identified but retained
#' @export


edgeFlag <- function(model_selected_data, radius = 825) {
  edge_flag_center_x <- 1024;
  edge_flag_center_y <- 1024;
  edge_flag_radius <- radius #set well edge flag parameters for plotting and flagging

  edge_flagged <- model_selected_data %>%
    dplyr::mutate(well_edge_flag =
                    ifelse(sqrt((AreaShape_Center_X - edge_flag_center_x)^2 +
                                  (AreaShape_Center_Y - edge_flag_center_y)^2) <= edge_flag_radius, FALSE, TRUE)) %>%
    dplyr::mutate(well_edge_flag_radius = edge_flag_radius)

  return(edge_flagged)
}
