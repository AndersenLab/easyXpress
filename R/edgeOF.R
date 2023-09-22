#' edgeOF
#'
#' This function will flag objects on the edge of wells that are often more difficult to segment properly with
#' CellProfiler.
#'
#' @param data A data frame output from the \code{modelSelection} function.
#' @param radius Radius in pixels away from image center with even illumination. The default
#' value is set to 825, which is standard for images taken with the imageXpress nano 2X objective.
#' @param center_x center x position of image in pixels. Default is 1024.
#' @param center_y center y position of image in pixels. Default is 1024.
#' @return A single data frame with \code{edge_ObjectFlag} and \code{well_edge_flag_radius}
#' variables added. The \code{edge_ObjectFlag} variable is coded as \code{"edge"} for objects with centroids outside
#' the radius provided, all other objects are coded as \code{NA_character}.
#' @export

edgeOF <- function(data, radius = 825, center_x = 1024, center_y = 1024) {
  edge_flag_center_x <- center_x;
  edge_flag_center_y <- center_y;
  edge_flag_radius <- radius #set well edge flag parameters for plotting and flagging

  edge_flagged <- data %>%
    dplyr::mutate(edge_ObjectFlag =
                    ifelse(sqrt((AreaShape_Center_X - edge_flag_center_x)^2 +
                                  (AreaShape_Center_Y - edge_flag_center_y)^2) <= edge_flag_radius, NA_character_, "edge")) %>%
    dplyr::mutate(well_edge_flag_radius = edge_flag_radius)

  return(edge_flagged)
}

