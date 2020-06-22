#' setFlags
#'
#' @param data CellProfiler data following model selection and edge flagging
#' @param cluster_flag Logical parameter; do you want worm objects
#' in a cluster to be excluded when calculating well outliers?
#' We recommend TRUE as the default.
#' @param well_edge_flag Logical parameter; do you want worm objects
#' in close proximity to the well edge to be excluded
#' when calculating well outliers? We recommend TRUE as the default.
#' @return A single data frame named raw_data that contains
#' all CellProfiler model outputs and experimental treatments
#' if a design file is used.
#' @export

setFlags <- function(data, cluster_flag = TRUE, well_edge_flag = TRUE) {

  # removes outliers function (tukey's fences)
  remove_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- stats::quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * stats::IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
  }

  if(!cluster_flag & !well_edge_flag) {
    print("NO FLAGS SELECTED FOR FILTERING")
    flag_data <- data %>%
      dplyr::group_by(Metadata_Plate, Metadata_Well) %>%
      dplyr::mutate(well_outlier_flag = (remove_outliers(worm_length_um)),
                    well_outlier_flag = ifelse(is.na(well_outlier_flag), TRUE, FALSE)) %>%
      dplyr::ungroup() %>%
      dplyr::full_join(.,data)

  }
  else if(!well_edge_flag) {
    print("FILTERING CLUSTER FLAGS ONLY")
    flag_data <- data %>%
      dplyr::filter(cluster_flag != T, well_edge_flag !=F) %>%
      dplyr::mutate(flag_removed = TRUE) %>% # record the flags that were removed
      dplyr::group_by(Metadata_Plate, Metadata_Well) %>%
      dplyr::mutate(well_outlier_flag = (remove_outliers(worm_length_um)),
                    well_outlier_flag = ifelse(is.na(well_outlier_flag), TRUE, FALSE)) %>%
      dplyr::ungroup() %>%
      dplyr::full_join(.,data) %>%
      dplyr::mutate(flags_removed= "rm_cluster_flag") # record the flags that were removed

  }
  else if(!cluster_flag) {
    print("FILTERING WELL EDGE FLAGS ONLY")
    flag_data <- data %>%
      dplyr::filter(cluster_flag != F, well_edge_flag !=T) %>%
      dplyr::mutate(flag_removed = TRUE) %>%
      dplyr::group_by(Metadata_Plate, Metadata_Well) %>%
      dplyr::mutate(well_outlier_flag = (remove_outliers(worm_length_um)),
                    well_outlier_flag = ifelse(is.na(well_outlier_flag), TRUE, FALSE)) %>%
      dplyr::ungroup() %>%
      dplyr::full_join(.,data) %>%
      dplyr::mutate(flags_removed= "rm_well_edge_flag")

  }
  else {
    print("FILTERING BOTH CLUSTER AND WELL EDGE FLAGS")
    flag_data <- data %>%
      dplyr::filter(cluster_flag != T, well_edge_flag !=T) %>%
      dplyr::mutate(flag_removed = TRUE) %>%
      dplyr::group_by(Metadata_Plate, Metadata_Well) %>%
      dplyr::mutate(well_outlier_flag = (remove_outliers(worm_length_um)),
                    well_outlier_flag = ifelse(is.na(well_outlier_flag), TRUE, FALSE)) %>%
      dplyr::ungroup() %>%
      dplyr::full_join(.,data) %>%
      dplyr::mutate(flags_removed= "rm_cluster_flag, rm_well_edge_flag")
  }

  return(flag_data)
}
