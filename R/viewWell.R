#' viewWell
#'
#'\code{viewWell} plots the processed well image
#' with object centroids colored by type.
#'
#' @param df Unsummarised CellProfiler data output from the
#' \code{Xpress} functon. Either the raw or processed data can be viewed.
#' @param img_dir The full path of the directory holding
#' the processed images.
#' @param plate Desired plate to be analyzed.
#' @param well Desired well to be analyzed.
#' @param boxplot should output include boxplot of object data?
#' @return A ggplot object with selected well image and boxplot.
#' @export

viewWell <- function(df, img_dir, plate, well, boxplot = TRUE) {
  #list all images in directory
  list_all_imgs <- list.files(img_dir)
  #dataframe of pathnames of images in directory
  df_all_imgs <- tibble::as_tibble(list.files(img_dir, full.names = TRUE)) %>%
    dplyr::rename(path = value) %>%
    dplyr::mutate(file_name = list_all_imgs) %>%
    dplyr::filter(stringr::str_detect(file_name, pattern = plate),
                  stringr::str_detect(file_name, pattern = well))

  #select image to be analyzed
  img <- png::readPNG(df_all_imgs$path)

  h<-dim(img)[1] # image height
  w<-dim(img)[2] # image width

  #add object type labels to dataframe
  well_data <- df %>%
    dplyr::filter(Metadata_Plate %in% c(plate),
                  Metadata_Well %in% c(well)) %>%
    dplyr::mutate(object_type = dplyr::case_when(
      cluster_flag == T & well_edge_flag == T & well_outlier_flag == T ~ "all_flags",
      cluster_flag == T & well_edge_flag == T & well_outlier_flag == F ~ "cluster/edge_flag",
      cluster_flag == T & well_edge_flag == F & well_outlier_flag == T ~ "cluster/outlier_flag",
      cluster_flag == T & well_edge_flag == F & well_outlier_flag == F ~ "cluster_flag",
      cluster_flag == F & well_edge_flag == T & well_outlier_flag == T ~ "edge/outlier_flag",
      cluster_flag == F & well_edge_flag == T & well_outlier_flag == F ~ "edge_flag",
      cluster_flag == F & well_edge_flag == F & well_outlier_flag == T ~ "outlier_flag",
      cluster_flag == T & well_edge_flag == T & is.na(well_outlier_flag) ~ "cluster/edge_flag",
      cluster_flag == T & well_edge_flag == F & is.na(well_outlier_flag) ~ "cluster_flag",
      cluster_flag == F & well_edge_flag == T & is.na(well_outlier_flag) ~ "edge_flag",
      cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == levels(model)[1] ~ as.character(glue::glue("{levels(model)[1]}_model")),
      cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == levels(model)[2] ~ as.character(glue::glue("{levels(model)[2]}_model")),
      cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == levels(model)[3] ~ as.character(glue::glue("{levels(model)[3]}_model")),
      cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == levels(model)[4] ~ as.character(glue::glue("{levels(model)[4]}_model")),
      cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == levels(model)[1] ~ as.character(glue::glue("{levels(model)[1]}_model")),
      cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == levels(model)[2] ~ as.character(glue::glue("{levels(model)[2]}_model")),
      cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == levels(model)[3] ~ as.character(glue::glue("{levels(model)[3]}_model")),
      cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == levels(model)[4] ~ as.character(glue::glue("{levels(model)[4]}_model"))))

  #determine well radius used
  well_radius <- df %>%
    dplyr::ungroup() %>%
    dplyr::distinct(well_edge_flag_radius) %>%
    dplyr::pull()

  #plot well image
  well_img <- well_data %>%
    ggplot2::ggplot(.) +
    ggplot2::aes(x = AreaShape_Center_X, y = AreaShape_Center_Y, fill = object_type) +
    ggplot2::annotation_custom(grid::rasterGrob(img, width=ggplot2::unit(1,"npc"), height=ggplot2::unit(1,"npc")), 0, w, 0, -h) +
    ggplot2::geom_point(shape = 21, alpha = 0.75, size=4) +
    ggplot2::scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
    ggplot2::scale_y_reverse(expand=c(0,0),limits=c(h,0)) +
    ggplot2::coord_equal() +
    ggplot2::theme_bw() +
    ggplot2::annotate("path",
                      x = w/2 + well_radius*cos(seq(0,2*pi,length.out=100)),
                      y = h/2 + well_radius*sin(seq(0,2*pi,length.out=100)), color = "red", alpha = 0.25) +
    ggplot2::labs(x = "", y = "", fill = "Object Class", title = df_all_imgs$file_name)

  if(boxplot == FALSE) {
    return(well_img)

  } else {
    #plot boxplot
    well_boxplot <- well_data %>%
      ggplot2::ggplot(.) +
      ggplot2::geom_boxplot(aes(x = Metadata_Well, y = worm_length_um), outlier.shape = NA) +
      ggplot2::geom_jitter(shape = 21, width = 0.25, size = 3, aes(x = Metadata_Well, y = worm_length_um, fill = object_type)) +
      ggplot2::labs(x ="") +
      ggplot2::theme_bw() +
      ggplot2::guides(fill = F)

    all_plots <- cowplot::plot_grid(well_img, well_boxplot, nrow = 1, rel_widths = c(1,0.25), align = "hv", axis = "tb")

    return(all_plots)
  }
}
