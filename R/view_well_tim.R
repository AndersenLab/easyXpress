#' viewWell
#'
#'\code{viewWell} plots the processed well image
#' with object centroids colored by type.
#'
#' @param data Unsummarised CellProfiler data output from the
#' \code{Xpress} functon. Either the raw or processed data can be viewed.
#' @param strain_name A character value for the strain to use in
#' the plots, e.g. \code{"N2"}.
#' @param drug_name A character value for the drug to use in
#' the plots, e.g. \code{"paraquat"}.
#' @param proc_img_dir The full path of the directory holding
#' the processed images.
#' @return A ggplot object with representative wells for each dose
#' of the selected drug and strain. The user cannot specify the
#' individual wells to be used. To view the results for a specific
#' well use the \code{viewWell} function.
#' @importFrom grid rasterGrob
#' @importFrom cowplot get_legend plot_grid ggdraw
#' @importFrom png readPNG
#' @importFrom rebus %R%
#' @importFrom data.table :=
#'
#' @export

# function to view a dose response of xpress data
viewWell <- function(data, plate, well, proc_img_dir) {

  # Flexible method to find FileName_RawBF. Flexibility b/c prefixes can be attached in some cases.
  # A faster option would be to rename variable and remove flexibility to accomadate prefix.
  RawBF_var_name <- data %>%
    .[stringr::str_detect(names(.), pattern = "FileName_RawBF")] %>%
    names(.)

  # subset dataframe to plate and well
  subset_dat <- data %>%
    dplyr::filter(Metadata_Plate == glue::glue({plate}), Metadata_Well == glue::glue({well})) %>%
    dplyr::select(FileName_RawBF := !!glue::glue("{RawBF_var_name}"), everything())

  # get raw image name
  raw_img_name <- subset_dat %>%
    dplyr::select(FileName_RawBF) %>%
    dplyr::pull(.)

  # set processed image name
  proc_img_name <- stringr::str_replace(raw_img_name, pattern = ".TIF", replacement = "_overlay.png")

  # find number of models
  n_models <- length(unique(data$model))

  # Pull model levels
  model_levels <- levels(data$model)

  # set object types given the number of models in dataframe.
  # 1 model is possible b/c additional models could be filterered.
  if(n_models == 2) {
    # set object_type
    plot_dat <- subset_dat %>%
      dplyr::mutate(object_type = case_when(
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
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == model_levels[1] ~ as.character(glue::glue("{model_levels[1]}_model")),
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == model_levels[2] ~ as.character(glue::glue("{model_levels[2]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == model_levels[1] ~ as.character(glue::glue("{model_levels[1]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == model_levels[2] ~ as.character(glue::glue("{model_levels[2]}_model"))))
  }

  if(n_models == 3) {
    # set object_type
    plot_dat <- subset_dat %>%
      dplyr::mutate(object_type = case_when(
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
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == model_levels[1] ~ as.character(glue::glue("{model_levels[1]}_model")),
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == model_levels[2] ~ as.character(glue::glue("{model_levels[2]}_model")),
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == model_levels[3] ~ as.character(glue::glue("{model_levels[3]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == model_levels[1] ~ as.character(glue::glue("{model_levels[1]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == model_levels[2] ~ as.character(glue::glue("{model_levels[2]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == model_levels[3] ~ as.character(glue::glue("{model_levels[3]}_model"))))
  }

  if(n_models == 4) {
    # set object_type
    plot_dat <- subset_dat %>%
      dplyr::mutate(object_type = case_when(
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
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == model_levels[1] ~ as.character(glue::glue("{model_levels[1]}_model")),
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == model_levels[2] ~ as.character(glue::glue("{model_levels[2]}_model")),
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == model_levels[3] ~ as.character(glue::glue("{model_levels[3]}_model")),
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == model_levels[4] ~ as.character(glue::glue("{model_levels[4]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == model_levels[1] ~ as.character(glue::glue("{model_levels[1]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == model_levels[2] ~ as.character(glue::glue("{model_levels[2]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == model_levels[3] ~ as.character(glue::glue("{model_levels[3]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == model_levels[4] ~ as.character(glue::glue("{model_levels[4]}_model"))))
  }

  # use stringr to find middle part of directory that contains the date and experiment name.
  date <- DGT %R% DGT %R% DGT %R% DGT %R% DGT %R% DGT %R% DGT %R% DGT
  separator <- rebus::char_class("/_-")
  run <- "RUN" %R% DGT %R% DGT
  name1 <- separator %R% date %R% separator %R% one_or_more(WRD)
  name2 <- date %R% separator %R% one_or_more(WRD)
  dir_date_name <- stringr::str_extract(proc_img_dir, pattern = name1)
  date_name <- stringr::str_extract(proc_img_dir, pattern = name2)

  #set well center
  edge_flag_center_x <- 1024
  edge_flag_center_y <- 1024
  edge_flag_radius <- plot_dat$well_edge_flag_radius

  #set object_colors
  kelly_colors <- c('#F2F3F4', '#222222', '#F3C300', '#875692', '#F38400',
                    '#A1CAF1', '#BE0032', '#C2B280', '#848482', '#008856',
                    '#E68FAC', '#0067A5', '#F99379', '#604E97', '#F6A600',
                    '#B3446C', '#DCD300', '#882D17', '#8DB600', '#654522', '#E25822', '#2B3D26')
  object_colors <- rlang::set_names(kelly_colors[1:length(unique(plot_dat$object_type))], unique(plot_dat$object_type))

  img <- readPNG(glue::glue("{proc_img_dir}/{proc_img_name}"))

  h<-dim(img)[1] # image height
  w<-dim(img)[2] # image width

  well_img <- ggplot(plot_dat) +
    aes(x = AreaShape_Center_X, y = AreaShape_Center_Y, fill = object_type) +
    scale_fill_manual(values = object_colors) +
    annotation_custom(grid::rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0, -h) + # The minus is needed to get the y scale reversed. these are pixel dim of image
    scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
    scale_y_reverse(expand=c(0,0),limits=c(h,0)) +  # The y scale is reversed because in image the vertical positive direction is typically downward.Also note the limits where h>0 is the first parameter.
    labs(x = "", y = "", title = glue::glue("{plot_dat %>% pull(Metadata_Date)}_Plate{plate}_{well}_{plot_dat %>% pull(drug)}_{plot_dat %>% pull(concentration_um)}uM_{plot_dat %>% pull(strain)}"), fill = "Object Class") +
    geom_point(shape = 21, alpha = 0.75) +
    coord_equal() +
    theme(legend.position = "none") +
    annotate("path",
             x=edge_flag_center_x+edge_flag_radius*cos(seq(0,2*pi,length.out=100)),
             y=edge_flag_center_y+edge_flag_radius*sin(seq(0,2*pi,length.out=100)), color = "red", alpha = 0.25)

  well_img_box_plot <- ggplot(plot_dat) +
    geom_boxplot(aes(x = Metadata_Well, y = worm_length_um), outlier.shape = NA) +
    geom_jitter(shape = 21, width = 0.25, size = 3, aes(x = Metadata_Well, y = worm_length_um, fill = object_type)) +
    scale_fill_manual(values = object_colors) +
    labs(x ="") +
    ylim(0,1000) +
    theme_bw() +
    theme(legend.position = "right")


  full_well_diagnostic <- cowplot::plot_grid(well_img, well_img_box_plot, nrow = 1, rel_widths = c(1,0.25), align = "hv", axis = "tb")

  # Return
  return(full_well_diagnostic)
}

