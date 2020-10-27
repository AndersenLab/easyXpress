#' viewDose
#'
#' #'\code{viewDose} plots representative processed well images
#' with object centroids colored by type for each concentration
#' of a drug.
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
#' @importFrom rebus %R% DGT WRD one_or_more char_class
#' @importFrom data.table :=
#' @importFrom ggplot2 element_blank ggplot theme margin
#'
#' @export

# function to view a dose response of xpress data
viewDose <- function(data, strain_name, drug_name, proc_img_dir){
  # Flexible method to find FileName_RawBF. Flexibility b/c prefixes can be attached in some cases.
  # A faster option would be to rename variable and remove flexibility to accomadate prefix.
  RawBF_var_name <- data %>%
    .[stringr::str_detect(names(.), pattern = "FileName_RawBF")] %>%
    names(.)

  # subset dataframe
  subset_dat <- data %>%
    dplyr::filter(drug == drug_name & strain == strain_name) %>%
    dplyr::group_by(concentration_um) %>%
    dplyr::select(FileName_RawBF := !!glue::glue("{RawBF_var_name}"), tidyselect::everything()) %>% #Rename
    dplyr::distinct(concentration_um, .keep_all = T) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(concentration_um)

  # Pull raw image names
  raw_img_names <- subset_dat %>%
    dplyr::select(FileName_RawBF) %>%
    dplyr::pull(.)

  # find number of models
  n_models <- length(unique(data$model))

  # Pull model levels
  model_levels <- levels(data$model)

  # set object types given the number of models in dataframe.
  # 1 model is possible b/c additional models could be filtered.
  if(n_models == 2) {
    # set object_type
    dose_dat <- data %>%
      dplyr::select(FileName_RawBF := !!glue::glue("{RawBF_var_name}"), tidyselect::everything()) %>%
      dplyr::filter(FileName_RawBF %in% raw_img_names) %>%
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
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == model_levels[1] ~ as.character(glue::glue("{model_levels[1]}_model")),
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == model_levels[2] ~ as.character(glue::glue("{model_levels[2]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == model_levels[1] ~ as.character(glue::glue("{model_levels[1]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == model_levels[2] ~ as.character(glue::glue("{model_levels[2]}_model"))))
  }

  if(n_models == 3) {
    # set object_type
    dose_dat <- data %>%
      dplyr::select(FileName_RawBF := !!glue::glue("{RawBF_var_name}"), tidyselect::everything()) %>%
      dplyr::filter(FileName_RawBF %in% raw_img_names) %>%
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
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == model_levels[1] ~ as.character(glue::glue("{model_levels[1]}_model")),
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == model_levels[2] ~ as.character(glue::glue("{model_levels[2]}_model")),
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == model_levels[3] ~ as.character(glue::glue("{model_levels[3]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == model_levels[1] ~ as.character(glue::glue("{model_levels[1]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == model_levels[2] ~ as.character(glue::glue("{model_levels[2]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == model_levels[3] ~ as.character(glue::glue("{model_levels[3]}_model"))))
  }

  if(n_models == 4) {
    # set object_type
    dose_dat <- data %>%
      dplyr::select(FileName_RawBF := !!glue::glue("{RawBF_var_name}"), tidyselect::everything()) %>%
      dplyr::filter(FileName_RawBF %in% raw_img_names) %>%
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
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == model_levels[1] ~ as.character(glue::glue("{model_levels[1]}_model")),
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == model_levels[2] ~ as.character(glue::glue("{model_levels[2]}_model")),
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == model_levels[3] ~ as.character(glue::glue("{model_levels[3]}_model")),
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == model_levels[4] ~ as.character(glue::glue("{model_levels[4]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == model_levels[1] ~ as.character(glue::glue("{model_levels[1]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == model_levels[2] ~ as.character(glue::glue("{model_levels[2]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == model_levels[3] ~ as.character(glue::glue("{model_levels[3]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == model_levels[4] ~ as.character(glue::glue("{model_levels[4]}_model"))))
  }

  # Make processed image names
  proc_img_names <- stringr::str_replace(raw_img_names, pattern = ".TIF", replacement = "_overlay.png")

  # get date and experiment name
  date <- as.character(stringr::str_split(proc_img_names[1], pattern = "-", simplify = T)[1])
  project <- as.character(stringr::str_split(proc_img_names[1], pattern = "-", simplify = T)[2])

  # make list of images for dose response
  dose_proc_img_list <- list()
  for(i in 1:length(unique(proc_img_names))){
    img <- png::readPNG(glue::glue("{proc_img_dir}/{proc_img_names[i]}"))
    dose_proc_img_list[[i]] <- img
  }

  # find dimensions of images. Assuming all are identical.
  h<-dim(dose_proc_img_list[[1]])[1] # image height
  w<-dim(dose_proc_img_list[[1]])[2] # image width

  #  plot well images for doses
  dose_plot_list <- NULL
  for(i in 1:length(proc_img_names)){

    #set well center
    edge_flag_center_x <- w/2
    edge_flag_center_y <- h/2
    edge_flag_radius <- unique(dose_dat$well_edge_flag_radius)

    #set object_colors
    kelly_colors <- c('#F2F3F4', '#222222', '#F3C300', '#875692', '#F38400',
                      '#A1CAF1', '#BE0032', '#C2B280', '#848482', '#008856',
                      '#E68FAC', '#0067A5', '#F99379', '#604E97', '#F6A600',
                      '#B3446C', '#DCD300', '#882D17', '#8DB600', '#654522', '#E25822', '#2B3D26')
    object_colors <- rlang::set_names(kelly_colors[1:length(unique(dose_dat$object_type))], unique(dose_dat$object_type))

    #make well plot
    well_plot <- ggplot(dose_dat %>%
                          dplyr::filter(FileName_RawBF == raw_img_names[i])) +
      ggplot2::aes(x = AreaShape_Center_X, y = AreaShape_Center_Y, color = object_type) +
      ggplot2::scale_color_manual(values = object_colors) +
      ggplot2::annotation_custom(
        grid::rasterGrob(dose_proc_img_list[[i]],
                         width=grid::unit(1,"npc"), height=grid::unit(1,"npc")), 0, w, 0, -h) + # The minus is needed to get the y scale reversed. these are pixel dim of image
      ggplot2::scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
      ggplot2::scale_y_reverse(expand=c(0,0),limits=c(h,0)) +  # The y scale is reversed because in image the vertical positive direction is typically downward.Also note the limits where h>0 is the first parameter.
      ggplot2::labs(y = "", x = glue::glue("{dose_dat %>%
                                  filter(FileName_RawBF == raw_img_names[[i]]) %>%
                                  pull(Metadata_Plate)}_{dose_dat %>%
                                  filter(FileName_RawBF == raw_img_names[[i]]) %>%
                                  pull(Metadata_Well)}_{dose_dat %>%
                                  filter(FileName_RawBF == raw_img_names[[i]]) %>%
                                  pull(concentration_um)}uM"),
           fill = "Object Class") +
      ggplot2::geom_point(shape = 19, alpha = 0.75, size = 0.8) +
      ggplot2::coord_equal() +
      ggplot2::theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.y=element_blank(),
            plot.margin = margin(0, 0, 0, 0),
            legend.position = "none") +
      ggplot2::annotate("path",
               x=edge_flag_center_x+edge_flag_radius*cos(seq(0,2*pi,length.out=100)),
               y=edge_flag_center_y+edge_flag_radius*sin(seq(0,2*pi,length.out=100)), color = "red", alpha = 0.25)

    #add plot to list
    dose_plot_list[[i]] <- well_plot
  }

  # get legend
  dose_legend <- cowplot::get_legend(ggplot(dose_dat) +
                                       ggplot2::aes(x = AreaShape_Center_X, y = AreaShape_Center_Y, color = object_type) +
                                       ggplot2::scale_color_manual(values = object_colors) +
                                       ggplot2::geom_point(shape = 19, alpha = 0.75, size = 0.75)+
                                       ggplot2::theme_bw()+
                                       ggplot2::guides(color = ggplot2::guide_legend(nrow = 1)) +
                                       ggplot2::theme(legend.position = "bottom",
                                             plot.margin = margin(0, 0, 0, 0)))

  # combine well plots
  dose_wells_plot <- cowplot::plot_grid(plotlist = dose_plot_list, ncol = length(proc_img_names))

  # make title
  title <- cowplot::ggdraw() +
    cowplot::draw_label(glue::glue("{date}_{project}_{drug_name}_{strain_name}"), fontface = 'bold', x = 0, hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 0)) # add margin on the left of the drawing canvas, so title is aligned with left edge of first plot

  # Make full dose plot
  full_plot <- cowplot::plot_grid(title, dose_wells_plot, dose_legend, ncol = 1, rel_heights = c(0.1, 1, 0.1))

  # return full
  return(full_plot)

}
