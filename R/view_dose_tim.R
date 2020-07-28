#' view_dose
#'
#' This function will plot processed images and overlay object types for a full dose response.
#'
#' @param df A processed data frame with raw image file names.
#' @param strain_name A strain to filter dataframe by.
#' @param drug_name A drug to filter dataframe by.
#' @param proc_img_dir A strain to filter dataframe by.
#' @param remote_server If true, use RCurl to connect to remote server with SFTP protocol.
#' @return A single data frame named raw_data that contains all CellProfiler model outputs and experimental treatments if a design file is used.
#' @export

view_dose <- function(df, strain_name, drug_name, proc_img_dir, remote_server = TRUE){
  # setup options for loging into remote server.
  # Paths to the public and private shh keys on the local machine must be valid.
  # The private key on the local machine must be included in the authorized_keys file on `qbiodata.bmbcb.northwestern.edu/home/.ssh/authorized_keys`.
  require(tidyverse)
  require(rebus)
  require(png)
  require(cowplot)

  if(remote_server == TRUE){

    opts <- list(
      ssh.public.keyfile = "~/.ssh2/id_rsa.pub", # local file path for public key
      ssh.private.keyfile = "~/.ssh2/id_rsa", # local file path for private key
      userpwd = readline(prompt = "Enter username:password: "),
      verbose = TRUE)

    #Adjust PATH for crul during R session to support sftp. This is PATH to the curl with sftp protocl support
    Sys.setenv(PATH=paste('/usr/local/opt/curl/bin', Sys.getenv('PATH'), sep=":"))
  }

  # Flexible method to find FileName_RawBF. Flexibility b/c prefixes can be attached in some cases.
  # A faster option would be to rename variable and remove flexibility to accomadate prefix
  RawBF_var_name <- df %>%
    .[str_detect(names(.), pattern = "FileName_RawBF")] %>%
    names(.)

  # subset dataframe
  subset_dat <- df %>%
    dplyr::filter(drug == drug_name & strain == strain_name) %>%
    dplyr::group_by(concentration_um) %>%
    dplyr::select(FileName_RawBF := !!glue::glue("{RawBF_var_name}"), everything()) %>% #Rename
    dplyr::distinct(concentration_um, .keep_all = T) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(concentration_um)

  # Pull raw image names
  raw_img_names <- subset_dat %>%
    dplyr::select(FileName_RawBF) %>%
    dplyr::pull(.)

  # find number of models
  n_models <- length(unique(df$model))

  # Pull model levels
  model_levels <- levels(df$model)

  # set object types given the number of models in dataframe.
  # 1 model is possible b/c additional models could be filterered.
  if(n_models == 2) {
    # set object_type
    dose_dat <- df %>%
      dplyr::select(FileName_RawBF := !!glue::glue("{RawBF_var_name}"), everything()) %>%
      dplyr::filter(FileName_RawBF %in% raw_img_names) %>%
      #testing model names from levels
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
    dose_dat <- df %>%
      dplyr::select(FileName_RawBF := !!glue::glue("{RawBF_var_name}"), everything()) %>%
      dplyr::filter(FileName_RawBF %in% raw_img_names) %>%
      #testing model names from levels
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
    dose_dat <- df %>%
      dplyr::select(FileName_RawBF := !!glue::glue("{RawBF_var_name}"), everything()) %>%
      dplyr::filter(FileName_RawBF %in% raw_img_names) %>%
      #testing model names from levels
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
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == levels(dose_dat$model[[1]])[1] ~ as.character(glue::glue("{levels(dose_dat$model[[1]])[1]}_model")),
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == levels(dose_dat$model[[1]])[2] ~ as.character(glue::glue("{levels(dose_dat$model[[1]])[2]}_model")),
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == levels(dose_dat$model[[1]])[3] ~ as.character(glue::glue("{levels(dose_dat$model[[1]])[3]}_model")),
        cluster_flag == F & well_edge_flag == F & is.na(well_outlier_flag) & model == levels(dose_dat$model[[1]])[4] ~ as.character(glue::glue("{levels(dose_dat$model[[1]])[4]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == levels(dose_dat$model[[1]])[1] ~ as.character(glue::glue("{levels(dose_dat$model[[1]])[1]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == levels(dose_dat$model[[1]])[2] ~ as.character(glue::glue("{levels(dose_dat$model[[1]])[2]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == levels(dose_dat$model[[1]])[3] ~ as.character(glue::glue("{levels(dose_dat$model[[1]])[3]}_model")),
        cluster_flag == F & well_edge_flag == F & well_outlier_flag == F & model == levels(dose_dat$model[[1]])[4] ~ as.character(glue::glue("{levels(dose_dat$model[[1]])[4]}_model"))))
  }

  # use stringr to find middle part of directory that contains the date and experiment name.
  date <- DGT %R% DGT %R% DGT %R% DGT %R% DGT %R% DGT %R% DGT%R% DGT
  separator <- char_class("/_-")
  run <- "RUN" %R% DGT%R% DGT
  name1 <- separator %R% date %R% separator %R% one_or_more(WRD)
  name2 <- date %R% separator %R% one_or_more(WRD)
  dir_date_name <- str_extract(proc_img_dir, pattern = name1)
  date_name <- str_extract(proc_img_dir, pattern = name2)

  # Make processed image names
  proc_img_names <- stringr::str_replace(raw_img_names, pattern = ".TIF", replacement = "_overlay.png")

  # Connect to remote server if needed
  if(remote_server == TRUE){

    # make list of images for dose response
    dose_proc_img_list <- list()
    for(i in 1:length(unique(proc_img_names))){
      img <- readPNG(RCurl::getBinaryURL(url = glue::glue("sftp://qbiodata.bmbcb.northwestern.edu/home/processed_images/{proc_img_dir}/{proc_img_names[i]}"), .opts = opts, curl = RCurl::getCurlHandle()))
      dose_proc_img_list[[i]] <- img
    }
  }
  else{
    # make list of images for dose response
    dose_proc_img_list <- list()
    for(i in 1:length(unique(proc_img_names))){
      img <- readPNG(glue::glue("{proc_img_dir}/{proc_img_names[i]}"))
      dose_proc_img_list[[i]] <- img
    }
  }
  # find dimensions of images
  h<-dim(dose_proc_img_list[[1]])[1] # image height
  w<-dim(dose_proc_img_list[[1]])[2] # image width

  #  plot well images for doses
  dose_plot_list <- NULL
  for(i in 1:length(proc_img_names)){

    #set well center
    edge_flag_center_x <- 1024
    edge_flag_center_y <- 1024
    edge_flag_radius <- unique(dose_dat$well_edge_flag_radius)

    #set object_colors
    kelly_colors <- c('#F2F3F4', '#222222', '#F3C300', '#875692', '#F38400',
                      '#A1CAF1', '#BE0032', '#C2B280', '#848482', '#008856',
                      '#E68FAC', '#0067A5', '#F99379', '#604E97', '#F6A600',
                      '#B3446C', '#DCD300', '#882D17', '#8DB600', '#654522', '#E25822', '#2B3D26')
    object_colors <- set_names(kelly_colors[1:length(unique(dose_dat$object_type))], unique(dose_dat$object_type))

    #make well plot
    well_plot <- ggplot(dose_dat %>%
                          dplyr::filter(FileName_RawBF == raw_img_names[i])) +
      aes(x = AreaShape_Center_X, y = AreaShape_Center_Y, color = object_type) +
      scale_color_manual(values = object_colors) +
      annotation_custom(grid::rasterGrob(dose_proc_img_list[[i]], width=unit(1,"npc"), height=unit(1,"npc")), 0, w, 0, -h) + # The minus is needed to get the y scale reversed. these are pixel dim of image
      scale_x_continuous(expand=c(0,0),limits=c(0,w)) +
      scale_y_reverse(expand=c(0,0),limits=c(h,0)) +  # The y scale is reversed because in image the vertical positive direction is typically downward.Also note the limits where h>0 is the first parameter.
      labs(y = "", x = glue::glue("P{dose_dat %>%
                                  filter(FileName_RawBF == raw_img_names[[i]]) %>%
                                  pull(Metadata_Plate)}_{dose_dat %>%
                                  filter(FileName_RawBF == raw_img_names[[i]]) %>%
                                  pull(Metadata_Well)}_{dose_dat %>%
                                  filter(FileName_RawBF == raw_img_names[[i]]) %>%
                                  pull(concentration_um)}uM"),
           fill = "Object Class") +
      geom_point(shape = 19, alpha = 0.75, size = 0.8) +
      coord_equal() +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.y=element_blank(),
            plot.margin = margin(0, 0, 0, 0),
            legend.position = "none") +
      annotate("path",
               x=edge_flag_center_x+edge_flag_radius*cos(seq(0,2*pi,length.out=100)),
               y=edge_flag_center_y+edge_flag_radius*sin(seq(0,2*pi,length.out=100)), color = "red", alpha = 0.25)

    #add plot to list
    dose_plot_list[[i]] <- well_plot
  }

  # get legend
  dose_legend <- get_legend(ggplot(dose_dat) +
                              aes(x = AreaShape_Center_X, y = AreaShape_Center_Y, color = object_type) +
                              scale_color_manual(values = object_colors) +
                              geom_point(shape = 19, alpha = 0.75, size = 0.75)+
                              theme_bw()+
                              guides(color = guide_legend(nrow = 1)) +
                              theme(legend.position = "bottom",
                                    plot.margin = margin(0, 0, 0, 0)))

  # combine well plots
  dose_wells_plot <- cowplot::plot_grid(plotlist = dose_plot_list, ncol = length(proc_img_names))

  # make title
  title <- cowplot::ggdraw() +
    draw_label(glue::glue("{date_name}_{drug_name}_{strain_name}"), fontface = 'bold', x = 0, hjust = 0) +
    theme(plot.margin = margin(0, 0, 0, 0)) # add margin on the left of the drawing canvas, so title is aligned with left edge of first plot

  # combine all
  plot_grid(title, dose_wells_plot, dose_legend, ncol = 1, rel_heights = c(0.1, 1, 0.1))

}
