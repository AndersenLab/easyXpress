#' viewOverlay
#'
#' @param data A data frame output from \code{modelSelection} or any \code{OF} function from easyXpress.
#' The dataframe should be filtered to contain only the wells to be plotted. Please Arrange the dataframe in the order that overlays are to be plotted.
#' @param proc.img.dir A variable name in \code{data} that holds the full PATH to the directory holding processed images matching the data.
#' The processed images must have the standard \code{_overlay.png} suffix and file name output from CellProfiler.
#' @param well.label A variable name in \code{data} to display as a well label. For example, \code{"Metadata_Well"}.
#' @param obj.label A variable name in \code{data} to label objects by. For example, \code{"model"}.
#' @param obj.shape Optional: A variable name in \code{data} to use for the shape of objects. The default is NULL.
#' @param obj.shape.pal Optional: A shape palette for objects. This is a vector of shape values with names for each unique value in \code{obj.shape}. NULL passes shape values to \code{obj.label}.
#' @param obj.color Optional: A variable name in \code{data} to color objects by. The default is NULL.
#' @param obj.col.pal Optional: A color palette for obj.labels. This is a vector of colors with names for each unique value in \code{obj.color}. NULL passes color values to \code{obj.label}.
#' @param text.anno Optional: a variable name in \code{data} used to display text annotation at an object center.
#' by default this value is shifted vertically to avoid plotting over object center.
#' @param file Optional: The full path for saving output plot. Default is NULL which will just return the ggplot2 plot.
#' @return A plot showing all the CellProfiler processed well overlays in \code{data} with objects annotated as desired.
#' @importFrom gridExtra arrangeGrob
#' @importFrom pals kelly
#' @export

viewOverlay <- function(data, proc.img.dir, well.label, obj.label,
                        obj.color = NULL, obj.col.pal = NULL,
                        obj.shape = NULL, obj.shape.pal = NULL,
                        text.anno = NULL, file = NULL) {
  # Check for excessive images


  # flexibility for FileName_RawBF vs. Image_FileName_RawBF in dat
  if("FileName_RawBF" %in% names(data)){
    data <- data %>%
      dplyr::rename_at(dplyr::vars(dplyr::matches("FileName_RawBF")), ~ "Image_FileName_RawBF")
  }

  # set all names so they match exactly! ugh - this feels dirty. Should update to use tunneling.
  # https://www.tidyverse.org/blog/2020/02/glue-strings-and-tidy-eval/
  ########################
  # vars <- c("ASS", "BARf")
  # d2 <- data %>%
  #   dplyr::mutate_at(dplyr::vars(!!!vars), function(x) as.character(x)) %>%
  #   dplyr::rename_at(dplyr::vars(!!!vars), function(x) paste0(x,"_ObjectFlag"))
  args.l <- as.list(match.call(expand.dots=FALSE))
  arg.df <- tibble::tibble(arg = names(unlist(args.l)), value = as.character(unname(unlist(args.l)))) %>%
    dplyr::filter(arg %in% c("well.label", "obj.label", "obj.color", "obj.shape", "text.anno")) %>%
    dplyr::mutate(value = paste0("^", value, "$"))
  if("well.label" %in% arg.df$arg){
  well.label <- arg.df %>% dplyr::filter(arg == "well.label") %>% dplyr::pull(value)
  }
  if("obj.label" %in% arg.df$arg){
  obj.label <- arg.df %>% dplyr::filter(arg == "obj.label") %>% dplyr::pull(value)
  }
  if("obj.color" %in% arg.df$arg){
  obj.color <- arg.df %>% dplyr::filter(arg == "obj.color") %>% dplyr::pull(value)
  }
  if("obj.shape" %in% arg.df$arg){
  obj.shape <- arg.df %>% dplyr::filter(arg == "obj.shape") %>% dplyr::pull(value)
  }
  if("text.anno" %in% arg.df$arg){
  text.anno <- arg.df %>% dplyr::filter(arg == "text.anno") %>% dplyr::pull(value)
  }

  # rename vars for later, do here to throw errors early
   plot_data_rename <-  data %>%
     dplyr::rename_at(dplyr::vars(dplyr::matches(well.label)), ~ "well_label") %>%
     dplyr::rename_at(dplyr::vars(dplyr::matches(obj.label)), ~ "obj_class")

  # rename vars for optional parameters
  if(!is.null(obj.color)){
    plot_data_rename <- plot_data_rename %>%
      dplyr::rename_at(dplyr::vars(dplyr::matches(arg.df %>% dplyr::filter(arg == "obj.color") %>% dplyr::pull(value))), ~ "obj_color")
  }
  if(!is.null(obj.shape)){
    plot_data_rename <- plot_data_rename %>%
      dplyr::rename_at(dplyr::vars(dplyr::matches(arg.df %>% dplyr::filter(arg == "obj.shape") %>% dplyr::pull(value))), ~ "obj_shape")
  }
  if(!is.null(text.anno)){
    plot_data_rename <- plot_data_rename %>%
      dplyr::rename_at(dplyr::vars(dplyr::matches(arg.df %>% dplyr::filter(arg == "text.anno") %>% dplyr::pull(value))), ~ "text_anno")
  }

  #make file list
  file_list_df <- data %>%
    dplyr::rename_at(dplyr::vars(dplyr::matches(proc.img.dir)), ~ "proc.img.dir") %>%
    dplyr::distinct(Image_FileName_RawBF, .keep_all = T) %>%
    dplyr::mutate(file_path = stringr::str_replace(Image_FileName_RawBF, pattern = ".TIF|.tif", replacement = "_overlay.png"),
                  file_path = paste0(proc.img.dir, file_path))

  file_list <- file_list_df %>%
    dplyr::pull(file_path)

  # Make array dimensions
  n <- length(file_list)
  nrow <- floor(sqrt(n)) # goes in nrow
  ncol <- ceiling(n/nrow) # do we need ceiling?
  max_dim <- max(nrow, ncol)

  # Check for too many images
  if(n > 96){
    # Ask if you really want to process more than 96 images
    permission = readline(prompt = message(glue::glue("WARNING: {n} images found, consider filtering data to fewer images.
                                                      Do you want to make an array of {n} images anyway? Enter (y/n): ")))
    if(!(permission %in% c("Y", "yes", "y", "YES"))) {
      message("OK, see ya!")
      return()
    }
  }
  # send a message that we're making a grob list
  message(glue::glue("Making grob list for {n} overlays"))

  # make a grob list
  img_grob_list <- list()

  # loop through file list and add to grob list (80sec/36overlays, when local)
  groblist_pb <- progress::progress_bar$new(total = length(unique(file_list)),
                                          format = "Making grob list [:bar] :percent eta: :eta",
                                          clear = FALSE)
  for(i in unique(file_list)) {
    # get the image
    img <- png::readPNG(glue::glue("{i}"))
    grob <- grid::rasterGrob(img)
    # add to list
    img_grob_list[[i]] <- grob
    # add tick
    groblist_pb$tick()
  }

  # get image dimensions from last img
  h<-dim(img)[1] # image height
  w<-dim(img)[2] # image width

  # Make gtable with layout
  img_grid <- do.call("arrangeGrob", c(img_grob_list, ncol=ncol, nrow=nrow))

  # Get array position df
  array_pos_df <- file_list_df %>%
    dplyr::mutate(array_x_pos = rep(0:(ncol-1), length.out = dplyr::n()),
                  array_y_pos = rep((nrow-1):0, each = ncol, length.out = dplyr::n())) %>%
    dplyr::select(Image_FileName_RawBF, array_x_pos, array_y_pos) # just take what's needed

  # Get plot df for adding points
  plot_data_df <- plot_data_rename %>%
    dplyr::left_join(array_pos_df, by = "Image_FileName_RawBF") %>%
    dplyr::mutate(obj_center_x = abs(AreaShape_Center_X) * (1/w) + array_x_pos,
                  obj_center_y = abs(AreaShape_Center_Y - h) * (1/h) + array_y_pos) # w = img width, h = img height

  # Generate plot message
  message(glue::glue("Making plot for {n} overlays"))

  # make color palette for plotting
  if(is.null(obj.col.pal) & is.null(obj.color)){
    # get a typical toxin color palette
    col_pal <- pals::kelly()[3:22] # get a list of colors from kelly
    obj_color_pal <- col_pal[1:length(unique(plot_data_df$obj_class))]
    names(obj_color_pal) <- unique(plot_data_df$obj_class)
  }

  if(is.null(obj.col.pal) & !is.null(obj.color)){
    n_colors <- length(unique(plot_data_df$obj_color))
    obj_color_pal <- seq(1:n_colors)
    names(obj_color_pal) <- unique(plot_data_df$obj_color)
  }

  if(!is.null(obj.col.pal) & is.null(obj.color)){
    obj_color_pal <- obj.col.pal
  }

  if(!is.null(obj.col.pal) & !is.null(obj.color)){
    obj_color_pal <- obj.col.pal
  }

  # make shape palette for plotting
  if(is.null(obj.shape.pal) & is.null(obj.shape)){
    obj_shape_pal <- rep(21, times = length(unique(plot_data_df$obj_class))) # set all to 21 if no shape palette provided
    names(obj_shape_pal) <- unique(plot_data_df$obj_class)
  }
  if(is.null(obj.shape.pal) & !is.null(obj.shape)){
    n_shapes <- length(unique(plot_data_df$obj_shape))
    obj_shape_pal <- seq(21:n_shapes)
    names(obj_shape_pal) <- unique(plot_data_df$obj_shape)
  }
  if(!is.null(obj.shape.pal) & is.null(obj.shape)){
    obj_shape_pal <- obj.shape.pal
    # ADD a catch for mismatch here
  }
  if(!is.null(obj.shape.pal) & !is.null(obj.shape)){
    obj_shape_pal <- obj.shape.pal
    # ADD a catch for mismatch here
  }

  # Make plot
  p <- ggplot2::ggplot(plot_data_df) +
    ggplot2::xlim(0, ncol) +
    ggplot2::ylim(-0.25, nrow) +
    {if(is.null(obj.color) & is.null(obj.shape))
      ggplot2::aes(x = obj_center_x, y = obj_center_y, color = obj_class, shape = obj_class)
    } +
    {if(!is.null(obj.color) & is.null(obj.shape))
      ggplot2::aes(x = obj_center_x, y = obj_center_y, color = obj_color, shape = obj_class)
    } +
    {if(is.null(obj.color) & !is.null(obj.shape))
      ggplot2::aes(x = obj_center_x, y = obj_center_y, color = obj_class, shape = obj_shape)
    } +
    {if(!is.null(obj.color) & !is.null(obj.shape))
      ggplot2::aes(x = obj_center_x, y = obj_center_y, color = obj_color, shape = obj_shape)
    } +
    ggplot2::annotation_custom(img_grid, 0, ncol, 0, nrow) +
    ggplot2::scale_shape_manual(name = "Object", values = obj_shape_pal) +
    ggplot2::scale_color_manual(name = "Object", values = obj_color_pal) +
    ggplot2::geom_point(alpha = 0.5, size = 1, stroke = 0.1) + # n = image number (n/12), stroke = 0 should remove outlines
    ggplot2::geom_label(inherit.aes = F,
                        aes(x = array_x_pos + 0.5, y = array_y_pos + 1, label = well_label),
                        label.padding = ggplot2::unit(.05, "line"), size = 1, fill = "white", color = "black",
                        label.size = NA, show.legend = F) +
    ggplot2::coord_equal() +
    ggplot2::theme_void() +
    theme(legend.justification = 'center',
          legend.direction = "horizontal",
          legend.position = c(0.5, (1/(nrow+1))*.25),
          legend.key.width = ggplot2::unit(1, "line"),
          legend.key.size = ggplot2::unit(1, "line"),
          legend.title = ggplot2::element_text(size = 2),
          legend.text = ggplot2::element_text(size = 2))

  # add text annotation to objects
  if(!is.null(text.anno)){
    p <- p +
      ggplot2::geom_text(aes(label = text_anno), size = 0.5, nudge_y = 0.03, show.legend = F)
  }

  # Return it or save it and retrun it
  if(is.null(file)){
    return(p)
  } else {
  message(glue::glue("Saving plot as: {file}"))
  ggplot2::ggsave(file = file, plot = p, width = ncol, height = nrow + 0.25, dpi = 2048)
  return(p)
  }
}
