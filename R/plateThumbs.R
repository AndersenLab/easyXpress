#' plateThumbs
#'
#' Make a full 96-well plate image in .png format from well thumbnail images.
#'
#' @param project_dir The full path to the project directory. It must contain
#'   the raw_image_thumbs folder holding .png thumbnail images of wells. The
#'   \code{wellThumbs} function can be used to generate the raw_image_thumbs
#'   folder and .png thumbnails from raw .TIFs.
#' @param plates a vector with plate numbers to make thumbs for. This vector is
#'   used to match plate patterns in the filelist.\cr \code{"all"} will make
#'   thumbs for all plates.\cr \code{13:72} will make thumbs for wells plates
#'   p13 - p72.\cr \code{c(1, 3, 106)} will make thumbs for plates p01, p03, and
#'   p106.
#' @return A folder named raw_plate_thumbs under the project directory
#'   containing individual .png files for each plate.
#' @importFrom imager load.image resize save.image
#' @importFrom dplyr %>%
#' @importFrom stats na.omit
#' @export
#'

plateThumbs <- function(project_dir, plates = "all") {
  #make a dataframe from thumbnail files
  file_list <- list.files(glue::glue("{project_dir}/raw_image_thumbs"), full.names = T)

  # return if file list is empty
  if(length(file_list) == 0) {
    warning("plateThumbs() could not find any .png files")
  }

  else {
    # get the project name from project_dir
    proj <- stringr::str_extract(project_dir, pattern = "([^/]+$)")

    # make raw_plate_thumbs directory if needed
    fs::dir_create(glue::glue("{project_dir}/raw_plate_thumbs"))

    if(!("all" %in% plates)) {
      # convert plate sequence to proper format
      plate_pattern_vector = stringr::str_pad(plates, width = 2, side = "left", pad = 0) %>% paste0("-p", ., "-")
      # filter file list to selected plates
      filtered_file_list <- purrr::map(plate_pattern_vector, str_subset, string = file_list) %>%
        Reduce(c, .)
    }

    else {
      # set filtered file list to the file list
      filtered_file_list <-  file_list
    }

    # make file df and add layout
    file_df <- tibble::tibble(name = filtered_file_list) %>%
      dplyr::mutate(plate = stringr::str_extract(name, pattern = "-p[:digit:]+-"),
                    plate = stringr::str_replace_all(plate, pattern = "-", replacement = ""),
                    well = stringr::str_extract(name, pattern = "[A-Z][0-9][0-9]_thumbnail.png"),
                    well = stringr::str_replace(well, pattern = "_thumbnail.png", replacement = "")) %>%
      dplyr::group_by(plate) %>%
      dplyr::mutate(layout = 1:dplyr::n()) %>%
      dplyr::ungroup()

    #make plate df
    plates <- unique(file_df$plate)
    n_plates <- length(plates)
    plate = stringr::str_pad(rep(plates, each = 96), width = 2, side = "left", pad = 0)
    well = tibble::tibble(row = rep(LETTERS[1:8], each = 12)) %>%
      dplyr::mutate(col = stringr::str_pad(rep(1:12, 8), width = 2, side = "left", pad = 0)) %>%
      dplyr::mutate(well = paste0(row, col)) %>%
      dplyr::pull(well) %>%
      rep(., times = n_plates)
    plate_df = tibble::tibble(plate, well)

    # join em and add layout values
    join_df <- plate_df %>%
      dplyr::left_join(file_df, by = c("plate", "well"))

    # make layouts for each plate then make array of images and save array
    for(i in unique(join_df$plate)) {
      uniq_plate <- join_df %>% dplyr::filter(plate == i)

      lay <- rbind(uniq_plate$layout[1:12],
                   uniq_plate$layout[13:24],
                   uniq_plate$layout[25:36],
                   uniq_plate$layout[37:48],
                   uniq_plate$layout[49:60],
                   uniq_plate$layout[61:72],
                   uniq_plate$layout[73:84],
                   uniq_plate$layout[85:96])

      # make grob list from unique plate
      grob_list <- list()
      file_list2 <- stats::na.omit(unique(uniq_plate$name))

      # get dimension of thumbs (assuming all are square and identical). For sizing png file below
      d_img <- dim(imager::load.image(file_list2[1]))
      img_w <- d_img[1]
      img_h <- d_img[2]

      for(j in unique(file_list2)) {
        # load the same image
        img <- imager::load.image(j)
        grob <- grid::rasterGrob(img)
        # add to list
        grob_list[[j]] <- grob
      }

      # Save the plates correctly
      message(glue::glue("Saving raw plate thumb {project_dir}/raw_plate_thumbs/{proj}_{unique(uniq_plate$plate)}.png"))
      uniq_plate_grid <- gridExtra::grid.arrange(grobs = grob_list, layout_matrix = lay)
      pw = (img_w*12)/300
      ph = (img_h*8)/300
      ggplot2::ggsave(file = glue::glue("{project_dir}/raw_plate_thumbs/{proj}_{unique(uniq_plate$plate)}.png"), uniq_plate_grid, dpi = 300, height = ph, width = pw)
    }
    message("plateThumbs Done")
  }
}
