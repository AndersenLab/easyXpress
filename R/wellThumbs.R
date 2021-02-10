#' wellThumbs
#'
#' Make .png thumbnails from .TIF files in the raw_images directory. Making
#' thumbs from .TIFs requires \href{https://imagemagick.org/}{ImageMagick} to be
#' installed on your system. To install imagemagick with Homebrew, enter:\cr
#' \code{brew install imagemagick}\cr into the terminal.
#'
#' @param project_dir The full path to the project directory containing the
#'   raw_images subdirectory.
#' @param plates a vector with plate numbers to make thumbs for. This vector is
#'   used to match plate patterns in the filelist.\cr \code{"all"} will make
#'   thumbs for wells in all plates.\cr \code{13:72} will make thumbs for wells
#'   in plates p13 - p72.\cr \code{c(1, 3, 106)} will make thumbs for wells in
#'   plates p01, p03, and p106.
#' @param max_dim The maximum dimension of the resized images in pixels. The
#'   default value is 512, which scales a 2048 pixel image to 6.25 percent of its
#'   original resolution.
#' @return A folder named raw_image_thumbs under the directory specified by
#'   \code{project_dir}.
#' @importFrom imager load.image resize save.image
#' @importFrom dplyr %>%
#' @export
#'

wellThumbs <- function(project_dir, plates = "all", max_dim = 512) {

  # get full file list of raw .TIF files from raw images directory
  file_list <- list.files(path = glue::glue("{project_dir}/raw_images"), pattern = "*.TIF", full.names = FALSE)

  # return if file list is empty
  if(length(file_list) == 0) {
    warning("wellThumbs() could not find any .TIF files")
  }
  else {
  # make raw_image_thumbs directory if needed
  fs::dir_create(glue::glue("{project_dir}/raw_image_thumbs"))

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
  # loop through all files, make thumbs and save them.
  for(i in unique(filtered_file_list)) {

    # setup image in R
    img <- imager::load.image(glue::glue("{project_dir}/raw_images/{i}"))

    # get raw img dimesions
    raw_max_dim <- max(dim(img))
    percentage <- 100*(max_dim/raw_max_dim)

    # resize to make thumbnail
    thumb <- imager::resize(img, -percentage, -percentage) # need negative for resize function

    # edit the file name for saving
    save_thumb_name <- stringr::str_replace(i, pattern = ".TIF", replacement = "_thumbnail.png")
    save_thumb_path <- glue::glue("{project_dir}/raw_image_thumbs/{save_thumb_name}")

    # Make message
    message(glue::glue("Saving thumbnail {save_thumb_name}"))

    # write the file
    imager::save.image(thumb, file = save_thumb_path)
  }
  message("wellThumbs Done")
  }
}
