#' tidyProject
#'
#' Tidies up .TIF files exported from imaging platform and makes thumbnails of
#' wells and plates.
#'
#' @param project_dir the full path to the project directory. This folder should
#'   contain .TIF images exported from the imaging platform.
#' @param rm_other logical, remove non-TIF files and folders after the .TIF
#'   files are moved to raw_images subfolder? Default is \code{FALSE}. Set to
#'   \code{TRUE} if you are certain you want to remove non-TIF files and folders
#'   in the \code{project_dir} after .TIFs are moved.
#' @param plates a vector with plate numbers to make thumbs for. This vector is
#'   used to match plate patterns in the filelist.\cr \code{"all"} will make
#'   thumbs for all plates.\cr \code{13:72} will make thumbs for wells plates
#'   p13 - p72.\cr \code{c(1, 3, 106)} will make thumbs for plates p01, p03, and
#'   p106.
#' @param max_dim The maximum dimension of the resized images in pixels. The
#'   default value is 512, which scales a 2048 pixel image to 6.25 percent of
#'   its original resolution.
#' @return Folders named raw_images, raw_image_thumbs, and raw_plate_thumbs are
#'   created under the directory specified by \code{project_dir}. These folders
#'   match the directory structure needed to process the images with
#'   \href{https://github.com/AndersenLab/CellProfiler}{AndersenLab/CellProfiler}.
#'
#' @export
#'

tidyProject <- function(project_dir, rm_other = FALSE, plates = "all", max_dim = 512) {
  # tidy the images
  easyXpress::tidyImages(project_dir, rm_other)

  # make well thumbs
  easyXpress::wellThumbs(project_dir, plates, max_dim)

  # make plate thumbs
  easyXpress::plateThumbs(project_dir, plates)
}
