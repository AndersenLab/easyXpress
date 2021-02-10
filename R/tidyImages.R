#' tidyImages
#'
#' Organize .TIF files exported from an imager platform by creating a raw_images
#' subdirectory in the project directory, moving all .TIF files into it, and
#' removing non-TIF files and folders if desired.
#'
#' @param project_dir the full path to the project directory. This folder should
#'   contain .TIF images exported from the imaging platform.
#' @param rm_other logical, remove non-TIF files and folders after the .TIF
#'   files are moved to raw_images subfolder? Default is \code{FALSE}. Set to
#'   \code{TRUE} if you are certain you want to remove non-TIF files and folders
#'   in the \code{project_dir} after .TIFs are moved.
#' @return A folder named raw_images under the directory specified with
#'   \code{project_dir}, which contains all .TIF image files for the project.
#' @importFrom dplyr %>%
#' @export
#'

tidyImages <- function(project_dir, rm_other = FALSE) {
  # make the raw_images directory
  message(glue::glue("making directory {project_dir}/raw_images"))
  system2(command = "mkdir", args = glue::glue("{project_dir}/raw_images"))

  # move all .TIF files exported from mol dev software to raw_images.
  permission = readline(prompt = glue::glue("Do you want to tidy all .TIF files in {project_dir} (y/n):  "))

  if(permission %in% c("Y", "yes", "y")) {
    message(glue::glue("moving .TIFs to {project_dir}/raw_images"))
    system2(command = "find", args = glue::glue("{project_dir} -type f -name \"*.TIF\" -exec mv '{{}}' {project_dir}/raw_images \\;"),
            stdout = FALSE, stderr = FALSE)
    if(rm_other == T) {
      # remove old directories (careful!)
      #get list of all directories not expected to be needed
      dirs <- tibble(dir = dir(project_dir)) %>%
        dplyr::filter(!(dir %in% c("raw_images", "raw_plate_thumbs", "raw_image_thumbs"))) %>%
        dplyr::mutate(dir_path = glue::glue("{project_dir}/{dir}")) %>%
        dplyr::pull(dir_path)
      message(glue::glue("Removing non-TIF files and other directories:"))
      message(paste(dirs, .sep = "\n"))
      unlink(dirs, recursive = T) # delete files listed in dirs
      message(glue::glue("DONE"))
    }
    else{
      message(glue::glue("Not removing non-TIF files and other directories"))
      message(glue::glue("DONE"))
    }
  }
  else{
    message(glue::glue("QUIT - no files moved"))
  }
}
