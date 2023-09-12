#' makeDesign
#'
#' This function will aid in making a design file for an imaging experiment (design.csv).
#'
#' @param proj The name of the project this design file will be used for. Remember that the project name always
#' follows the 8-digit date in project directories. For example, the project directory \code{20200130_toxin01A}
#' has a project name of \code{"toxin01A"}. The project name will be used to join the design file to the
#' \code{cellprofiler-nf} output data by the \code{Metadata_Experiment} variable, so it is critical that they
#' match exactly.
#' @param n.plate The number of plates in the experiment.
#' @param n.row The number of rows in the plates. Default is 8 for a 96-well plate.
#' @param n.col The number of columns in the plates. Default is 12 for a 96-well plate.
#' @param assay.type OPTIONAL: The type of assay run. For example, "48h".
#' @param od OPTIONAL: The final optical density of bacterial food used in the assay. For example, 10.
#' @param food OPTIONAL: A character description of the food used to perform the assay. For example, "15hHB101_20220727".
#' @return A single data frame with the variables \code{"Metadata_Plate"} and \code{"Metadata_Well"} variables that can be used
#'  as a design file for the \code{readXpress} function. The data frame can be augmented further to include specific drugs and dilutents
#'  or other experimental design attributes if desired. The variables output include:
#' @details
#' The variables output are as follows:
#'
#' | Variables          | Class         |
#' | ------------------ |:-------------:|
#' | Metadata_Experiment| chr           |
#' | Metadata_Plate     | chr           |
#' | Metadata_Well      | chr           |
#' | strain             | chr           |
#' | drug               | chr           |
#' | concentration_um   | dbl           |
#' | bleach             | int           |
#' | diluent            | chr           |
#' | assay_type         | chr           |
#' | plate              | dbl           |
#' | row                | chr           |
#' | col                | chr           |
#' | food               | chr           |
#' | od                 | int           |
#' | well_censor        | chr           |
#' | well_censor_reason | chr           |
#' | notes              | chr           |
#' @md
#' @importFrom dplyr %>%
#' @export

makeDesign <- function(proj, n.plate, n.row = 8, n.col = 12, assay.type = NULL, food = NULL, od = NULL) {

  # setup a plate with correct padding
  Metadata_Plate <- paste0("p", stringr::str_pad(seq(1:n.plate), width = 3, side = "left", pad = "0"))
  row <- LETTERS[1:n.row]
  col <- stringr::str_pad(seq(1:n.col), width = 2, side = "left", pad = "0")

  # make a design file of appropriate length
  design <- tidyr::crossing(Metadata_Plate, row, col) %>%
    dplyr::mutate(Metadata_Experiment = proj,
                  assay_type = ifelse(is.null(assay.type), NA_character_, assay.type),
                  Metadata_Well = paste0(row, col),
                  plate = as.numeric(stringr::str_replace_all(Metadata_Plate, pattern = "^p00|^p0|^p", replacement = "")),
                  row,
                  col = stringr::str_remove(col, pattern = "^0"),
                  food = ifelse(is.null(food), NA_character_, food),
                  od = ifelse(is.null(od), NA_integer_, od),
                  bleach = NA_integer_,
                  strain = NA_character_,
                  drug = NA_character_,
                  concentration_um = NA_real_,
                  diluent = NA_character_,
                  well_censor = NA_character_,
                  well_censor_reason = NA_character_,
                  notes = NA_character_) %>%
    dplyr::select(Metadata_Experiment,
                  Metadata_Plate,
                  Metadata_Well,
                  strain,
                  drug,
                  concentration_um,
                  bleach,
                  diluent,
                  assay_type,
                  plate,
                  row,
                  col,
                  food,
                  od,
                  well_censor,
                  well_censor_reason,
                  notes)
  # out
  return(design)
}
