#' modelSelection
#'
#' This function will assign the appropriate CellProfiler model
#' to each primary object in a raw_data object output
#' by the readXpress function.
#'
#' @param df A single data frame containing worm object measurements
#' from each CellProfiler model used in an assay. A model variable is required.
#' @return A single data frame named model_selected that contains
#' the best CellProfiler model for detecting worm objects within
#' each primary object detected by CellProfiler.
#' @importFrom dplyr %>%
#' @export

modelSelection <- function(df) {
  # identify number of worm models used
  model_num <- length(unique(df$model))

  # extract appropriate model levels from worm counts
  model_names <- df %>%
    dplyr::group_by(model) %>%
    dplyr::summarize(worm_count = dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(worm_count)) %>%
    dplyr::pull(model)

  #load model selection file based on number of models selected.
  if (model_num == 1) {
    # read generalized model selection df
    generalized_model_selection_df <- easyXpress::model_select_1

    # exctract generalized model names
    generalized_model_names <- stats::na.omit(
      unique(generalized_model_selection_df$model_select))

    # replace generalized model names with model names from df
    model_selection_df <- generalized_model_selection_df %>%
      data.table::setnames(., old = as.vector(colnames(generalized_model_selection_df[1])), new = model_names, skip_absent = TRUE) %>%
      dplyr::mutate_all(~stringr::str_replace_all(., generalized_model_names[1], model_names[1])) %>%
      dplyr::mutate_at(dplyr::vars(model_names), as.numeric)

    message(glue::glue("Found {model_num} worm model in data."))
    for(i in 1:model_num){
      message(glue::glue("\n{model_names[i]}"))
    }


  }  else if (model_num == 2) {
    # read generalized model selection df
    generalized_model_selection_df <- easyXpress::model_select_2

    # exctract generalized model names
    generalized_model_names <- stats::na.omit(
          unique(generalized_model_selection_df$model_select))

    # replace generalized model names with model names from df
    model_selection_df <- generalized_model_selection_df %>%
      data.table::setnames(., old = as.vector(colnames(generalized_model_selection_df[1:2])), new = model_names, skip_absent = TRUE) %>%
      dplyr::mutate_all(~stringr::str_replace_all(., generalized_model_names[1], model_names[1])) %>%
      dplyr::mutate_all(~stringr::str_replace_all(., generalized_model_names[2], model_names[2])) %>%
      dplyr::mutate_at(dplyr::vars(model_names), as.numeric)

    message(glue::glue("Found {model_num} worm models in data."))
    for(i in 1:model_num){
      message(glue::glue("\n{model_names[i]}"))
    }

  } else if (model_num == 3) {
    # read generalized model selection df
    generalized_model_selection_df <- easyXpress::model_select_3

    # exctract generalized model names
    generalized_model_names <- stats::na.omit(
          unique(generalized_model_selection_df$model_select))

    # replace generalized model names with model names from df
    model_selection_df <- generalized_model_selection_df %>%
      data.table::setnames(., old = as.vector(colnames(generalized_model_selection_df[1:3])), new = model_names, skip_absent = TRUE) %>%
      dplyr::mutate_all(~stringr::str_replace_all(., generalized_model_names[1], model_names[1])) %>%
      dplyr::mutate_all(~stringr::str_replace_all(., generalized_model_names[2], model_names[2])) %>%
      dplyr::mutate_all(~stringr::str_replace_all(., generalized_model_names[3], model_names[3])) %>%
      dplyr::mutate_at(dplyr::vars(model_names), as.numeric)

    message(glue::glue("Found {model_num} worm models in data."))
    for(i in 1:model_num){
      message(glue::glue("\n{model_names[i]}"))
    }

  } else if (model_num == 4) {
    # read generalized model selection df
    generalized_model_selection_df <- easyXpress::model_select_4

    # exctract generalized model names
    generalized_model_names <- stats::na.omit(
          unique(generalized_model_selection_df$model_select))

    # replace generalized model names with model names from df
    model_selection_df <- generalized_model_selection_df %>%
      data.table::setnames(., old = as.vector(colnames(generalized_model_selection_df[1:4])), new = model_names, skip_absent = TRUE) %>%
      dplyr::mutate_all(~stringr::str_replace_all(., generalized_model_names[1], model_names[1])) %>%
      dplyr::mutate_all(~stringr::str_replace_all(., generalized_model_names[2], model_names[2])) %>%
      dplyr::mutate_all(~stringr::str_replace_all(., generalized_model_names[3], model_names[3])) %>%
      dplyr::mutate_all(~stringr::str_replace_all(., generalized_model_names[4], model_names[4])) %>%
      dplyr::mutate_at(dplyr::vars(model_names), as.numeric)

    message(glue::glue("Found {model_num} worm models in data."))
    for(i in 1:model_num){
      message(glue::glue("\n{model_names[i]}"))
    }
  }
  # give a message
  message("\nSelecting best model for each Parent_WormObject.")
  #join combination file with raw data
  suppressMessages(model_selected_df <- df %>%
    dplyr::group_by(Metadata_Experiment, Metadata_Plate, Metadata_Well, Parent_WormObjects, model) %>%
    dplyr::mutate(num_worms = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Metadata_Experiment, Metadata_Plate, Metadata_Well, Parent_WormObjects) %>%
    dplyr::distinct(model, .keep_all = T) %>%
    dplyr::ungroup() %>%
    dplyr::select(Metadata_Experiment, Metadata_Plate, Metadata_Well,
                  Parent_WormObjects, model, num_worms) %>%
    tidyr::spread(model, num_worms) %>%
    dplyr::mutate_at(dplyr::vars(tidyselect::one_of(model_names)),
                     ~dplyr::case_when(is.na(.) ~ 0,
                                           . == 1 ~ 1,
                                           . >= 2 ~ 2)) %>%
    dplyr::left_join(model_selection_df) %>%
    dplyr::full_join(., df) %>%
    dplyr::filter(model == model_select) %>%
    dplyr::mutate(model = factor(model, levels = model_names)))

  # finsih it
  message("\nDONE")
  return(model_selected_df)
}
