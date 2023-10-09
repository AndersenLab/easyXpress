#' classifierOF
#'
#' This function will flag non-worm objects using machine learning.
#'
#' @param data A data frame output from the \code{modelSelection} function.
#' @param model Specify one of the built in models. Currently only "gbm2x" is available. This classifier was
#' trained on over 1,000 worm objects selected from a large GWAS experiment and classifies objects as "worm" or "non-worm" with ~90% accuracy.
#' NOTE: The model was trained to classify poorly segmented worms as 'non-worm' so true worms are often classified as "non-worm".
#' @param thresh The probability threshold for flagging objects based on the classifier. By default the thresh is st to 0.6.
#' Only the objects the classifier predicts to be improperly segmented, with a probability greater than \code{thres}, will be flagged.
#' @return A single data frame identical to the input data with the \code{classifier_ObjectFlag} variable added.
#' The \code{classifier_ObjectFlag} variable is coded as \code{"classifier"} for objects that are called non-worm by the 2X classifier.
#' All other objects are coded as \code{NA_character}, or if there are NAs in any of the variables used to classify objects they are coded as \code{"classErr"}. The \code{gbm2x_worm_prob} variable provides the probability that the object is a properly segmented worm.
#' @export

classifierOF <- function(data, model = "gbm2x", thresh = 0.6){
  if(model == "gbm2x") {
    # get the classifier
    gbm2x <- easyXpress::gbm2x

    # look for missing values or columns in expected model data
    # Specify column names to check
    check <- names(gbm2x$trainingData)[-1]

    # send error if one or more variables are missing
    if(F %in% (check %in% names(data))) {
      message("ERROR: The data does not contain the variables expected for the gbm2x classifier. Consider running the latest version of cellprofiler-nf. You can find the expected variables with <names(easyXpress::gbm2x$trainingData)>")
      stop()
    }

    # Filter rows with NAs in any of the specified columns
    nas <- data %>%
      dplyr::filter(rowSums(is.na(dplyr::select(., dplyr::all_of(check)))) > 0) %>%
      dplyr::mutate(gbm2x_worm_prob = NA_real_,
                    gbm2x_class = NA_character_,
                    classifier_ObjectFlag = "classErr")

    if(nrow(nas) > 0) {
      # filter to rows with no NAs in expected variables
      f.data <- data %>%
        dplyr::filter(rowSums(!is.na(dplyr::select(., dplyr::all_of(check)))) == length(check))

      # message about it
      message(glue::glue('WARNING: There are {nrow(nas)} rows with NAs in one or more object variables used for the classifier. These rows are flagged as "classErr" in the output.'))

    } else {
      # assign to data
      f.data <- data
    }

    # use it
    gbm2x_prob <- stats::predict(gbm2x, newdata = f.data, type = "prob")
    gbm2x_class <- stats::predict(gbm2x, newdata = f.data)

    # add these predictions
    classifier_df <- f.data %>%
      dplyr::bind_cols(gbm2x_prob, gbm2x_class = gbm2x_class) %>%
      # dplyr::mutate(classifier_ObjectFlag = dplyr::case_when(gbm2x_class == "non-worm" ~ "classifier",
      #                                                 gbm2x_class == "worm" ~ NA_character_,
      #                                                 TRUE ~ "ERROR")) %>%
      dplyr::mutate(classifier_ObjectFlag = dplyr::case_when(`non-worm` > thresh ~ "classifier",
                                                             `non-worm` <= thresh ~ NA_character_,
                                                              TRUE ~ "ERROR")) %>%
      dplyr::select(-`non-worm`) %>%
      dplyr::rename(gbm2x_worm_prob = worm) %>%
      dplyr::bind_rows(nas) %>%
      dplyr::arrange(Metadata_Experiment, Metadata_Plate, Metadata_Well, Parent_WormObjects)


    # test for unclassified objects
    if("ERROR" %in% classifier_df$classifier_ObjectFlag) {
      # return warning
      n.err <- sum(classifier_df$classifier_ObjectFlag == "ERROR") %>%
        dplyr::filter(classifier_ObjectFlag == "ERROR")
    }
  } else {
    stop(glue::glue('Only the gbm2x classifer model is currently supported. Please set the argement model = "gbm2X"'))
  }
  # retun it
  return(classifier_df)
}
