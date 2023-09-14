#' classifierOF
#'
#' This function will flag non-worm objects using machine learning.
#'
#' @param data A data frame output from the \code{modelSelection} function.
#' @param model Specify one of the built in models. Currently only "gbm2x" is available. This classifier was
#' trained on over 1,000 worm objects selected from a large GWAS experiment and classifies objects as "worm" or "non-worm" with ~90% accuracy.
#' NOTE: The model was trained to classify poorly segmented worms as 'non-worm' so true worms are often classified as "non-worm".
#' @return A single data frame identical to the input data with the \code{classifier_ObjectFlag} variable added.
#' The \code{classifier_ObjectFlag} variable is coded as \code{"classifier"} for objects that are called non-worm by the 2X classifier.
#' All other objects are coded as \code{NA_character}. The \code{gbm2x_worm_prob} variable provides the probability that the object is a properly segmented worm.
#' @export

classifierOF <- function(data, model = "gbm2x"){
  if(model == "gbm2x") {
    # get the classifier
    gbm2x <- easyXpress::gbm2x

    # use it
    gbm2x_prob <- stats::predict(gbm2x, newdata = data, type = "prob")
    gbm2x_class <- stats::predict(gbm2x, newdata = data)

    # add these predictions
    classifier_df <- data %>%
      dplyr::bind_cols(gbm2x_prob, gbm2x_class = gbm2x_class) %>%
      dplyr::mutate(classifier_ObjectFlag = dplyr::case_when(gbm2x_class == "non-worm" ~ "classifier",
                                                      gbm2x_class == "worm" ~ NA_character_,
                                                      TRUE ~ "ERROR")) %>%
      dplyr::select(-`non-worm`) %>%
      dplyr::rename(gbm2x_worm_prob = worm)

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
