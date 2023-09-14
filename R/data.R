#' The standard gradient boosted classifier for AndersenLab 2X images
#'
#' This stochastic gradient boosted classifier was trained with 1,089 worm objects selected from a broad range of experimental conditions
#' using the caret R package (v6.0-92) (Kuhn 2008). The classifier accuracy was 89.6%, which was estimated by resampling the training data
#' with 10-fold cross-validation.
#'
#' @format ## `gbm2x`
#' A Large train object with 24 elements.
#' @source <https://github.com/AndersenLab/2021_GWA_data_cleaning>
#'
"gbm2x"


#' Generalized model for one worm object
#'
#' This data frame contains the generalized model selection dataframe
#' for a one worm model CellProfiler run.
#'
#' @name model_select_1
#' @format A dataframe with 3 rows and 4 variables:
#' \describe{
#'   \item{m1}{number of objects identified as model number 1}
#'   \item{model_select}{final selected model}
#'   \item{model_flag}{flag classifier}
#'   \item{cluster_flag}{indicates presence of a cluster}
#' }
NULL


#' Generalized model for two worm objects
#'
#' This data frame contains the generalized model selection dataframe
#' for a two worm model CellProfiler run.
#'
#' @name model_select_2
#' @format A dataframe with 9 rows and 5 variables:
#' \describe{
#'   \item{m1}{number of objects identified as model number 1}
#'   \item{m2}{number of objects identified as model number 2}
#'   \item{model_select}{final selected model}
#'   \item{model_flag}{flag classifier}
#'   \item{cluster_flag}{indicates presence of a cluster}
#' }
NULL


#' Generalized model for three worm objects
#'
#' This data frame contains the generalized model selection dataframe
#' for a three worm model CellProfiler run.
#'
#' @name model_select_3
#' @format A dataframe with 27 rows and 6 variables:
#' \describe{
#'   \item{m1}{number of objects identified as model number 1}
#'   \item{m2}{number of objects identified as model number 2}
#'   \item{m3}{number of objects identified as model number 3}
#'   \item{model_select}{final selected model}
#'   \item{model_flag}{flag classifier}
#'   \item{cluster_flag}{indicates presence of a cluster}
#' }
NULL


#' Generalized model for four worm objects
#'
#' This data frame contains the generalized model selection dataframe
#' for a four worm model CellProfiler run.
#'
#' @name model_select_4
#' @format A dataframe with 81 rows and 7 variables:
#' \describe{
#'   \item{m1}{number of objects identified as model number 1}
#'   \item{m2}{number of objects identified as model number 2}
#'   \item{m3}{number of objects identified as model number 3}
#'   \item{m4}{number of objects identified as model number 4}
#'   \item{model_select}{final selected model}
#'   \item{model_flag}{flag classifier}
#'   \item{cluster_flag}{indicates presence of a cluster}
#' }
NULL
