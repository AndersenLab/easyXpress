#' nWF
#'
#' This function will flag wells that have too many or too few objects.
#'
#' @param data A data frame output from the \code{summarizeWells} function.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Variable(s) used to group data for ploting. Variable names in data are supplied separated by commas and without quotes.
#' @param max A numeric value used to flag wells with too many objects. The default is \code{30}.
#' @param min A numeric value used to flag wells with too few objects. The default is \code{5}.
#' @param plot Logical, if \code{TRUE}, the default. A ggplot2 object will be returned showing the distribution of object counts in wells (n).
#' @return Either a single data frame identical to the input data with the \code{n_WellFlag} variable added.
#' The \code{n_WellFlag} variable is coded as \code{"n<min"} or \code{n>max} for wells with object counts (n) outside the range set by \code{max} and \code{min}.
#' Or, if \code{plot = T}, a list with two elements, the first element is the data frame, the other is the diagnostic plot.
#' @export

nWF <- function(data, ..., max = 30, min = 5, plot = T) {
  # add flag
  d <- data %>%
    dplyr::mutate(n_WellFlag = dplyr::case_when(n > max ~ paste0("n>", max),
                                                n < min ~ paste0("n<", min),
                                                n >= min & n <= max ~ NA_character_,
                                                TRUE ~ "ERROR"))
  # message
  message("The n_WellFlag is set in the output data.")

  if(plot == T) {
    # plot it with thresh
    p <- ggplot2::ggplot(d) +
      ggplot2::aes(x = n) +
      ggplot2::geom_histogram(bins = 30) +
      ggplot2::geom_vline(xintercept = max, linetype = 2, color = "red") +
      ggplot2::geom_vline(xintercept = min, linetype = 2, color = "red") +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(ggplot2::vars(...)) +
      ggplot2::labs(title = glue::glue("n_WellFlag: {min} <= n <= {max}"),
                    y = "Well object count (n)")

    # return
    message(glue::glue("A diagnostic plot for checking the object number thresholds (max, min) is returned. See <out>$nWF.p"))
    out <- list(nWF.d = d, nWF.p = p)
    return(out)
  } else {
    # return data only
    return(d)
  }
}

