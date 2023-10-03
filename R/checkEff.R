#' checkEff
#'
#' A function to visualize experimental effects and detect potential issues in the data.
#'
#' @param data A data frame output from any easyXpress function used after the \code{modelSelection} function.
#' @param ... <[rlang::`dyn-dots`]> Variable(s) used to facet the data in the output plot. Variable names can be listed in succession.
#' @param x The independent variable to use as the x-axis of the plot. For example, strain. No quotes are needed.
#' @param y The dependent variable to use as the y-zxis of the plot. For example, median_wormlength_um. No quotes are needed.
#' @param fill The variable used to fill data points.
#' @param size The size of the points plotted with \code{ggplot2::geom_jitter}. The default is 1.5.
#' @param scales How to plot axes for facets. Should scales be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y")?
#' @return A box plot of the data with points jittered behind. The plot is faceted by any variables supplied with ...,
#' @export

checkEff <- function(data, ..., x, y, fill = NULL, size = 1.5, scales = "fixed") {

  # check for non-continuous x, group if necessary
  if(is.character(data %>% dplyr::pull({{x}})) |
     is.factor(data %>% dplyr::pull({{x}})) |
     is.logical(data %>% dplyr::pull({{x}}))) {
    p <- ggplot2::ggplot(data) +
      ggplot2::aes(x = {{x}}, y = {{y}}) +
      ggplot2::facet_wrap(ggplot2::vars(...), scales = scales) +
      theme_bw() +
      theme(strip.background = ggplot2::element_rect(
        color="black", fill="white", size=0.5, linetype="solid"),
        panel.grid = element_blank())
  } else {
    p <- ggplot2::ggplot(data) +
      ggplot2::aes(x = {{x}}, y = {{y}}, group = {{x}}) +
      ggplot2::facet_wrap(ggplot2::vars(...), scales = scales) +
      theme_bw() +
      theme(strip.background = ggplot2::element_rect(
        color="black", fill="white", size=0.5, linetype="solid"),
        panel.grid = element_blank())
  }

  # check on optional fill parameter
  if(as.character(rlang::enquo(fill))[2] != "NULL"){
    p.out <- p + ggplot2::geom_jitter(aes(fill = as.character({{fill}})),
                                      position = ggplot2::position_jitter(),
                                      size = size,
                                      shape = 21) +
      ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.35, fill = "white") +
      ggplot2::labs(fill = rlang::enquo(fill))
  } else {
    p.out <- p + ggplot2::geom_jitter(width = 0.25,
                                      size = size,
                                      fill = "grey",
                                      shape = 21) +
      ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.35, fill = "white" )
  }
  # return it
  return(p.out)
}
