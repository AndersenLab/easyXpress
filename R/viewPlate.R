#' viewPlate
#'
#'\code{viewPlate} plots the processed plate.
#'
#' @param df Summarised CellProfiler data output from the
#' \code{Xpress} or \code{process} functons. Either the raw or processed data can be viewed.
#' @param plate Desired plate to be analyzed.
#' @return A ggplot object with selected plate.
#' @importFrom ggplot2 ggplot aes geom_point theme_bw theme labs
#'
#' @export


viewPlate <- function(df, plate) {
  plot_dat <- df %>%
    dplyr::filter(Metadata_Plate == glue::glue("{plate}")) %>%
    dplyr::mutate(Well = Metadata_Well) %>%
    tidyr::separate(Metadata_Well, into=c("Row","Column"), sep=c("(?<=[A-Za-z])(?=[0-9])")) %>%
    dplyr::mutate(Column = as.numeric(Column))

  # to view summarized stats by well
  plt <- plotly::ggplotly(ggplot(plot_dat) +
                    aes(x = factor(Column, levels = c(1:12)),
                        y = factor(Row, levels = c("H", "G", "F", "E", "D", "C", "B", "A")),
                        color = mean_wormlength_um, text = Well, key = n) +
                    geom_point(size = 2) +
                    theme_bw(16) +
                    theme(legend.position = "right") +
                    labs(x = "Column", y = "Row", title = glue::glue("Plate: { plate}")) +
                    viridis::scale_color_viridis(), tooltip = "key")

  return(plt)
}
