#' checkWF
#'
#' @param data A data frame output from any WellFlag (WF) function from easyXpress.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Variable(s) used to group data. Variable names in data are supplied separated by commas and without quotes.
#' @param plot Logical, if \code{TRUE}, the defualt, a ggplot2 well will be returned in a list.
#' For example, \code{drug, strain}.
#' @return If \code{plot = TRUE} a list with two elements is returned. The first element (d) is a data frame summarized by ...,
#' with the numbers of wells flagged and retained by each WF applied to the data. The second element (p)
#' is a ggplot2 object showing the wells retained after all filtering and faceted by ...
#' If \code{plot = FALSE} only a data frame is returned.
#' @importFrom dplyr contains
#' @export

checkWF <- function(data, ..., plot = T){
  # Find the WellFlags in data in the order they appear
  uf <- names(data %>% dplyr::select(contains("_WellFlag")))
  #uf.short <- sub(uf, pattern = "_WellFlag", replacement = "")
  uf.short <- unlist(lapply(data[uf], function(x) unique(na.omit(x))))
  names(uf.short) <- NULL

  # send an error if needed
  if(length(uf) == 0 ) {
    stop("No WellFlags detected, did you specify the correct data? See checkWF() help for details.")
  }

  # tell us about what was found
  message(glue::glue("{length(uf)} WellFlags detected in data. They were applied in the following order:"))
  for(i in 1:length(uf)) {
    message(glue::glue("{uf[i]}"))
  }

  # Get a single WellFlag vector based on the order in which the flags were run
  wellFlag <- data %>%
    dplyr::select(contains("_WellFlag")) %>%
    tidyr::unite(wellFlag, sep = "_", na.rm = T) %>%
    tidyr::separate(wellFlag, into = "wellFlag", sep = "_", extra = "drop", remove = T) %>%
    dplyr::pull(wellFlag)

  # summarized by
  sum_by <- paste(names(data %>% dplyr::select(...)), collapse = ", ")
  message(glue::glue("The data are summarized by: {sum_by}"))

  # Add it to data and summarize by grouping variables if provided.
  summary <- data %>%
    dplyr::bind_cols(wellFlag = wellFlag) %>%
    dplyr::mutate(grand_n = dplyr::n(),
                  grouping = sum_by) %>%
    dplyr::group_by(...) %>%
    dplyr::mutate(group_n = dplyr::n()) %>%
    dplyr::group_by(..., wellFlag) %>%
    dplyr::mutate(wellFlag_n = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(wellFlag = ifelse(wellFlag == "" | is.na(wellFlag), "noFlag", wellFlag),
                  wellFlag_group_perc = wellFlag_n / group_n) %>%
    # set levels from user flag order
    dplyr::mutate(wellFlag = factor(wellFlag, levels = c(uf.short, "noFlag"))) %>%
    dplyr::arrange(..., wellFlag) %>%
    dplyr::distinct(..., grouping, wellFlag, wellFlag_group_perc, grand_n, group_n, wellFlag_n) %>%
    dplyr::select(..., grouping, wellFlag, wellFlag_group_perc, grand_n, group_n, wellFlag_n)


  if(plot == T) {
    # make a plot
    p <- ggplot2::ggplot(summary %>% dplyr::group_by(...)) +
      ggplot2::aes(x = "group", y = wellFlag_group_perc, fill = wellFlag, label = wellFlag_n) +
      ggplot2::geom_col() +
      ggplot2::geom_text(size = 3, position = ggplot2::position_stack(vjust = 0.5)) +
      ggplot2::facet_wrap(ggplot2::vars(...)) +
      theme_bw() +
      labs(x = "", y = "fraction") +
      theme(strip.background = ggplot2::element_rect(
        color="black", fill="white", size=0.5, linetype="solid"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())
    # return data and plot
    message("Returning list with elements d (the summary data frame) and p (the summary plot)")
    out <- list(d = summary, p = p)
    return(out)
  } else {
    message("No summary plots made. Set plot = T to make plots")
    # return data only
    return(summary)
  }
}
