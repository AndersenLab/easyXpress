#' checkBalance
#'
#' A function to visualize experimenta balance after filtering objects and wells.
#' This function requires a design data frame.
#'
#' @param data A data frame output from any easyXpress function after the \code{summarizeWells} function.
#' @param ... <[rlang::`dyn-dots`]> Variable(s) used to facet the data in the output plot. Variable names can be listed in succession.
#' @param design A design data frame like the one output from the \code{readXpress} function or one read into R from the raw design file(s).
#' All the variables specified in \code{...} and \code{x} must be present in the design data frame.
#' @param x The independent variable to use as the x-axis of the plot. For example, strain. No quotes are needed.
#' @param size The size of the number label used for plotting. The default is 3.
#' @return A list containing two elements.
#' 1) A data frame used to make the diagnostic plot
#' 2) A plot showing the data completeness post filtering. It is faceted by \code{...}.
#' @export

checkBalance <- function(data, ..., design, x, size = 3) {
  # get the vars
  vars <- names(data %>% dplyr::select(..., {{x}}))
  # warn if design has NAs for strain or drug
  if(anyNA(design$strain)) {
    stop(message(glue::glue("ERROR: There are {sum(is.na(design$strain))} rows with NAs for strain in the design file. Please review the design file.")))
  }
  if(anyNA(design$drug)) {
    stop(message(glue::glue("ERROR: There are {sum(is.na(design$drug))} rows with NAs for drug in the design file. Please review the design file.")))
  }
  if(!("well.id" %in% names(data))) {
    stop(message(glue::glue("ERROR: There is no well.id column in the data. Ensure the data frame has a well.id variable, which is added via summarizeWells.")))
  }
  if(F %in% (vars %in% names(design))) {
    stop(message(paste("ERROR: One or more of the specified variables <", paste(vars, collapse = ", "), "> are not found in the design. Please add them to use this function.")))
  }
  # get the design select what's needed and add a well id
  d.wells = design %>%
    dplyr::mutate(well.id = paste(Metadata_Experiment, Metadata_Plate, Metadata_Well, sep = "_")) %>%
    dplyr::select(well.id, {{x}}, ...)

  # get lost wells
  d.wells$lost <- !(d.wells$well.id %in% data$well.id)

  # get counts using the design
  summary <- d.wells %>%
    dplyr::left_join(data) %>%
    dplyr::group_by(..., {{x}}) %>%
    dplyr::mutate("{{x}}" := as.character({{x}})) %>%
    dplyr::mutate(g.total = dplyr::n(),
                  n.lost = sum(lost == T),
                  n.kept = g.total - n.lost,
                  perc.lost = n.lost/g.total,
                  perc.kept = n.kept/g.total) %>%
    dplyr::distinct(..., {{x}}, g.total, n.lost, n.kept, perc.lost, perc.kept) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = n.lost:n.kept, names_to = "n.type", values_to = "n") %>%
    tidyr::pivot_longer(cols = perc.lost:perc.kept, names_to = "perc.type", values_to = "perc")  %>%
    dplyr::mutate(type = dplyr::case_when(n.type == "n.lost" & perc.type == "perc.lost" ~ "lost",
                                          n.type == "n.kept" & perc.type == "perc.kept" ~ "kept",
                                          TRUE ~ NA_character_)) %>%
    dplyr::filter(!is.na(type)) %>%
    dplyr::select(-n.type, -perc.type) %>%
    dplyr::mutate(type = factor(type, levels = c("lost", "kept")))

  p <- ggplot2::ggplot(summary) +
    aes(x = {{x}}, y = perc, fill = type, label = n) +
    ggplot2::geom_col() +
    ggplot2::geom_text(size = size, position = ggplot2::position_stack(vjust = 0.5)) +
    ggplot2::geom_text(aes(x = {{x}}, y = 1.05, label = g.total), size = size) +
    ggplot2::ylim(0,1.05) +
    ggplot2::facet_wrap(ggplot2::vars(...)) +
    theme_bw() +
    labs(y = "fraction") +
    theme(strip.background = ggplot2::element_rect(
      color="black", fill="white", size=0.5, linetype="solid"))

  # return data and plot
  message("Returning list with elements d (the summary data frame) and p (the summary plot)")
  out <- list(d = summary, p = p)
  return(out)
}
