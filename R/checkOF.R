#' checkOF
#'
#' @param data A data frame output from any ObjectFlag (OF) function from easyXpress.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Variable(s) used to group data. Variable names in data are supplied separated by commas and without quotes.
#' For example, \code{drug, strain}.
#' @param plot Logical, if \code{TRUE}, the default, a ggplot2 object will be returned in a list.
#' @return If \code{plot = TRUE} a list with two elements is returned. The first element (d) is a data frame summarized by ...,
#' with the numbers of objects flagged and retained by each OF applied to the data. The second element (p)
#' is a ggplot2 object showing the objects retained after all filtering and faceted by ...
#' If \code{plot = FALSE} only a data frame is returned.
#' @importFrom dplyr contains
#' @export

checkOF <- function(data, ..., plot = T){
  # Find the ObjectFlags in data in the order they appear
  uf <- names(data %>% dplyr::select(contains("_ObjectFlag")))
  #uf.short <- sub(uf, pattern = "_ObjectFlag", replacement = "")
  uf.short <- unlist(lapply(data[uf], function(x) unique(na.omit(x))))
  names(uf.short) <- NULL

  # send an error if needed
  if(length(uf) == 0 ) {
    stop("No ObjectFlags detected, did you specify the correct data? See checkOF() help for details.")
  }

  # tell us about what was found
  message(glue::glue("{length(uf)} ObjectFlags detected in data. They were applied in the following order:"))
  for(i in 1:length(uf)) {
    message(glue::glue("{uf[i]}"))
  }

  # Get a single ObjectFlag vector based on the order in which the flags were run
  objectFlag <- data %>%
    dplyr::select(contains("_ObjectFlag")) %>%
    tidyr::unite(objectFlag, sep = "_", na.rm = T) %>%
    tidyr::separate(objectFlag, into = "objectFlag", sep = "_", extra = "drop", remove = T) %>%
    dplyr::pull(objectFlag)

  #Find model names to cull data
  model_names <- levels(data$model)

  # summarized by
  sum_by <- paste(names(data %>% dplyr::select(...)), collapse = ", ")
  message(glue::glue("The data are summarized by: {sum_by}"))

  # Add it to data and summarize by grouping variables if provided.
  summary <- data %>%
    dplyr::select(-contains("Worm_"),
                  -contains("Distance_"),
                  -contains("Intensity_"),
                  -contains("Location_"),
                  -contains("AreaShape_"),
                  -contains("Distance_"),
                  -contains("Image"),
                  -contains("flag"),
                  -contains("Number"),
                  -dplyr::one_of(model_names),
                  -contains("model"),
                  -contains("Parent_")) %>%
    dplyr::bind_cols(objectFlag = objectFlag) %>%
    dplyr::mutate(grand_n = dplyr::n(),
                  grouping = sum_by) %>%
    dplyr::group_by(...) %>%
    dplyr::mutate(group_n = dplyr::n()) %>%
    dplyr::group_by(..., objectFlag) %>%
    dplyr::mutate(objectFlag_n = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(objectFlag = ifelse(objectFlag == "" | is.na(objectFlag), "noFlag", objectFlag),
                  objectFlag_group_perc = objectFlag_n / group_n) %>%
    # set levels from user flag order
    dplyr::mutate(objectFlag = factor(objectFlag, levels = c(uf.short, "noFlag"))) %>%
    dplyr::arrange(..., objectFlag) %>%
    dplyr::distinct(..., grouping, objectFlag, objectFlag_group_perc, grand_n, group_n, objectFlag_n) %>%
    dplyr::select(..., grouping, objectFlag, objectFlag_group_perc, grand_n, group_n, objectFlag_n)


  if(plot == T) {
    # make a plot
    p <- ggplot2::ggplot(summary %>% dplyr::group_by(...)) +
      ggplot2::aes(x = "group", y = objectFlag_group_perc, fill = objectFlag, label = objectFlag_n) +
      ggplot2::geom_col() +
      ggplot2::geom_text(size = 3, position = ggplot2::position_stack(vjust = 0.5)) +
      #geom_text(aes(label=asterisks, group = ...)) +
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
