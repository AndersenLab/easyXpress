#' checkModelsPlot
#'
#' A function to help detect potential issues in the data by visualizing the size and type of objects across grouping variables.
#'
#' @param data A data frame output from the \code{modelSelection} function or any \code{OF} function from easyXpress.
#' @param OF Select one of \code{"filter", "label", "ignore"}. The default is \code{"filter"}, which will filter out all objects flagged by OFs.
#' \code{"label"} will fill flagged objects by OFs and fill objects not flagged with model names. \code{"ignore"} will fill all objects by model names.
#' @param ... Variable(s) used to summarize data. Variable names can be listed in succession.
#' @return A plot faceted by the grouping variables ..., with objects labelled by the CellProfiler worm model and OFs if specified.
#' @importFrom dplyr contains
#' @export

checkModelsPlot <- function(data, OF, ...) {
  # Find the ObjectFlags in data in the order they appear
  uf <- names(data %>% dplyr::select(contains("_ObjectFlag")))
  uf.short <- sub(uf, pattern = "_ObjectFlag", replacement = "")

  # Get a single ObjectFlag vector based on the order in which the flags were run
  objectFlag <- data %>%
    dplyr::select(contains("_ObjectFlag")) %>%
    tidyr::unite(objectFlag, sep = "_", na.rm = T) %>%
    tidyr::separate(objectFlag, into = "objectFlag", sep = "_", extra = "drop", remove = T) %>%
    dplyr::pull(objectFlag)

  #Find model names to cull data
  model_names <- levels(data$model)

  # Add it to data and summarize by grouping variables if provided.
  data.OF <- data %>%
    dplyr::mutate(animal_length_um = worm_length_um) %>%
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
                  -contains("model_"),
                  -contains("Parent_"))
  dplyr::bind_cols(objectFlag = objectFlag)

  if(!(OF %in% c("ignore", "label", "filter"))) {
    stop('Invalid OF argument. Please set the OF argument to either "ignore", "label", or "filter"')
  }

  if(OF == "ignore") {
    message("Ignoring ObjectFlags if present, all objects will be plotted and labelled by model.")
    # label the objects by flags and models
    data.p <- data.OF %>%
      dplyr::mutate(obj = model,
                    obj = factor(obj, levels = ))
  }

  if(OF == "label") {
    # tell us about what was found
    message(glue::glue("{length(uf)} ObjectFlags detected in data. They were applied in the following order:"))
    for(i in 1:length(uf)) {
      message(glue::glue("{uf[i]}"))
    }
    message("The flagged objects will be labelled in the plot.")
    # label the objects by flags and models
    data.p <- data.OF %>%
      dplyr::mutate(obj = dplyr::case_when(objectFlag != "" ~ objectFlag,
                                           objectFlag == "" ~ as.character(model),
                                           TRUE ~ "ERROR"))
  }

  if(OF == "filter") {
    # tell us about what was found
    message(glue::glue("{length(uf)} ObjectFlags detected in data. They were applied in the following order:"))
    for(i in 1:length(uf)) {
      message(glue::glue("{uf[i]}"))
    }
    message("The flagged objects will be filtered from the plot.")
    # label the objects by flags and models
    data.p <- data.OF %>%
      dplyr::filter(objectFlag == "") %>%
      dplyr::mutate(obj = model,
                    obj = factor(obj, levels = ))

  }

  # catch full filter
  if(nrow(data.p) == 0){
    stop('All data filtered due to flags. Please review OF function settings or use OF = "ignore"')
  }

  # plot them
  p <- ggplot2::ggplot(data.p) +
    ggplot2::geom_jitter(size = 1, aes(x = "group", y = animal_length_um, color = obj)) +
    ggplot2::geom_violin(aes(x = "group", y = animal_length_um),
                fill = NA, draw_quantiles = c(.25, .5, .75),
                size = 0.25) +
    ggplot2::stat_summary(aes(x = "group", y = animal_length_um, color = model),
                 geom = "point", fun = "mean", col = "black",
                 size = 3, shape = 20, fill = "black") +
    ggplot2::facet_wrap(ggplot2::vars(...)) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "", y = "Object length (um)") +
    ggplot2::theme(strip.background = ggplot2::element_rect(
      color="black", fill="white", size=0.5, linetype="solid"),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.y = element_blank())

  # return it
  return(p)
}
