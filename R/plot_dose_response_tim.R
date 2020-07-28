#' plot.dose.response
#'
#' This is a plotting function for quick dose-response curve generation with CellProfiler data. This function loops through each exposure and generates rough dose-response curves for each.
#'
#' @param data Processed summarized data to be plotted, preferably from process_data output.
#' @param outdir String specifying output directory for dose-response plots. Can exist or be a directory you'd like to create.
#' @param traits Traits to be displayed in the dose-response. Default plots mean worm length (um) only.
#' @param ... Treatment to loop through.
#' @return A plot of the dose-response of interest
#' @export


plot.dose.response <- function(data, outdir, traits = "ALL", ...){
  require(tidyverse)

  ## Create output directory
  suppressWarnings(dir.create(outdir))

  ## Nest data according to function
  x <- data %>%
    # dplyr::group_by(...) %>%
    dplyr::group_by(drug) %>%
    nest()
  plot.dat <- x$data
  plot.factors <- x[[1]]

  ## All Traits Output
  if(traits == "ALL") {

    dose.plot <- function(x,y){
      suppressWarnings(dir.create(paste(outdir, y, sep = "/")))
      setwd(paste(outdir, y, sep = "/"))

      lengthtraits = colnames(as.data.frame(x))[grep(colnames(as.data.frame(x)), pattern = "length")]
      for(j in 1:length(lengthtraits)){
        plot <- ggplot(data = as.data.frame(x),
                       mapping = aes(x = factor(concentration_um),

                                     alpha = 0.85)) +
          geom_boxplot(aes_string(y = lengthtraits[[j]]), outlier.shape = NA) +
          guides(alpha = FALSE) +
          geom_jitter(aes_string(y = lengthtraits[[j]])) +
          theme_bw() +
          facet_wrap(.~Metadata_Experiment, scales = "free") +
          theme(legend.position = "top") +
          xlab("Dose (uM)") +
          ggtitle(y)
        ggsave(plot, filename = paste(y,lengthtraits[[j]],"pdf", sep = "."), width = 8,height = 5)
      }
      suppressMessages(setwd(outdir))
    }
    purrr::map2(plot.dat, plot.factors, dose.plot)
  }
  ## Just Mean Worm Length Output
  else {
    dose.plot <- function(x,y){
      suppressWarnings(dir.create(paste(outdir, y, sep = "/")))
      setwd(paste(outdir, y, sep = "/"))
      plot <- ggplot(data = as.data.frame(x),
                     mapping = aes(x = factor(concentration_um),
                                   y = mean_wormlength_um,
                                   alpha = 0.85)) +
        geom_boxplot(outlier.shape = NA) +
        guides(alpha = FALSE) +
        geom_jitter() +
        ylim(c(0,1000)) +
        theme_bw() +
        facet_wrap(.~Metadata_Experiment, scales = "free_x") +
        theme(legend.position = "top") +
        xlab("Dose (uM)") +
        ggtitle(y)
      ggsave(plot, filename = paste(y,"mean.worm.length","pdf", sep = "."),width = 8,height = 5)
      suppressMessages(setwd(outdir))
    }
    purrr::map2(plot.dat, plot.factors, dose.plot)
  }

}
