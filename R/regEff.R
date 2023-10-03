#' regEff
#'
#' A function to regress out the effect a confounding variable on a dependent variable.
#'
#' @param data A data frame output from any easyXpress function used after the \code{modelSelection} function.
#' @param ... <[rlang::`dyn-dots`]> Variable(s) used to group the data prior to regression. Variable names can be listed in succession. For example, \code{drug}.
#' @param d.var The dependent variable to use in the linear model. Supply the variable without quotes. For example, \code{median_wormlength_um_delta}.
#' @param c.var The confounding variable to use in the linear model. Supply the variable without quotes. For example, \code{bleach}.
#' @return A list including four elements.
#' 1) A data frame, \code{<out>$d}, with three variables added. These include, \code{<d.var>_reg},
#' \code{<c.var>_reg_coeff}, and \code{<c.var>_reg_sig}.
#' 2) A diagnostic plot, \code{<out>$p1}, showing the effect of the confounding variable (\code{c.var}) on the dependent variable (\code{d.var}).
#' 3) A diagnositc plot, \code{<out>$p2}, showing the regression coefficients of the confounding variable (\code{c.var}) on the y-axis and the dependent variable (\code{d.var}) on the x-axis.
#' 4) A list, \code{<out>$models} of model outputs for each group An ANOVA summary table with the effects.
#' @export

regEff <- function(data, ..., d.var, c.var) {
  # look at the arguments
  args.l <- as.list(match.call(expand.dots=FALSE))
  arg.df <- tibble::tibble(arg = names(unlist(args.l)), value = as.character(unname(unlist(args.l))))

  # get vars as needed
  vars <- c(as.character(rlang::enquo(d.var))[2], as.character(rlang::enquo(c.var))[2])

  # get data classes
  d.c <- class(data %>% dplyr::pull({{d.var}}))
  c.c <- class(data %>% dplyr::pull({{c.var}}))

  # report data classes for dependent and confounding variable.
  message(glue::glue('The dependent variable `{vars[1]}` is class {d.c}. Please ensure this is correct.'))
  message(glue::glue('The confounding variable `{vars[2]}` is class {c.c}. Please ensure this is correct.'))


  # regression groups
  group_by <- paste(names(data %>% dplyr::select(...)), collapse = ", ")
  message(glue::glue("The data are grouped by: `{group_by}`"))

  # filter if nas present in c and d
  f <- data %>%
    dplyr::filter(!is.na({{d.var}}) & !is.na({{c.var}}))

  # report on filters if persent
  if(nrow(f) != nrow(data)) {
    message(glue::glue("There were {nrow(data %>% dplyr::filter(is.na({{d.var}})))} rows filtered with NAs for {vars[1]}"))
    message(glue::glue("There were {nrow(data %>% dplyr::filter(is.na({{c.var}})))} rows filtered with NAs for {vars[2]}"))
  }

  # apply the linear model to nested data
  m <- f %>%
    dplyr::group_by(...) %>%
    tidyr::nest() %>%
    #dplyr::mutate(model = purrr::map(data, .f = ~ stats::lm(data = ., formula = .x[[vars[1]]] ~ .x[[vars[2]]] - 1))) %>%
    dplyr::mutate(model = purrr::map(data, .f = ~ rlang::inject(stats::lm(data = ., formula = !!rlang::enexpr(d.var) ~ !!rlang::enexpr(c.var) - 1)))) %>%
    dplyr::ungroup()

  # get the model summaries  and residuals to add to the data
  m.sum <- NULL
  #m.resid <- NULL
  for(i in 1:nrow(m)) {
    # get the model outputs
    coeffs <- tibble::as_tibble(data.table::data.table(stats::coef(summary(m[[i,length(m)]][[1]])), keep.rownames = 'term')) %>%
      dplyr::mutate(term = stringr::str_replace(term, pattern = glue::glue("{vars[2]}"),replacement = "")) %>%
      dplyr::rename("{{c.var}}" := term,
                    "{{c.var}}_coeff" := Estimate,
                    "{{c.var}}_se" := `Std. Error`,
                    "{{c.var}}_tvalue" := `t value`,
                    "{{c.var}}_pvalue" := `Pr(>|t|)`) %>%
      dplyr::select({{c.var}}, dplyr::everything())

    # get the grouping vars
    group.vars.c <- m %>%
      dplyr::select(-data, -model) %>%
      dplyr::slice(rep(i, each = nrow(coeffs)))

    # add these
    coeffs.out <- cbind(group.vars.c, coeffs)

    # add to the list
    m.sum[[i]] <- coeffs.out
  }
  # bind these data frames by row
  coeffs.d <- dplyr::bind_rows(m.sum)

  # join model summary and recalculate the residuals
  suppressMessages(d <- f %>%
    dplyr::group_by(...) %>%
    # recalculate residuals, faster than pulling out in loop
    dplyr::mutate("{{d.var}}_reg" := rlang::inject(stats::residuals(stats::lm(!!rlang::enexpr(d.var) ~ !!rlang::enexpr(c.var) - 1)))) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(., coeffs.d))

  # plot the effect in context
  p1 <- rlang::inject(easyXpress::checkEff(data = !!rlang::enexpr(f), ..., x = !!rlang::enexpr(c.var), y = !!rlang::enexpr(d.var))) +
    ggplot2::labs(subtitle = glue::glue("{vars[2]} effect on {vars[1]}")) +
    ggplot2::theme(strip.background = ggplot2::element_rect(
      color="black", fill="white", size=0.5, linetype="solid"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1, size = 5))

  # plot the coeffs and their standard error from the models
  # setup plot vars by position, ugh
  coeffs.p <- coeffs.d
  coeffs.p$min <- coeffs.p[[length(coeffs.p)-3]] - coeffs.p[[length(coeffs.p)-2]]
  coeffs.p$max <- coeffs.p[[length(coeffs.p)-4]] + coeffs.p[[length(coeffs.p)-3]]
  coeffs.p$y <- coeffs.p[[length(coeffs.p)-5]]

  # plot regression coefficients
  p2 <- ggplot(coeffs.p) +
    ggplot2::aes(x = {{c.var}},
                  y = y,
                  ymin = min,
                  ymax = max) +
    ggplot2::geom_pointrange(size = 0.5, shape = 21, stroke = 0.25) +
    ggplot2::facet_wrap(ggplot2::vars(...)) +
    ggplot2::labs(title = "Regression coefficients +/- s.e.",
                  subtitle = glue::glue("lm({vars[1]} ~ {vars[2]} - 1)"),
                  y = glue::glue("{vars[2]} coefficient")) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(
      color="black", fill="white", size=0.5, linetype="solid"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1, size = 5))

  #return
  return(list(d = d, p1 = p1, p2 = p2, model = m))
}
