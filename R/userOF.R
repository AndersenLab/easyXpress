#' userOF
#'
#' This function will flag objects based on a user provided variable in the input data.
#'
#' @param data A data frame output from the \code{modelSelection} or any \code{OF} functions with one or more user defined variables that are to be used as flags.
#' Note, the varible(s) can contain multiple values or a single value if desired. The only reason to include multiple variables with a single value each is that this can preserve the order in which the flags were assigned.
#' In some cases this may be helpful. If a user is not concerned about preserving the order in which the flags are assigned then specifying a single variable with multiple values
#' will be sufficient to preserve the flag labels.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Specify one or more variables a user would like to use for flagging objects. For example, <length> or <length, area>.
#' These variables will be converted into an easyXpress compatible flags in the output data.
#' This means it they are converted into class \code{chr} and have the \code{_ObjectFlag} suffix appended to their name if necessary.
#' This way the user can supply whatever flags they like to the data and use them for downstream processing with the easyXpress framework.
#' The order of the flags are infered from the position of the \code{vars} in the input data.
#' @return A single data frame identical to the input data but with the \code{var} variable renamed to \code{<var>_ObjectFlag} and set to
#' class \code{chr} if necessary.
#' @export

userOF <- function(data, ...) {
  # rename
  d <- data %>%
    dplyr::mutate_at(dplyr::vars(...), function(x)
      as.character(x)) %>%
    dplyr::rename_at(dplyr::vars(...), function(x)
      paste0(x, "_ObjectFlag"))

  # message
  args.l <- as.list(match.call(expand.dots = FALSE))
  dots <-
    tibble::tibble(arg = names(unlist(args.l)), value = as.character(unname(unlist(args.l)))) %>%
    dplyr::filter(!(arg %in% c("", "data"))) %>%
    dplyr::pull(value)
  for (i in unique(dots)) {
    message(glue::glue(
      "Converting {i} into an easyXpress compatible object flag (OF)."
    ))
  }

  # fix any redundant suffixes if necessary
  if (T %in% grepl(x = names(d),
                   pattern = "objectflag_objectflag",
                   ignore.case = TRUE)) {
    # get the doubled names
    rn.names <-
      names(d)[grepl(x =  names(d),
                     pattern = "objectflag_objectflag",
                     ignore.case = TRUE)]
    # message
    for (i in unique(rn.names)) {
      message(glue::glue("WARNING: Fixing redundant suffix for {i}"))
    }
    # fix them
    d <- d %>%
      dplyr::rename_at(dplyr::vars(!!rn.names), function(x)
        gsub(
          x,
          pattern = "objectflag_objectflag",
          replacement = "ObjectFlag",
          ignore.case = T
        ))
  }

  # return
  return(d)
}
