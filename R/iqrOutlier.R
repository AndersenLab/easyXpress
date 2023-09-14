#' iqrOutlier
#'
#' This function will identify outliers using the interquartile range (IQR).
#'
#' @param x A numerical vector to perform outlier removal on.
#' @param na.rm Logical, if \code{TRUE}, NAs are removed from the claculation.
#' @param thresh Numeric, a constant to multiply the IQR by for detecting outiers.
#' @return A vector with outliers set the NA.
#' @export

iqrOutlier <- function(x, na.rm = TRUE, thresh) {
  qnt <- stats::quantile(x, probs=c(.25, .75), na.rm = na.rm)
  H <- thresh * stats::IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
