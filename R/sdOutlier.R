#' sdOutlier
#'
#' This function will identify outliers using the standard deviation.
#'
#' @param x A numerical vector to perform outlier removal on.
#' @param na.rm Logical, if \code{TRUE}, NAs are removed from the claculation.
#' @param thresh Numeric, a constant to multiply the SD by for detecting outiers.
#' @return A vector with outliers set the NA.
#' @export

sdOutlier <- function(x, na.rm = TRUE, thresh) {
  mean <- mean(x, na.rm = na.rm)
  sd <- sd(x, na.rm = na.rm)
  H <- thresh * sd
  y <- x
  y[x < (mean - H)] <- NA
  y[x > (mean + H)] <- NA
  y
}
