% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/delta.R
\name{delta}
\alias{delta}
\title{delta}
\usage{
delta(
  data,
  ...,
  WF = "filter",
  vars = c("median_wormlength_um", "cv_wormlength_um"),
  doseR = F
)
}
\arguments{
\item{data}{A data frame output from any \code{WF} function.}

\item{...}{<[`dynamic-dots`][rlang::dyn-dots]> Variable(s) used to group data. It is recommended to group data to independent bleaches for all strains. Variable names in data are supplied separated by commas and without quotes.
For example, the typical variables for grouping are \code{Metadata_Experiment, bleach, strain}.}

\item{WF}{Select \code{"filter"} or \code{"ignore"}. The default, \code{"filter"}, will filter out all flagged wells before calculating the delta from control, if present.
\code{"ignore"} will calculate the delta including all flagged data. Be careful using \code{"ignore"}, it is only included for diagnostic purposes.}

\item{vars}{The well summary statistics to perform the delta calculation on. These are supplied in a character vector. For example, the default is set to \code{c("median_wormlength_um", "cv_wormlength_um")}.}

\item{doseR}{Logical, is this dose response data? The default, \code{doseR = FALSE},
expects control data to be recorded in the design file a particular way. Specifically, the drug and diluent variables should be identical for controls,
e.g, \code{drug = DMSO, diluent = DMSO, concentration_um = 0}. If \code{doseR = TRUE}, the controls are expected to be coded differently,
.e.g, \code{drug = ABZ, diluent = DMSO, concentration_um = 0}. Warning messages are produced if the controls do not fit expectations, but try to ensure the
controls are coded properly without relying this function to catch all edge cases.}
}
\value{
A data frame identical to the input but with control delta variables added, i.e., \code{median_wormlength_um_delta} and \code{cv_wormlength_um_delta}.
The median values for the control conditions are also added as \code{control_median_wormlength_um} and \code{control_cv_wormlength_um}.
}
\description{
This function will calculate the difference in well summary statistics between the experimental condition and the median control condition within a group.
The proper grouping variables are supplied with \code{...}.
}
