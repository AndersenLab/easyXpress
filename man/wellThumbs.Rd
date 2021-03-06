% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wellThumbs.R
\name{wellThumbs}
\alias{wellThumbs}
\title{wellThumbs}
\usage{
wellThumbs(project_dir, plates = "all", max_dim = 512)
}
\arguments{
\item{project_dir}{The full path to the project directory containing the
raw_images subdirectory.}

\item{plates}{a vector with plate numbers to make thumbs for. This vector is
used to match plate patterns in the filelist.\cr \code{"all"} will make
thumbs for wells in all plates.\cr \code{13:72} will make thumbs for wells
in plates p13 - p72.\cr \code{c(1, 3, 106)} will make thumbs for wells in
plates p01, p03, and p106.}

\item{max_dim}{The maximum dimension of the resized images in pixels. The
default value is 512, which scales a 2048 pixel image to 6.25 percent of its
original resolution.}
}
\value{
A folder named raw_image_thumbs under the directory specified by
  \code{project_dir}.
}
\description{
Make .png thumbnails from .TIF files in the raw_images directory. Making
thumbs from .TIFs requires \href{https://imagemagick.org/}{ImageMagick} to be
installed on your system. To install imagemagick with Homebrew, enter:\cr
\code{brew install imagemagick}\cr into the terminal.
}
