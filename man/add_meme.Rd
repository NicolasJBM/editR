% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_meme.R
\name{add_meme}
\alias{add_meme}
\title{Insert a meme slide}
\usage{
add_meme(
  text_above = "",
  image_url = "",
  text_below = "",
  font_size = 2.5,
  bkg_fit = "contain",
  trans_back = "slide",
  trans_data = "fade"
)
}
\arguments{
\item{text_above}{Character. Sentence written above the image_url or video.}

\item{image_url}{Character. Address of the background image_url.}

\item{text_below}{Character. Sentence written below the image_url or video.}

\item{font_size}{Numeric. Size of the font in "em".}

\item{bkg_fit}{Character. "cover" or "contain".}

\item{trans_back}{Character. Type of transition for the background: fade, slide, convex, concave, or zoom.}

\item{trans_data}{Character. Type of transition for data: fade, slide, convex, concave, or zoom.}
}
\value{
Character. Write the first two rows of a RevealJS slide formatted for rmarkdown.
}
\description{
Function writing a meme as a revealjs slide.
}
\author{
Nicolas Mangin
}
