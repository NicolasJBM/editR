% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_list.R
\name{add_list}
\alias{add_list}
\title{Animate bullet points}
\usage{
add_list(bullet_points, numbered = FALSE, animation = "semi-fade-out")
}
\arguments{
\item{bullet_points}{Character vector. One entry for each bullet point to write.}

\item{numbered}{Logical. Whether the bullet points should be numbered.}

\item{animation}{Character. The kind of animation: "semi-fade-out", "grow", "shrink", "zoom-in", "strike", "fade-out", "fade-up", "fade-down", "fade-right", "fade-left", "highlight-red", "highlight-green", "highlight-blue"}
}
\value{
Write the HTML code to animate bullet points in slides.
}
\description{
Function writing the HTML code to animate bullet points in slides.
}
\author{
Nicolas Mangin
}
