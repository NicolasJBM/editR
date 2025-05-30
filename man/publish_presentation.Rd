% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/publish_presentation.R
\name{publish_presentation}
\alias{publish_presentation}
\title{Export a slide presentation}
\usage{
publish_presentation(intake, selected_document, course_paths)
}
\arguments{
\item{intake}{List. Selected intake to retrieve the position of the presentation.}

\item{selected_document}{Character. Name of the note destined to be published.}

\item{course_paths}{List. List of paths to either databases or documents.}
}
\value{
Write presentation in the appropriate subfolder of presentations.
}
\description{
Function copying a presentation in the appropriate tree subfolder in the presentation folder
}
\author{
Nicolas Mangin
}
