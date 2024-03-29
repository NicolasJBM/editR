% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/publish_note.R
\name{publish_note}
\alias{publish_note}
\title{Format a note as a blog post}
\usage{
publish_note(selected_document, course_paths, translation = FALSE)
}
\arguments{
\item{selected_document}{Character. Name of the note destined to be published.}

\item{course_paths}{List. List of paths to either databases or documents.}

\item{translation}{Logical. Whether the source document is in the folder "translated" rather than "original".}
}
\value{
Write post in the folder "4_materials/blog".
}
\description{
Function reformating a note as a blog post with adequate tags.
}
\author{
Nicolas Mangin
}
