% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_infobox.R
\name{make_infobox}
\alias{make_infobox}
\title{Create infoboxes}
\usage{
make_infobox(course_data, selected_file, infotype)
}
\arguments{
\item{course_data}{Reactive. Function containing all the course data loaded with the course.}

\item{selected_file}{Character. Name of the file for which statistics should be displayed.}

\item{infotype}{Character. Whether the infobox should display statistics about "ratings", "views", or "results" (comments are not implemented yet).}
}
\value{
Shinydashboard infoboxes
}
\description{
Make infoboxes for ratings, views, results, or comments.
}
\author{
Nicolas Mangin
}
