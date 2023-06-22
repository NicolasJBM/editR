#' @name display_question
#' @title Display a question in the interface.
#' @author Nicolas Mangin
#' @description Function creating the user interface to see formatted documents.
#' @param selected Tibble. Selected document.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @param test_parameters Tibble. Test parameters if the document to display is a specific version of a question selected for a test.
#' @return User interface showing the question within the interface, not as a modal.
#' @importFrom knitr knit2html
#' @export

display_question <- function(selected, course_paths, test_parameters = NA){
  doctype <- selected$type[1]
  filepath <- selected$filepath
  base::load(course_paths()$databases$propositions)
  base::load(course_paths()$databases$translations)
  test_parameters <- test_parameters
  docformat <- "html"
  record_solution <- FALSE
  if (!base::file.exists(filepath)){
    shiny::h2("The document could not be rendered. Try to correct it and then refresh.")
  } else {
    shiny::fluidRow(
      shiny::column(
        12,
        base::suppressWarnings(
          shiny::withMathJax(shiny::HTML(knitr::knit2html(
            text = base::readLines(filepath), quiet = TRUE, template = FALSE
          )))
        )
      )
    )
  }
}
