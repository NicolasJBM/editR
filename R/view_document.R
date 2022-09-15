#' @name view_document
#' @title Display a document for view only.
#' @author Nicolas Mangin
#' @description Function creating the user interface to see formatted documents.
#' @param selected Tibble. Selected document.
#' @param original Logical. Whether the document is the original (TRUE) or a translation (FALSE).
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return User interface showing the formatted document.
#' @importFrom knitr knit2html
#' @importFrom rmarkdown render
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny HTML
#' @importFrom shiny withMathJax
#' @importFrom shinydashboardPlus box
#' @export

view_document <- function(selected, original, course_data, course_paths){
  
  doctype <- selected$type[1]
  filepath <- selected$filepath
  
  if (doctype %in% c("Note","Page","Slide","Video","Game","Case")){
    
    doc <- base::readLines(filepath)
    
    if (!original){
      translated_title <- doc[stringr::str_detect(doc, "^exextra\\[title\\]:")]
      translated_title <- stringr::str_remove(translated_title, "^exextra\\[title\\]:")
      selected$title <- translated_title
    }
    
    doc <- doc[1:(base::match('Meta-information', doc)-1)]
    
    yaml <- editR::make_yaml(selected, doctype)
    doc <- c(yaml, doc)
    
    rmdpath <- base::paste0(
      course_paths()$subfolders$course,
      "/temporary/tmpdoc.Rmd"
    )
    base::writeLines(doc, rmdpath, useBytes = TRUE)
    rmarkdown::render(rmdpath, encoding="UTF-8", quiet = TRUE) |>
      base::suppressWarnings()
    title <- selected |>
      editR::make_title_display(course_data)
    
    if (doctype == "Slide"){
      ui <- shinydashboardPlus::box(
        width = 12, title = title, solidHeader = TRUE, status = "primary",
        collapsible = FALSE, collapsed = FALSE, height = "550px",
        shiny::tags$iframe(src="temporary/tmpdoc.html", height = 520, width = "100%")
      )
    } else {
      ui <- shinydashboardPlus::box(
        width = 12, title = title, solidHeader = TRUE, status = "primary",
        collapsible = FALSE, collapsed = FALSE, height = "750px",
        shiny::tags$iframe(src="temporary/tmpdoc.html", height = 750, width="100%")
      )
    }
    
  } else {
    
    base::load(course_paths()$databases$tags)
    title <- selected |>
      editR::make_title_display(course_data)
    
    base::load(course_paths()$databases$propositions)
    test_parameters <- NA
    as_latex <- FALSE
    record_version <- FALSE
    
    ui <- shinydashboardPlus::box(
      width = 12, title = title, solidHeader = TRUE,
      status = "primary", collapsible = FALSE, collapsed = FALSE,
      height = "750px",
      shiny::fluidRow(
        shiny::column(
          12,
          base::suppressWarnings(
            shiny::withMathJax(shiny::HTML(knitr::knit2html(
              text = base::readLines(filepath),
              fragment.only = TRUE, quiet = TRUE
            )))
          )
        )
      )
    )
    
  }
  
  return(ui)
}
