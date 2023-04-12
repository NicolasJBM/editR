#' @name view_document
#' @title Display a document for view only.
#' @author Nicolas Mangin
#' @description Function creating the user interface to see formatted documents.
#' @param selected Tibble. Selected document.
#' @param original Logical. Whether the document is the original (TRUE) or a translation (FALSE).
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @param test_parameters Tibble. Test parameters if the document to display is a specific version of a question selected for a test.
#' @return User interface showing the formatted document.
#' @importFrom knitr knit2html
#' @importFrom rmarkdown render
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny HTML
#' @importFrom shiny withMathJax
#' @importFrom shinydashboardPlus box
#' @importFrom fs dir_delete
#' @importFrom quarto quarto_render
#' @export

view_document <- function(selected, original, course_data, course_paths, test_parameters = NA){
  
  doctype <- selected$type[1]
  filepath <- selected$filepath
  
  if (doctype %in% c("Note","Page","Slide","Video","Game","Tutorial","Case")){
    
    shinybusy::show_modal_spinner(
      spin = "orbit",
      text = "Preparing the document..."
    )
    
    qmdpath <- base::paste0(course_paths()$subfolders$edit, "/index.qmd")
    htmlpath1 <- base::paste0(course_paths()$subfolders$edit, "/index.html")
    htmlpath2 <- stringr::str_remove(
      htmlpath1,
      base::paste0(course_paths()$subfolders$course, "/")
    )
    if (base::file.exists(htmlpath1)) base::file.remove(htmlpath1)
    doc <- base::readLines(filepath)
    
    if (!original){
      translated_title <- doc[stringr::str_detect(doc, "^exextra\\[title\\]:")]
      translated_title <- stringr::str_remove(translated_title, "^exextra\\[title\\]:")
      selected$title <- translated_title
    }
    
    doc <- doc[1:(base::match('Meta-information', doc)-1)]
    
    yaml <- editR::make_yaml(selected, doctype)
    doc <- c(yaml, doc)
    
    base::writeLines(doc, qmdpath, useBytes = TRUE)
    
    base::try(quarto::quarto_render(qmdpath, quiet = TRUE), silent = TRUE)
    
    title <- selected |>
      editR::make_title_display(course_data)
    
    if (!base::file.exists(htmlpath1)){
      ui <- shinydashboardPlus::box(
        width = 12, title = title, solidHeader = TRUE, status = "primary",
        collapsible = FALSE, collapsed = FALSE, height = "500px",
        shiny::h2("The document could not be rendered. Try to correct it and then refresh.")
      )
    } else if (doctype == "Slide"){
      ui <- shinydashboardPlus::box(
        width = 12, title = title, solidHeader = TRUE, status = "primary",
        collapsible = FALSE, collapsed = FALSE, height = "500px",
        shiny::tags$iframe(src=htmlpath2, height = 520, width = "100%")
      )
    } else {
      ui <- shinydashboardPlus::box(
        width = 12, title = title, solidHeader = TRUE, status = "primary",
        collapsible = FALSE, collapsed = FALSE, height = "700px",
        shiny::tags$iframe(src=htmlpath2, height = 750, width="100%")
      )
    }
    
    shinybusy::remove_modal_spinner()
    
  } else {
    
    base::load(course_paths()$databases$tags)
    title <- selected |>
      editR::make_title_display(course_data)
    
    base::load(course_paths()$databases$propositions)
    base::load(course_paths()$databases$translations)
    test_parameters <- test_parameters
    docformat <- "html"
    record_solution <- FALSE
    
    ui <- shinydashboardPlus::box(
      width = 12, title = title, solidHeader = TRUE,
      status = "primary", collapsible = FALSE, collapsed = FALSE,
      height = "750px",
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
    )
    
  }
  
  return(ui)
}
