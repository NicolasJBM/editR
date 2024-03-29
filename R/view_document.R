#' @name view_document
#' @title Preview a document.
#' @author Nicolas Mangin
#' @description Function creating the user interface to see formatted documents.
#' @param selected Tibble. Selected document.
#' @param original Logical. Whether the document is the original (TRUE) or a translation (FALSE).
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return User interface showing the formatted document.
#' @importFrom quarto quarto_render
#' @importFrom shiny h2
#' @importFrom shiny req
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @importFrom shinydashboardPlus box
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove
#' @export

view_document <- function(selected, original, course_paths){
  
  title <- NULL
  
  doctype <- selected$type[1]
  filepath <- selected$filepath
    
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
  
  if (!base::file.exists(htmlpath1)){
    ui <- shinydashboardPlus::box(
      width = 12, title = title, solidHeader = TRUE, status = "primary",
      collapsible = FALSE, collapsed = FALSE, height = "500px",
      shiny::h2("The document could not be rendered. Try to correct it and then refresh.")
    )
  } else {
    shiny::req(base::file.exists(htmlpath2))
    utils::browseURL(htmlpath2)
  }
  
  shinybusy::remove_modal_spinner()
}
