#' @name compare_server
#' @title Compare documents
#' @author Nicolas Mangin
#' @description Module facilitating the comparison and edition of documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @param courses Reactive. Function containing a list of paths to other courses.
#' @return Saved edited documents.
#' @importFrom diffr diffr
#' @importFrom diffr renderDiffr
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom purrr map2_lgl
#' @importFrom shiny isolate
#' @importFrom shiny moduleServer
#' @importFrom shiny NS
#' @importFrom shiny observe
#' @importFrom shiny reactive
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shiny updateSelectInput
#' @importFrom tibble tibble
#' @export


compare_server <- function(id, course_paths, courses){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    comparison <- NULL
    comparison_file <- NULL
    comparison_time <- NULL
    differ <- NULL
    files <- NULL
    original <- NULL
    original_older <- NULL
    
    originalfiles <- shiny::reactive({
      shiny::req(!base::is.na(course_paths()$subfolders$original))
      base::list.files(course_paths()$subfolders$original)
    })
    
    shiny::observe({
      shiny::req(base::length(courses()) > 1)
      all_courses <- c("", courses()$path)
      base::names(all_courses) <- c("Select a course", courses()$course)
      shiny::updateSelectInput(
        session, "slctcompfold", choices = all_courses, selected = ""
      )
    })
    
    comparisonfolder <- shiny::reactive({
      shiny::req(!base::is.null(input$slctcompfold))
      base::paste0(
        input$slctcompfold, "/basis/1_documents/1_original"
      )
    })
    
    comparisonfiles <- shiny::reactive({
      shiny::req(!base::is.null(comparisonfolder()))
      shiny::req(base::dir.exists(comparisonfolder()))
      base::list.files(comparisonfolder(), full.names = FALSE)
    })
    
    commonfiles <- shiny::reactive({
      shiny::req(!base::is.null(originalfiles()))
      shiny::req(!base::is.null(comparisonfiles()))
      base::intersect(originalfiles(), comparisonfiles())
    })
    
    selection <- shiny::reactive({
      shiny::req(!base::is.null(commonfiles()))
      file_selection <- tibble::tibble(
        files = commonfiles()
      ) |>
        dplyr::mutate(
          original_file = base::paste0(course_paths()$subfolders$original, "/", files),
          comparison_file = base::paste0(comparisonfolder(), "/", files)
        ) |>
        dplyr::mutate(
          differ = purrr::map2_lgl(
            original_file, comparison_file,
            function(x,y) !base::identical(base::readLines(x), base::readLines(y))
          )
        )|>
        dplyr::filter(differ == TRUE)
      if (input$keeprecent){
        file_selection <- file_selection |>
          dplyr::mutate(
            original_older = purrr::map2_lgl(
              original_file, comparison_file,
              editR::is_original_older
            )
          ) |>
          dplyr::filter(original_older == TRUE)
      } else {
        file_selection <- file_selection
      }
      base::as.character(file_selection$files)
    })
    
    selected_file <- editR::selection_server("selectfile", selection)
    
    output$comparison <- diffr::renderDiffr({
      shiny::req(!base::is.null(selected_file()))
      original_file = base::paste0(course_paths()$subfolders$original, "/", selected_file())
      comparison_file = base::paste0(comparisonfolder(), "/", selected_file())
      diffr::diffr(
        original_file,
        comparison_file,
        contextSize = input$slctcontextSize,
        minJumpSize = input$slctminJumpSize,
        wordWrap = input$slctwordwrap
      )
    })
    
  })
}

