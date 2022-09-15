#' @name translate_server
#' @title Translate documents
#' @author Nicolas Mangin
#' @description Module facilitating the translation of documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param filtered Reactive. List of pre-selected documents.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Create and save documents' translations in a dedicated folder.
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom shiny moduleServer
#' @importFrom shiny NS
#' @importFrom shiny reactive
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr replace_na
#' @importFrom tidyr unnest
#' @export


translate_server <- function(id, filtered, course_data, course_paths){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    code <- NULL
    langiso <- NULL
    language <- NULL
    status <- NULL
    title <- NULL
    translation <- NULL
    type <- NULL
    
    languages <- shiny::reactive({
      course_data()$languages
    })
    
    documents <- shiny::reactive({
      filtered() |>
        dplyr::select(code, main = language, translation = translations, title) |>
        dplyr::mutate(translation = purrr::map(
          translation, function(x){
            y <- stringr::str_split(x, pattern = " ", simplify = TRUE)
            tibble::tibble(translation = base::as.character(y))
          }
        )) |>
        tidyr::unnest(translation) |>
        tidyr::pivot_longer(
          cols = c("main", "translation"),
          names_to = "type", values_to = "langiso"
        ) |>
        dplyr::filter(langiso %in% languages()$langiso) |>
        dplyr::mutate(status = "Existing")
    })
    
    
    output$slctdocument <- shiny::renderUI({
      codes <- documents()$code
      base::names(codes) <- base::paste(
        documents()$code, " - ", documents()$title
      )
      shiny::selectInput(
        ns("slctcode"), "Document:", choices = codes, width = "100%"
      )
    })
    
    
    language_status <- shiny::reactive({
      dplyr::select(languages(), langiso, language) |>
        dplyr::mutate(code = input$slctcode) |>
        dplyr::left_join(
          dplyr::select(documents(), code, langiso, type, status),
          by = c("langiso", "code")
        ) |>
        tidyr::replace_na(base::list(type = "translation", status = "Missing"))
    })
    
    output$slctlanguage <- shiny::renderUI({
      shiny::req(!base::is.null(input$slctcode))
      shiny::req(!base::is.null(input$translationstatus))
      translations <- language_status() |>
        dplyr::filter(
          type == "translation",
          status %in% input$translationstatus
        )
      languages <- translations$langiso
      base::names(languages) <- translations$language
      shiny::selectInput(
        ns("slctlang"), "Language:", choices = languages, width = "100%"
      )
    })
    
    output$newtranslation <- shiny::renderUI({
      shiny::req(!base::is.null(input$slctlang))
      shiny::req(!base::is.null(language_status()))
      missing <- language_status() |>
        dplyr::filter(status == "Missing")
      shiny::req(input$slctlang %in% missing$langiso)
      shiny::actionButton(
        ns("createnewtranslation"),"New",
        icon = shiny::icon("wand-magic-sparkles"),
        style = "background-color:#003366; color:#FFF; width:100%; margin-top:25px;"
      )
    })
    
    
    
    
    output$vieworiginal <- shiny::renderUI({
      
      shiny::req(input$slctcode)
      shiny::req(!base::is.null(filtered()))
      shiny::req(input$slctcode %in% filtered()$code)
      
      selected <- filtered() |>
        dplyr::filter(code == input$slctcode)
      
      editR::view_document(selected, course_data, course_paths)
    })
      
    output$viewtranslation <- shiny::renderUI({
      
    })
      
    output$edittranslation <- shiny::renderUI({
      
    })
    
    
    output$translatepropositions <- rhandsontable::renderRHandsontable({
      
    })
    
  })
}

