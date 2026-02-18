#' @name translate_server
#' @title Translate documents
#' @author Nicolas Mangin
#' @description Module facilitating the translation of documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param filtered Reactive. List of pre-selected documents.
#' @param tree Character. Name of the tree.
#' @param tbltree Reactive. Function containing a list of documents as a classification tree compatible with jsTreeR.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Create and save documents' translations in the dedicated basis subfolder.
#' @importFrom dplyr anti_join
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr select
#' @importFrom knitr knit2html
#' @importFrom purrr map
#' @importFrom purrr map_lgl
#' @importFrom rhandsontable hot_col
#' @importFrom rhandsontable hot_cols
#' @importFrom rhandsontable hot_context_menu
#' @importFrom rhandsontable hot_to_r
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom rstudioapi navigateToFile
#' @importFrom shiny HTML
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny isolate
#' @importFrom shiny modalDialog
#' @importFrom shiny moduleServer
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny showModal
#' @importFrom shiny tagList
#' @importFrom shiny withMathJax
#' @importFrom shinyAce aceEditor
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shinyalert shinyalert
#' @importFrom shinydashboardPlus box
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr replace_na
#' @importFrom tidyr unite
#' @importFrom tidyr unnest
#' @importFrom shinybusy show_modal_spinner
#' @export


translate_server <- function(id, filtered, tree, tbltree, course_data, course_paths){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    code <- NULL
    langiso <- NULL
    language <- NULL
    status <- NULL
    title <- NULL
    translation <- NULL
    type <- NULL
    document <- NULL
    explanation <- NULL
    translated_explanation <- NULL
    item <- NULL
    proposition <- NULL
    translated_proposition <- NULL
    propositions <- NULL
    keep <- NULL
    success <- NULL
    discrimination <- NULL
    answers <- NULL
    flag <- NULL
    
    
    languages <- shiny::reactive({
      course_data()$languages
    })
    
    documents <- shiny::reactive({
      filtered() |>
        dplyr::select(code, original = language, translation = translations, title) |>
        dplyr::mutate(translation = purrr::map(
          translation, function(x){
            y <- stringr::str_split(x, pattern = " ", simplify = TRUE)
            tibble::tibble(translation = base::as.character(y))
          }
        )) |>
        tidyr::unnest(translation) |>
        tidyr::pivot_longer(
          cols = c("original", "translation"),
          names_to = "type", values_to = "langiso"
        ) |>
        dplyr::filter(langiso %in% languages()$langiso) |>
        dplyr::mutate(status = "Existing")
    })
    
    
    document_list <- shiny::reactive({
      basis <- documents() |>
        dplyr::select(code, title) |>
        base::unique() |>
        tidyr::unite("title", code, title, remove = FALSE)
      codes <- basis$code
      base::names(codes) <- basis$title
      codes
    })
    selected_code <- editR::selection_server("selectdoc", document_list)
    
    
    language_status <- shiny::reactive({
      dplyr::select(languages(), langiso, language, flag) |>
        dplyr::mutate(code = selected_code()) |>
        dplyr::left_join(
          dplyr::select(documents(), code, langiso, type, status),
          by = c("langiso", "code")
        ) |>
        tidyr::replace_na(base::list(type = "translation", status = "Missing"))
    })
    
    output$slctlanguage <- shiny::renderUI({
      shiny::req(!base::is.null(selected_code()))
      translations <- language_status() |>
        dplyr::filter(type == "translation")
      shinyWidgets::radioGroupButtons(
        inputId = ns("slctlang"), label = NULL, 
        choiceNames = base::lapply(
          base::seq_along(translations$langiso), 
          function(i) shiny::tagList(
            shiny::tags$img(src = translations$flag[i], width = 20, height = 15),
            translations$language[i]
          )
        ),
        choiceValues = translations$langiso,
        status = "primary", justified = FALSE, size = "sm",
        checkIcon = base::list(yes = shiny::icon("check"))
      )
    })
    
    document_to_translate <- shiny::reactive({
      shiny::req(selected_code())
      shiny::req(!base::is.null(filtered()))
      shiny::req(selected_code() %in% filtered()$code)
      if (!base::is.null(input$originalrefresh)) input$originalrefresh
      document_to_translate <- filtered() |>
        dplyr::filter(code == selected_code())
      document_to_translate$filepath <- base::paste0(
        course_paths()$subfolders$original, "/", document_to_translate$file
      )
      document_to_translate
    })
    
    translated_document <- shiny::reactive({
      shiny::req(input$slctlang)
      shiny::req(!base::is.null(language_status()))
      shiny::req(!base::is.null(document_to_translate()))
      if (!base::is.null(input$translationrefresh)) input$translationrefresh
      selected_language_status <- language_status() |>
        dplyr::filter(langiso == input$slctlang)
      shiny::req(selected_language_status$status == "Existing")
      original_language <- language_status() |>
        dplyr::filter(type == "original")
      translated_document <- document_to_translate()
      translated_document$file <- stringr::str_replace_all(
        translated_document$file,
        original_language$langiso[1],
        selected_language_status$langiso[1]
      )
      translated_document$filepath <- base::paste0(
        course_paths()$subfolders$translated, "/", translated_document$file
      )
      translated_document
    })
    
    
    
    # Create translation #######################################################
    
    shiny::observeEvent(input$createnewtranslation, {
      shiny::req(!base::is.null(document_to_translate()))
      shiny::req(input$slctlang)
      
      selected_language_status <- language_status() |>
        dplyr::filter(langiso == input$slctlang)
      
      if (selected_language_status$status == "Missing"){
        
        shinybusy::show_modal_spinner(
          spin = "orbit",
          text = "Translating the document..."
        )
        
        original_language <- language_status() |>
          dplyr::filter(type == "original")
        translated_document <- document_to_translate()
        translated_document$file <- stringr::str_replace_all(
          translated_document$file,
          original_language$langiso[1],
          selected_language_status$langiso[1]
        )
        translated_document$filepath <- base::paste0(
          course_paths()$subfolders$translated, "/", translated_document$file
        )
        
        if (input$maketranslation){
          translation <- document_to_translate()$filepath |>
            editR::translate_document(langiso = base::tolower(input$slctlang))
        } else {
          translation <- base::list(translated = base::readLines(document_to_translate()$filepath))
        }
        
        base::writeLines(
          base::unlist(translation$translated),
          translated_document$filepath
        )
        
        base::load(course_paths()$databases$propositions)
        base::load(course_paths()$databases$translations)
        langiso <- input$slctlang
        
        translations <- editR::translate_propositions(
          propositions,
          translations,
          langiso
        )
        
        base::save(translations, file = course_paths()$databases$translations)
        
        shinybusy::remove_modal_spinner()
        
        shinyalert::shinyalert(
          "Translation created",
          "Update documents and reload the course to edit it.",
          "success", TRUE, TRUE
        )
      } else {
        shinyalert::shinyalert(
          "Existing translation!",
          "You cannot create a translation which already exists.",
          "warning", TRUE, TRUE
        )
      }
    })
    
    
    
    # Original ##############################################################
    
    output$editoriginal <- shiny::renderUI({
      shiny::req(!base::is.null(document_to_translate()))
      shiny::req(base::file.exists(document_to_translate()$filepath))
      lines <- base::readLines(base::paste0(document_to_translate()$filepath))
      shinydashboardPlus::box(
        width = 12, title = "Original", solidHeader = TRUE,
        status = "navy", collapsible = FALSE, collapsed = FALSE,
        height = "750px",
        shiny::fluidRow(
          shiny::column(
            3,
            shiny::actionButton(
              ns("originalinrstudio"), "RStudio",
              icon = shiny::icon("r-project"),
              style = "background-color:#003366;color:#FFF;
                width:100%;margin-bottom:10px;"
            )
          ),
          shiny::column(
            3,
            shiny::actionButton(
              ns("originalrefresh"), "Refresh",
              icon = shiny::icon("rotate"),
              style = "background-color:#006699;color:#FFF;
                width:100%;margin-bottom:10px;"
            )
          ),
          shiny::column(
            3,
            shiny::actionButton(
              ns("saveoriginal"), "Save",
              icon = shiny::icon("floppy-disk"),
              style = "background-color:#006633;color:#FFF;
                width:100%;margin-bottom:10px;"
            )
          ),
          shiny::column(
            3,
            shiny::actionButton(
              ns("originalpreview"), "Preview", icon = shiny::icon("eye"),
              style = "background-color:#660033;color:#FFF;
                width:100%;margin-bottom:10px;"
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(12, shinyAce::aceEditor(
            outputId = ns("editedoriginal"), value = lines, mode = "markdown",
            wordWrap = TRUE, debounce = 10, autoComplete = "live",
            height = "500"
          ))
        )
      )
    })
    
    shiny::observeEvent(input$originalinrstudio, {
      document_to_translate <- shiny::isolate({ document_to_translate() })
      shiny::req(!base::is.null(document_to_translate))
      rstudioapi::navigateToFile(document_to_translate$filepath)
    })
    
    shiny::observeEvent(input$saveoriginal, {
      document_to_translate <- shiny::isolate({ document_to_translate() })
      editedoriginal <- shiny::isolate({ input$editedoriginal })
      shiny::req(!base::is.null(editedoriginal))
      base::writeLines(editedoriginal, document_to_translate$filepath, useBytes = TRUE)
      shinyalert::shinyalert(
        "Original saved", "Click on the refresh button to display changes.",
        type = "success"
      )
    })
    
    shiny::observeEvent(input$originalpreview, {
      if (document_to_translate()$type %in% c("Free","Statements","Alternatives","Computation","Essay","Problem")){
        base::load(course_paths()$databases$propositions)
        base::load(course_paths()$databases$translations)
        test_parameters <- NA
        docformat <- "html"
        record_solution <- FALSE
        shiny::showModal(shiny::modalDialog(
          title = "Question preview",
          shiny::renderUI({
            base::suppressWarnings(
              shiny::withMathJax(shiny::HTML(knitr::knit2html(
                text = base::readLines(document_to_translate()$filepath),
                quiet = TRUE, template = FALSE
              )))
            )
          }),
          easyClose = TRUE
        ))
      } else {
        editR::view_document(document_to_translate(),TRUE,course_paths)
      }
    })
    
    
    
    # Translation ##############################################################
    
    output$edittranslation <- shiny::renderUI({
      shiny::req(!base::is.null(translated_document()))
      shiny::req(base::file.exists(translated_document()$filepath))
      lines <- base::readLines(base::paste0(translated_document()$filepath))
      shinydashboardPlus::box(
        width = 12, title = "Translation", solidHeader = TRUE,
        status = "maroon", collapsible = FALSE, collapsed = FALSE,
        height = "750px",
        shiny::fluidRow(
          shiny::column(
            3,
            shiny::actionButton(
              ns("translationinrstudio"), "RStudio",
              icon = shiny::icon("r-project"),
              style = "background-color:#003366;color:#FFF;
                width:100%;margin-bottom:10px;"
            )
          ),
          shiny::column(
            3,
            shiny::actionButton(
              ns("translationrefresh"), "Refresh",
              icon = shiny::icon("rotate"),
              style = "background-color:#006699;color:#FFF;
                width:100%;margin-bottom:10px;"
            )
          ),
          shiny::column(
            3,
            shiny::actionButton(
              ns("savetranslation"), "Save",
              icon = shiny::icon("floppy-disk"),
              style = "background-color:#006633;color:#FFF;
                width:100%;margin-bottom:10px;"
            )
          ),
          shiny::column(
            3,
            shiny::actionButton(
              ns("translationpreview"), "Preview", icon = shiny::icon("eye"),
              style = "background-color:#660033;color:#FFF;
                width:100%;margin-bottom:10px;"
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(12, shinyAce::aceEditor(
            outputId = ns("editedtranslation"), value = lines, mode = "markdown",
            wordWrap = TRUE, debounce = 10, autoComplete = "live",
            height = "500"
          ))
        )
      )
    })
    
    shiny::observeEvent(input$translationinrstudio, {
      translated_document <- shiny::isolate({ translated_document() })
      shiny::req(!base::is.null(translated_document))
      rstudioapi::navigateToFile(translated_document$filepath)
    })
    
    shiny::observeEvent(input$translationrefresh, {
      
    })
    
    shiny::observeEvent(input$savetranslation, {
      translated_document <- shiny::isolate({ translated_document() })
      editedtranslation <- shiny::isolate({ input$editedtranslation })
      shiny::req(!base::is.null(editedtranslation))
      base::writeLines(editedtranslation, translated_document$filepath, useBytes = TRUE)
      shinyalert::shinyalert(
        "Translation saved", "Click on the refresh button to display changes.",
        type = "success"
      )
    })
    
    shiny::observeEvent(input$translationpreview, {
      if (translated_document()$type %in% c("Free","Statements","Alternatives","Computation","Essay","Problem")){
        base::load(course_paths()$databases$propositions)
        base::load(course_paths()$databases$translations)
        test_parameters <- NA
        docformat <- "html"
        record_solution <- FALSE
        shiny::showModal(shiny::modalDialog(
          title = "Question preview",
          shiny::renderUI({
            base::suppressWarnings(
              shiny::withMathJax(shiny::HTML(knitr::knit2html(
                text = base::readLines(translated_document()$filepath),
                quiet = TRUE, template = FALSE
              )))
            )
          }),
          easyClose = TRUE
        ))
      } else {
        editR::view_document(translated_document(),FALSE,course_paths)
      }
    })
    
    
    
    # Display statistics #######################################################
    
    doc_for_stats <- shiny::reactive({
      shiny::req(!base::is.null(translated_document()))
      translated_document()$file
    })
    
    output$ratingsstatistics <- shiny::renderUI({
      shiny::req(!base::is.null(doc_for_stats()))
      shiny::req(doc_for_stats() != "")
      make_infobox(course_data, doc_for_stats(), "ratings")
    })
    output$viewsstatistics <- shiny::renderUI({
      shiny::req(!base::is.null(doc_for_stats()))
      shiny::req(doc_for_stats() != "")
      make_infobox(course_data, doc_for_stats(), "views")
    })
    output$resultsstatistics <- shiny::renderUI({
      shiny::req(!base::is.null(doc_for_stats()))
      shiny::req(doc_for_stats() != "")
      make_infobox(course_data, doc_for_stats(), "results")
    })
    
    
    
    # Edit propositions ########################################################
    
    selected_propositions <- shiny::reactive({
      shiny::req(selected_code())
      base::load(course_paths()$databases$propositions)
      selection <- course_data()$documents |>
        dplyr::filter(code == selected_code()) |>
        dplyr::select(type, code, document) |>
        base::unique()
      if (selection$type %in% c("Note","Page","Slide","Script")){
        propositions |>
          dplyr::mutate(keep = purrr::map_lgl(document, function(x,y){
            stringr::str_detect(y, x)
          }, selection$document)) |>
          dplyr::filter(keep == TRUE) |>
          dplyr::select(-keep)
      } else if (selection$type == "Statements") {
        propositions |>
          dplyr::mutate(keep = purrr::map_lgl(document, function(x,y){
            stringr::str_detect(y, x)
          }, selection$document)) |>
          dplyr::filter(keep == TRUE, type == "Statements") |>
          dplyr::select(-keep)
      } else {
        propositions |>
          dplyr::filter(code == selected_code())
      }
    })
    
    selected_translations <- shiny::reactive({
      shiny::req(selected_propositions())
      shiny::req(input$slctlang)
      base::load(course_paths()$databases$translations)
      translations <- translations |>
        dplyr::mutate_all(base::as.character)|>
        dplyr::filter(language == input$slctlang)
      selected_propositions() |>
        dplyr::select(item, proposition,explanation) |>
        dplyr::left_join(translations, by = "item") |>
        tidyr::replace_na(base::list(language = input$slctlang)) |>
        dplyr::select(
          item, language,
          proposition, translated_proposition,
          explanation, translated_explanation
        )
    })
    
    output$translatepropositions <- rhandsontable::renderRHandsontable({
      shiny::req(!base::is.null(selected_translations()))
      
      if (base::file.exists(course_paths()$databases$item_parameters)){
        base::load(course_paths()$databases$item_parameters)
        item_parameters <- item_parameters |>
          dplyr::select(item, language, answers, success, discrimination)
        selected <- selected_translations() |>
          dplyr::left_join(item_parameters, by = c("item","language"))
      } else {
        selected <- selected_translations() |>
          dplyr::mutate(answers = NA, success = NA, discrimination = NA)
      }
      
      selected |>
        rhandsontable::rhandsontable(
          height = 750, width = "100%", rowHeaders = NULL, stretchH = "all"
        ) |>
        rhandsontable::hot_col(c(1,2,3,5,7,8,9), readOnly = TRUE) |>
        rhandsontable::hot_cols(
          colWidths = c("8%","3%","20%","20%","20%","20%","3%","3%","3%")
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = FALSE, allowColEdit = FALSE
        )
    })
    
    shiny::observeEvent(input$saveproptranslation, {
      shiny::req(!base::is.null(input$translatepropositions))
      base::load(course_paths()$databases$translations)
      translated_propositions <- rhandsontable::hot_to_r(input$translatepropositions) |>
        dplyr::select(item, language, translated_proposition, translated_explanation)
      not_edited <- translations |>
        dplyr::anti_join(translated_propositions, by = c("item", "language"))
      translations <- dplyr::bind_rows(not_edited, translated_propositions) |>
        dplyr::arrange(item)
      base::save(translations, file = course_paths()$databases$translations)
      shinyalert::shinyalert(
        "Translation saved",
        "The translations of propositions have been saved. Refresh the question to update it.",
        "success", TRUE, TRUE
      )
    })
    
  })
}

