#' @name translate_server
#' @title Translate documents
#' @author Nicolas Mangin
#' @description Module facilitating the translation of documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param filtered Reactive. List of pre-selected documents.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param tree Reactive. Function containing a list of documents as a classification tree compatible with jsTreeR
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Create and save documents' translations in a dedicated folder.
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rstudioapi navigateToFile
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny isolate
#' @importFrom shiny moduleServer
#' @importFrom shiny NS
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shinyAce aceEditor
#' @importFrom shinyalert shinyalert
#' @importFrom shinydashboardPlus box
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr replace_na
#' @importFrom tidyr unnest
#' @export


translate_server <- function(id, filtered, course_data, tree, course_paths){
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
    
    output$translationaction <- shiny::renderUI({
      shiny::req(!base::is.null(input$slctlang))
      shiny::req(!base::is.null(language_status()))
      missing <- language_status() |>
        dplyr::filter(status == "Missing")
      if (input$slctlang %in% missing$langiso){
        shiny::actionButton(
          ns("createnewtranslation"),"New",
          icon = shiny::icon("wand-magic-sparkles"),
          style = "background-color:#003366; color:#FFF; width:100%; margin-top:25px;"
        )
      } else {
        shiny::actionButton(
          ns("publishtranslation"), "Publish", icon = shiny::icon("print"),
          style = "background-color:#330066;color:#FFF;
          width:100%;margin-top:25px;"
        )
      }
    })
    
    document_to_translate <- shiny::reactive({
      shiny::req(input$slctcode)
      shiny::req(!base::is.null(filtered()))
      shiny::req(input$slctcode %in% filtered()$code)
      document_to_translate <- filtered() |>
        dplyr::filter(code == input$slctcode)
      document_to_translate$filepath <- base::paste0(
        course_paths()$subfolders$original, "/", document_to_translate$file
      )
      document_to_translate
    })
    
    translated_document <- shiny::reactive({
      shiny::req(input$slctlang)
      shiny::req(!base::is.null(language_status()))
      shiny::req(!base::is.null(document_to_translate()))
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
    
    
    output$vieworiginal <- shiny::renderUI({
      shiny::req(!base::is.null(document_to_translate()))
      editR::view_document(document_to_translate(), TRUE, course_data, course_paths)
    })
    
    output$viewtranslation <- shiny::renderUI({
      shiny::req(!base::is.null(document_to_translate()))
      shiny::req(base::file.exists(translated_document()$filepath))
      input$savetranslation
      input$refreshtranslation
      editR::view_document(translated_document(), FALSE, course_data, course_paths)
    })
    
    output$edittranslation <- shiny::renderUI({
      shiny::req(!base::is.null(translated_document()))
      shiny::req(base::file.exists(translated_document()$filepath))
      input$refreshtranslation
      lines <- base::readLines(base::paste0(translated_document()$filepath))
      shinydashboardPlus::box(
        width = 12, title = "Edition", solidHeader = TRUE,
        status = "navy", collapsible = FALSE, collapsed = FALSE,
        height = "750px",
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::actionButton(
              ns("savetranslation"), "Save", icon = shiny::icon("floppy-disk"),
              style = "background-color:#006633;color:#FFF;
                width:100%;margin-bottom:10px;"
            )
          ),
          shiny::column(
            4,
            shiny::actionButton(
              ns("translationinrstudio"), "RStudio",
              icon = shiny::icon("r-project"),
              style = "background-color:#222222;color:#FFF;
                width:100%;margin-bottom:10px;"
            )
          ),
          shiny::column(
            4,
            shiny::actionButton(
              ns("refreshtranslation"), "Refresh",
              icon = shiny::icon("rotate"),
              style = "background-color:#003399;color:#FFF;
                width:100%;margin-bottom:10px;"
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(12, shinyAce::aceEditor(
            outputId = ns("editedtranslation"), value = lines, mode = "markdown",
            wordWrap = TRUE, debounce = 10, autoComplete = "live",
            height = "750px"
          ))
        )
      )
    })
    
    shiny::observeEvent(input$savetranslation, {
      translated_document <- shiny::isolate({ translated_document() })
      editedtranslation <- shiny::isolate({ input$editedtranslation })
      shiny::req(!base::is.null(editedtranslation))
      base::writeLines(editedtranslation, translated_document$filepath, useBytes = TRUE)
    })
    
    shiny::observeEvent(input$translationinrstudio, {
      translated_document <- shiny::isolate({ translated_document() })
      shiny::req(!base::is.null(translated_document))
      rstudioapi::navigateToFile(translated_document$filepath)
    })
    
    
    
    # Create translation #######################################################
    
    shiny::observeEvent(input$createnewtranslation, {
      shiny::req(!base::is.null(document_to_translate()))
      shiny::req(input$slctlang)
      selected_language_status <- language_status() |>
        dplyr::filter(langiso == input$slctlang)
      shiny::req(selected_language_status$status == "Missing")
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
      base::file.copy(
        from = document_to_translate()$filepath,
        to = translated_document$filepath
      )
      shinyalert::shinyalert(
        "Translation created",
        "Update documents and reload the course to edit it.",
        "success", TRUE, TRUE
      )
    })
    
    
    # Publish translation ######################################################
    shiny::observeEvent(input$publishtranslation, {
      selected_document <- translated_document()$file[1]
      if (translated_document()$type == "Note"){
        editR::publish_note(selected_document, course_paths(), translation = TRUE)
      } else if (translated_document()$type[1] == "Page"){
        shinyalert::shinyalert(
          "Go to design!", "All languages in multi-language textbooks are published at once with the main version.",
          type = "warning"
        )
      } else if (translated_document()$type[1] == "Slide"){
        editR::publish_presentation(tree(), selected_document, course_paths())
      } else if (translated_document()$type[1] == "Video"){
        editR::publish_script(selected_document, course_paths(), translation = TRUE)
      } else if (translated_document()$type[1] == "Tutorial"){
        
      } else if (translated_document()$type[1] == "Game"){
        
      } else if (translated_document()$type[1] == "Case"){
        
      } else {
        shinyalert::shinyalert(
          "Go to test!", "Questions can only be published in tests.",
          type = "warning"
        )
      }
      
    })
    
    
    # Open publication folder ##################################################
    shiny::observeEvent(input$opentransfolder, {
      doctype <- translated_document()$type[1]
      if (doctype == "Note"){
        folder <- course_paths()$subfolders$blog
      } else if (doctype == "Page"){
        folder <- course_paths()$subfolders$textbooks
      } else if (doctype == "Slide"){
        folder <- course_paths()$subfolders$presentations
      } else if (doctype == "Video"){
        folder <- course_paths()$subfolders$scripts
      } else if (doctype == "Tutorial"){
        folder <- course_paths()$subfolders$tutorials
      } else if (doctype == "Game"){
        folder <- course_paths()$subfolders$games
      } else if (doctype == "Case"){
        folder <- course_paths()$subfolders$cases
      } else if (doctype == "Question"){
        folder <- course_paths()$subfolders$original
      }
      if (base::dir.exists(folder)){
        if (.Platform['OS.type'] == "windows"){
          shell.exec(folder)
        } else {
          system2("open", folder)
        }
      } else {
        shinyalert::shinyalert(
          "Non-existing folder", "It seems that the folder you are trying to open does not exist. Did you already export files in it?",
          type = "error"
        )
      }
    })
    
    
    
    # Edit propositions ########################################################
    
    selected_propositions <- shiny::reactive({
      shiny::req(input$slctcode)
      base::load(course_paths()$databases$propositions)
      selection <- course_data()$documents |>
        dplyr::filter(code == input$slctcode) |>
        dplyr::select(type, code, document) |>
        base::unique()
      if (selection$type %in% c("Note","Page","Slide","Video","Game","Case","Statements")){
        propositions |>
          dplyr::mutate(keep = purrr::map_lgl(document, function(x,y){
            stringr::str_detect(y, x)
          }, selection$document)) |>
          dplyr::filter(keep == TRUE) |>
          dplyr::select(-keep)
      } else {
        propositions |>
          dplyr::filter(code == input$slctcode)
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
      selected_translations() |>
        rhandsontable::rhandsontable(
          height = 750, width = "100%", rowHeaders = NULL, stretchH = "all"
        ) |>
        rhandsontable::hot_col(c(1,2,3,5), readOnly = TRUE) |>
        rhandsontable::hot_cols(
          colWidths = c("10%","5%","20%","22%","20%","23%")
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

