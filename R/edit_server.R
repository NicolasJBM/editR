#' @name edit_server
#' @title Edit documents
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation or modification of documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param filtered Reactive. List of pre-selected documents.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param tree Reactive. Function containing a list of documents as a classification tree compatible with jsTreeR
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @param doctype Character. Whether the document is a "Note", "Page", "Slide", "Video", "Game", or "Case" (questions are handled by another module).
#' @return Save the new or modified page in the folder "2_documents/main_language/".
#' @importFrom dplyr anti_join
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr select
#' @importFrom rhandsontable hot_col
#' @importFrom rhandsontable hot_cols
#' @importFrom rhandsontable hot_context_menu
#' @importFrom rhandsontable hot_to_r
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom rstudioapi navigateToFile
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny isolate
#' @importFrom shiny modalButton
#' @importFrom shiny modalDialog
#' @importFrom shiny moduleServer
#' @importFrom shiny NS
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny removeModal
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shiny showModal
#' @importFrom shiny sliderInput
#' @importFrom shiny tagList
#' @importFrom shiny wellPanel
#' @importFrom shinyAce aceEditor
#' @importFrom shinyalert shinyalert
#' @importFrom shinydashboardPlus box
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @export


edit_server <- function(
    id, filtered, course_data, tree, course_paths, doctype
){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {

    section <- NULL
    authors <- NULL
    type <- NULL
    data <- NULL
    discrimination <- NULL
    document <- NULL
    explanation <- NULL
    item <- NULL
    keywords <- NULL
    language <- NULL
    modifications <- NULL
    proposition <- NULL
    success <- NULL
    value <- NULL
    code <- NULL
    tags <- NULL


    # Load data ################################################################

    selection <- shiny::reactive({
      shiny::req(!base::is.null(filtered()))
      if (doctype == "Question"){
        filtered() |>
          dplyr::filter(type %in% c("Statements","Alternatives","Computation","Essay","Problem"))
      } else {
        filtered() |>
          dplyr::filter(type == doctype)
      }
    })

    prefix <- shiny::reactive({
      base::switch(
        doctype,
        Note = "N",
        Page = "P",
        Slide = "S",
        Video = "V",
        Game = "G",
        Case = "C",
        Question = "Q"
      )
    })

    templates_path <- shiny::reactive({
      base::switch(
        doctype,
        Note = course_paths()$subfolders$templates_note,
        Page = course_paths()$subfolders$templates_page,
        Slide = course_paths()$subfolders$templates_slide,
        Video = course_paths()$subfolders$templates_video,
        Game = course_paths()$subfolders$templates_game,
        Case = course_paths()$subfolders$templates_case,
        Question = course_paths()$subfolders$templates_question
      )
    })

    template_files <- shiny::reactive({
      base::list.files(templates_path())
    })
    


    # Select document ##########################################################

    output$selectdoc <- shiny::renderUI({
      shiny::req(!base::is.null(selection()))
      if (base::length(selection()$title) > 0){
        doc_list <- c("", selection()$file)
        base::names(doc_list) <- c(
          "", base::paste(selection()$code, " - ", selection()$title)
        )
      }  else doc_list <- ""
      if (base::length(input$selected) == 0){
        if (base::length(doc_list) > 1){
          selection <- doc_list[2]
        } else selection <- ""
      } else {
        selection <- input$selecteddoc
      }
      shiny::selectInput(
        ns("selecteddoc"), "Select a document:", choices = doc_list,
        selected = selection, width = "100%"
      )
    })



    # Display statistics #######################################################

    output$ratingsstatistics <- shiny::renderUI({
      shiny::req(!base::is.null(input$selecteddoc))
      shiny::req(input$selecteddoc != "")
      make_infobox(course_data, input$selecteddoc, "ratings")
    })
    output$viewsstatistics <- shiny::renderUI({
      shiny::req(!base::is.null(input$selecteddoc))
      shiny::req(input$selecteddoc != "")
      make_infobox(course_data, input$selecteddoc, "views")
    })
    output$resultsstatistics <- shiny::renderUI({
      shiny::req(!base::is.null(input$selecteddoc))
      shiny::req(input$selecteddoc != "")
      make_infobox(course_data, input$selecteddoc, "results")
    })



    # Display document #########################################################

    document_to_edit <- shiny::reactive({
      shiny::req(!base::is.null(selection()))
      shiny::req(input$selecteddoc)
      shiny::req(input$selecteddoc %in% selection()$file)
      to_edit <- selection() |>
        dplyr::filter(file == input$selecteddoc)
      to_edit$filepath <- base::paste0(
        course_paths()$subfolders$original, "/", to_edit$file
      )
      to_edit
    })
    
    output$viewdoc <- shiny::renderUI({
      shiny::req(!base::is.null(document_to_edit()))
      input$savedoc
      input$docrefresh
      editR::view_document(document_to_edit(), TRUE, course_data, course_paths)
    })



    # Edit document ############################################################

    output$editdoc <- shiny::renderUI({
      shiny::req(!base::is.null(document_to_edit()))
      lines <- base::readLines(document_to_edit()$filepath)
      shinydashboardPlus::box(
        width = 12, title = "Edition", solidHeader = TRUE,
        status = "navy", collapsible = FALSE, collapsed = FALSE,
        height = "750px",
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::actionButton(
              ns("savedoc"), "Save", icon = shiny::icon("floppy-disk"),
              style = "background-color:#006633;color:#FFF;
                width:100%;margin-bottom:10px;"
            )
          ),
          shiny::column(
            4,
            shiny::actionButton(
              ns("docinrstudio"), "RStudio",
              icon = shiny::icon("r-project"),
              style = "background-color:#222222;color:#FFF;
                width:100%;margin-bottom:10px;"
            )
          ),
          shiny::column(
            4,
            shiny::actionButton(
              ns("docrefresh"), "Refresh",
              icon = shiny::icon("rotate"),
              style = "background-color:#003399;color:#FFF;
                width:100%;margin-bottom:10px;"
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(12, shinyAce::aceEditor(
            outputId = ns("editeddoc"), value = lines, mode = "markdown",
            wordWrap = TRUE, debounce = 10, autoComplete = "live",
            height = "750px"
          ))
        )
      )
    })

    shiny::observeEvent(input$savedoc, {
      document_to_edit <- shiny::isolate({ document_to_edit() })
      editeddoc <- shiny::isolate({ input$editeddoc })
      shiny::req(!base::is.null(editeddoc))
      base::writeLines(editeddoc, document_to_edit$filepath, useBytes = TRUE)
    })

    shiny::observeEvent(input$docinrstudio, {
      document_to_edit <- shiny::isolate({ document_to_edit() })
      shiny::req(!base::is.null(document_to_edit))
      rstudioapi::navigateToFile(document_to_edit$filepath)
    })



    # Create document ##########################################################

    shiny::observeEvent(input$newdoc, {
      shiny::showModal(
        shiny::modalDialog(
          style = "background-color:#001F3F;color:#FFF;margin-top:300px;",
          shiny::selectInput(
            ns("slcttemplatebasis"), "Based on the following template:",
            choices = template_files(), selected = "", width = "100%"
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              ns("createdoc"), "OK", icon = shiny::icon("check"),
              style = "background-color:#007777;color:#FFF;"
            )
          )
        )
      )
    })

    shiny::observeEvent(input$createdoc, {
      shiny::removeModal()
      newname <- editR::make_new_name(prefix(), course_paths)
      newfile <- base::paste0(newname, ".Rmd")
      if (input$slcttemplatebasis == "") {
        lines <- c(
          "",
          "",
          "Meta-information",
          "================",
          "exextra[title]:New document.  ",
          "exextra[type]:", doctype, "  ",
          base::paste0(
            "exextra[document]:",
            stringr::str_remove(newname, "_...Rmd$"),
            "  "
          ),
          "exextra[tag_custom]:  "
        )
      } else {
        lines = base::readLines(
          base::paste0(
            templates_path(), "/",
            input$slcttemplatebasis
          )
        )
        lines <- stringr::str_replace_all(
          lines, base::paste0(prefix(), "XXXXXXXXX"),
          stringr::str_remove(newname, "_...Rmd$")
        )
      }
      base::writeLines(
        lines,
        base::paste0(
          course_paths()$subfolders$original, "/", newname
        ), useBytes = TRUE
      )
      shinyalert::shinyalert(
        "Document created!", "Update documents and reload the course to see it.",
        type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
      )
    })


    # Edit propositions ########################################################
    
    propositions <- shiny::reactive({
      shiny::req(doctype == "Question")
      shiny::req(!base::is.null(selection()))
      input$refreshprop
      base::load(course_paths()$databases$propositions)
      propositions
    })
    
    targeted_documents <- shiny::reactive({
      shiny::req(doctype == "Question")
      shiny::req(input$selecteddoc)
      shiny::req(!base::is.null(selection()))
      selected_question <- selection() |>
        dplyr::filter(file == input$selecteddoc)
      targeted_documents <- selected_question$document[1] |>
        stringr::str_split(pattern = " ", simplify = TRUE) |>
        base::unique() |>
        base::sort()
      targeted_documents
    })
    
    propositions_for_question <- shiny::reactive({
      shiny::req(doctype == "Question")
      shiny::req(!base::is.null(targeted_documents()))
      shiny::req(base::length(targeted_documents()) > 0)
      selected_question <- selection() |>
        dplyr::filter(file == input$selecteddoc)
      slcttype <- selected_question$type[1]
      slctlang <- selected_question$language[1]
      if (slcttype == "Statements"){
        propositions() |>
          dplyr::filter(
            document %in% targeted_documents(),
            type == slcttype,
            language == slctlang
          )
      } else {
        propositions() |>
          dplyr::filter(
            code == selected_question$code[1],
            type == slcttype,
            language == slctlang
          )
      }
    })
    
    output$selectprop <- shiny::renderUI({
      shiny::req(doctype == "Question")
      shiny::req(base::length(targeted_documents()) > 0)
      shiny::req(!base::is.null(propositions_for_question()))
      shiny::wellPanel(
        shiny::actionButton(
          ns("saveprop"), "Save propositions",
          icon = shiny::icon("floppy-disk"),
          style = "background-color:#009933;color:#FFF;width:100%"
        ),
        shiny::tags$hr(),
        shiny::actionButton(
          ns("refreshprop"), "Refresh propositions",
          icon = shiny::icon("floppy-disk"),
          style = "background-color:#003399;color:#FFF;width:100%"
        ),
        shiny::tags$hr(),
        shiny::selectInput(
          ns("slctpropdoc"), "Select a document:",
          choices = base::unique(propositions_for_question()$document),
          selected = base::unique(propositions_for_question()$document),
          multiple = TRUE
        ),
        shiny::tags$hr(),
        shiny::sliderInput(
          ns("slctpropval"), "Select a value range:",
          min = 0, max = 1, value = c(0,1)
        )
      )
    })
    
    propositions_to_edit <- shiny::reactive({
      shiny::req(doctype == "Question")
      shiny::req(base::length(targeted_documents()) > 0)
      shiny::req(!base::is.null(propositions_for_question()))
      shiny::req(!base::is.null(input$slctpropdoc))
      shiny::req(!base::is.null(input$slctpropval))
      propositions_for_question() |>
        dplyr::filter(
          document %in% input$slctpropdoc,
          value >= input$slctpropval[1],
          value <= input$slctpropval[2]
        )
    })
    
    output$editprop <- rhandsontable::renderRHandsontable({
      shiny::req(doctype == "Question")
      shiny::req(base::length(targeted_documents()) > 0)
      shiny::req(!base::is.null(propositions()))
      shiny::req(!base::is.null(propositions_to_edit()))
      
      existing_names <- propositions() |>
        dplyr::select(item) |> base::unlist() |>
        base::as.character() |> base::unique()
      newitemid <- editR::name_new_item(existing_names)
      levellanguage <- propositions_to_edit()$language[1]
      leveltype <- propositions_to_edit()$type[1]
      levelcode <- dplyr::case_when(
        leveltype == "Statements" ~ base::as.character(NA),
        TRUE ~ propositions_to_edit()$code[1]
      )
      levelscale <- c("logical","qualitative","percentage")
      
      tmprow <- tibble::tibble(
        item = newitemid,
        language = levellanguage,
        code = levelcode,
        type = leveltype,
        document = base::factor(targeted_documents()[1], levels = targeted_documents()),
        modifications = 1,
        proposition = base::as.character(NA),
        value = 0,
        scale = base::factor("logical", levels = levelscale),
        explanation = base::as.character(NA),
        keywords = base::as.character(NA),
        success = base::as.numeric(NA),
        discrimination = base::as.numeric(NA)
      )
      
      if (base::nrow(propositions_to_edit()) > 0){
        itemsublist <- propositions_to_edit() |>
          dplyr::mutate(
            code = base::factor(code, levels = levelcode),
            type = base::factor(type, levels = leveltype),
            document = base::factor(document, levels = targeted_documents()),
            scale = base::factor(scale, levels = levelscale)
          ) |>
          dplyr::arrange(code, document, dplyr::desc(value), proposition) |>
          dplyr::left_join(
            course_data()$item_parameters,
            by = c("item","language")
          ) |>
          dplyr::select(
            item, language, code, type, document, modifications, proposition,
            value, scale, explanation, keywords, success, discrimination
          ) |>
          dplyr::bind_rows(tmprow)
      } else {
        itemsublist <- tmprow
      }
      
      itemsublist |>
        rhandsontable::rhandsontable(
          height = 750, width = "100%", rowHeaders = NULL, stretchH = "all"
        ) |>
        rhandsontable::hot_col(c(1,2,12,13), readOnly = TRUE) |>
        rhandsontable::hot_cols(
          colWidths = c(
            "7%","2%","7%","7%","7%","3%","18%","3%",
            "5%","25%","10%","3%","3%"
          )
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = FALSE, allowColEdit = FALSE
        )
    })
    
    shiny::observeEvent(input$saveprop, {
      shiny::req(!base::is.null(input$editprop))
      modified <- rhandsontable::hot_to_r(input$editprop) |>
        dplyr::mutate_if(base::is.factor, base::as.character)
      
      if (base::is.na(modified[base::nrow(modified), "proposition"])){
        modified <- modified[-base::nrow(modified),]
      }
      
      propositions <- shiny::isolate({ propositions() })
      
      modified <- modified |>
        dplyr::select(base::names(propositions))
      
      not_modified <- propositions |>
        dplyr::anti_join(modified, by = c("item","language"))
      
      propositions <- not_modified |>
        dplyr::bind_rows(modified) |>
        dplyr::filter(!base::is.na(type), !base::is.na(value)) |>
        dplyr::arrange(item)
      
      base::save(propositions, file = course_paths()$databases$propositions)
      
      shinyalert::shinyalert(
        "Propositions saved!", "Refresh to see changes.",
        type = "success", inputId = "acknowledgesaveprop"
      )
    })

    
    
    # Publish document #########################################################

    shiny::observeEvent(input$publishdocs, {

      if (doctype == "Note"){

      } else if (doctype == "Page"){

        editR::publish_textbook(
          tree(),
          course_paths(),
          course_data()$languages
        )

      } else if (doctype == "Slide"){

      } else if (doctype == "Video"){

      } else if (doctype == "Game"){

      } else if (doctype == "Case"){

      } else if (doctype == "Question"){
        
      }

    })


  })
}

