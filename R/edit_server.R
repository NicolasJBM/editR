#' @name edit_server
#' @title Edit documents
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation or modification of documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param filtered Reactive. List of pre-selected documents.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param intake Reactive. Function containing a list of documents as a classification tree compatible with jsTreeR
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @param doctype Character. Whether the document is a "Presentation", "Video" ,"Page", "Paper", or question
#' @return Save the new or modified page in the folder "2_documents/main_language/".
#' @importFrom chartR display_curve
#' @importFrom dplyr anti_join
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr select
#' @importFrom knitr knit2html
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
#' @importFrom shiny modalButton
#' @importFrom shiny modalDialog
#' @importFrom shiny moduleServer
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny removeModal
#' @importFrom shiny renderPlot
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shiny showModal
#' @importFrom shiny sliderInput
#' @importFrom shiny tagList
#' @importFrom shiny withMathJax
#' @importFrom shinyAce aceEditor
#' @importFrom shinyalert shinyalert
#' @importFrom shinydashboardPlus box
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @export


edit_server <- function(
    id, filtered, course_data, intake, course_paths, doctype
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
    answers <- NULL
    retire <- NULL
    V1 <- NULL
    tag <- NULL
    translations <- NULL
    

    # Load data ################################################################

    selection <- shiny::reactive({
      shiny::req(base::length(course_paths()) == 2)
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
        Presentation = "S",
        Video = "V",
        Page = "P",
        Paper = "N",
        Question = "Q"
      )
    })

    templates_path <- shiny::reactive({
      shiny::req(base::length(course_paths()) == 2)
      base::switch(
        doctype,
        Paper = course_paths()$subfolders$templates_paper,
        Page = course_paths()$subfolders$templates_page,
        Presentation = course_paths()$subfolders$templates_presentation,
        Video = course_paths()$subfolders$templates_video,
        Question = course_paths()$subfolders$templates_question
      )
    })

    template_files <- shiny::reactive({
      shiny::req(!base::is.null(templates_path()))
      base::list.files(templates_path())
    })
    


    # Select document ##########################################################
    document_list <- shiny::reactive({
      shiny::req(!base::is.null(selection()))
      shiny::req(nrow(selection()) > 0)
      if (base::length(selection()$title) > 0){
        doc_list <- c(selection()$file)
        base::names(doc_list) <- c(
          base::paste(selection()$code, " - ", selection()$title)
        )
      }  else doc_list <- ""
      doc_list
    })

    selected_document <- editR::selection_server("slctdoc", document_list)
    
    output$pathintree <- shiny::renderUI({
      shiny::req(!base::is.null(selected_document()))
      shiny::req(base::length(selected_document()) == 1 & selected_document() != "")
      editR::make_tree_path(selected_document(), intake()$tbltree) |>
        shiny::HTML()
    })
    
    
    # Edit metainformation #####################################################
    
    shiny::observeEvent(input$editmetainfo, {
      
      shiny::req(!base::is.null(selected_document()))
      tags <- course_data()$tags
      documents <- course_data()$documents
      selected_file <- selected_document()
      
      preptags <- tags |>
        dplyr::filter(value != "", !base::is.na(value)) |>
        dplyr::select(tag, value)
      
      doclist <- documents$code
      base::names(doclist) <- base::paste(documents$code, documents$title, sep = " - ")
      
      document <- documents |>
        dplyr::filter(file == selected_file)
      
      filename <- document$file[1]
      
      commontags <- c("title","authors","type","document")
      
      document <- document |>
        dplyr::select(-file,-code,-language,-translations,-modified) |>
        base::t() |>
        base::as.data.frame() |>
        dplyr::rename(input = V1) |>
        tibble::rownames_to_column("tag") |>
        dplyr::left_join(preptags, by = "tag") |>
        dplyr::group_by(tag, input) |>
        dplyr::summarise(choices = base::list(value), .groups = "drop") |>
        dplyr::mutate(
          input = purrr::map_chr(input, function(x) if (base::is.na(x) | x == "") NA else x),
          choices = dplyr::case_when(
            tag == "document" ~ base::list(doclist),
            TRUE ~ choices
          )
        )
      
      document <- document |>
        dplyr::mutate(tag = base::factor(
          tag,
          levels = c(commontags, base::setdiff(document$tag, commontags))
        )) |>
        dplyr::arrange(tag)
      
      ui <- base::list()
      
      for (i in 1:base::nrow(document)){
        
        possible_choices <- document$choices[[i]] |>
          base::unlist()
        
        if (!base::is.na(possible_choices[1])){
          ui[[i]] <- shiny::selectInput(
            inputId = ns(document$tag[[i]]),
            label = document$tag[[i]],
            choices = possible_choices,
            selected = base::unlist(stringr::str_split(document$input[[i]], " ")),
            multiple = TRUE,
            width = "100%"
          )
        } else {
          ui[[i]] <- shiny::textInput(
            inputId = ns(document$tag[[i]]),
            label = document$tag[[i]],
            value = document$input[[i]],
            width = "100%"
          )
        }
      }
      
      shiny::showModal(
        shiny::modalDialog(
          style = "background-color:#001F3F;color:#FFF;margin-top:50px;",
          ui,
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              ns("writemetainfo"), "Write", icon = shiny::icon("edit"),
              style = "background-color:#006699;color:#FFF;"
            )
          )
        )
      )
    })
    
    shiny::observeEvent(input$writemetainfo, {
      shiny::removeModal()
      tags <- course_data()$tags
      selected_file <- selected_document()
      
      document_to_edit <- shiny::isolate({ document_to_edit() })
      shiny::req(base::file.exists(document_to_edit$filepath))
      editeddoc <- base::readLines(document_to_edit$filepath)
      end <- base::which(stringr::str_detect(editeddoc, stringr::fixed("exextra[title]")))-1
      cutdoc <- editeddoc[1:end]
      
      alltags <- c("title","authors","type","document", base::unique(tags$tag))
      metainfo <- base::list()
      for (tag in alltags){
        metainfo[[tag]] <- base::paste0(
          "exextra[", tag,"]: ",
          base::paste(input[[tag]], collapse = " "),
          "  "
        )
      }
      metainfo <- base::as.character(metainfo)
      
      fulldoc <- c(cutdoc, metainfo, "  ")
      
      base::writeLines(fulldoc, document_to_edit$filepath, useBytes = TRUE)
      
      shinyalert::shinyalert(
        "Meta-information updated!", "Refresh the file or reload the course to see it.",
        type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
      )
    })
    
    
    
    # Display statistics #######################################################

    output$ratingsstatistics <- shiny::renderUI({
      shiny::req(!base::is.null(selected_document()))
      shiny::req(selected_document() != "")
      editR::make_infobox(course_data, selected_document(), "ratings")
    })
    output$viewsstatistics <- shiny::renderUI({
      shiny::req(!base::is.null(selected_document))
      shiny::req(selected_document() != "")
      editR::make_infobox(course_data, selected_document(), "views")
    })
    output$resultsstatistics <- shiny::renderUI({
      shiny::req(!base::is.null(selected_document))
      shiny::req(selected_document() != "")
      editR::make_infobox(course_data, selected_document(), "results")
    })
    
    output$questioncurve <- shiny::renderPlot({
      shiny::req(!base::is.null(selected_document()))
      shiny::req(!base::is.null(course_data()$document_models))
      shiny::req(selected_document() %in% course_data()$document_models$file)
      selected_model <- course_data()$document_models |>
        dplyr::filter(file == selected_document())
      chartR::display_curve(selected_model$data[[1]])
    })
    
    
    
    # Display document #########################################################

    document_to_edit <- shiny::reactive({
      shiny::req(!base::is.null(selection()))
      shiny::req(selected_document())
      shiny::req(selected_document() %in% selection()$file)
      to_edit <- selection() |>
        dplyr::filter(file == selected_document())
      to_edit$filepath <- base::paste0(
        course_paths()$subfolders$original, "/", to_edit$file
      )
      to_edit
    })
    
    output$docinfo <- shiny::renderUI({
      editR::make_title_display(document_to_edit(), course_data)
    })
    
    
    # Edit document ############################################################

    edited_lines <- shiny::reactive({
      shiny::req(!base::is.null(document_to_edit()))
      if (!base::is.null(input$docrefresh)) input$docrefresh
      base::readLines(document_to_edit()$filepath)
    })
    
    output$editdoc <- shiny::renderUI({
      shiny::req(!base::is.null(edited_lines()))
      shinydashboardPlus::box(
        width = 12, title = "Edition", solidHeader = TRUE,
        status = "navy", collapsible = FALSE, collapsed = FALSE,
        height = "750px",
        shiny::fluidRow(
          shiny::column(
            3,
            shiny::actionButton(
              ns("docinrstudio"), "RStudio",
              icon = shiny::icon("r-project"),
              style = "background-color:#003366;color:#FFF;
                width:100%;margin-bottom:10px;"
            )
          ),
          shiny::column(
            3,
            shiny::actionButton(
              ns("docrefresh"), "Refresh",
              icon = shiny::icon("rotate"),
              style = "background-color:#006699;color:#FFF;
                width:100%;margin-bottom:10px;"
            )
          ),
          shiny::column(
            3,
            shiny::actionButton(
              ns("savedoc"), "Save",
              icon = shiny::icon("floppy-disk"),
              style = "background-color:#006633;color:#FFF;
                width:100%;margin-bottom:10px;"
            )
          ),
          shiny::column(
            3,
            shiny::actionButton(
              ns("docpreview"), "Preview", icon = shiny::icon("eye"),
              style = "background-color:#660033;color:#FFF;
                width:100%;margin-bottom:10px;"
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(12, shinyAce::aceEditor(
            outputId = ns("editeddoc"), value = edited_lines(),
            mode = "markdown", wordWrap = TRUE, debounce = 10,
            autoComplete = "live", height = "500"
          ))
        )
      )
    })
    
    shiny::observeEvent(input$docinrstudio, {
      document_to_edit <- shiny::isolate({ document_to_edit() })
      shiny::req(!base::is.null(document_to_edit))
      rstudioapi::navigateToFile(document_to_edit$filepath)
    })
    
    shiny::observeEvent(input$savedoc, {
      document_to_edit <- shiny::isolate({ document_to_edit() })
      editeddoc <- shiny::isolate({ input$editeddoc })
      shiny::req(!base::is.null(editeddoc))
      base::writeLines(editeddoc, document_to_edit$filepath, useBytes = TRUE)
      shinyalert::shinyalert(
        "Document saved", "Click on the refresh button to create a preview.",
        type = "success"
      )
    })
    
    shiny::observeEvent(input$docpreview, {
      if (doctype == "Question"){
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
                text = base::readLines(document_to_edit()$filepath),
                quiet = TRUE, template = FALSE
              )))
            )
          }),
          easyClose = TRUE
        ))
      } else {
        editR::view_document(document_to_edit(),TRUE,course_paths)
      }
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
      shiny::req(base::length(course_paths()) == 2)
      shiny::req(doctype %in% c("Paper","Page","Presentation","Video","Question"))
      input$refreshprop
      input$acknowledgesaveprop
      base::load(course_paths()$databases$propositions)
      propositions
    })
    
    targeted_documents <- shiny::reactive({
      shiny::req(!base::is.null(propositions()))
      shiny::req(selected_document())
      shiny::req(!base::is.null(selection()))
      selected_document <- selection() |>
        dplyr::filter(file == selected_document())
      targeted_documents <- selected_document$document[1] |>
        stringr::str_split(pattern = " ", simplify = TRUE) |>
        base::unique() |>
        base::sort()
      targeted_documents
    })
    
    propositions_for_document <- shiny::reactive({
      shiny::req(!base::is.null(propositions()))
      shiny::req(!base::is.null(targeted_documents()))
      selected_document <- selection() |>
        dplyr::filter(file == selected_document())
      if (selected_document$type %in% c("Paper","Page","Presentation","Video")) {
        propositions() |>
          dplyr::filter(
            document %in% targeted_documents()
          )
      } else if (selected_document$type == "Statements") {
        propositions() |>
          dplyr::filter(
            type == selected_document$type,
            document %in% targeted_documents()
          )
      } else {
        propositions() |>
          dplyr::filter(
            code == selected_document$code,
            type == selected_document$type
          )
      }
    })
    
    output$selectprop <- shiny::renderUI({
      shiny::req(base::length(targeted_documents()) > 0)
      shiny::req(!base::is.null(propositions_for_document()))
      tgtdoc <- c(
        targeted_documents(),
        base::unique(propositions_for_document()$document)
      )
      tgttype <- c(base::unique(propositions_for_document()$type))
      
      shinydashboardPlus::box(
        width = 12, title = "Selection", solidHeader = TRUE,
        status = "purple", collapsible = FALSE, collapsed = FALSE,
        height = "250px",
        shiny::actionButton(
          ns("saveprop"), "Save propositions",
          icon = shiny::icon("floppy-disk"),
          style = "background-color:#006600;color:#FFF;width:100%"
        ),
        shiny::tags$hr(),
        shiny::selectInput(
          ns("slctpropdoc"), "Select a document:",
          choices = tgtdoc,
          selected = tgtdoc,
          multiple = TRUE
        ),
        shiny::tags$hr(),
        shiny::selectInput(
          ns("slctproptype"), "Select a type:",
          choices = tgttype,
          selected = tgttype,
          multiple = TRUE
        ),
        shiny::tags$hr(),
        shiny::sliderInput(
          ns("slctpropval"), "Select a value range:",
          min = 0, max = 1, value = c(0,1)
        ),
        shiny::tags$hr(),
        shiny::numericInput(
          ns("itmnbr"), "Additional items:",
          min = 1, max = 100, step = 1 ,value = 1
        ),
        shinyWidgets::materialSwitch(
          inputId = ns("sortbydoc"),
          label = "Sort by document", 
          status = "primary",
          value = FALSE
        ),
        shinyWidgets::materialSwitch(
          inputId = ns("sortbyval"),
          label = "Sort by value", 
          status = "primary",
          value = FALSE
        ),
        shinyWidgets::materialSwitch(
          inputId = ns("sortbyprp"),
          label = "Sort by proposition", 
          status = "primary",
          value = FALSE
        ),
        shinyWidgets::materialSwitch(
          inputId = ns("sortbyitm"),
          label = "Sort by item", 
          status = "primary",
          value = TRUE
        )
      )
    })
    
    propositions_to_edit <- shiny::reactive({
      shiny::req(!base::is.null(propositions_for_document()))
      shiny::req(!base::is.null(input$slctpropdoc))
      shiny::req(!base::is.null(input$slctpropval))
      propositions_for_document() |>
        dplyr::filter(
          document %in% c(NA, input$slctpropdoc),
          type %in% input$slctproptype,
          value >= input$slctpropval[1],
          value <= input$slctpropval[2]
        )
    })
    
    output$editprop <- rhandsontable::renderRHandsontable({
      
      shiny::req(!base::is.null(selected_document()))
      shiny::req(base::length(targeted_documents()) > 0)
      shiny::req(!base::is.null(propositions()))
      shiny::req(!base::is.null(propositions_to_edit()))
      
      selected_document <- selection() |>
        dplyr::filter(file == selected_document())
      
      existing_names <- propositions() |>
        dplyr::select(item) |> base::unlist() |>
        base::as.character() |> base::unique()
      newitemid <- editR::name_new_item(existing_names, input$itmnbr)
      
      if (selected_document$type %in% c("Presentation","Video","Page","Paper")){
        levelcode <- base::unique(c(targeted_documents(), propositions_to_edit()$code))
        slctcode <- NA
        leveltype <- c("Statements","Alternatives","Computation","Essay","Problem")
      } else if (selected_document$type == "Statements") {
        levelcode <- selected_document$code
        slctcode <- NA
        leveltype <- selected_document$type
      } else {
        levelcode <- selected_document$code
        slctcode <- levelcode[1]
        leveltype <- selected_document$type
      }
      levellanguage <- selected_document$language
      leveldocs <- base::union(targeted_documents(), propositions_to_edit()$document)
      levelscale <- c("logical","qualitative","percentage")
      
      tmprow <- tibble::tibble(
        item = newitemid,
        language = base::factor(levellanguage[1], levels = levellanguage),
        code = base::factor(slctcode, levels = levelcode),
        type = base::factor(leveltype[1], levels = leveltype),
        document = base::factor(leveldocs[1], levels = leveldocs),
        modifications = 1,
        proposition = base::as.character(NA),
        value = 0,
        scale = base::factor(levelscale[1], levels = levelscale),
        explanation = base::as.character(NA),
        keywords = base::as.character(NA),
        retire = FALSE,
        answers = base::as.numeric(NA),
        success = base::as.numeric(NA),
        discrimination = base::as.numeric(NA)
      )
      
      if (base::nrow(propositions_to_edit()) > 0){
        itemsublist <- propositions_to_edit() |>
          dplyr::mutate(
            code = base::factor(code, levels = levelcode),
            type = base::factor(type, levels = leveltype),
            document = base::factor(document, levels = leveldocs),
            scale = base::factor(scale, levels = levelscale)
          )
        
        if (input$sortbyitm) itemsublist <- dplyr::arrange(itemsublist, item)
        if (input$sortbyprp) itemsublist <- dplyr::arrange(itemsublist, proposition)
        if (input$sortbyval) itemsublist <- dplyr::arrange(itemsublist, dplyr::desc(value))
        if (input$sortbydoc) itemsublist <- dplyr::arrange(itemsublist, document)
        
        itemsublist <- itemsublist |>
          #dplyr::left_join(
          #  course_data()$item_parameters,
          #  by = c("item","language")
          #) |>
          dplyr::select(
            item, language, code, type, document, modifications, proposition,
            value, scale, explanation, keywords, retire#, answers, success, discrimination
          ) |>
          dplyr::bind_rows(tmprow)
      } else {
        itemsublist <- tmprow
      }
      
      itemsublist |>
        rhandsontable::rhandsontable(
          height = 750, width = "100%", rowHeaders = NULL, stretchH = "all"
        ) |>
        rhandsontable::hot_col(c(1,2,13,14,15), readOnly = TRUE) |>
        rhandsontable::hot_cols(
          colWidths = c(
            "6%","2%","6%","6%","7%","3%","18%","3%",
            "5%","22%","10%","3%","3%","3%","3%"
          ),
          manualColumnResize = TRUE
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
        dplyr::arrange(item) |>
        dplyr::filter(proposition != "", !base::is.na(proposition))
      
      base::save(propositions, file = course_paths()$databases$propositions)
      
      base::Sys.sleep(1)
      
      shinyalert::shinyalert(
        "Propositions saved!", "Refresh to see changes.",
        type = "success", inputId = "acknowledgesaveprop"
      )
    })

    
    
    # Publish document #########################################################

    shiny::observeEvent(input$publishdocs, {
      if (doctype == "Presentation"){
        editR::publish_presentation(intake(), selected_document(), course_paths())
      } else if (doctype == "Video"){
        editR::publish_video(selected_document(), course_paths())
      } else if (doctype == "Page"){
        editR::publish_textbook(intake(), course_paths(), course_data()$languages)
      } else if (doctype == "Paper"){
        editR::publish_paper(selected_document(), course_paths())
      } else if (doctype == "Question"){
        shinyalert::shinyalert(
          "Go to test", "Questions can only be published in tests.",
          type = "warning"
        )
      }
    })
    
    
    # Open folder ##############################################################
    
    shiny::observeEvent(input$openfolder, {
      if (doctype == "Presentation"){
        folder <- course_paths()$subfolders$presentations
      } else if (doctype == "Video"){
        folder <- course_paths()$subfolders$videos
      } else if (doctype == "Page"){
        folder <- course_paths()$subfolders$textbooks
      } else if (doctype == "Paper"){
        folder <- course_paths()$subfolders$papers
      } else if(doctype == "Question"){
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


  })
}

