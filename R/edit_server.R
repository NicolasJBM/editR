#' @name edit_server
#' @title Edit documents
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation or modification of documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param filtered Reactive. List of pre-selected documents.
#' @param tree Character. Name of the tree.
#' @param tbltree Reactive. Function containing a list of documents as a classification tree compatible with jsTreeR.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
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
    id, filtered, tree, tbltree, course_data, course_paths
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
    label <- NULL
    outcome <- NULL
    modified <- NULL
    

    # Select document ##########################################################
    document_list <- shiny::reactive({
      shiny::req(!base::is.null(filtered()))
      shiny::req(nrow(filtered()) > 0)
      if (base::length(filtered()$title) > 0){
        doc_list <- c(filtered()$file)
        base::names(doc_list) <- c(
          base::paste(filtered()$code, " - ", filtered()$title)
        )
      }  else doc_list <- ""
      doc_list
    })

    selected_document <- editR::selection_server("slctdoc", document_list)
    
    document_to_edit <- shiny::reactive({
      shiny::req(!base::is.null(filtered()))
      shiny::req(selected_document())
      shiny::req(selected_document() %in% filtered()$file)
      to_edit <- filtered() |>
        dplyr::filter(file == selected_document())
      to_edit$filepath <- base::paste0(
        course_paths()$subfolders$original, "/", to_edit$file
      )
      to_edit$preview <- base::paste0(
        course_paths()$subfolders$preview, "/",
        stringr::str_replace(to_edit$file, ".Rmd$",".html")
      )
      to_edit
    })
    
    output$docinfo <- shiny::renderUI({
      editR::make_title_display(document_to_edit(), course_data)
    })
    
    output$pathintree <- shiny::renderUI({
      shiny::req(!base::is.null(selected_document()))
      shiny::req(base::length(selected_document()) == 1 & selected_document() != "")
      editR::make_tree_path(selected_document(), tbltree()) |>
        shiny::HTML()
    })
    
    prefix <- shiny::reactive({
      shiny::req(!base::is.null(document_to_edit()))
      base::switch(
        document_to_edit()$type[[1]],
        Presentation = "S",
        Video = "V",
        Page = "P",
        Paper = "N",
        Statements = "Q",
        Alternatives = "Q",
        Computation = "Q",
        Essay = "Q",
        Problem = "Q"
      )
    })
    
    templates_path <- shiny::reactive({
      shiny::req(base::length(course_paths()) == 2)
      shiny::req(!base::is.null(prefix()))
      base::switch(
        prefix(),
        N = course_paths()$subfolders$templates_paper,
        P = course_paths()$subfolders$templates_page,
        S = course_paths()$subfolders$templates_presentation,
        V = course_paths()$subfolders$templates_video,
        Q = course_paths()$subfolders$templates_question
      )
    })
    
    template_files <- shiny::reactive({
      shiny::req(!base::is.null(templates_path()))
      base::list.files(templates_path())
    })
    
    # Edit metainformation #####################################################
    
    shiny::observeEvent(input$editmetainfo, {
      
      shiny::req(!base::is.null(selected_document()))
      
      selected_file <- selected_document()
      
      types <- course_data()$document_types$type
      
      outcomelist <- course_data()$outcomes |>
        dplyr::left_join(dplyr::filter(course_data()$outlabels, language  == "US"), by = "outcome") |>
        dplyr::select(outcome, label) |>
        base::unique()
      outcomes <- outcomelist$outcome
      base::names(outcomes) <- outcomelist$label
      
      documents <- course_data()$documents
      doclist <- documents$code
      base::names(doclist) <- base::paste(documents$code, documents$title, sep = " - ")
      
      commontags <- tibble::tibble(
        tag = c("title","authors","type","document"),
        filter = c("pattern","pattern","selection","multiple"),
        choices = base::list(c(""),c(""),types,doclist)
      )
      
      preptags <- course_data()$tags |>
        dplyr::select(tag, filter, value) |>
        stats::na.omit() |>
        base::unique() |>
        dplyr::group_by(tag, filter) |>
        dplyr::summarise(choices = base::list(value)) |>
        dplyr::bind_rows(commontags)
      
      document <- documents |>
        dplyr::filter(file == selected_file)
      
      filename <- document$file[1]
      
      document <- document |>
        dplyr::select(-file,-code,-language,-translations,-modified) |>
        base::t() |>
        base::as.data.frame() |>
        dplyr::rename(input = V1) |>
        tibble::rownames_to_column("tag") |>
        dplyr::full_join(preptags, by = "tag")
      
      ui <- base::list()
      
      for (i in 1:base::nrow(document)){
        
        if (document$tag[[i]] == "tag_outcome")
          possible_choices <- outcomes else
            possible_choices <- base::unlist(document$choices[[i]])
        
        if (document$filter[[i]] == "selection"){
          ui[[i]] <- shiny::selectInput(
            inputId = ns(document$tag[[i]]),
            label = document$tag[[i]],
            choices = possible_choices,
            selected = base::unlist(stringr::str_split(document$input[[i]], " ")),
            multiple = FALSE,
            width = "100%"
          )
        } else if (document$filter[[i]] == "multiple"){
          ui[[i]] <- shiny::selectInput(
            inputId = ns(document$tag[[i]]),
            label = document$tag[[i]],
            choices = possible_choices,
            selected = base::unlist(stringr::str_split(document$input[[i]], " ")),
            multiple = TRUE,
            width = "100%"
          )
        } else if (document$filter[[i]] %in% c("value","range")){
          if (document$input[[i]] == "" | document$input[[i]] == "NA"){
            val <- 0
          } else val <- base::as.numeric(document$input[[i]])
          ui[[i]] <- shiny::numericInput(
            inputId = ns(document$tag[[i]]),
            label = document$tag[[i]],
            value = val,
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
    
    output$opendefexui <- shiny::renderUI({
      shiny::req(!base::is.null(document_to_edit()))
      if (document_to_edit()$type[[1]] %in% c("Presentation","Script","Page","Paper")){
        lab <- "Edit definitions"
      } else {
        lab <- "Edit exercices"
      }
      shiny::actionButton(
        ns("opendefex"), label = lab, icon = shiny::icon("edit"),
        style = "background-color:#006699;color:#FFF;width:100%;height:115px;margin-top:10px;"
      )
    })
    
    shiny::observeEvent(input$opendefex, {
      shiny::req(!base::is.null(document_to_edit()))
      if (document_to_edit()$type[[1]] %in% c("Presentation","Script","Page","Paper")){
        pathfile <- base::paste0(course_paths()$subfolders$databases, "/definitions.xlsx")
      } else {
        pathfile <- base::paste0(course_paths()$subfolders$databases, "/exercises.xlsx")
      }
      if (base::file.exists(pathfile)){
        if (base::Sys.info()[1] == "Windows"){
          base::shell.exec(pathfile)
        } else {
          base::system2(pathfile)
        }
      } else {
        shinyalert::shinyalert(
          "Non-existing file", "It seems that the file you are trying to open does not exist. Did you already create it?",
          type = "error"
        )
      }
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
            2,
            shiny::actionButton(
              ns("docinrstudio"), "RStudio",
              icon = shiny::icon("r-project"),
              style = "background-color:#003366;color:#FFF;
                width:100%;margin-bottom:10px;"
            )
          ),
          shiny::column(
            2,
            shiny::actionButton(
              ns("docrefresh"), "Refresh",
              icon = shiny::icon("rotate"),
              style = "background-color:#006699;color:#FFF;
                width:100%;margin-bottom:10px;"
            )
          ),
          shiny::column(
            2,
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
              ns("docpreview"), "Preview", icon = shiny::icon("print"),
              style = "background-color:#660033;color:#FFF;
                width:100%;margin-bottom:10px;"
            )
          ),
          shiny::column(
            3,
            shiny::actionButton(
              ns("openpreview"), "Open file", icon = shiny::icon("eye"),
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
      if (document_to_edit()$type[[1]] %in%
          c("Statements","Alternatives","Computation","Essay","Problem")){
        shinybusy::show_modal_spinner(
          spin = "orbit",
          text = "Preparing the document..."
        )
        exams2forms::exams2webquiz(
          file = document_to_edit()$file[[1]],
          name = stringr::str_remove(document_to_edit()$file[[1]], ".Rmd$"),
          title = "Preview",
          dir = course_paths()$subfolders$preview,
          edir = course_paths()$subfolders$original,
          check = TRUE, box = TRUE, solution = TRUE
        )
        shinybusy::remove_modal_spinner()
      } else {
        editR::view_document(document_to_edit(),TRUE,course_paths)
      }
    })
    
    
    shiny::observeEvent(input$openpreview, {
      shiny::req(!base::is.null(document_to_edit()))
      shiny::req(base::file.exists(document_to_edit()$preview[[1]]))
      utils::browseURL(document_to_edit()$preview[[1]])
    })
    
    
    output$previewdoc <- shiny::renderUI({
      shiny::req(!base::is.null(document_to_edit()))
      shiny::req(base::file.exists(document_to_edit()$preview[[1]]))
      lines <- base::readLines(document_to_edit()$preview[[1]])
      shiny::tags$iframe(srcdoc = shiny::HTML(lines), width = "100%", height = 600, seamless="seamless")
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
      shiny::req(!base::is.null(prefix()))
      shiny::req(!base::is.null(document_to_edit()))
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
          "exextra[type]:", document_to_edit()$type[[1]], "  ",
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
    

    # Publish document #########################################################

    shiny::observeEvent(input$publishdocs, {
      if (document_to_edit()$type[[1]] == "Presentation"){
        editR::publish_presentation(tree, tbltree(), selected_document(), course_paths())
      } else if (document_to_edit()$type[[1]] == "Script"){
        editR::publish_script(selected_document(), course_paths())
      } else if (document_to_edit()$type[[1]] == "Page"){
        editR::publish_textbook(tree, tbltree(), course_paths(), course_data()$languages)
      } else if (document_to_edit()$type[[1]] == "Paper"){
        editR::publish_paper(selected_document(), course_paths())
      } else if (document_to_edit()$type[[1]] == "Question"){
        shinyalert::shinyalert(
          "Go to test", "Questions can only be published in tests.",
          type = "warning"
        )
      }
    })
    
    
    # Open folder ##############################################################
    
    shiny::observeEvent(input$openfolder, {
      if (document_to_edit()$type[[1]] == "Presentation"){
        folder <- course_paths()$subfolders$presentations
      } else if (document_to_edit()$type[[1]] == "Script"){
        folder <- course_paths()$subfolders$videos
      } else if (document_to_edit()$type[[1]] == "Page"){
        folder <- course_paths()$subfolders$textbooks
      } else if (document_to_edit()$type[[1]] == "Paper"){
        folder <- course_paths()$subfolders$papers
      } else if(document_to_edit()$type[[1]] == "Question"){
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


  })
}

