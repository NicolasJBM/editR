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
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom rmarkdown render
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
#' @importFrom shiny reactiveValues
#' @importFrom shiny removeModal
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shiny showModal
#' @importFrom shiny tagList
#' @importFrom shiny textInput
#' @importFrom shinyAce aceEditor
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @importFrom shinydashboardPlus box
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace_all
#' @export


edit_server <- function(
    id, filtered, course_data, tree, course_paths, doctype
){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {

    type <- NULL
    language <- NULL
    section <- NULL
    authors <- NULL



    # Load data ################################################################

    selection <- shiny::reactive({
      shiny::req(!base::is.null(filtered()))
      filtered() |>
        dplyr::filter(type == doctype)
    })

    prefix <- shiny::reactive({
      base::switch(
        doctype,
        Note = "N",
        Page = "P",
        Slide = "S",
        Video = "V",
        Game = "G",
        Case = "C"
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
        Case = course_paths()$subfolders$templates_case
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

    output$viewdoc <- shiny::renderUI({

      shiny::req(input$selecteddoc)
      shiny::req(!base::is.null(selection()))
      shiny::req(input$selecteddoc %in% selection()$file)

      input$savedoc
      input$docrefresh

      selected <- selection() |>
        dplyr::filter(file == input$selecteddoc)

      filepath <- base::paste0(
        course_paths()$subfolders$original, "/", input$selecteddoc
      )
      doc <- base::readLines(filepath)
      doc <- doc[1:(base::match('Meta-information', doc)-1)]


      if (doctype == "Note"){

        yaml <- c(
          '---',
          base::paste0('title: ', selected$title[1]),
          'subtitle: <hr>',
          'output:',
          '  rmarkdown::html_document:',
          '    self_contained: false',
          '    css: ["format/css/notes.css", "format/css/fa.css"]',
          '    fig_width: 8',
          '    fig_height: 6',
          '    fig_caption: true',
          '    mathjax: default',
          'csl: format/csl/apa.csl',
          'bibliography: data/references.bib',
          '---'
        )

      } else if (doctype == "Page"){

        yaml <- c(
          '---',
          base::paste0('title: ', selected$title[1]),
          'subtitle: <hr>',
          'output:',
          '  rmarkdown::html_document:',
          '    self_contained: false',
          '    css: ["format/css/pages.css", "format/css/fa.css"]',
          '    fig_width: 8',
          '    fig_height: 6',
          '    fig_caption: true',
          '    mathjax: default',
          'csl: format/csl/apa.csl',
          'bibliography: data/references.bib',
          '---'
        )

      } else if (doctype == "Slide"){

        if ("tag_authors" %in% base::names(selected)){
          authors <- selected$tag_authors[[1]]
        } else {
          authors <- ""
        }

        yaml <- c(
          '---',
          base::paste0('title: <large> ', selected$title[[1]],' </large>'),
          base::paste0('author: <hr> ', authors),
          base::paste0('date: ', base::format(base::Sys.time(), '%B %d, %Y')),
          'output:',
          '  revealjs::revealjs_presentation:',
          '    self_contained: false',
          '    reveal_plugins: ["menu","chalkboard"]',
          '    incremental: true',
          '    highlight: pygments',
          '    center: true',
          '    transition: slide',
          '    background_transition: slide',
          '    reveal_options:',
          '      showNotes: true',
          '      slideNumber: true',
          '      previewLinks: true',
          '    fig_width: 8',
          '    fig_height: 6',
          '    fig_caption: true',
          '    css: format/css/slides.css',
          '    mathjax: default',
          'csl: format/csl/apa.csl',
          'bibliography: data/references.bib',
          '---'
        )

      } else if (doctype == "Video"){

        yaml <- c(
          '---',
          base::paste0('title: ', selected$title[1]),
          'subtitle: <hr>',
          'output:',
          '  rmarkdown::html_document:',
          '    self_contained: false',
          '    css: ["format/css/videos.css", "format/css/fa.css"]',
          '    fig_width: 8',
          '    fig_height: 6',
          '    fig_caption: true',
          '    mathjax: default',
          'csl: format/csl/apa.csl',
          'bibliography: data/references.bib',
          '---'
        )

      } else if (doctype == "Game"){

        yaml <- c(
          '---',
          base::paste0('title: ', selected$title[1]),
          'subtitle: <hr>',
          'output:',
          '  rmarkdown::html_document:',
          '    self_contained: false',
          '    css: ["format/css/games.css", "format/css/fa.css"]',
          '    fig_width: 8',
          '    fig_height: 6',
          '    fig_caption: true',
          '    mathjax: default',
          'csl: format/csl/apa.csl',
          'bibliography: data/references.bib',
          '---'
        )

      } else {

        yaml <- c(
          '---',
          base::paste0('title: ', selected$title[1]),
          'subtitle: <hr>',
          'output:',
          '  rmarkdown::html_document:',
          '    self_contained: false',
          '    css: ["format/css/cases.css", "format/css/fa.css"]',
          '    fig_width: 8',
          '    fig_height: 6',
          '    fig_caption: true',
          '    mathjax: default',
          'csl: format/csl/apa.csl',
          'bibliography: data/references.bib',
          '---'
        )

      }

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
        shinydashboardPlus::box(
          width = 12, title = title, solidHeader = TRUE, status = "primary",
          collapsible = FALSE, collapsed = FALSE, height = "550px",
          shiny::tags$iframe(src="temporary/tmpdoc.html", height = 520, width = "100%")
        )
      } else {
        shinydashboardPlus::box(
          width = 12, title = title, solidHeader = TRUE, status = "primary",
          collapsible = FALSE, collapsed = FALSE, height = "750px",
          shiny::tags$iframe(src="temporary/tmpdoc.html", height = 750, width="100%")
        )
      }
    })



    # Edit document ############################################################

    output$editdoc <- shiny::renderUI({
      shiny::req(input$selecteddoc)
      shiny::req(!base::is.null(selection()))
      input$docrefresh
      if (input$selecteddoc %in% selection()$file){
        lines <- base::readLines(base::paste0(
          course_paths()$subfolders$original, "/", input$selecteddoc
        ))
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
      } else NULL
    })

    shiny::observeEvent(input$savedoc, {
      selected_doc <- shiny::isolate({ input$selecteddoc })
      edited_doc <- shiny::isolate({ input$editeddoc })
      shiny::req(selected_doc != "")
      shiny::req(!base::is.null(edited_doc))
      base::writeLines(
        input$editeddoc,
        base::paste0(course_paths()$subfolders$original, "/", selected_doc),
        useBytes = TRUE
      )
    })

    shiny::observeEvent(input$docinrstudio, {
      shiny::req(input$selecteddoc != "")
      rstudioapi::navigateToFile(
        base::paste0(course_paths()$subfolders$original, "/", input$selecteddoc)
      )
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

      }

    })


  })
}

