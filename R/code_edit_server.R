#' @name code_edit_server
#' @title Edit functions, templates, and styles
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation or modification of functions, templates, or stylesheets used as or on documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Save the new or modified function in the appropriate function, template, or edition folder.
#' @importFrom rstudioapi navigateToFile
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny icon
#' @importFrom shiny modalButton
#' @importFrom shiny modalDialog
#' @importFrom shiny moduleServer
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny removeModal
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shiny showModal
#' @importFrom shiny textInput
#' @importFrom shinyAce aceEditor
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shinyalert shinyalert
#' @importFrom stringr str_detect
#' @export


code_edit_server <- function(id, course_paths){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    output$selectsubtype <- shiny::renderUI({
      shiny::req(base::length(course_paths()) == 2)
      shiny::req(!base::is.null(input$codetype))
      if (input$codetype == "Function"){
        shinyWidgets::radioGroupButtons(
          inputId = ns("subtype"),label = "Sub-type:", 
          choices = "Function",
          status = "danger", justified = TRUE, size = "sm",
          checkIcon = base::list(yes = shiny::icon("check"))
        )
      } else if (input$codetype == "Document"){
        shinyWidgets::radioGroupButtons(
          inputId = ns("subtype"),label = "Sub-type:", 
          choices = c("Note","Slide","Video","Page","Tutorial","Game","Case","Question"),
          status = "danger", justified = TRUE, size = "sm",
          checkIcon = base::list(yes = shiny::icon("check"))
        )
      } else if (input$codetype == "Report"){
        shinyWidgets::radioGroupButtons(
          inputId = ns("subtype"),label = "Sub-type:", 
          choices = c("Feedback","Analysis"),
          status = "danger", justified = TRUE, size = "sm",
          checkIcon = base::list(yes = shiny::icon("check"))
        )
      } else {
        shinyWidgets::radioGroupButtons(
          inputId = ns("subtype"),label = "Sub-type:", 
          choices = c("tex","css","js"),
          status = "danger", justified = TRUE, size = "sm",
          checkIcon = base::list(yes = shiny::icon("check"))
        )
      }
    })
    
    
    folder_path <- shiny::reactive({
      shiny::req(!base::is.null(input$subtype))
      base::switch(
        input$subtype,
        Function = course_paths()$subfolders$functions,
        Note = course_paths()$subfolders$templates_note,
        Page = course_paths()$subfolders$templates_page,
        Slide = course_paths()$subfolders$templates_slide,
        Video = course_paths()$subfolders$templates_video,
        Game = course_paths()$subfolders$templates_game,
        Tutorial = course_paths()$subfolders$templates_tutorial,
        Case = course_paths()$subfolders$templates_case,
        Question = course_paths()$subfolders$templates_question,
        Feedback = course_paths()$subfolders$templates_report,
        Analysis = course_paths()$subfolders$templates_report,
        tex = course_paths()$subfolders$exams,
        css = course_paths()$subfolders$css,
        js = course_paths()$subfolders$js
      )
    })
    
    code_list <- shiny::reactive({
      shiny::req(!base::is.null(folder_path()))
      codelist <- base::list.files(folder_path(), full.names = FALSE)
      if (input$subtype == "Feedback"){
        codelist <- codelist[stringr::str_detect(codelist, "feedback_.._")]
        codelist <- codelist[stringr::str_detect(codelist, ".Rmd$")]
      }
      if (input$subtype == "Analysis"){
        codelist <- codelist[stringr::str_detect(codelist, "analysis_.._")]
        codelist <- codelist[stringr::str_detect(codelist, ".qmd$")]
      }
      if (input$subtype == "tex"){
        codelist <- codelist[stringr::str_detect(codelist, base::paste0(input$subtype, "$"))]
      }
      codelist
    })
    
    selected_code <- editR::selection_server("slctcode", code_list)
    
    code_path <- shiny::reactive({
      shiny::req(!base::is.null(selected_code()))
      base::paste0(folder_path(), "/", selected_code())
    })
    
    output$editcode <- shiny::renderUI({
      shiny::req(!base::is.null(code_path()))
      shiny::req(base::file.exists(code_path()))
      input$refreshcode
      code <- base::readLines(code_path())
      codemode <- base::switch(
        input$subtype,
        Function = "r",
        Note = "markdown",
        Slide = "markdown",
        Video = "markdown",
        Page = "markdown",
        Tutorial = "markdown",
        Game = "markdown",
        Case = "markdown",
        Question = "markdown",
        Report = "markdown",
        tex = "tex",
        css = "css",
        js = "javascript"
      )
      shinyAce::aceEditor(
        outputId = ns("editedcode"), value = code, mode = codemode,
        wordWrap = TRUE, debounce = 10, autoComplete = "live", height = "700px"
      )
    })
    
    shiny::observeEvent(input$savecode, {
      shiny::req(!base::is.null(input$editedcode))
      base::writeLines(input$editedcode, code_path(), useBytes = TRUE)
      shinyalert::shinyalert(
        "Code saved!", "You can continue coding.",
        type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
      )
    })
    
    shiny::observeEvent(input$codeinrstudio, {
      shiny::req(!base::is.null(code_path()))
      shiny::req(base::file.exists(code_path()))
      rstudioapi::navigateToFile(code_path())
    })
    
    shiny::observeEvent(input$deletecode, {
      shiny::req(!base::is.null(code_path()))
      shiny::req(base::file.exists(code_path()))
      shinyalert::shinyalert(
        "Are you sure?",
        base::paste0(
          "Are you sure you want to delete the code ", selected_code(), "?"
        ),
        type = "warning", closeOnEsc = FALSE, closeOnClickOutside = TRUE,
        inputId = "confirmdeletecode", showCancelButton = TRUE
      )
    })
    
    shiny::observeEvent(input$confirmdeletecode, {
      shiny::req(!base::is.null(code_path()))
      shiny::req(base::file.exists(code_path()))
      if (input$confirmdeletecode){
        base::unlink(code_path())
        shinyalert::shinyalert(
          "Code deleted",
          base::paste0("The code for ", input$deletecode," has been deleted. Reload the course to refresh lists."),
          type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      }
    })
    
    shiny::observeEvent(input$newcode, {
      shiny::req(!base::is.null(folder_path()))
      shiny::req(!base::is.null(selected_code()))
      codelist <- base::list.files(folder_path(), full.names = FALSE)
      shiny::showModal(
        shiny::modalDialog(
          style = "background-color:#001F3F;color:#FFF;margin-top:300px;",
          shiny::textInput(
            ns("newcodename"), "Name of the new code:", value = "",
            width = "100%"
          ),
          shiny::selectInput(
            ns("slctcodebasis"), "Based on the following code:",
            choices = codelist, selected = selected_code(), width = "100%"
          ),
          footer = tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              ns("createcode"), "OK", icon = shiny::icon("check"),
              style = "background-color:#007777;color:#FFF;"
            )
          )
        )
      )
    })
    
    shiny::observeEvent(input$createcode, {
      shiny::removeModal()
      shiny::req(!base::is.null(folder_path()))
      shiny::req(!base::is.null(input$slctcodebasis))
      shiny::req(!base::is.null(input$newcodename))
      shiny::req(input$newcodename != "")
      codelist <- base::list.files(folder_path(), full.names = FALSE)
      newcodename <- input$newcodename
      if (newcodename %in% codelist){
        shinyalert::shinyalert(
          "Name already used!",
          base::paste0("Sorry, but a code named ", newcodename, " already exists. Please use a different name."),
          type = "error", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      } else {
        if (input$slctcodebasis == "") lines <- "" else {
          lines = base::readLines(base::paste0(folder_path(), "/", input$slctcodebasis))
        }
        base::writeLines(
          lines,
          base::paste0(folder_path(), "/", newcodename), useBytes = TRUE
        )
        shinyalert::shinyalert(
          "Code created!",
          base::paste0(
            "The code ", newcodename, " has been created. Reload the course to refresh lists."
          ),
          type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      }
    })
    
  })
}

