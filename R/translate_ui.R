#' @name translate_ui
#' @title Translate documents
#' @author Nicolas Mangin
#' @description Module facilitating the translation of documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Create and save documents' translations in a dedicated folder.
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny NS
#' @importFrom shiny uiOutput
#' @importFrom shinyWidgets checkboxGroupButtons
#' @export


translate_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::uiOutput(ns("slctdocument"))
      ),
      shiny::column(
        3,
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("translationstatus"), label = "Status",
          choices = c("Existing", "Missing"),
          selected = c("Existing", "Missing"),
          justified = TRUE, status = "primary",
          checkIcon = base::list(
            yes = shiny::icon("check"),
            no = shiny::icon("xmark")
          )
        )
      ),
      shiny::column(
        2,
        shiny::uiOutput(ns("slctlanguage"))
      ),
      shiny::column(
        2,
        shiny::uiOutput(ns("translationaction"))
      ),
      shiny::column(
        2,
        shiny::actionButton(
          ns("opentransfolder"), "Open folder", icon = shiny::icon("folder-open"),
          style = "background-color:#660033;color:#FFF;
          width:100%;margin-top:25px;"
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(4, shiny::uiOutput(ns("vieworiginal"))),
      shiny::column(4, shiny::uiOutput(ns("viewtranslation"))),
      shiny::column(4, shiny::uiOutput(ns("edittranslation")))
    ),
    shiny::tags$hr(),
    shiny::fluidRow(
      shiny::actionButton(
        ns("saveproptranslation"),
        "Save", icon = shiny::icon("floppy-disk"),
        style = "background-color:#063;color:#FFF;width:300px;"
      ),
      shiny::tags$hr(),
      rhandsontable::rHandsontableOutput(ns("translatepropositions"))
    )
  )
}

