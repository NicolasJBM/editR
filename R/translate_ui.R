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
        6,
        editR::selection_ui(ns("selectdoc"))
      ),
      shiny::column(4, shiny::uiOutput(ns("slctlanguage"))),
      shiny::column(
        2,
        shiny::actionButton(
          ns("createnewtranslation"),"New",
          icon = shiny::icon("wand-magic-sparkles"),
          style = "background-color:#000066; color:#FFF; width:100%;margin-bottom:10px;"
        ),
        shiny::actionButton(
          ns("publishtranslation"), "Publish", icon = shiny::icon("print"),
          style = "background-color:#330066;color:#FFF;width:100%;margin-bottom:10px;"
        ),
        shiny::actionButton(
          ns("opentransfolder"), "Open folder", icon = shiny::icon("folder-open"),
          style = "background-color:#660000;color:#FFF;width:100%;margin-bottom:10px;"
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(1),
      shiny::column(4, shiny::uiOutput(ns("editoriginal"))),
      shiny::column(4, shiny::uiOutput(ns("edittranslation"))),
      shiny::column(
        3,
        shiny::uiOutput(ns("ratingsstatistics")),
        shiny::uiOutput(ns("viewsstatistics")),
        shiny::uiOutput(ns("resultsstatistics"))
      )
    ),
    shiny::tags$hr(),
    shiny::fluidRow(
      shiny::actionButton(
        ns("saveproptranslation"),
        "Save", icon = shiny::icon("floppy-disk"),
        style = "background-color:#006600;color:#FFF;width:300px;"
      ),
      shiny::tags$hr(),
      rhandsontable::rHandsontableOutput(ns("translatepropositions"))
    )
  )
}

