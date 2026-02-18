#' @name translate_ui
#' @title Translate documents
#' @author Nicolas Mangin
#' @description Module facilitating the translation of documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Create and save documents' translations in the dedicated basis subfolder.
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny uiOutput
#' @export


translate_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        8,
        editR::selection_ui(ns("selectdoc"))
      ),
      shiny::column(
        2,
        shiny::actionButton(
          ns("createnewtranslation"),"New",
          icon = shiny::icon("wand-magic-sparkles"),
          style = "background-color:#000066;color:#FFF;width:100%;height:115px;margin-bottom:10px;"
        )
      ),
      shiny::column(
        2,
        shiny::uiOutput(ns("slctlanguage")),
        shinyWidgets::materialSwitch(
          inputId = ns("maketranslation"),
          label = "Pre-translate", 
          status = "primary",
          right = FALSE
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(4, shiny::uiOutput(ns("editoriginal"))),
      shiny::column(4, shiny::uiOutput(ns("edittranslation"))),
      shiny::column(
        4,
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

