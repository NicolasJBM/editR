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
      shiny::column(2,shiny::uiOutput(ns("slctlanguage"))),
      shiny::column(
        2,
        shiny::actionButton(
          ns("createnewtranslation"),"New",
          icon = shiny::icon("wand-magic-sparkles"),
          style = "background-color:#000066;color:#FFF;width:100%;height:115px;margin-bottom:10px;"
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("edittranslation"))),
      shiny::column(6, shiny::uiOutput(ns("previewtranslation")))
    )
  )
}

