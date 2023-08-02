#' @name edit_ui
#' @title Edit documents
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation or modification of documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save the new or modified document in the original documents folder.
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny plotOutput
#' @importFrom shiny uiOutput
#' @export


edit_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(10, editR::selection_ui(ns("slctdoc"))),
      shiny::column(
        2,
        shiny::fluidRow(
          shiny::column(
            12,
            shiny::actionButton(
              ns("newdoc"), "New", icon = shiny::icon("wand-magic-sparkles"),
              style = "background-color:#000066;color:#FFF;width:100%;margin-bottom:10px;"
            ),
            shiny::actionButton(
              ns("publishdocs"), "Publish", icon = shiny::icon("print"),
              style = "background-color:#330066;color:#FFF;width:100%;margin-bottom:10px;"
            ),
            shiny::actionButton(
              ns("openfolder"), "Open folder", icon = shiny::icon("folder-open"),
              style = "background-color:#660000;color:#FFF;width:100%;margin-bottom:10px;"
            )
          )
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::uiOutput(ns("pathintree")),
        shiny::tags$hr(),
        shiny::uiOutput(ns("docinfo"))
      ),
      shiny::column(5, shiny::uiOutput(ns("editdoc"))),
      shiny::column(
        4,
        shiny::uiOutput(ns("ratingsstatistics")),
        shiny::uiOutput(ns("viewsstatistics")),
        shiny::uiOutput(ns("resultsstatistics")),
        shiny::plotOutput(ns("questioncurve"))
      )
    ),
    shiny::fluidRow(
      shiny::column(2, shiny::uiOutput(ns("selectprop"))),
      shiny::column(10, rhandsontable::rHandsontableOutput(ns("editprop")))
    )
  )
}

