#' @name edit_ui
#' @title Edit documents
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation or modification of documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save the new or modified document in the original documents folder.
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny NS
#' @importFrom shiny plotOutput
#' @importFrom shiny uiOutput
#' @export


edit_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(9, editR::selection_ui(ns("slctdoc"))),
      shiny::column(
        3,
        shiny::actionButton(
          ns("newdoc"), "New", icon = shiny::icon("wand-magic-sparkles"),
          style = "background-color:#003366;color:#FFF;
          width:100%;margin-bottom:10px;"
        ),
        shiny::actionButton(
          ns("publishdocs"), "Publish", icon = shiny::icon("print"),
          style = "background-color:#330066;color:#FFF;
          width:100%;margin-bottom:10px;"
        ),
        shiny::actionButton(
          ns("openfolder"), "Open folder", icon = shiny::icon("folder-open"),
          style = "background-color:#660033;color:#FFF;
          width:100%;margin-bottom:10px;"
        )
      )
    ),
    shiny::uiOutput(ns("ratingsstatistics")),
    shiny::uiOutput(ns("viewsstatistics")),
    shiny::uiOutput(ns("resultsstatistics")),
    shiny::fluidRow(
      shiny::column(12, align="center", shiny::h3(shiny::textOutput(ns("pathintree")))),
      shiny::column(6, shiny::uiOutput(ns("viewdoc"))),
      shiny::column(6, shiny::uiOutput(ns("editdoc")))
    ),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::uiOutput(ns("selectprop")),
        shiny::plotOutput(ns("displaycurve"))
      ),
      shiny::column(9, rhandsontable::rHandsontableOutput(ns("editprop")))
    )
  )
}

