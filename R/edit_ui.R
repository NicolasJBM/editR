#' @name edit_ui
#' @title Edit documents
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation or modification of documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save the new or modified document in the original documents folder.
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny NS
#' @importFrom shiny uiOutput
#' @export


edit_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("selectdoc"))),
      shiny::column(
        3,
        shiny::actionButton(
          ns("newdoc"), "New", icon = shiny::icon("wand-magic-sparkles"),
          style = "background-color:#003366;color:#FFF;
          width:100%;margin-top:25px;"
        )
      ),
      shiny::column(
        3,
        shiny::actionButton(
          ns("publishdocs"), "Publish", icon = shiny::icon("print"),
          style = "background-color:#330066;color:#FFF;
          width:100%;margin-top:25px;"
        )
      )
    ),
    shiny::uiOutput(ns("ratingsstatistics")),
    shiny::uiOutput(ns("viewsstatistics")),
    shiny::uiOutput(ns("resultsstatistics")),
    shiny::fluidRow(
      shiny::column(6, shiny::uiOutput(ns("viewdoc"))),
      shiny::column(6, shiny::uiOutput(ns("editdoc")))
    )
  )
}

