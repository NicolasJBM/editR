#' @name edit_propositions_ui
#' @title Edit documents
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation or modification of documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save the new or modified document in the original documents folder.
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shiny NS
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny uiOutput
#' @importFrom shinydashboardPlus box
#' @export


edit_propositions_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(2, shiny::uiOutput(ns("selectprop"))),
      shiny::column(
        10,
        shinydashboardPlus::box(
          width = 12, title = "Selection", solidHeader = TRUE,
          status = "primary", collapsible = TRUE, collapsed = FALSE,
          rhandsontable::rHandsontableOutput(ns("editprop"))
        )
      )
    ),
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

