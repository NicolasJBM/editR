#' @name compare_ui
#' @title Compare documents
#' @author Nicolas Mangin
#' @description Module facilitating the comparison and edition of documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Saved edited documents.
#' @importFrom diffr diffrOutput
#' @importFrom shiny actionButton
#' @importFrom shiny checkboxInput
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny NS
#' @importFrom shiny numericInput
#' @importFrom shiny uiOutput
#' @importFrom shiny wellPanel
#' @importFrom shiny selectInput
#' @export


compare_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        4,
        shinydashboardPlus::box(
          width = 12, title = "Filters", solidHeader = TRUE,
          status = "navy", collapsible = FALSE, collapsed = FALSE,
          height = "700px",
          shiny::selectInput(
            ns("slctcompfold"), "Select the comparison folder:",
            choices = "", selected = "", width = "100%"
          ),
          shiny::checkboxInput(ns("keeprecent"), "Keep only more recent files."),
          shiny::column(12, editR::selection_ui(ns("selectfile"))),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::numericInput(
                ns("slctcontextSize"),
                "Context size:",
                min = 1, max = 5,
                value = 1, width = "100%"
              )
            ),
            shiny::column(
              6,
              shiny::numericInput(
                ns("slctminJumpSize"),
                "Jump size:",
                min = 1, max = 20,
                value = 1, width = "100%"
              )
            )
        ),
        shiny::checkboxInput(
          ns("slctwordwrap"),
          "Word wrap", value = TRUE,
          width = "100%"
        )
        )
      ),
      shiny::column(
        8,
        diffr::diffrOutput(ns("comparison"), width = "100%", height = "500px")
      )
    ),
    shiny::fluidRow(
      
    )
  )
}

