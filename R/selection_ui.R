#' @name selection_ui
#' @title Sleect from a vector
#' @author Nicolas Mangin
#' @description Module facilitating the selection of a specific value in a vector.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return A list of course data.
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny NS
#' @importFrom shiny selectInput
#' @importFrom shiny uiOutput
#' @export



selection_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        2,
        shiny::actionButton(
          ns("prv"), "",
          icon = shiny::icon("circle-chevron-left"),
          style = "background-color:#003366;color:#FFF;width:100%;
            height:75px;font-size:30px;text-align:center;"
        )
      ),
      shiny::column(
        8,
        shiny::selectInput(
          ns("slctvalue"), "Selected:",
          choices = "", selected = "",
          width = "100%"
        )
      ),
      shiny::column(
        2,
        shiny::actionButton(
          ns("nxt"), "",
          icon = shiny::icon("circle-chevron-right"),
          style = "background-color:#0099CC;color:#FFF;width:100%;
            height:75px;font-size:30px;text-align:center;"
        )
      )
    ),
    shiny::fluidRow(shiny::column(12, shiny::uiOutput(ns("progression"))))
  )
}
