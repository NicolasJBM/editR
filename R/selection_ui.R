#' @name selection_ui
#' @title Select from a vector
#' @author Nicolas Mangin
#' @description Module facilitating the selection of a specific value in a vector by cycling through elements of a vector.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param selection_label Character. Label to indicate what is being selected.
#' @return The selected element of the supplied vector.
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny selectInput
#' @importFrom shiny uiOutput
#' @export



selection_ui <- function(id, selection_label = "Document:"){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        2,
        shiny::actionButton(
          ns("prv"), "",
          icon = shiny::icon("circle-chevron-left"),
          style = "background-color:#003366;color:#FFF;width:100%;
            height:75px;font-size:25px;text-align:center;padding:0;margin:0;"
        )
      ),
      shiny::column(
        8,
        shiny::selectInput(
          ns("slctvalue"), selection_label,
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
            height:75px;font-size:25px;text-align:center;padding:0;margin:0;"
        )
      )
    ),
    shiny::fluidRow(shiny::column(12, shiny::uiOutput(ns("progression"))))
  )
}
