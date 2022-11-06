#' @name selection_server
#' @title Sleect from a vector
#' @author Nicolas Mangin
#' @description Module facilitating the selection of a specific value in a vector.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param selection_base Character vector. List of values from which to choose.
#' @return A list of course data.
#' @importFrom shiny moduleServer
#' @importFrom shiny NS
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny updateSelectInput
#' @importFrom shinyWidgets progressBar
#' @export



selection_server <- function(id, selection_base){
  ns <- shiny::NS(id)
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      modrval <- shiny::reactiveValues()
      shiny::observe({
        shiny::updateSelectInput(
          session, "slctvalue",
          choices = selection_base(),
          selected = selection_base()[1]
        )
      })
      
      shiny::observeEvent(input$prv, {
        shiny::req(base::length(selection_base()) > 0)
        shiny::updateSelectInput(
          session, "slctvalue",
          selected = selection_base()[base::max(c(1, modrval$rank-1))]
        )
      })
      
      output$progression <- shiny::renderUI({
        shiny::req(!base::is.null(selection_base()))
        shiny::req(!base::is.null(modrval$rank))
        shiny::req(!base::is.na(modrval$rank))
        shiny::req(base::length(selection_base()) > 1)
        shiny::req(base::length(modrval$rank) > 0)
        shinyWidgets::progressBar(
          id = ns("progressbar"), value = modrval$rank,
          total = base::length(selection_base()), status = "info"
        )
      })
      
      shiny::observeEvent(input$nxt, {
        shiny::req(base::length(selection_base()) > 0)
        shiny::updateSelectInput(
          session, "slctvalue",
          selected = selection_base()[base::max(c(1, modrval$rank+1))]
        )
      })
      
      shiny::observe({
        shiny::req(input$slctvalue != "")
        modrval$rank <- base::match(input$slctvalue, selection_base())
        modrval$value <- input$slctvalue
      })
      
      selection <- shiny::reactive({
        shiny::req(base::length(selection_base()) > 0)
        modrval$value
      })
      
      return(selection)
    }
  )
}
