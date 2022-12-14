#' @name code_edit_ui
#' @title Edit functions
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation or modification of functions used in documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save the new or modified function in the folder "1_preparation/functions".
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny NS
#' @importFrom shiny uiOutput
#' @importFrom shinyWidgets radioGroupButtons
#' @export


code_edit_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        4,
        shinyWidgets::radioGroupButtons(
          inputId = ns("codetype"),label = "Type of code", 
          choices = c("Function","Document","Report","Format"),
          status = "primary", justified = TRUE, size = "sm",
          checkIcon = base::list(yes = shiny::icon("check"))
        ),
        shiny::uiOutput(ns("selectsubtype")),
        shiny::column(12, editR::selection_ui(ns("slctcode")))
      ),
      shiny::column(
        6,
        shiny::uiOutput(ns("editcode"))
      ),
      shiny::column(
        2,
        shiny::actionButton(
          ns("savecode"), "Save",
          icon = shiny::icon("floppy-disk"),
          style = "background-color:#006633;color:#FFF;
          width:100%;margin-top:25px;"
        ),
        shiny::actionButton(
          ns("codeinrstudio"), "RStutio",
          icon = shiny::icon("r-project"),
          style = "background-color:#222222;color:#FFF;
          width:100%;margin-top:25px;"
        ),
        shiny::actionButton(
          ns("refreshcode"), "Refresh",
          icon = shiny::icon("rotate"),
          style = "background-color:#000099;color:#FFF;
          width:100%;margin-top:25px;"
        ),
        shiny::actionButton(
          ns("deletecode"), "Delete",
          icon = shiny::icon("trash-can"),
          style = "background-color:#660033;color:#FFF;
          width:100%;margin-top:25px;"
        ),
        shiny::actionButton(
          ns("newcode"), "New",
          icon = shiny::icon("wand-magic-sparkles"),
          style = "background-color:#003366;color:#FFF;
          width:100%;margin-top:25px;"
        )
      )
    )
  )
}

