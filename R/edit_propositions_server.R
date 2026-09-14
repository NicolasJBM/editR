#' @name edit_propositions_server
#' @title Edit propositions
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation or modification of propositions.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param filtered Reactive. List of pre-selected documents.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Interface to edit propositions and their translations
#' @importFrom dplyr anti_join
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr select
#' @importFrom editR name_new_item
#' @importFrom rhandsontable hot_col
#' @importFrom rhandsontable hot_cols
#' @importFrom rhandsontable hot_context_menu
#' @importFrom rhandsontable hot_to_r
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny icon
#' @importFrom shiny isolate
#' @importFrom shiny moduleServer
#' @importFrom shiny numericInput
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shinyWidgets checkboxGroupButtons
#' @importFrom shinyWidgets virtualSelectInput
#' @importFrom shinyalert shinyalert
#' @importFrom shinydashboardPlus box
#' @importFrom tibble tibble
#' @export


edit_propositions_server <- function(
    id, filtered, course_data, course_paths
){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    code <- NULL
    document <- NULL
    explanation <- NULL
    item <- NULL
    keywords <- NULL
    language <- NULL
    modifications <- NULL
    proposition <- NULL
    retire <- NULL
    slctcode <- NULL
    type <- NULL
    value <- NULL
    
    propositions <- shiny::reactive({
      shiny::req(base::length(course_paths()) == 2)
      input$refreshprop
      input$acknowledgesaveprop
      base::load(course_paths()$databases$propositions)
      propositions
    })
    
    translations <- shiny::reactive({
      shiny::req(base::length(course_paths()) == 2)
      input$refreshprop
      input$acknowledgesaveprop
      base::load(course_paths()$databases$translations)
      translations
    })
    
    selected_propositions <- shiny::reactive({
      shiny::req(!base::is.null(propositions()))
      shiny::req(!base::is.null(filtered()))
      selected_documents <- base::unique(c(
        filtered()$code,
        base::unique(filtered()$document) |>
          stringr::str_split(" ") |>
          base::unlist() |>
          base::as.character()
      ))
      propositions() |>
        dplyr::filter(code %in% selected_documents | document %in% selected_documents)
    })
    
    selected_translations <- shiny::reactive({
      shiny::req(!base::is.null(selected_propositions()))
      translations() |>
        dplyr::filter(item %in% selected_propositions()$item)
    })
    
    output$selectprop <- shiny::renderUI({
      shiny::req(!base::is.null(selected_propositions()))
      
      sorting_options <- c("document","value","proposition","item")
      
      codes <- base::unique(selected_propositions()$code, filtered()$code)
      if (base::length(codes) > 8){
        selectcodes <- shinyWidgets::virtualSelectInput(
          inputId = ns("slctpropcode"),
          label = "Select codes:", 
          choices = codes,
          selected = codes,
          multiple = TRUE,
          width = "100%"
        )
      } else {
        selectcodes <- shinyWidgets::checkboxGroupButtons(
          inputId = ns("slctpropcode"), 
          label = "Select codes:",
          choices = codes,
          selected = codes,
          status = "success",
          justified = FALSE,
          direction = "horizontal",
          size = "normal",
          checkIcon = base::list(yes = shiny::icon("check"))
        )
      }
      
      documents <- base::unique(selected_propositions()$document) |>
        stringr::str_split(" ") |>
        base::unlist() |>
        base::as.character() |>
        base::unique()
      if (base::length(documents) > 8){
        selectdocuments <- shinyWidgets::virtualSelectInput(
          inputId = ns("slctpropdoc"),
          label = "Select documents:", 
          choices = documents,
          selected = documents,
          multiple = TRUE, 
          width = "100%"
        )
      } else {
        selectdocuments <- shinyWidgets::checkboxGroupButtons(
          inputId = ns("slctpropdoc"),
          label = "Select documents:", 
          choices = documents,
          selected = documents,
          status = "success",
          justified = FALSE,
          direction = "horizontal",
          size = "normal",
          checkIcon = base::list(yes = shiny::icon("check"))
        )
      }
      
      types <- base::unique(selected_propositions()$type)
      values <- base::unique(selected_propositions()$value)
      scales <- base::unique(selected_propositions()$scale)
      items <- base::unique(selected_propositions()$item)
      if (base::length(items) > 8){
        selectitems <- shinyWidgets::virtualSelectInput(
          inputId = ns("slctitems"),
          label = "Select items:", 
          choices = items,
          selected = items,
          multiple = TRUE, 
          width = "100%"
        )
      } else {
        selectitems <- shinyWidgets::checkboxGroupButtons(
          inputId = ns("slctitems"),
          label = "Select items:", 
          choices = items,
          selected = items,
          status = "success",
          justified = FALSE,
          direction = "horizontal",
          size = "normal",
          checkIcon = base::list(yes = shiny::icon("check"))
        )
      }
      
      languages <- base::unique(translations()$language)
      
      shinydashboardPlus::box(
        width = 12, title = "Selection", solidHeader = TRUE,
        status = "purple", collapsible = FALSE, collapsed = FALSE,
        height = "250px",
        
        shiny::selectInput(
          ns("sortby"), "Sort by:",
          choices = sorting_options,
          selected = sorting_options,
          multiple = TRUE
        ),
        shiny::numericInput(
          ns("itmnbr"), "Additional items:",
          min = 1, max = 100, step = 1 ,value = 1
        ),
        shiny::actionButton(
          ns("saveprop"), "Save propositions",
          icon = shiny::icon("floppy-disk"),
          style = "background-color:#006600;color:#FFF;width:100%"
        ),
        
        shiny::tags$hr(),
        selectcodes,
        selectdocuments,
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("slctproptype"),
          label = "Select types:", 
          choices = types,
          selected = types,
          status = "success",
          justified = FALSE,
          direction = "horizontal",
          size = "normal",
          checkIcon = base::list(yes = shiny::icon("check"))
        ),
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("slctpropvalue"),
          label = "Select values:", 
          choices = values,
          selected = values,
          status = "success",
          justified = FALSE,
          direction = "horizontal",
          size = "normal",
          checkIcon = base::list(yes = shiny::icon("check"))
        ),
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("slctpropscale"),
          label = "Select scales:", 
          choices = scales,
          selected = scales,
          status = "success",
          justified = FALSE,
          direction = "horizontal",
          size = "normal",
          checkIcon = base::list(yes = shiny::icon("check"))
        ),
        selectitems,
        
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("slctproplanguages"),
          label = "Select languages:", 
          choices = languages,
          selected = languages,
          status = "success",
          justified = FALSE,
          direction = "horizontal",
          size = "normal",
          checkIcon = base::list(yes = shiny::icon("check"))
        )
      )
    })
    
    propositions_to_edit <- shiny::reactive({
      shiny::req(!base::is.null(selected_propositions()))
      shiny::req(!base::is.null(input$slctpropcode))
      shiny::req(!base::is.null(input$slctpropdoc))
      shiny::req(!base::is.null(input$slctproptype))
      shiny::req(!base::is.null(input$slctpropvalue))
      shiny::req(!base::is.null(input$slctpropscale))
      shiny::req(!base::is.null(input$slctitems))
      shiny::req(!base::is.null(input$sortby))
      
      toedit <- selected_propositions() |>
        dplyr::filter(
          code %in% c(NA,input$slctpropcode),
          document %in% c(NA,input$slctpropdoc),
          type %in% c(NA,input$slctproptype),
          value %in% c(NA,base::as.numeric(input$slctpropvalue)),
          scale %in% c(NA,input$slctpropscale),
          item %in% c(NA,input$slctitems)
        )
      
      if ("document" %in% input$sortby){
        toedit <- dplyr::arrange(toedit, document)
      }
      
      if ("value" %in% input$sortby){
        toedit <- dplyr::arrange(toedit, value)
      }
      
      if ("proposition" %in% input$sortby){
        toedit <- dplyr::arrange(toedit, proposition)
      }
      
      if ("item" %in% input$sortby){
        toedit <- dplyr::arrange(toedit, item)
      }
      
      toedit
    })
    
    translations_to_edit <-  shiny::reactive({
      shiny::req(!base::is.null(translations()))
      shiny::req(!base::is.null(propositions_to_edit()))
      shiny::req(!base::is.null(input$slctproplanguages))
      
      translations() |>
        dplyr::filter(
          item %in% propositions_to_edit()$item,
          language %in% input$slctproplanguages
        ) |>
        dplyr::mutate(item = base::factor(item, levels = propositions_to_edit()$item)) |>
        dplyr::arrange(item) |>
        dplyr::mutate(item = base::as.character(item))
    })
    
    output$editprop <- rhandsontable::renderRHandsontable({
      shiny::req(!base::is.null(propositions_to_edit()))
      
      existing_items <- propositions() |>
        dplyr::select(item) |> base::unlist() |>
        base::as.character() |> base::unique()
      newitemid <- editR::name_new_item(existing_items, input$itmnbr)
      
      levelcode <- c(NA, base::unique(propositions_to_edit()$code), base::unique(base::setdiff(filtered()$code, propositions_to_edit()$code)))
      leveltype <- c(NA, "Statements","Alternatives","Computation","Essay","Problem")
      levellanguage <- propositions()$language[[1]]
      levelscale <- c(NA, "logical","percentage","qualitative")
      leveldoc <- c(NA, base::unique(propositions_to_edit()$document), base::setdiff(course_data()$documents$code, base::unique(propositions_to_edit()$document)))
      
      tmprow <- tibble::tibble(
        item = newitemid,
        language = base::factor(levellanguage[1], levels = levellanguage),
        code = base::factor(levelcode[1], levels = levelcode),
        type = base::factor(leveltype[1], levels = leveltype),
        document = base::factor(leveldoc[1], levels = leveldoc),
        modifications = 1,
        proposition = base::as.character(NA),
        value = 0,
        scale = base::factor(levelscale[1], levels = levelscale),
        explanation = base::as.character(NA),
        keywords = base::as.character(NA),
        retire = FALSE,
        answers = base::as.numeric(NA),
        success = base::as.numeric(NA),
        discrimination = base::as.numeric(NA)
      )
      
      if (base::nrow(propositions_to_edit()) > 0){
        itemsublist <- propositions_to_edit() |>
          dplyr::mutate(
            code = base::factor(code, levels = levelcode),
            type = base::factor(type, levels = leveltype),
            document = base::factor(document, levels = leveldoc),
            scale = base::factor(scale, levels = levelscale)
          )
        
        itemsublist <- itemsublist |>
          #dplyr::left_join(
          #  course_data()$item_parameters,
          #  by = c("item","language")
          #) |>
          dplyr::select(
            item, language, code, type, document, modifications, proposition,
            value, scale, explanation, keywords, retire#, answers, success, discrimination
          ) |>
          dplyr::bind_rows(tmprow)
      } else {
        itemsublist <- tmprow
      }
      
      itemsublist |>
        rhandsontable::rhandsontable(
          height = 750, width = "100%", rowHeaders = NULL, stretchH = "all"
        ) |>
        rhandsontable::hot_col(c(1,2,13,14,15), readOnly = TRUE) |>
        rhandsontable::hot_cols(
          colWidths = c(
            "6%","2%","6%","6%","7%","3%","18%","3%",
            "5%","22%","10%","3%","3%","3%","3%"
          ),
          manualColumnResize = TRUE
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = FALSE, allowColEdit = FALSE
        )
    })
    
    shiny::observeEvent(input$saveprop, {
      shiny::req(!base::is.null(input$editprop))
      modified <- rhandsontable::hot_to_r(input$editprop) |>
        dplyr::mutate_if(base::is.factor, base::as.character)
      
      if (base::is.na(modified[base::nrow(modified), "proposition"])){
        modified <- modified[-base::nrow(modified),]
      }
      
      propositions <- shiny::isolate({ propositions() })
      
      modified <- modified |>
        dplyr::select(base::names(propositions))
      
      not_modified <- propositions |>
        dplyr::anti_join(modified, by = c("item","language"))
      
      propositions <- not_modified |>
        dplyr::bind_rows(modified) |>
        dplyr::filter(!base::is.na(type), !base::is.na(value)) |>
        dplyr::arrange(item) |>
        dplyr::filter(proposition != "", !base::is.na(proposition))
      
      base::save(propositions, file = course_paths()$databases$propositions)
      
      base::Sys.sleep(1)
      
      shinyalert::shinyalert(
        "Propositions saved!", "Refresh to see changes.",
        type = "success", inputId = "acknowledgesaveprop"
      )
    })
    
  })
}

