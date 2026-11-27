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
#' @importFrom dplyr across
#' @importFrom dplyr all_of
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
    outcome <- NULL
    explanation <- NULL
    item <- NULL
    keywords <- NULL
    language <- NULL
    modifications <- NULL
    proposition <- NULL
    translated_proposition <- NULL
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
      input$acknowledgesavetrans
      base::load(course_paths()$databases$translations)
      translations
    })
    
    output$selectprop <- shiny::renderUI({
      
      sorting_options <- c("code","outcome","type","value","proposition","item")
      
      codes <- base::unique(filtered()$code)
      selectcodes <- shinyWidgets::virtualSelectInput(
        inputId = ns("slctpropcode"),
        label = "Select codes:", 
        choices = codes,
        selected = codes,
        multiple = TRUE,
        width = "100%"
      )
      
      outcomes <- base::unique(course_data()$outcomes$outcome) |>
        stringr::str_split(" ") |>
        base::unlist() |>
        base::as.character() |>
        base::unique()
      types <- c("Statements","Alternatives","Computation","Essay","Problem")
      values <- c(0,1)
      scales <- c("logical","percentage","qualitative")
      selectoutcomes <- shinyWidgets::virtualSelectInput(
        inputId = ns("slctpropout"),
        label = "Select outcomes:", 
        choices = outcomes,
        selected = outcomes,
        multiple = TRUE, 
        width = "100%"
      )
      
      shinydashboardPlus::box(
        width = 12, title = "Selection", solidHeader = TRUE,
        status = "purple", collapsible = FALSE, collapsed = FALSE,
        height = "250px",
        shiny::actionButton(
          ns("saveprop"), "Save propositions",
          icon = shiny::icon("floppy-disk"),
          style = "background-color:#006600;color:#FFF;width:100%"
        ),
        shiny::tags$hr(),
        selectcodes,
        selectoutcomes,
        shinyWidgets::virtualSelectInput(
          inputId = ns("slctproptype"),
          label = "Select types:", 
          choices = types,
          selected = types,,
          multiple = TRUE, 
          width = "100%"
        ),
        shinyWidgets::virtualSelectInput(
          inputId = ns("slctpropvalue"),
          label = "Select values:", 
          choices = values,
          selected = values,
          multiple = TRUE, 
          width = "100%"
        ),
        shinyWidgets::virtualSelectInput(
          inputId = ns("slctpropscale"),
          label = "Select scales:", 
          choices = scales,
          selected = scales,
          multiple = TRUE, 
          width = "100%"
        ),
        shiny::selectInput(
          ns("sortby"), "Sort by:",
          choices = sorting_options,
          selected = c("item"),
          multiple = TRUE, 
          width = "100%"
        ),
        shiny::selectInput(
          ns("slctretire"), "Keep:",
          choices = c("All","Active","Retired"),
          selected = "All",
          multiple = FALSE, 
          width = "100%"
        ),
        shiny::numericInput(
          ns("itmnbr"), "Additional items:",
          min = 1, max = 100, step = 1 ,value = 1, 
          width = "100%"
        )
      )
    })
    
    propositions_to_edit <- shiny::reactive({
      shiny::req(!base::is.null(propositions()))
      shiny::req(!base::is.null(input$slctpropcode))
      shiny::req(!base::is.null(input$slctpropout))
      shiny::req(!base::is.null(input$slctproptype))
      shiny::req(!base::is.null(input$slctpropvalue))
      shiny::req(!base::is.null(input$slctpropscale))
      shiny::req(!base::is.null(input$slctretire))
      shiny::req(!base::is.null(input$sortby))
      
      if (input$slctretire == "Active"){
        preselected <- dplyr::filter(propositions(), retire == FALSE)
      } else if (input$slctretire == "Retired"){
        preselected <- dplyr::filter(propositions(), retire == TRUE)
      } else {
        preselected <- propositions()
      }
      
      preselected |>
        dplyr::filter(
          code %in% input$slctpropcode | base::is.na(code) | code == "",
          outcome %in% input$slctpropout | base::is.na(outcome) | outcome == "",
          type %in% input$slctproptype,
          value %in% base::as.numeric(input$slctpropvalue),
          scale %in% input$slctpropscale
        ) |>
        dplyr::arrange(dplyr::across(dplyr::all_of(input$sortby)))
    })
    
    output$editprop <- rhandsontable::renderRHandsontable({
      
      shiny::req(!base::is.null(propositions_to_edit()))
      
      existing_items <- propositions() |>
        dplyr::select(item) |> base::unlist() |>
        base::as.character() |> base::unique()
      
      newitemid <- editR::name_new_item(existing_items, input$itmnbr)
      
      levelcode <- base::unique(filtered()$code)
      leveltype <- c("Statements","Alternatives","Computation","Essay","Problem")
      levellanguage <- filtered()$language[[1]]
      levelscale <- c("logical","percentage","qualitative")
      levelout <- base::unique(course_data()$outcomes$outcome)
      
      tmprow <- tibble::tibble(
        item = newitemid,
        language = base::factor(levellanguage[[1]], levels = levellanguage),
        code = base::factor(levelcode[[1]], levels = levelcode),
        type = base::factor(filtered()$type[[1]], levels = leveltype),
        outcome = base::factor(NA, levels = levelout),
        modifications = 1,
        proposition = base::as.character(NA),
        value = 0,
        scale = base::factor(levelscale[[1]], levels = levelscale),
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
            outcome = base::factor(outcome, levels = levelout),
            scale = base::factor(scale, levels = levelscale)
          )
        
        itemsublist <- itemsublist |>
          #dplyr::left_join(
          #  course_data()$item_parameters,
          #  by = c("item","language")
          #) |>
          dplyr::select(
            item, language, code, type, outcome, modifications, proposition,
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
        dplyr::filter(
          !base::is.na(item),
          item != "",
          proposition != "",
          !base::is.na(proposition),
          !base::is.na(code) | type == "Statements"
        ) |>
        dplyr::mutate(
          language = dplyr::case_when(
            language == "" ~ filtered()$language[[1]],
            base::is.na(language) ~ filtered()$language[[1]],
            TRUE ~ language
          ),
          scale = dplyr::case_when(
            scale == "" ~ "logical",
            base::is.na(scale) ~ "logical",
            TRUE ~ scale
          )
        )
      
      base::save(propositions, file = course_paths()$databases$propositions)
      
      base::Sys.sleep(1)
      
      shinyalert::shinyalert(
        "Propositions saved!", "Refresh to see changes.",
        type = "success", inputId = "acknowledgesaveprop"
      )
    })
    
    
    
    translations_to_edit <-  shiny::reactive({
      shiny::req(!base::is.null(translations()))
      shiny::req(!base::is.null(propositions_to_edit()))
      shiny::req(!base::is.null(input$slctproplanguage))
      propositions_to_edit() |>
        dplyr::select(item) |>
        dplyr::left_join(
          dplyr::filter(
            translations(),
            item %in% propositions_to_edit()$item,
            language %in% input$slctproplanguage
          ),
          by = "item"
        ) |>
        dplyr::mutate(
          item = base::factor(item, levels = propositions_to_edit()$item),
          language = dplyr::case_when(
            base::is.na(language) ~ input$slctproplanguage,
            TRUE ~ language
          )
        ) |>
        dplyr::arrange(item) |>
        dplyr::mutate(item = base::as.character(item))
    })
    
    
    output$translateprop <- rhandsontable::renderRHandsontable({
      shiny::req(!base::is.null(translations_to_edit()))
      
      translations_to_edit() |>
        rhandsontable::rhandsontable(
          height = 750, width = "100%", rowHeaders = NULL, stretchH = "all"
        ) |>
        rhandsontable::hot_col(c(1,2), readOnly = TRUE) |>
        rhandsontable::hot_cols(
          colWidths = c(
            "10%","5%","25%","45%","15%"
          ),
          manualColumnResize = TRUE
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = FALSE, allowColEdit = FALSE
        )
    })
    
    shiny::observeEvent(input$saveproptranslation, {
      shiny::req(!base::is.null(input$translateprop))
      modified <- rhandsontable::hot_to_r(input$translateprop) |>
        dplyr::mutate_if(base::is.factor, base::as.character)
      
      translations <- shiny::isolate({ translations() })
      
      not_modified <- translations |>
        dplyr::anti_join(modified, by = c("item","language"))
      
      translations <- not_modified |>
        dplyr::bind_rows(modified) |>
        dplyr::filter(!base::is.na(item), !base::is.na(language)) |>
        dplyr::arrange(item) |>
        dplyr::filter(
          translated_proposition != "",
          !base::is.na(translated_proposition)
        )
      
      base::save(translations, file = course_paths()$databases$translations)
      
      base::Sys.sleep(1)
      
      shinyalert::shinyalert(
        "Translations saved!", "Refresh to see changes.",
        type = "success", inputId = "acknowledgesavetrans"
      )
    })
    
    
  })
}

