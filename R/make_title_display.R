#' @name make_title_display
#' @title Create a title for a document
#' @author Nicolas Mangin
#' @description Function making a title with all relevant information for document displays
#' @param selected_document Tibble. Document selected for display.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @return Character. Title to display for the document.
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom tidyr pivot_longer
#' @importFrom stats na.omit
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom shiny span
#' @importFrom shiny icon
#' @export


make_title_display <- function(selected_document, course_data){

  type <- NULL

  selected_document <- selected_document |>
    dplyr::left_join(
      dplyr::select(course_data()$document_types, type, icon),
      by = "type"
    )

  selection_tags <- selected_document |>
    dplyr::select(dplyr::starts_with("tag_"))

  selection_tags <- selection_tags |>
    tidyr::pivot_longer(
      cols = base::names(selection_tags), names_to = "tag", values_to = "value"
    ) |>
    stats::na.omit() |>
    dplyr::left_join(course_data()$tags, by = c("tag","value")) |>
    dplyr::select(icon) |>
    dplyr::mutate(icon = purrr::map(icon, shiny::icon))

  title <- shiny::span(
    shiny::icon(selected_document$icon[1]), selection_tags$icon,
    base::paste0(" - ", selected_document$file[1], " - "),
    selected_document$title[1]
  )

  return(title)
}

