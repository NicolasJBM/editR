#' @name publish_presentation
#' @title Export a slide presentation
#' @author Nicolas Mangin
#' @description Function copying a presentation in the appropriate tree folder in 4_materials/presentation
#' @param tree List.
#' @param selected_document Character. Name of the note destined to be published.
#' @param course_paths List.
#' @return Write presentation in the folder "4_materials/presentation".
#' @importFrom shinyalert shinyalert
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom fs dir_copy
#' @importFrom fs dir_delete
#' @importFrom shinyalert shinyalert
#' @importFrom stringr str_remove
#' @importFrom tidyr unite
#' @export



publish_presentation <- function(tree, selected_document, course_paths){
  
  position <- NULL
  title <- NULL
  
  origin <- course_paths$subfolders$temp
  
  if (base::length(tree$course) > 1){
    folder <- base::paste0(
      course_paths$subfolders$presentations,
      "/", tree$course$tree[[1]]
    )
    presentation_name <- tree$tbltree |>
      dplyr::filter(file == selected_document) |>
      dplyr::select(position, title) |>
      tidyr::unite(presentation_name, position, title, sep = " - ")
    presentation_name <- presentation_name$presentation_name
  } else {
    folder <- base::paste0(
      course_paths$subfolders$presentations,
      "/no_selected_tree"
    )
    date = base::as.character(base::Sys.Date())
    presentation_name <- base::paste0(
      date, " - ", stringr::str_remove(selected_document, ".Rmd$")
    )
  }
  destination <- base::paste0(
    folder, "/", presentation_name
  )
  if (!base::dir.exists(folder)) base::dir.create(folder)
  if (base::dir.exists(destination)) fs::dir_delete(destination)
  
  fs::dir_copy(origin, destination)
  
  shinyalert::shinyalert(
    title = "Presentation published!",
    text = "You can now access the files in the course materials folder.",
    type = "success"
  )
}

