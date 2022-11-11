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
  code <- NULL
  
  document <- stringr::str_remove_all(selected_document, ".Rmd$")
  language <- stringr::str_extract(document, "..$")
  slctcode <- stringr::str_remove_all(document, "_..$")
  origin <- course_paths$subfolders$temp
  
  if (base::length(tree$course) > 1){
    coursename <- stringr::str_remove(tree$course$tree[[1]], ".RData$")
    prefix <- tree$tbltree |>
      dplyr::filter(code == slctcode) |>
      dplyr::select(position)
    position <- prefix$position[1]
  } else {
    coursename <- "/no_selected_tree"
    position <- ""
  }
  
  folder <- base::paste0(course_paths$subfolders$presentations, "/", coursename)
  if (!base::dir.exists(folder)) base::dir.create(folder)
  
  folder <- base::paste0(folder, "/", language)
  if (!base::dir.exists(folder)) base::dir.create(folder)
  
  title <- base::readLines(base::paste0(course_paths$subfolders$temp, "/index.Rmd"))
  title <- title[2] |>
    stringr::str_remove("title: <large>") |>
    stringr::str_remove("</large>") |>
    stringr::str_remove_all("[:punct:]") |>
    base::trimws() |>
    stringr::str_replace_all(" ", "_")
  
  destination <- base::paste0(folder, "/", position, "_", title)
  if (base::dir.exists(destination)) fs::dir_delete(destination)
  fs::dir_copy(origin, destination)
  
  shinyalert::shinyalert(
    title = "Presentation published!",
    text = "You can now access the files in the course materials folder.",
    type = "success"
  )
}

