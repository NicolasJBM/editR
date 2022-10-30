#' @name publish_script
#' @title Format a video as a script
#' @author Nicolas Mangin
#' @description Function taking only the lines supposed to be uttered and exporting them into a .txt file which can be read by a prompter.
#' @param selected_document Character. Name of the note destined to be published.
#' @param course_paths List.
#' @return Write post in the folder "4_materials/blog".
#' @importFrom shinyalert shinyalert
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove_all
#' @export



publish_script <- function(selected_document, course_paths){
  
  # Write post
  document <- base::paste0(course_paths$subfolders$original, "/", selected_document)
  
  tags <- editR::get_tags(document)
  
  title <- tags$title[[1]]
  date <- base::as.character(base::Sys.Date())
  filename <- selected_document |>
    stringr::str_remove_all(".Rmd$")
  
  to_remove <- base::list.files(course_paths$subfolders$scripts, full.names = TRUE)
  to_remove <- to_remove[stringr::str_detect(to_remove, filename)]
  if (base::length(to_remove) > 0) base::file.remove(to_remove)
  
  lines <- base::readLines(document)
  lines <- lines[stringr::str_detect(lines, "^>")]
  
  script <- c()
  for (i in base::seq_len(base::length(lines))) script <- c(script, lines[i], "")
  script <- stringr::str_remove_all(script, "> ")
  
  # Save post
  filename <- base::paste0(
    filename, " - ", date, " - ", title, ".txt"
  )
  base::writeLines(
    script,
    base::paste0(course_paths$subfolders$scripts,  "/", filename),
    useBytes = TRUE
  )
  
  shinyalert::shinyalert(
    title = "Script published!",
    text = "You can now access the files in the course materials folder.",
    type = "success"
  )
}

