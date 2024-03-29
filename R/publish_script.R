#' @name publish_script
#' @title Format a video as a script
#' @author Nicolas Mangin
#' @description Function taking only the lines supposed to be uttered and exporting them into a .txt file which can be read for a prompter.
#' @param selected_document Character. Name of the note destined to be published.
#' @param course_paths List. List of paths to either databases or documents.
#' @param translation Logical. Whether the source document is in the folder "translated" rather than "original".
#' @return Write post in the folder "4_materials/blog".
#' @importFrom shinyalert shinyalert
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove_all
#' @export



publish_script <- function(selected_document, course_paths, translation = FALSE){
  
  if (translation){
    source_folder <- course_paths$subfolders$translated
  } else {
    source_folder <- course_paths$subfolders$original
  }
  
  # Write post
  document <- base::paste0(source_folder, "/", selected_document)
  
  tags <- editR::get_tags(document)
  
  title <- tags$title[[1]]
  date <- base::as.character(base::Sys.Date())
  filename <- selected_document |>
    stringr::str_remove_all(".Rmd$")
  
  language <- stringr::str_extract(filename, "..$")
  if (translation){
    destination_folder <- base::paste0(
      course_paths$subfolders$scripts, "/", language
    )
  } else {
    destination_folder <- course_paths$subfolders$scripts
  }
  if (!base::dir.exists(destination_folder)) base::dir.create(destination_folder)
  
  to_remove <- base::list.files(destination_folder, full.names = TRUE)
  to_remove <- to_remove[stringr::str_detect(to_remove, filename)]
  if (base::length(to_remove) > 0) base::file.remove(to_remove)
  
  lines <- base::readLines(document)
  lines <- lines[stringr::str_detect(lines, "^>")]
  
  script <- base::character(0)
  for (i in base::seq_len(base::length(lines))) script <- c(script, lines[i], "")
  script <- stringr::str_remove_all(script, "> ")
  
  # Save post
  filename <- base::paste0(
    filename, " - ", date, " - ", title, ".txt"
  )
  base::writeLines(
    script,
    base::paste0(destination_folder,  "/", filename),
    useBytes = TRUE
  )
  
  shinyalert::shinyalert(
    title = "Script published!",
    text = "You can now access the files in the course materials folder.",
    type = "success"
  )
}

