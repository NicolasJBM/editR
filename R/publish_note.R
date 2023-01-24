#' @name publish_note
#' @title Format a note as a blog post
#' @author Nicolas Mangin
#' @description Function reformating a note as a blog post with adequate tags.
#' @param selected_document Character. Name of the note destined to be published.
#' @param course_paths List.
#' @param translation Logical. Whether the source document is in the folder "translated" rather than "original".
#' @return Write post in the folder "4_materials/blog".
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove_all
#' @export



publish_note <- function(selected_document, course_paths, translation = FALSE){
  
  authors <- NULL
  
  if (translation){
    source_folder <- course_paths$subfolders$translated
  } else {
    source_folder <- course_paths$subfolders$original
  }
  
  # Get information
  document <- base::paste0(source_folder, "/", selected_document)
  
  tags <- editR::get_tags(document)
  
  title <- tags$title[[1]]
  date <- base::as.character(base::Sys.Date())
  if ("authors" %in% base::names(tags)) {
    authors <- tags$authors[[1]]
  } else {
    authors <- ""
  }
  
  filename <- selected_document |>
    stringr::str_remove_all(".Rmd$")
  
  language <- stringr::str_extract(filename, "..$")
  if (translation){
    destination_folder <- base::paste0(
      course_paths$subfolders$blog, "/", language
    )
  } else {
    destination_folder <- course_paths$subfolders$blog
  }
  if (!base::dir.exists(destination_folder)) base::dir.create(destination_folder)
  
  lines <- base::readLines(document)
  lines <- lines[1:(base::match('Meta-information', lines)-1)]
  
  tags <- dplyr::select(tags, dplyr::starts_with("tag_"))
  
  tags <- base::paste('"', base::as.character(tags[1,]), '"', sep = "") |>
    base::paste(collapse = ", ")
  
  # Remove prior publication
  
  to_remove <- base::list.files(destination_folder, full.names = TRUE)
  to_remove <- to_remove[stringr::str_detect(to_remove, filename)]
  if (base::length(to_remove) > 0) base::file.remove(to_remove)
  
  # Write post
  
  post <- c(
    '---',
    base::paste0('title: "', title, '"'),
    base::paste0('author: "', authors, '"'),
    base::paste0('date: "', date, '"'),
    base::paste0('categories: ["', language,'"]'),
    base::paste0('tags: [', tags,']'),
    'csl: apa.csl',
    'bibliography: references.bib',
    '---',
    '',
    lines
  )
  
  # Save post
  filename <- base::paste0(
    filename, " - ", date, " - ", title, ".Rmd"
  )
  base::writeLines(
    post,
    base::paste0(destination_folder,  "/", filename),
    useBytes = TRUE
  )
  
  # Save csl
  cslfile <- base::paste0(course_paths$subfolders$csl, "/apa.csl")
  if (base::file.exists(cslfile)) {
    base::file.copy(
      from = cslfile,
      to = base::paste0(destination_folder, "/apa.csl")
    )
  }
  
  # Save references
  bibfile <- base::paste0(course_paths$subfolders$data, "/references.bib")
  if (base::file.exists(bibfile)) {
    base::file.copy(
      from = bibfile,
      to = base::paste0(destination_folder, "/references.bib")
    )
  }
  
  shinyalert::shinyalert(
    title = "Blog post published!",
    text = "You can now access the files in the course materials folder.",
    type = "success"
  )
}

