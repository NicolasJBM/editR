#' @name publish_note
#' @title Format a note as a blog post
#' @author Nicolas Mangin
#' @description Function reformating a note as a blog post with adequate tags.
#' @param selected_document Character. Name of the note destined to be published.
#' @param course_paths List.
#' @return Write post in the folder "4_materials/blog".
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove_all
#' @export



publish_note <- function(selected_document, course_paths){
  
  tag_authors <- NULL
  
  # Write post
  document <- base::paste0(course_paths$subfolders$original, "/", selected_document)
  tags <- editR::get_tags(document)
  tags <- tags |> dplyr::mutate(tag_trial1 = "yo", tag_trial2 = "man")
  
  title <- tags$title[[1]]
  date <- base::as.character(base::Sys.Date())
  if ("tag_authors" %in% base::names(tags)) {
    authors <- tags$tag_authors[[1]]
  } else {
    authors <- ""
  }
  categories <- selected_document |>
    stringr::str_remove_all(".Rmd$") |>
    stringr::str_extract("..$")
  
  lines <- base::readLines(document)
  lines <- lines[1:(base::match('Meta-information', lines)-1)]
  
  tags <- dplyr::select(tags, dplyr::starts_with("tag_"))
  
  if ("tag_authors" %in% base::names(tags)) tags <- dplyr::select(tags, -tag_authors)
  tags <- base::paste('"', base::as.character(tags[1,]), '"', sep = "") |>
    base::paste(collapse = ", ")
  
  post <- c(
    '---',
    base::paste0('title: "', title, '"'),
    base::paste0('author: "', authors, '"'),
    base::paste0('date: "', date, '"'),
    base::paste0('categories: ["', categories,'"]'),
    base::paste0('tags: [', tags,']'),
    'csl: apa.csl',
    'bibliography: references.bib',
    '---',
    '',
    lines
  )
  
  # Save post
  base::writeLines(
    post,
    base::paste0(course_paths$subfolders$blog,  "/", selected_document),
    useBytes = TRUE
  )
  
  # Save csl
  cslfile <- base::paste0(course_paths$subfolders$csl, "/apa.csl")
  if (base::file.exists(cslfile)) {
    base::file.copy(
      from = cslfile,
      to = base::paste0(course_paths$subfolders$blog, "/apa.csl")
    )
  }
  
  # Save references
  bibfile <- base::paste0(course_paths$subfolders$data, "/references.bib")
  if (base::file.exists(bibfile)) {
    base::file.copy(
      from = bibfile,
      to = base::paste0(course_paths$subfolders$blog, "/references.bib")
    )
  }
  
  shinybusy::remove_modal_spinner()
  
  shinyalert::shinyalert(
    title = "Blog post published!",
    text = "You can now access the files in the course materials folder.",
    type = "success"
  )
}

