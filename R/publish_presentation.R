#' @name publish_presentation
#' @title Export a slide presentation
#' @author Nicolas Mangin
#' @description Function copying a presentation in the appropriate tree folder in 4_materials/presentation
#' @param tree List.
#' @param selected_document Character. Name of the note destined to be published.
#' @param course_paths List.
#' @return Write presentation in the folder "4_materials/presentation".
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom fs dir_copy
#' @importFrom shinyalert shinyalert
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @export



publish_presentation <- function(tree, selected_document, course_paths){
  
  code <- NULL
  position <- NULL
  title <- NULL
  
  # find path to the selected presentation and all its translations
  slctcode <- stringr::str_remove_all(selected_document, "_...Rmd$")
  alldocuments <- c(
    base::list.files(course_paths$subfolders$original, full.names = TRUE),
    base::list.files(course_paths$subfolders$translated, full.names = TRUE)
  )
  allversions <- alldocuments[stringr::str_detect(alldocuments, slctcode)]
  
  # Create folders
  coursename <- stringr::str_remove(tree$course$tree[[1]], ".RData$")
    coursefolder <- base::paste0(course_paths$subfolders$presentations, "/", coursename)
  if (!base::dir.exists(coursefolder)) base::dir.create(coursefolder)
  position <- tree$tbltree |>
    dplyr::filter(code == slctcode) |>
    dplyr::select(position, title) |>
    dplyr::mutate(position = stringr::str_remove_all(stringr::str_remove_all(position, "\\."), "[0]+$")) |>
    dplyr::select(position)
  position <- position$position[1]
  
  # Populate with common files
  bibfile <- base::paste0(course_paths$subfolders$edit, "/data/references.bib")
  if (base::file.exists(bibfile)) {
    base::file.copy(
      from = bibfile,
      to = base::paste0(coursefolder, "/references.bib")
    )
  }
  
  cslfile <- base::paste0(course_paths$subfolders$edit, "/format/csl/apa.csl")
  if (base::file.exists(cslfile)) {
    base::file.copy(
      from = cslfile,
      to = base::paste0(coursefolder, "/apa.csl")
    )
  }
  
  cssfile <- base::paste0(course_paths$subfolders$edit, "/format/css/slides.css")
  if (base::file.exists(cssfile)) {
    base::file.copy(
      from = cssfile,
      to = base::paste0(coursefolder, "/slides.css")
    )
  }
  
  # Identify all languages for the selected presentation and build quarto presentation
  alllanguages <- base::tolower(base::unique(base::unlist(
    stringr::str_extract_all(allversions, "(?<=_)[A-Z]{2}(?=\\.Rmd$)")
  )))
  
  for (lang in alllanguages){
    subfolder <- base::paste0(coursefolder, "/", lang)
    if (!base::dir.exists(subfolder)) base::dir.create(subfolder)
    filepath <- allversions[stringr::str_detect(allversions, base::paste0(slctcode, "_", base::toupper(lang), ".Rmd$"))]
    
    doc <- base::readLines(filepath)
    doctitle <- doc[stringr::str_detect(doc, "^exextra\\[title\\]:")]
    doctitle <- base::trimws(stringr::str_remove(doctitle, "^exextra\\[title\\]:"))
    docauthor <- doc[stringr::str_detect(doc, "^exextra\\[tag_authors\\]:")]
    docauthor <- base::trimws(stringr::str_remove(docauthor, "^exextra\\[tag_authors\\]:"))
    doccontent <- doc[1:(base::match('Meta-information', doc)-1)]
    
    qmdfolder <- base::paste0(
      subfolder, "/", position, "_",
      base::tolower(stringr::str_replace_all(stringr::str_remove_all(doctitle, "[[:punct:]]"), " ", "_"))
    )
    if (.Platform['OS.type'] == "windows" & base::nchar(qmdfolder) > 249){
      qmdfolder <- base::paste0(subfolder, "/", position)
    }
    if (!base::dir.exists(qmdfolder)) base::dir.create(qmdfolder)
    qmdpath <- base::paste0(qmdfolder, "/index.qmd")
    
    yaml <- c(
      '---',
      base::paste0('title: <large> ', doctitle,' </large>'),
      base::paste0('author: ', docauthor),
      base::paste0('date: ', base::format(base::Sys.time(), format = "%Y-%m-%d")),
      'format:',
      '  revealjs:',
      '    transition: slide',
      '    background-transition: slide',
      '    incremental: true',
      '    highlight: pygments',
      '    center: true',
      '    menu:',
      '      side: left',
      '      width: wide',
      '    slide-number: true',
      '    progress: true',
      '    show-notes: true',
      '    chalkboard: true',
      '    html-math-method:',
      '      method: mathjax',
      '      url: "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"',
      '    css:',
      '      - ../../slides.css',
      '      - "https://cdn.jsdelivr.net/npm/reveal.js-plugins/menu/font-awesome/css/fontawesome.css"',
      'bibliography: ../../references.bib',
      'csl: ../../apa.csl',
      '---',
      ''
    )
    
    presentation <- c(yaml, doccontent)
    base::writeLines(presentation, qmdpath, useBytes = TRUE)
  }
  
  shinyalert::shinyalert(
    title = "Presentation published!",
    text = "You can now access the files in the course materials folder.",
    type = "success"
  )
}

