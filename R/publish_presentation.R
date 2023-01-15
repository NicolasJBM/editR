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
#' @importFrom quarto quarto_render
#' @importFrom purrr safely
#' @export



publish_presentation <- function(tree, selected_document, course_paths){
  
  code <- NULL
  position <- NULL
  title <- NULL
  
  shinybusy::show_modal_spinner(
    spin = "orbit",
    text = "Publishing presentation..."
  )
  
  # find path to the selected presentation and all its translations
  slctcode <- stringr::str_remove_all(selected_document, "_...Rmd$")
  alldocuments <- c(
    base::list.files(course_paths$subfolders$original, full.names = TRUE),
    base::list.files(course_paths$subfolders$translated, full.names = TRUE)
  )
  allversions <- alldocuments[stringr::str_detect(alldocuments, slctcode)]
  
  # define useful paths
  datafolder <- base::paste0(course_paths$subfolders$edit, "/data")
  formatfolder <- base::paste0(course_paths$subfolders$edit, "/format")
  templatefolder <- base::paste0(course_paths$subfolders$edit, "/templates/presentations")
  presentationfolder <- course_paths$subfolders$presentations
  
  # Create course folder
  coursename <- stringr::str_remove(tree$course$tree[[1]], ".RData$")
  coursefolder <- base::paste0(presentationfolder, "/", coursename)
  if (!base::dir.exists(coursefolder)) base::dir.create(coursefolder)
  
  # Populate course folder with common files
  bibfile <- base::paste0(datafolder, "/references.bib")
  if (base::file.exists(bibfile)) {
    base::file.copy(
      from = bibfile,
      to = base::paste0(coursefolder, "/references.bib")
    )
  }
  
  cslfile <- base::paste0(formatfolder, "/csl/apa.csl")
  if (base::file.exists(cslfile)) {
    base::file.copy(
      from = cslfile,
      to = base::paste0(coursefolder, "/apa.csl")
    )
  }
  
  cssfile <- base::paste0(formatfolder, "/css/slides.css")
  if (base::file.exists(cssfile)) {
    base::file.copy(
      from = cssfile,
      to = base::paste0(coursefolder, "/slides.css")
    )
  }
  
  titlefile <- base::paste0(templatefolder, "/title-slide.html")
  if (base::file.exists(titlefile)) {
    base::file.copy(
      from = titlefile,
      to = base::paste0(coursefolder, "/title-slide.html")
    )
  }
  
  logofile <- base::paste0(templatefolder, "/logo.png")
  if (base::file.exists(logofile)) {
    base::file.copy(
      from = logofile,
      to = base::paste0(coursefolder, "/logo.png")
    )
  }
  
  # Get document position in tree and
  position <- tree$tbltree |>
    dplyr::filter(code == slctcode) |>
    dplyr::select(position, title) |>
    dplyr::mutate(position = stringr::str_remove_all(stringr::str_remove_all(position, "\\."), "[0]+$")) |>
    dplyr::select(position)
  
  # Identify all languages for the selected presentation and build quarto presentation
  position <- position$position[1]
  
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
    docdate <- base::format(base::Sys.time(), format = "%Y-%m-%d")
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
      base::paste0('date: ', docdate),
      'execute:',
      '  eval: true',
      '  echo: false',
      '  warning: false',
      '  error: false',
      '  include: true',
      'format:',
      '  revealjs:',
      '    transition: slide',
      '    background-transition: slide',
      '    incremental: true',
      '    highlight: pygments',
      '    center: true',
      '    template-partials:',
      '      - ../../title-slide.html',
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
      '    fig-width: 9',
      '    fig-height: 5',
      '    reference-location: document',
      base::paste0('    footer: "', doctitle, " - ", docauthor, " - ", docdate,'"'),
      '    css:',
      '      - ../../slides.css',
      '      - "https://cdn.jsdelivr.net/npm/reveal.js-plugins/menu/font-awesome/css/fontawesome.css"',
      'bibliography: ../../references.bib',
      'csl: ../../apa.csl',
      '---',
      ""
    )
    
    presentation <- c(yaml, doccontent)
    base::writeLines(presentation, qmdpath, useBytes = TRUE)
    
    purrr::safely(quarto::quarto_render(qmdpath, quiet = TRUE))
  }
  
  shinybusy::remove_modal_spinner()
  
  shinyalert::shinyalert(
    title = "Presentation published!",
    text = "You can now access the files in the course materials folder.",
    type = "success"
  )
}

