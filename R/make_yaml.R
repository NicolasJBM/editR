#' @name make_yaml
#' @title Create yaml for various documents
#' @author Nicolas Mangin
#' @description Make YAML to be inserted at the beginning of the document to knit.
#' @param selected Tibble. Document for which the de YAML should be created.
#' @param doctype Character. Whether the document is a "Note", "Page", "Slide", "Video", "Game", or "Case" (Questions have no YAML).
#' @return YAML as a character vector.
#' @importFrom dplyr filter
#' @export

make_yaml <- function(selected, doctype){
  
  if ("tag_authors" %in% base::names(selected)){
    authors <- selected$tag_authors[[1]]
  } else authors <- ""
  
  if (doctype == "Note"){
    
    yaml <- c(
      '---',
      base::paste0('title: ', selected$title[1]),
      'subtitle: <hr>',
      'output:',
      '  rmarkdown::html_document:',
      '    self_contained: false',
      '    css: ["format/css/notes.css", "format/css/fa.css"]',
      '    fig_width: 8',
      '    fig_height: 6',
      '    fig_caption: true',
      '    mathjax: default',
      'csl: format/csl/apa.csl',
      'bibliography: data/references.bib',
      '---'
    )
    
  } else if (doctype == "Page"){
    
    yaml <- c(
      '---',
      base::paste0('title: ', selected$title[1]),
      'subtitle: <hr>',
      'output:',
      '  rmarkdown::html_document:',
      '    self_contained: false',
      '    css: ["format/css/pages.css", "format/css/fa.css"]',
      '    fig_width: 8',
      '    fig_height: 6',
      '    fig_caption: true',
      '    mathjax: default',
      'csl: format/csl/apa.csl',
      'bibliography: data/references.bib',
      '---'
    )
    
  } else if (doctype == "Slide"){
    
    yaml <- c(
      '---',
      base::paste0('title: <large> ', selected$title[[1]],' </large>'),
      base::paste0('author: ', authors),
      base::paste0('date: ', base::format(base::Sys.time(), format = "%Y-%m-%d")),
      'output:',
      '  revealjs::revealjs_presentation:',
      '    self_contained: false',
      '    reveal_plugins: ["menu","chalkboard"]',
      '    incremental: true',
      '    highlight: pygments',
      '    center: true',
      '    transition: slide',
      '    background_transition: slide',
      '    reveal_options:',
      '      showNotes: true',
      '      slideNumber: true',
      '      previewLinks: true',
      '    fig_width: 8',
      '    fig_height: 6',
      '    fig_caption: true',
      '    css: format/css/slides.css',
      '    mathjax: default',
      'csl: format/csl/apa.csl',
      'bibliography: data/references.bib',
      '---'
    )
    
  } else if (doctype == "Video"){
    
    yaml <- c(
      '---',
      base::paste0('title: ', selected$title[1]),
      'subtitle: <hr>',
      'output:',
      '  rmarkdown::html_document:',
      '    self_contained: false',
      '    css: ["format/css/videos.css", "format/css/fa.css"]',
      '    fig_width: 8',
      '    fig_height: 6',
      '    fig_caption: true',
      '    mathjax: default',
      'csl: format/csl/apa.csl',
      'bibliography: data/references.bib',
      '---'
    )
    
  } else if (doctype == "Game"){
    
    yaml <- c(
      '---',
      base::paste0('title: ', selected$title[1]),
      'subtitle: <hr>',
      'output:',
      '  rmarkdown::html_document:',
      '    self_contained: false',
      '    css: ["format/css/games.css", "format/css/fa.css"]',
      '    fig_width: 8',
      '    fig_height: 6',
      '    fig_caption: true',
      '    mathjax: default',
      'csl: format/csl/apa.csl',
      'bibliography: data/references.bib',
      '---'
    )
    
  } else {
    
    yaml <- c(
      '---',
      base::paste0('title: ', selected$title[1]),
      'subtitle: <hr>',
      'output:',
      '  rmarkdown::html_document:',
      '    self_contained: false',
      '    css: ["format/css/cases.css", "format/css/fa.css"]',
      '    fig_width: 8',
      '    fig_height: 6',
      '    fig_caption: true',
      '    mathjax: default',
      'csl: format/csl/apa.csl',
      'bibliography: data/references.bib',
      '---'
    )
    
  }
  
  return(yaml)
}
