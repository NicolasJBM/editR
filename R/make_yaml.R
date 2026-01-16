#' @name make_yaml
#' @title Create yaml for various documents
#' @author Nicolas Mangin
#' @description Make YAML to be inserted at the beginning of the document exported to be rendered via quarto.
#' @param selected Tibble. Document for which the de YAML should be created.
#' @param doctype Character. Whether the document is a "Note", "Page", "Slide", "Script", "Game", or "Case" (Questions have no YAML).
#' @return YAML as a character vector.
#' @export

make_yaml <- function(selected, doctype){
  
  title <- selected$title[1]
  authors <- selected$authors[1]
  date <- base::as.character(base::Sys.Date())
  
  if (doctype == "Note"){
    
    yaml <- c(
      '---',
      base::paste0('title: ', title),
      base::paste0('author: ', authors),
      base::paste0('date: ', date),
      '',
      'execute:',
      '  eval: true',
      '  echo: false',
      '  output: asis',
      '  warning: false',
      '  error: false',
      '  include: true',
      '',
      'format:',
      '  html:',
      '    html-math-method:',
      '      method: mathjax',
      '      url: "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"',
      '    fig-width: 9',
      '    fig-height: 5',
      '    css:',
      '      - format/css/notes.css',
      '      - format/css/all.css',
      '      - "https://cdn.jsdelivr.net/npm/reveal.js-plugins/menu/font-awesome/css/fontawesome.css"',
      '',
      'csl: format/csl/apa.csl',
      '',
      'bibliography: data/references.bib',
      '---'
    )
    
  } else if (doctype == "Page"){
    
    yaml <- c(
      '---',
      base::paste0('title: ', selected$title[1]),
      '',
      'execute:',
      '  eval: true',
      '  echo: false',
      '  output: asis',
      '  warning: false',
      '  error: false',
      '  include: true',
      '',
      'format:',
      '  html:',
      '    html-math-method:',
      '      method: mathjax',
      '      url: "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"',
      '    fig-width: 9',
      '    fig-height: 5',
      '    css:',
      '      - format/css/pages.css',
      '      - format/css/all.css',
      '      - "https://cdn.jsdelivr.net/npm/reveal.js-plugins/menu/font-awesome/css/fontawesome.css"',
      '',
      'csl: format/csl/apa.csl',
      '',
      'bibliography: data/references.bib',
      '---'
    )
    
  } else if (doctype == "Slide"){
    
    doctitle <- selected$title[[1]]
    docauthor <- authors
    docdate <- base::format(base::Sys.time(), format = "%Y-%m-%d")
    
    yaml <- c(
      '---',
      base::paste0('title: <large> ', doctitle,' </large>'),
      base::paste0('author: ', docauthor),
      base::paste0('date: ', docdate),
      '',
      'execute:',
      '  eval: true',
      '  echo: false',
      '  output: asis',
      '  warning: false',
      '  error: false',
      '  include: true',
      '',
      'format:',
      '  revealjs:',
      '    transition: slide',
      '    background-transition: slide',
      '    incremental: true',
      '    highlight: pygments',
      '    center: true',
      '    template-partials:',
      '      - templates/presentations/title-slide.html',
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
      '      - format/css/slides.css',
      '      - format/css/all.css',
      '      - "https://cdn.jsdelivr.net/npm/reveal.js-plugins/menu/font-awesome/css/fontawesome.css"',
      '',
      'bibliography: data/references.bib',
      '',
      'csl: format/csl/apa.csl',
      '---',
      ""
    )
    
  } else if (doctype == "Script"){
    
    yaml <- c(
      '---',
      base::paste0('title: ', selected$title[1]),
      '',
      'execute:',
      '  eval: true',
      '  echo: false',
      '  output: asis',
      '  warning: false',
      '  error: false',
      '  include: true',
      '',
      'format:',
      '  html:',
      '    html-math-method:',
      '      method: mathjax',
      '      url: "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"',
      '    fig-width: 9',
      '    fig-height: 5',
      '    css:',
      '      - format/css/scripts.css',
      '      - "https://cdn.jsdelivr.net/npm/reveal.js-plugins/menu/font-awesome/css/fontawesome.css"',
      '',
      'csl: format/csl/apa.csl',
      '',
      'bibliography: data/references.bib',
      '---'
    )
    
  } else if (doctype == "Game"){
    
    yaml <- c(
      '---',
      base::paste0('title: ', selected$title[1]),
      '---'
    )
    
  } else if (doctype == "Tutorial"){
    
    yaml <- c(
      '---',
      base::paste0('title: ', selected$title[1]),
      '---'
    )
    
  } else if (doctype == "Case"){
    
    yaml <- c(
      '---',
      base::paste0('title: ', selected$title[1]),
      '---'
    )
    
  } else { # Report
    
    yaml <- c(
      '---',
      base::paste0('title: ', selected$title[1]),
      '',
      'execute:',
      '  eval: true',
      '  echo: false',
      '  output: asis',
      '  warning: false',
      '  error: false',
      '  include: true',
      '',
      'format:',
      '  html:',
      '    html-math-method:',
      '      method: mathjax',
      '      url: "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"',
      '    fig-width: 9',
      '    fig-height: 5',
      '    css:',
      '      - format/css/reports.css',
      '      - format/css/all.css',
      '      - "https://cdn.jsdelivr.net/npm/reveal.js-plugins/menu/font-awesome/css/fontawesome.css"',
      '',
      'csl: format/csl/apa.csl',
      '',
      'bibliography: data/references.bib',
      '---'
    )
    
  }
  
  return(yaml)
}
