#' @name publish_textbook
#' @title Compile a textbook
#' @author Nicolas Mangin
#' @description Function loading a tree and compiling the corresponding textbook. Pages are formated and organized so that they can then be added to a blogdown project based in the hugo theme "learn"
#' @param tree List.
#' @param course_paths List.
#' @param languages Tibble
#' @return Write folders and pages to disk in the folder "4_materials/textbooks".
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr safely
#' @importFrom quarto quarto_render
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_to_lower
#' @export



publish_textbook <- function(tree, course_paths, languages){

  langiso <- NULL
  language <- NULL
  path <- NULL
  prefix <- NULL
  order <- NULL
  tags <- NULL
  code <- NULL
  section <- NULL

  if (base::length(tree$textbook) == 1){

    shinyalert::shinyalert(
      title = "Please select a tree.",
      text = "You must first select a classification tree to perform this action.",
      type = "error"
    )

  } else {
    
    shinybusy::show_modal_spinner(
      spin = "orbit",
      text = "Publishing textbook..."
    )
    
    # Create quarto YAML
    textbook <- tree$textbook
    main_sections <- dplyr::filter(textbook, base::nchar(section) == 1)
    
    quarto_yaml1 <- c(
      'project:',
      '  type: website',
      '  render: ',
      '    - "*.qmd"',
      '    - "!template/"',
      '',
      'website:',
      base::paste0('  title: "', tree$course$course[1],'"'),
      '  navbar:',
      '    logo: template/logo.png',
      '    search: true',
      '    pinned: true',
      '    background: dark',
      '    left:'
    )
    
    quarto_yaml2 <- c()
    for (i in 1:(base::nrow(main_sections))){
      add <- c(
        base::paste0('      - text: "', main_sections$title[i], '"'),
        base::paste0('        file: section', i, '/index.qmd')
      )
      quarto_yaml2 <- c(quarto_yaml2, add)
    }
    
    quarto_yaml3 <- c( 
      '    right: ',
      '      - icon: github',
      '        url: https://github.com/NicolasJBM',
      '  sidebar:'
    )
    
    quarto_yaml4 <- c(
      base::paste0('    - id: section', main_sections$section[1]),
      base::paste0('      title: "', main_sections$title[1], '"'),
      base::paste0('      href: section1/index.qmd'),
      '      search: true',
      '      style: floating',
      '      background: light',
      '      collapse-level: 2',
      base::paste0('      contents: section', main_sections$section[1])
    )
    
    for (i in 2:(base::nrow(main_sections))){
      add <- c(
        base::paste0('    - id: section', main_sections$section[i]),
        base::paste0('      title: "', main_sections$title[i], '"'),
        base::paste0('      href: section', i, '/index.qmd'),
        base::paste0('      contents: section', main_sections$section[i])
      )
      quarto_yaml4 <- c(quarto_yaml4, add)
    }
    
    quarto_yaml5 <- c(
      '  page-navigation: true',
      '  reader-mode: true',
      '  page-footer:',
      base::paste0('    center: "', tree$course$authors, ' (', tree$course$year, ') ', tree$course$course[1], ' ', '"'),
      '    border: true',
      '    background: dark',
      '',
      'bibliography: template/references.bib',
      '',
      'csl: template/apa.csl',
      '',
      'format:',
      '  html:',
      '    citations-hover: true',
      '    footnotes-hover: true',
      '    anchor-sections: true',
      '    smooth-scroll: true',
      '    css: template/pages.css',
      '    include-in-header:',
      '      - template/in_header.txt',
      '    include-after-body:',
      '      - template/after_body.txt',
      '',
      'toc: true',
      ''
    )
    
    yaml <- c(quarto_yaml1, quarto_yaml2, quarto_yaml3, quarto_yaml4, quarto_yaml5)
    
    # define useful paths
    datafolder <- base::paste0(course_paths$subfolders$edit, "/data")
    formatfolder <- base::paste0(course_paths$subfolders$edit, "/format")
    templatefolder <- base::paste0(course_paths$subfolders$edit, "/templates/textbooks")
    textbookfolder <- course_paths$subfolders$textbooks
    orginal_path <- course_paths$subfolders$original
    translated_path <- course_paths$subfolders$translated
    coursename <- stringr::str_remove(tree$course$tree[1], ".RData$")
    coursefolder <- base::paste0(textbookfolder, "/", coursename)
    
    other_languages <- languages |>
      dplyr::filter(langiso != textbook$language[1]) |>
      dplyr::select(language = langiso) |>
      dplyr::mutate(languageweb = stringr::str_to_lower(language))

    # Create the textbook folder structure
    textbook <- textbook |>
      dplyr::mutate(
        original_path = base::paste0(orginal_path, "/", file),
        translated_path = base::paste0(translated_path, "/", file),
        folder = base::paste0(textbookfolder, "/", folder)
      )
    
    for (folder in textbook$folder){
      if (!base::dir.exists(folder)) base::dir.create(folder)
    }
    template_folder <- base::paste0(coursefolder, "/template")
    if (!base::dir.exists(template_folder)) base::dir.create(template_folder)

    # Fill in the textbook for the original language
    for (i in base::seq_len(base::nrow(textbook))){
      from = textbook$original_path[i]
      to = base::paste0(
        textbook$folder[i], "/index.qmd"
      )
      lines <- base::readLines(from)
      core <- lines[1:(base::match('Meta-information', lines)-1)]

      core <- stringr::str_replace_all(core, "SOURCENAME", coursename)
      
      if (textbook$section[i] != ""){
        section_number <- base::paste0(textbook$section[i], ' - ')
      } else section_number <- ""

      header <- c(
        '---',
        base::paste0('title: ', section_number, textbook$title[i]),
        base::paste0("order: ", textbook$order[i]),
        '---',
        '<hr>'
      )

      newlines <- c(header, core)
      base::writeLines(newlines, to, useBytes = TRUE)
    }
    
    
    # Write YAML and course 
    yamlpath <- base::paste0(coursefolder, "/_quarto.yml")
    base::writeLines(yaml, yamlpath)
    base::save(
      textbook,
      file = base::paste0(coursefolder, "/template/textbook.RData")
    )
    
    # Retrieve files from data
    bibfile <- base::paste0(datafolder, "/references.bib")
    if (base::file.exists(bibfile)) {
      base::file.copy(
        from = bibfile,
        to = base::paste0(coursefolder, "/template/references.bib")
      )
    }
    
    # Retrieve files from format
    cslfile <- base::paste0(formatfolder, "/csl/apa.csl")
    if (base::file.exists(cslfile)) {
      base::file.copy(
        from = cslfile,
        to = base::paste0(coursefolder, "/template/apa.csl")
      )
    }
    
    cssfile <- base::paste0(formatfolder, "/css/pages.css")
    if (base::file.exists(cssfile)) {
      base::file.copy(
        from = cssfile,
        to = base::paste0(coursefolder, "/template/pages.css")
      )
    }
    
    # Retrieve files from template
    rprojfile <- base::paste0(templatefolder, "/textbook.Rproj")
    base::file.copy(
      from = rprojfile,
      to = base::paste0(coursefolder, "/textbook.Rproj")
    )
    
    logofile <- base::paste0(templatefolder, "/logo.png")
    base::file.copy(
      from = logofile,
      to = base::paste0(coursefolder, "/template/logo.png")
    )
    
    ratingfile <- base::paste0(templatefolder, "/rating.js")
    base::file.copy(
      from = ratingfile,
      to = base::paste0(coursefolder, "/template/rating.js")
    )
    
    commentingfile <- base::paste0(templatefolder, "/commenting.js")
    base::file.copy(
      from = commentingfile,
      to = base::paste0(coursefolder, "/template/commenting.js")
    )
    
    headerfile <- base::paste0(templatefolder, "/in_header.txt")
    base::file.copy(
      from = headerfile,
      to = base::paste0(coursefolder, "/template/in_header.txt")
    )
    
    afterbodyfile <- base::paste0(templatefolder, "/after_body.txt")
    base::file.copy(
      from = afterbodyfile,
      to = base::paste0(coursefolder, "/template/after_body.txt")
    )
    
    purrr::safely(quarto::quarto_render(yamlpath, quiet = TRUE))
    
    
    
    
    
    # Publish other languages files:
    # copy textbook in new folder and
    # replace original files by translated files when exists.
    
    
    
    
    
    shinybusy::remove_modal_spinner()

    shinyalert::shinyalert(
      title = "Textbook published!",
      text = "You can now access the files in the course materials folder.",
      type = "success"
    )
  }
}

