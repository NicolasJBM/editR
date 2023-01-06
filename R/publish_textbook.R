#' @name publish_textbook
#' @title Compile a textbook
#' @author Nicolas Mangin
#' @description Function loading a tree and compiling the corresponding textbook. Pages are formated and organized so that they can then be added to a blogdown project based in the hugo theme "learn"
#' @param tree List.
#' @param course_paths List.
#' @param languages Tibble
#' @return Write folders and pages to disk in the folder "4_materials/textbooks".
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_spinner
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_to_lower
#' @importFrom tibble tibble
#' @importFrom tidyr separate
#' @importFrom tidyr unnest
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
  translated_path <- NULL

  if (base::length(tree$textbook) == 1){

    shinyalert::shinyalert(
      title = "Please select a tree.",
      text = "You must first select a classification tree to perform this action.",
      type = "error"
    )

  } else {
    
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
    

    textbook <- tree$textbook
    orginal_path <- course_paths$subfolders$original
    textbooks_path <- course_paths$subfolders$textbooks
    tree2compile <- stringr::str_remove(tree$course$tree[1], ".RData$")
    other_languages <- languages |>
      dplyr::filter(langiso != textbook$language[1]) |>
      dplyr::select(language = langiso) |>
      dplyr::mutate(languageweb = stringr::str_to_lower(language))

    shinybusy::show_modal_spinner(
      spin = "orbit",
      text = "Publishing textbook..."
    )

    # Create the textbook folder structure
    textbook <- textbook |>
      dplyr::mutate(
        original_path = base::paste0(orginal_path, "/", file),
        translated_path = base::paste0(translated_path, "/", file),
        folder = base::paste0(textbooks_path, "/", folder)
      )
    
    for (folder in textbook$folder){
      if (!base::dir.exists(folder)) base::dir.create(folder)
    }
    template_folder <- base::paste0(textbook$folder[1], "/template")
    if (!base::dir.exists(template_folder)) base::dir.create(template_folder)
    

    # Fill in the textbook for the original language
    for (i in base::seq_len(base::nrow(textbook))){
      from = textbook$original_path[i]
      to = base::paste0(
        textbook$folder[i], "/index.qmd"
      )
      lines <- base::readLines(from)
      core <- lines[1:(base::match('Meta-information', lines)-1)]

      core <- stringr::str_replace_all(core, "SOURCENAME", tree2compile)
      
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
    
    # Make a project
     
    rprojfile <- base::paste0(course_paths$subfolders$textbooks, "/template/textbook.Rproj")
    base::file.copy(
      from = rprojfile,
      to = base::paste0(textbook$folder[1], "/textbook.Rproj")
    )
    
    base::writeLines(yaml, base::paste0(textbook$folder[1], "_quarto.yml"))
    
    
    
    # Import references and formatting files
    
    base::save(
      textbook,
      file = base::paste0(textbook$folder[1], "/template/textbook.RData")
    )
    
    bibfile <- base::paste0(course_paths$subfolders$edit, "/data/references.bib")
    if (base::file.exists(bibfile)) {
      base::file.copy(
        from = bibfile,
        to = base::paste0(textbook$folder[1], "/template/references.bib")
      )
    }
    
    cslfile <- base::paste0(course_paths$subfolders$edit, "/format/csl/apa.csl")
    if (base::file.exists(cslfile)) {
      base::file.copy(
        from = cslfile,
        to = base::paste0(textbook$folder[1], "/template/apa.csl")
      )
    }
    
    cssfile <- base::paste0(course_paths$subfolders$edit, "/format/css/pages.css")
    if (base::file.exists(cssfile)) {
      base::file.copy(
        from = cssfile,
        to = base::paste0(textbook$folder[1], "/template/pages.css")
      )
    }
    
    logofile <- base::paste0(course_paths$subfolders$textbooks, "/template/logo.png")
    base::file.copy(
      from = logofile,
      to = base::paste0(textbook$folder[1], "/template/logo.png")
    )
    
    ratingfile <- base::paste0(course_paths$subfolders$textbooks, "/template/rating.js")
    base::file.copy(
      from = ratingfile,
      to = base::paste0(textbook$folder[1], "/template/rating.js")
    )
    
    commentingfile <- base::paste0(course_paths$subfolders$textbooks, "/template/commenting.js")
    base::file.copy(
      from = commentingfile,
      to = base::paste0(textbook$folder[1], "/template/commenting.js")
    )
    
    headerfile <- base::paste0(course_paths$subfolders$textbooks, "/template/in_header.txt")
    base::file.copy(
      from = headerfile,
      to = base::paste0(textbook$folder[1], "/template/in_header.txt")
    )
    
    afterbodyfile <- base::paste0(course_paths$subfolders$textbooks, "/template/after_body.txt")
    base::file.copy(
      from = afterbodyfile,
      to = base::paste0(textbook$folder[1], "/template/after_body.txt")
    )
    
    
    
    
    
    # Publish other languages files
    
    
    
    
    
    shinybusy::remove_modal_spinner()

    shinyalert::shinyalert(
      title = "Textbook published!",
      text = "You can now access the files in the course materials folder.",
      type = "success"
    )
  }
}

