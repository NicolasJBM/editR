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

  if (base::length(tree$textbook) == 1){

    shinyalert::shinyalert(
      title = "Please select a tree.",
      text = "You must first select a classification tree to perform this action.",
      type = "error"
    )

  } else {
    
    main_sections <- dplyr::filter(textbook, base::nchar(section) == 1)
    
    quarto_yaml1 <- c(
      'project:',
      '  type: website',
      '  render: ',
      '    - "*.qmd"',
      '    - "!data/"',
      '    - "!format/"',
      '',
      'website:',
      '  title: "template"',
      '  navbar:',
      '    logo: images/logo.png',
      '    search: true',
      '    pinned: true',
      '    background: dark',
      '    left:'
    )
    
    quarto_yaml2 <- base::paste0('      - sidebar:section', main_sections$section)
    
    quarto_yaml3 <- c( 
      '    right: ',
      '      - icon: github',
      '        url: https://github.com/NicolasJBM',
      '  sidebar:',
    )
    
    quarto_yaml4 <- c(
      base::paste0('    - id: section', main_sections$section[1]),
      base::paste0('      title: "', main_sections$title[1], '"'),
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
        base::paste0('      contents: section', main_sections$section[i])
      )
      quarto_yaml4 <- c(quarto_yaml4, add)
    }
    
    quarto_yaml5 <- c(
      '  page-navigation: true',
      '  reader-mode: true',
      '  page-footer:',
      '    center: "Name of the author"',
      '    border: true',
      '    background: dark',
      '',
      'bibliography: data/references.bib',
      '',
      'csl: format/csl/apa.csl',
      '',
      'format:',
      '  html:',
      '  css: format/css/pages.css',
      '',
      'toc: true',
      ''
    )
    
    

    textbook <- tree$textbook
    orginal_path <- course_paths$subfolders$original
    textbooks_path <- course_paths$subfolders$textbooks
    tree2compile <- stringr::str_remove(tree$course$tree[1], ".RData$")
    bibliography <- tree$course$bib[1]
    csl <- tree$course$csl[1]
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
        folder = paste0(textbooks_path, "/", folder)
      )
    for (folder in textbook$folder){
      if (!base::dir.exists(folder)) base::dir.create(folder)
    }



    # Fill in the textbook for the original language
    for (i in base::seq_len(base::nrow(textbook))){
      from = textbook$original_path[i]
      to = base::paste0(
        textbook$folder[i], "/_index.", textbook$languageweb[i],".Rmd"
      )
      lines <- base::readLines(from)
      core <- lines[1:(base::match('Meta-information', lines)-1)]

      core <- stringr::str_replace_all(core, "SOURCENAME", tree2compile)

      header <- c(
        '---',
        base::paste0('title: ', textbook$title[i]),
        base::paste0("order: ", textbook$order[i]),
        '---',
        '<hr>',
        '<br>'
      )

      newlines <- c(header, core)
      base::writeLines(newlines, to, useBytes = TRUE)
    }


    # Publish other languages files

    



    # Save textbook structure
    base::save(
      textbook,
      file = base::paste0(textbook$folder[1], "/textbook.RData")
    )



    # Save references
    bibfile <- base::paste0(course_paths$subfolders$temp, "/data/references.bib")
    if (base::file.exists(bibfile)) {
      base::file.copy(
        from = bibfile,
        to = base::paste0(textbook$folder[1], "/", tree2compile,".bib")
      )
    }



    shinybusy::remove_modal_spinner()

    shinyalert::shinyalert(
      title = "Textbook published!",
      text = "You can now access the files in the course materials folder.",
      type = "success"
    )
  }
}

