#' @name publish_textbook
#' @title Compile a textbook
#' @author Nicolas Mangin
#' @description Function loading a tree and compiling the corresponding textbook. Pages are formatted and organized so that they can then be added to a quarto website.
#' @param tree List.
#' @param course_paths List.
#' @param languages Tibble
#' @return Write folders and pages to disk in the folder "4_materials/textbooks".
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
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
      '  pre-render: prepare.R',
      '  render: ',
      '    - "*.qmd"',
      '    - "!template/"',
      '',
      'execute:',
      '  eval: true',
      '  echo: false',
      '  output: asis',
      '  warning: false',
      '  error: false',
      '  include: true',
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
      '        url: https://github.com',
      '      - icon: linkedin',
      '        url: https://www.linkedin.com',
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
      'bibliography: data/references.bib',
      '',
      'csl: template/apa.csl',
      '',
      'format:',
      '  html:',
      '    theme: yeti',
      '    highlight-style: dracula',
      '    citations-hover: true',
      '    footnotes-hover: true',
      '    anchor-sections: true',
      '    smooth-scroll: true',
      '    css:',
      '      - template/pages.css',
      '      - template/all.css',
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
    formatfolder <- base::paste0(course_paths$subfolders$edit, "/format")
    datafolder <- base::paste0(course_paths$subfolders$edit, "/data")
    templatefolder <- base::paste0(course_paths$subfolders$edit, "/templates/textbooks")
    textbookfolder <- course_paths$subfolders$textbooks
    orginal_path <- course_paths$subfolders$original
    translated_path <- course_paths$subfolders$translated
    coursename <- stringr::str_remove(tree$course$tree[1], ".RData$")
    coursefolder <- base::paste0(textbookfolder, "/", coursename, "_", textbook$language[1])
    
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
    data_folder <- base::paste0(coursefolder, "/data")
    if (!base::dir.exists(data_folder)) base::dir.create(data_folder)

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
        '<br>'
      )

      newlines <- c(header, core)
      base::writeLines(newlines, to, useBytes = TRUE)
    }
    
    
    # Write YAML and course 
    yamlpath <- base::paste0(coursefolder, "/_quarto.yml")
    base::writeLines(yaml, yamlpath)
    base::save(
      textbook,
      file = base::paste0(data_folder, "/textbook.RData")
    )
    
    # Retrieve files from data
    envfile <- base::paste0(datafolder, "/environment.RData")
    if (base::file.exists(envfile)) {
      base::file.copy(
        from = envfile,
        to = base::paste0(data_folder, "/environment.RData"),
        overwrite = TRUE
      )
    }
    
    bibfile <- base::paste0(datafolder, "/references.bib")
    if (base::file.exists(bibfile)) {
      base::file.copy(
        from = bibfile,
        to = base::paste0(data_folder, "/references.bib"),
        overwrite = TRUE
      )
    }
    
    # Retrieve files from format
    cslfile <- base::paste0(formatfolder, "/csl/apa.csl")
    if (base::file.exists(cslfile)) {
      base::file.copy(
        from = cslfile,
        to = base::paste0(template_folder, "/apa.csl"),
        overwrite = TRUE
      )
    }
    
    csspagefile <- base::paste0(formatfolder, "/css/pages.css")
    if (base::file.exists(csspagefile)) {
      base::file.copy(
        from = csspagefile,
        to = base::paste0(template_folder, "/pages.css"),
        overwrite = TRUE
      )
    }
    
    cssallfile <- base::paste0(formatfolder, "/css/all.css")
    if (base::file.exists(cssallfile)) {
      base::file.copy(
        from = cssallfile,
        to = base::paste0(template_folder, "/all.css"),
        overwrite = TRUE
      )
    }
    
    jsscriptsfile <- base::paste0(formatfolder, "/js/scripts.js")
    base::file.copy(
      from = jsscriptsfile,
      to = base::paste0(template_folder, "/scripts.js"),
      overwrite = TRUE
    )
    
    # Retrieve files from template
    rprojfile <- base::paste0(templatefolder, "/textbook.Rproj")
    base::file.copy(
      from = rprojfile,
      to = base::paste0(coursefolder, "/textbook.Rproj"),
      overwrite = TRUE
    )
    
    preparefile <- base::paste0(templatefolder, "/prepare.R")
    base::file.copy(
      from = preparefile,
      to = base::paste0(coursefolder, "/prepare.R"),
      overwrite = TRUE
    )
    
    logofile <- base::paste0(templatefolder, "/logo.png")
    base::file.copy(
      from = logofile,
      to = base::paste0(template_folder, "/logo.png"),
      overwrite = TRUE
    )
    
    headerfile <- base::paste0(templatefolder, "/in_header.txt")
    base::file.copy(
      from = headerfile,
      to = base::paste0(template_folder, "/in_header.txt"),
      overwrite = TRUE
    )
    
    afterbodyfile <- base::paste0(templatefolder, "/after_body.txt")
    base::file.copy(
      from = afterbodyfile,
      to = base::paste0(template_folder, "/after_body.txt"),
      overwrite = TRUE
    )
    
    base::try(base::suppressWarnings(
      quarto::quarto_render(yamlpath, quiet = TRUE)
    )) 
    
    
    
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

