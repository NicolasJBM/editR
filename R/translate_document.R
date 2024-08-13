#' @name translate_document
#' @title Translate documents
#' @author Nicolas Mangin
#' @description Function translating non-translated propositions and explanations in the selected language.
#' @param filepath Character. Path to the original file to be translated.
#' @param langiso Character. ISO2 code of the language in lower cases.
#' @return Tibble. Translated propositions and explanations.
#' @importFrom dplyr anti_join
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom purrr map2_chr
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace_all
#' @importFrom stringr fixed
#' @importFrom tibble rowid_to_column
#' @importFrom tibble tibble
#' @export



translate_document <- function(filepath, langiso){
  
  characters <- NULL
  chunk <- NULL
  item <- NULL
  original <- NULL
  meta <- NULL
  translated <- NULL
  progress <- NULL
  
  lines <- tibble::tibble(
    original = base::readLines(filepath)
  ) |>
    tibble::rowid_to_column("item") |>
    dplyr::mutate(
      chunk = base::as.numeric(stringr::str_detect(original, "```") | stringr::str_detect(original, stringr::fixed("$$"))),
      chunk = base::cumsum(chunk),
      chunk = chunk %% 2 == 1,
      characters = base::nchar(original),
      meta = base::as.numeric(stringr::str_detect(original, "Meta-information")),
      meta = base::cumsum(meta),
      meta = meta %% 2 == 1
    )
  
  shiny::withProgress({
    translation <- lines |>
      dplyr::filter(chunk == FALSE, characters > 5, meta == FALSE) |>
      tibble::rowid_to_column("progress") |>
      dplyr::mutate(progress = base::as.numeric(progress)) |>
      dplyr::mutate(progress = progress / base::max(progress)) |>
      dplyr::mutate(
        translated = purrr::map2_chr(progress, original, editR::translate, langiso = langiso)
      ) |>
      dplyr::select(-progress)
  })
  
  export <- lines |>
    dplyr::anti_join(translation, by = "item") |>
    dplyr::bind_rows(translation) |>
    dplyr::mutate(translated = dplyr::case_when(
      chunk == TRUE ~ original,
      characters <= 5 ~ original,
      meta == TRUE ~ original,
      TRUE ~ translated
    )) |>
    dplyr::mutate(translated = stringr::str_replace_all(
      translated,
      'language.?=.?"en"',
      base::paste0('language = "', langiso, '"')
    )) |>
    dplyr::arrange(item)
  
  return(export)
}

  


  

