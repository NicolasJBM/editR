#' @name translate_propositions
#' @title Translate propositions
#' @author Nicolas Mangin
#' @description Function translating non-translated propositions and explanations in the selected language.
#' @param propositions Tibble. All the propositions.
#' @param translations Tibble. Table associating translations to propositions.
#' @param langiso Character. ISO2 code of the language in which propositions and explanations should be translated.
#' @return Tibble. Translated propositions and explanations.
#' @importFrom dplyr anti_join
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map2_chr
#' @export


translate_propositions <- function(propositions, translations, langiso){
  
  explanation <- NULL
  item <- NULL
  language <- NULL
  proposition <- NULL
  translated_explanation <- NULL
  translated_proposition <- NULL
  type <- NULL
  
  already_translated <- translations |>
    dplyr::filter(language == langiso) |>
  dplyr::select(item)
  
  to_translate <- propositions |>
    dplyr::anti_join(already_translated, by = "item") |>
    dplyr::select(type, item, proposition, explanation)
  
  if (base::nrow(to_translate) > 0){
    translated <- to_translate |>
      dplyr::mutate(
        translated_proposition = purrr::map2_chr(item, proposition, editR::translate, langiso = langiso),
        translated_explanation = purrr::map2_chr(item, explanation, editR::translate, langiso = langiso)
      ) |>
      dplyr::mutate(translated_proposition = dplyr::case_when(
        type == "Computation" ~ proposition,
        TRUE ~ translated_proposition),
        language = langiso
      ) |>
      dplyr::select(
        item,
        language,
        translated_proposition,
        translated_explanation
      )
    
    translations <- translations |>
      dplyr::bind_rows(translated) |>
      dplyr::arrange(item)
  } else {
    translations <- translations
  }
  
  return(translations)
}


