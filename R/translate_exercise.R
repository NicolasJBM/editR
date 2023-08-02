#' @name translate_exercise
#' @title Translate exercise
#' @author Nicolas Mangin
#' @description Function replacing propositions and explanations by their translations
#' @param exercise Tibble. Exercise table in the main language.
#' @param translations Tibble. Table associating translations to propositions.
#' @param languiso Character. ISO2 code of the language in which propositions and explanations should be translated.
#' @return Tibble. Translated exercise.
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @export


translate_exercise <- function(exercise, translations, languiso){
  
  explanation <- NULL
  item <- NULL
  language <- NULL
  proposition <- NULL
  translated_explanation <- NULL
  translated_proposition <- NULL
  
  selection <- translations |>
    dplyr::filter(language == languiso, item %in% exercise$item) |>
    dplyr::select(
      item,
      proposition = translated_proposition,
      explanation = translated_explanation
    )
  
  translated <- exercise |>
    dplyr::select(-explanation, -proposition) |>
    dplyr::left_join(selection, by = "item")
  
  translated[, base::names(exercise)]
}
