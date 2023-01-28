#' @name translate_exercise
#' @title Translate exercise
#' @author Nicolas Mangin
#' @description Function replacing propositions and explanation by their translations
#' @param exercise Tibble. Exercise table in the main language.
#' @param translations Tibble. Table associating translations to propositions.
#' @param languiso Character. ISO2 code of the language.
#' @return Tibble. Tanslated exercise.
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr left_join
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
  
  translated[,base::names(exercise)]
}
