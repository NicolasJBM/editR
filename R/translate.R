#' @name translate
#' @title Translate a character string
#' @author Nicolas Mangin
#' @description Function translating character strings.
#' @param progress Charater. Identify the item or row which is being translated.
#' @param text Charater. Character string which is being translated.
#' @param langiso Character. ISO2 code of the language in which propositions and explanations should be translated.
#' @return Tibble. Translated propositions and explanations.
#' @importFrom purrr quietly
#' @importFrom polyglotr create_translation_table
#' @export


translate <- function(progress = NA, text, langiso){
  if (!base::is.na(progress)) shiny::setProgress(progress)
  base::Sys.sleep(1+base::round(stats::runif(1)*4,0))
  safe_translation <- purrr::quietly(polyglotr::create_translation_table)
  langiso <- base::tolower(langiso)
  z <- safe_translation(text, languages = langiso)
  base::as.character(z$result[langiso])
}
