#' @name get_tags
#' @title Get a document's tags
#' @author Nicolas Mangin
#' @description Function reading a document and retrieving the tags listed in the meta-information at the end.
#' @param path character. Path to the document for which meta-information should be retrieved.
#' @return A single-row tibble with all the tags from a document.
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove_all
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr separate
#' @export


get_tags <- function(path){
  tag <- NA
  content <- NA
  lines <- base::readLines(path)
  metainfo <- lines[base::match('Meta-information', lines):base::length(lines)][-c(1,2)]
  metainfo <- metainfo[stringr::str_detect(metainfo, "exextra")]
  metainfo <- tibble::tibble(
    metainfo = metainfo
  ) |>
    dplyr::mutate(metainfo = stringr::str_remove_all(metainfo, "exextra\\[")) |>
    dplyr::mutate(metainfo = stringr::str_remove_all(metainfo, "\\]")) |>
    dplyr::mutate(metainfo = base::trimws(metainfo)) |>
    tidyr::separate("metainfo", into = c("tag","content"), sep = ":")|>
    base::suppressWarnings() |>
    dplyr::mutate(
      tag = base::trimws(tag),
      content = base::trimws(content)
    ) |>
    tidyr::pivot_wider(names_from = "tag", values_from = "content")
}
