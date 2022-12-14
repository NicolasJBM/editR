#' @name make_tree_path
#' @title Report the position of a document in a tree
#' @author Nicolas Mangin
#' @description Function showing the path to the selected document.
#' @param doc Character. File name of the selected document.
#' @param tree Tibble. Tree in which the document should be positioned.
#' @return Character string indicating the path to the document in the tree.
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_remove_all
#' @export


make_tree_path <- function(doc, tree){
  
  position <- NULL
  text <- NULL
  
  classification <- tree |>
    dplyr::select(position, file, text) |>
    dplyr::mutate(position = stringr::str_remove_all(position, "\\.|0"))
  docpos <- classification |>
    dplyr::filter(file == doc) |>
    dplyr::select(position) |>
    base::as.character()
  depth <- base::nchar(docpos)
  sections <- base::character(depth)
  for (i in base::seq_len(depth)){
    section <- classification |>
      dplyr::filter(position == base::substr(docpos, 1, i)) |>
      dplyr::select(text)
    sections[i] <- section$text[1]
  }
  
  base::paste(sections, collapse = " | ")
}
