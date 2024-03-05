#' @name make_tree_path
#' @title Report the position of a document in a tree.
#' @author Nicolas Mangin
#' @description Function showing the path to the selected document in the selected tree.
#' @param doc Character. File name of the selected document.
#' @param tree Tibble. Tree in which the document should be positioned.
#' @return Character string indicating the path to the document in the tree.
#' @importFrom dplyr filter
#' @importFrom dplyr mutate_all
#' @importFrom dplyr select
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_split
#' @importFrom tidyr separate
#' @export


make_tree_path <- function(doc, tree){
  
  position <- NULL
  title <- NULL
  path <- NULL
  
  levels <- base::length(stringr::str_split(tree$position[[1]], "-", simplify = TRUE))+1
  
  classification <- tree |>
    dplyr::filter(file == doc) |>
    dplyr::select(path, file, title) |>
    tidyr::separate(path, into = base::paste0("LEV",base::seq_len(levels)), sep = "/", fill = "right") |>
    dplyr::mutate_all(function(x,y) stringr::str_remove_all(x,y), y = doc)
  
  classification <- base::as.character(classification[1,])
  classification <- base::as.character(stats::na.omit(classification[classification != ""]))
  
  depth <- base::length(classification)
  
  for (i in base::seq_len(depth)){
    if (i == 1){
      classification[i] <- base::paste0('<p style="color:#555555;font-weight:bold;font-size:1em;">', i, " - ", classification[i], '</p>')
    } else if (i < depth) {
      classification[i] <- base::paste0('<p style="color:#000099;font-weight:bold;font-size:1.25em;">', i, " - ", classification[i], '</p>')
    } else {
      classification[i] <- base::paste0('<p style="color:#990000;font-weight:bold;font-size:1.5em;">', i, " - ", classification[i], '</p>')
    }
  }
  
  base::paste0('<span style="font-size:1em;">', classification, '</span>')
}
