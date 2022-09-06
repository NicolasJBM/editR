#' @name make_new_name
#' @title Create a document name
#' @author Nicolas Mangin
#' @description Function listing the names of the existing documents and making a new name by adding one to the highest value. This ensures that no old number is assigned again.
#' @param firstletter Character. "N" for notes, "P" for pages, "S" for slides, "V" for video scripts, "G" for games, "C" for cases, "Q" for questions.
#' @param course_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return Character. Name of a new document to be created.
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_split
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#' @importFrom purrr map_dbl
#' @importFrom stringr str_remove_all
#' @export


make_new_name <- function(firstletter, course_paths){

  V1 <- NULL

  existing <- base::list.files(course_paths()$subfolders$original) |>
    stringr::str_remove_all(".Rmd") |>
    stringr::str_split(pattern = "_", simplify = TRUE) |>
    base::as.data.frame()

  if (base::nrow(existing) > 0){
    language <- base::paste0("_", existing[1,2])
    existing <- existing |>
      dplyr::filter(stringr::str_detect(V1, base::paste0("^", firstletter)))
  } else language <- "_US"

  if (base::nrow(existing) > 0){
    existing <- existing |>
      dplyr::mutate(increments = purrr::map_dbl(V1, function(x){
        base::as.numeric(stringr::str_remove_all(
          x, base::paste0("^", firstletter)
        ))
      }))
    newname <- base::max(existing$increment)+1
  } else newname <- 0

  newname <- base::paste(
    c(
      firstletter,
      base::paste(base::rep(0,9-base::nchar(newname)), collapse = ""),
      newname,
      language,
      ".Rmd"
    ),
    collapse = ""
  )
  return(newname)
}

