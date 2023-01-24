#' @name is_original_older
#' @title Check older file
#' @author Nicolas Mangin
#' @description Function returning whether the ortiginal file is older than the alternative file.
#' @param original_file Character. Absolute path to the original file.
#' @param alternative_file Character. Absolute path to the alternative (comparison or translation) file.
#' @return Logical. TRUE if the original file is older, FALSE otherwise.
#' @importFrom lubridate as_date
#' @export



is_original_older <- function(original_file, alternative_file){
  original_time <- lubridate::as_date(base::file.info(original_file)$mtime)
  comparison_time <- lubridate::as_date(base::file.info(alternative_file)$mtime)
  original_older <- original_time < comparison_time
  return(original_older)
}

