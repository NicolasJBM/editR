#' @name add_form
#' @title Add a form for comments
#' @author Nicolas Mangin
#' @description Function writing the HTML code to give the possibility to make comments.
#' @param sourcename Character. 
#' @param filename Character. 
#' @param message Character. 
#' @return Write the HTML code for the form in the page
#' @export


add_form <- function(
  sourcename,
  filename,
  message = "If you have questions or comments about this page, please tell me below:"
){
  base::writeLines(
    c(
      '<hr>',
      '<div>',
      '  <p class="text-center">',
      base::paste0('  ', message),
      '  </p>',
      base::paste0('  <form id="comment-form" name="', sourcename, '-', filename, '">'),
      '    <textarea  type="text" class="commentry" name="inputcomment" required></textarea>',
      '    <input type="submit" class="comsubmit" />',
      '  </form>',
      '  <div id="form-success"></div>',
      '</div>'
    )
  )
}
