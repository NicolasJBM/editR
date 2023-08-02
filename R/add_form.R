#' @name add_form
#' @title Add a form for comments
#' @author Nicolas Mangin
#' @description Function writing the HTML code to give the possibility to make comments at the end of a page or presentation. Note that this requires to adapt the javascript function so that the comment goes to the appropriate Google form.
#' @param sourcename Character. Name of the publication (blogpost, textbook, or presentation)
#' @param filename Character. Code and language of the document.
#' @param message Character. Comment entered by the reader.
#' @return Write the HTML code for the form in the page
#' @export


add_form <- function(
  sourcename,
  filename,
  message = "Please let us know below if you have questions or comments about this page:  "
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
