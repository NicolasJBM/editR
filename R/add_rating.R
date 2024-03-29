#' @name add_rating
#' @title Add star rating
#' @author Nicolas Mangin
#' @description Function writing the HTML code to include the page star rating. Note that this requires to adapt the javascript function so that the comment goes to the appropriate Google form.
#' @param sourcename Character. Name of the publication (blogpost, textbook, or presentation)
#' @param filename Character. Code and language of the document.
#' @param message Character. Comment entered by the reader.
#' @return Write the HTML code for the star rating in the page
#' @export


add_rating <- function(
  sourcename,
  filename,
  message = "Please let us know below how clear and understandable this page was for you:  "
){
  base::writeLines(
    c(
      '<hr>',
      '<section id="rating" class="rating-widget">',
      '  <p class="text-center">',
      base::paste0('  ', message),
      '  </p>',
      base::paste0(
        '  <div id="', sourcename, '-', filename,
        '" class="rating-stars text-center">'
      ),
      '  <ul id="stars">',
      '  <li class="star" title="Bad" data-value="1">',
      '  <i class="fa fa-star fa-fw"></i>',
      '  </li>',
      '  <li class="star" title="Poor" data-value="2">',
      '  <i class="fa fa-star fa-fw"></i>',
      '  </li>',
      '  <li class="star" title="Fair" data-value="3">',
      '  <i class="fa fa-star fa-fw"></i>',
      '  </li>',
      '  <li class="star" title="Good" data-value="4">',
      '  <i class="fa fa-star fa-fw"></i>',
      '  </li>',
      '  <li class="star" title="Excellent" data-value="5">',
      '  <i class="fa fa-star fa-fw"></i>',
      '  </li>',
      '  </ul>',
      '  </div>',
      '  <div class="success-box">',
      '  <div class="clearfix">',
      '  </div>',
      '  <div class="text-message">',
      '  </div>',
      '  <div class="clearfix">',
      '  </div>',
      '  </div>',
      '</section>'
    )
  )
}
