#' @name add_meme
#' @title Insert a slide
#' @author Nicolas Mangin
#' @description Function writing a revealjs slide according to the user specifications.
#' @param text_above Character. Sentence written above the image or video.
#' @param image Character. Address of the background image.
#' @param text_below Character. Sentence written below the image or video. 
#' @param font_size Numeric. Size of the font in "em".
#' @param transback Character. Type of transition for the background: fade, slide, convex, concave, or zoom.
#' @param transdata Character. Type of transition for data: fade, slide, convex, concave, or zoom.
#' @return Character. Write the first two rows of a RevealJS slide formatted for rmarkdown.
#' @importFrom dplyr case_when
#' @export


add_meme <- function(
    text_above = "",
    image = "",
    text_below = "",
    font_size = 2.5,
    transback = "slide",
    transdata = "fade"
) {
  properties <- base::paste0(
    '## {data-transition="', transdata,
    '" data-background-transition="', transback,
    '" data-background-color="#222',
    '" data-background="', image,
    '"}'
  )
  lines <- c(
    properties,
    base::paste0('<div class="meme" style="font-size: ', font_size, 'em;" >'),
    '  <p class="top">', text_above,'</p>',
    '  <p class="bottom">', text_below,'</p>',
    '</div>'
  )
  base::writeLines(lines)
}
