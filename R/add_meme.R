#' @name add_meme
#' @title Insert a meme slide
#' @author Nicolas Mangin
#' @description Function writing a meme as a revealjs slide.
#' @param text_above Character. Sentence written above the image_url or video.
#' @param image_url Character. Address of the background image_url.
#' @param text_below Character. Sentence written below the image_url or video. 
#' @param font_size Numeric. Size of the font in "em".
#' @param trans_back Character. Type of transition for the background: fade, slide, convex, concave, or zoom.
#' @param trans_data Character. Type of transition for data: fade, slide, convex, concave, or zoom.
#' @return Character. Write the first two rows of a RevealJS slide formatted for rmarkdown.
#' @export



add_meme <- function(
    text_above = "",
    image_url = "",
    text_below = "",
    font_size = 2.5,
    trans_back = "slide",
    trans_data = "fade"
) {
  properties <- base::paste0(
    '## {data-transition="', trans_data,
    '" data-background-transition="', trans_back,
    '" data-background-color="#222',
    '" data-background="', image_url,
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
