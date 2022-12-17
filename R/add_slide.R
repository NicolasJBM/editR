#' @name add_slide
#' @title Insert a slide
#' @author Nicolas Mangin
#' @description Function writing a revealjs slide according to the user specifications.
#' @param text Character. Text of the slide.
#' @param format Character. "section" for a section, "titled" for a slide with title, and "untitled" for a slide without title.
#' @param colorback Character. Background color.
#' @param image Character. Address of the background image.
#' @param video Character. Address of the background video.
#' @param webpage Character. Address of the background website.
#' @param transback Character. Type of transition for the background: fade, slide, convex, concave, or zoom.
#' @param transdata Character. Type of transition for data: fade, slide, convex, concave, or zoom.
#' @param state Character. "dimbg140" to "dimbg20" (20 by 20) for a decreasing brightness and "blurbg5" for a blur effect.
#' @return Character. Write the first two rows of a RevealJS slide formatted for rmarkdown.
#' @importFrom dplyr case_when
#' @export


add_slide <- function(
    text = "",
    format = "titled",
    colorback = "#FFFFFF",
    image = "",
    video = "",
    webpage="",
    transback = "slide",
    transdata = "fade",
    state = "dimbg100"
) {
  head <- dplyr::case_when(
    format == "section" ~ base::paste0("# ", text),
    TRUE ~ base::paste0("## ", text)
  )
  properties <- dplyr::case_when(
    image != "" & webpage != "" ~ base::paste0(
      ' {data-transition="', transdata,
      '" data-background-transition="', transback,
      '" data-background-color="', colorback,
      '" data-background="', image,
      '" data-background-iframe="', webpage,
      '" data-state="', state,
      '" data-background-size="cover"}'
    ),
    image != "" ~ base::paste0(
      ' {data-transition="', transdata,
      '" data-background-transition="', transback,
      '" data-background-color="', colorback,
      '" data-background="', image,
      '" data-state="', state,
      '" data-background-size="cover"}'
    ),
    video != "" ~ base::paste0(
      ' {data-transition="', transdata,
      '" data-background-transition="', transback,
      '" data-background-color="', colorback,
      '" data-background-video="', video,
      '" data-state="', state,
      '" data-background-size="cover"}'
    ),
    webpage != "" ~ base::paste0(
      ' {data-transition="', transdata,
      '" data-background-transition="', transback,
      '" data-background-color="', colorback,
      '" data-background-iframe="', webpage,
      '" data-state="', state,
      '" data-background-size="cover"}'
    ),
    TRUE ~ base::paste0(
      ' {data-transition="', transdata,
      '" data-background-transition="', transback,
      '" data-background-color="', colorback,
      '"}'
    )
  )
  text_to_write <- base::paste0(head, properties)
  if (format == "titled"){
    base::writeLines(c(text_to_write, "<hr>"))
  } else {
    base::writeLines(text_to_write)
  }
}
