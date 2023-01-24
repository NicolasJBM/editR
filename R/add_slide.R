#' @name add_slide
#' @title Insert a slide
#' @author Nicolas Mangin
#' @description Function writing a quarto revealjs slide according to the user specifications.
#' @param title Character. Title of the slide. If NA, the slide does not include separation.
#' @param section Logical. True if this is a section-title slide.
#' @param color Character. Background color.
#' @param gradient Character. Specify a css gradient as background instead of the background color (e.g. "-webkit-radial-gradient(top, circle cover, #246 0%, #013 80%)").
#' @param size Character. Whether the background should be "contain", "cover", or a specific size (e.g. "100px")
#' @param opacity Integer. From 0 for totally transparent to 1 for totally opaque
#' @param trans_data Character. Type of transition for data: fade, slide, convex, concave, or zoom.
#' @param trans_back Character. Type of transition for the background: fade, slide, convex, concave, or zoom.
#' @param image_url Character. URL of the background image or css specification for complex backgrounds.
#' @param image_position Character. Whether the background should be displayed at the "top", "right", "bottom", "left", "center", a=or another position.
#' @param image_repeat Character. Whether the image should "no-repeat", "repeat", "repeat-x" or repeat-y".
#' @param video_url Character. URL of the background video.
#' @param video_loop Character. "true" if the video should loop, "false" otherwise.
#' @param video_mute Character. "true" if the video should be muted, "false" otherwise.
#' @param iframe_url Character. URL of the background website.
#' @param iframe_interactive Character. "true" if the iframe should be interactive, "false" otherwise.
#' @param classes Character vector. Specifiy the names of other formatting classes which should be applied to the slide.
#' @param visible Logical. FALSE if the slide should be hidden.
#' @return Character. Write the first rows of a RevealJS slide formatted for quarto.
#' @importFrom dplyr case_when
#' @export



add_slide <- function(
    title = NA,
    section = FALSE,
    color = "#FFFFFF",
    gradient = NA,
    size = "cover",
    opacity = 1,
    trans_data = "fade",
    trans_back = "slide",
    image_url = NA,
    image_position = "center",
    image_repeat = "no-repeat",
    video_url = NA,
    video_loop = "false",
    video_mute = "true",
    iframe_url = NA,
    iframe_interactive = "false",
    classes = NA, # .scrollable .smaller
    visible = TRUE
){
  
  head <- dplyr::case_when(
    !base::is.na(title) & section ~ base::paste0("# ", title),
    !base::is.na(title) ~ base::paste0("## ", title),
    TRUE ~ base::paste0("##")
  )
  
  if (base::is.na(gradient)){
    start <- base::paste0(
      ' {background-color="', color,
      '" background-size="', size,
      '" background-opacity=', opacity,
      ' transition="', trans_data,
      '" background-transition="', trans_back
    )
  } else {
    start <- base::paste0(
      ' {background="', gradient,
      '" background-size="', size,
      '" background-opacity=', opacity,
      ' transition="', trans_data,
      '" background-transition="', trans_back
    )
  }
  
  if (!base::is.na(image_url)){
    content <- base::paste0(
      '" background-image="', image_url,
      '" background-position="', image_position,
      '" background-repeat="', image_repeat,
      '"'
    )
  } else if (!base::is.na(video_url)){
    content <- base::paste0(
      '" background-video="', video_url,
      '" background-video-loop="', video_loop,
      '" background-video-muted="', video_mute,
      '"'
    )
  } else if (!base::is.na(iframe_url)){
    content <- base::paste0(
      '" background-iframe="', iframe_url,
      '" background-interactive="', iframe_interactive,
      '"'
    )
  } else {
    content <- '"'
  }
  
  if (!base::is.na(classes)){
    classes <- base::paste(classes, sep = " .")
    classes <- base::paste0(' .', classes)
  } else classes <- ''
  if (visible){
    hide <- ''
  } else hide <- ' visibility="hidden"'
  
  end <- base::paste0(classes, hide, "}")
  
  text_to_write <- base::paste0(head, start, content, end)
  if (!base::is.na(title) & !section){
    base::writeLines(c(text_to_write, "<hr>"))
  } else {
    base::writeLines(text_to_write)
  }
}
