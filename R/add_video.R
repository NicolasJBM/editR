#' @name add_video
#' @title Embed a video
#' @author Nicolas Mangin
#' @description Function embedding a video as an iframe in a html page
#' @param vid Character. ID of the video.
#' @param width Character. Width of the video.
#' @param height Character. Height of the video.
#' @param start Numeric. Time at which the video should start.
#' @param host Character. Name of the hosting platform. For now, only "Youtube" is supported.
#' @return Write the HTML code embedding the videos.
#' @export


add_video <- function(vid, width = "100%", height = "100%", start = 0, host = "Youtube"){
  if (host == "Youtube"){
    lines <- base::paste0(
      '{{< video https://www.youtube.com/embed/',
      vid,
      ' width="', width,
      '" height="', height,
      '" start="', start,
      '" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture; fullscreen;" >}}'
    )
  }
  base::writeLines(lines)
}


