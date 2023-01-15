#' @name add_video
#' @title Embed video
#' @author Nicolas Mangin
#' @description Function embedding a video as an ifram in an html page
#' @param vid Character. ID of the video.
#' @param start Numeric. Time at which the video should start.
#' @param host Character. Name of the hosting platform. For now, only "Youtube" is supported.
#' @return Write the HTML code embedding the videos.
#' @export


add_video <- function(vid, start = 0, host = "Youtube"){
  if (host == "Youtube"){
    iframe <- c(
      '::: {.video}',
      base::paste0('{{< video https://www.youtube.com/embed/', vid),
      base::paste0('  start="', start,'"'),
      '  aspect-ratio="21x9" ',
      '  frameborder="0" ',
      '  allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture"',
      '  allowfullscreen',
      '>}}',
      ':::'
    )
  }
  base::writeLines(iframe)
}


