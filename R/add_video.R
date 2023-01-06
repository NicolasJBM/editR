#' @name add_video
#' @title Embed video
#' @author Nicolas Mangin
#' @description Function embedding a video as an ifram in an html page
#' @param vid Character. ID of the video.
#' @param host Character. Name of the hosting platform. For now, only "Youtube" is supported.
#' @return Write the HTML code embedding the videos.
#' @export


add_video <- function(vid, host = "Youtube"){
  if (host == "Youtube"){
    iframe <- c(
      '<br>',
      '<div class="video">',
      base::paste0(
        '  <iframe width="560" height="315" src="https://www.youtube.com/embed/',
        vid,
        '" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'
      ),
      '</div>',
      '<br>'
    )
  }
  base::writeLines(iframe)
}

