#' @name add_list
#' @title Animate bullet points
#' @author Nicolas Mangin
#' @description Function writing the HTML code to animate bullet points in slides.
#' @param  bullet_points Character vector. One entry for each bullet point to write.
#' @param  numbered Logical. Whether the bullet points should be numbered.
#' @param  animation Character. The kind of animation: "semi-fade-out", "grow", "shrink", "zoom-in", "strike", "fade-out", "fade-up", "fade-down", "fade-right", "fade-left", "highlight-red", "highlight-green", "highlight-blue"
#' @return Write the HTML code to animate bullet points in slides.
#' @export


add_list <- function(
  bullet_points, numbered = FALSE, animation = "semi-fade-out"
){
  bulletnbr <- base::length(bullet_points)
  displaybullet <- base::character(0)
  if (numbered){
    for (i in base::seq_len((bulletnbr-1))){
      displaybullet <- c(
        displaybullet,
        base::paste0(
          i, '. <span class="fragment ', animation, '"> ',
          bullet_points[i], ' </span>'
        )
      )
    }
    displaybullet <- c(
      displaybullet,
      base::paste0(bulletnbr, '. ', bullet_points[bulletnbr])
    )
  } else {
    for (i in base::seq_len((bulletnbr-1))){
      displaybullet <- c(
        displaybullet,
        base::paste0(
          '- <span class="fragment ', animation, '"> ',
          bullet_points[i], ' </span>'
        )
      )
    }
    displaybullet <- c(
      displaybullet,
      base::paste0('- ', bullet_points[bulletnbr])
    )
  }
  base::writeLines(displaybullet)
}

