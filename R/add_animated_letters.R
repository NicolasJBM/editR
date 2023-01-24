#' @name add_animated_letters
#' @title Apply css animation
#' @author Nicolas Mangin
#' @description Apply css animation separately for each letter of the entered text
#' @param text Character Text to which the animation should be applied letter by letter.
#' @param animation Character. The kind of animation: "semi-fade-out", "grow", "shrink", "zoom-in", "strike", "fade-out", "fade-up", "fade-down", "fade-right", "fade-left", "highlight-red", "highlight-green", "highlight-blue"
#' @param color Character. CSS specification of the font size.
#' @param size Character. CSS specification of the font color.
#' @return Write the quarto code for the animation.
#' @export


add_animated_letters <- function(
  text = "You can write here a sentence.", animation = "neon", color = "#FFF;", size = "1em"
){
  text <- base::unlist(base::strsplit(text, " "))
  words <- c()
  for (w in 1:base::length(text)){
    letters <- c()
    for (l in 1:base::nchar(text[w])){
      letters[l] <- base::paste0("<span>", substr(text[w], l, l),"</span>")
    }
    words[w] <- base::paste0('<b>', base::paste(letters, collapse = ""), '</b>')
  }
  base::writeLines(c(
    base::paste0('<div class = "', animation, '" style="color: ', color,'; font-size: ', size, ';">'),
    base::paste(words, collapse = "&nbsp;"),
    '</div>'
  ))
}

