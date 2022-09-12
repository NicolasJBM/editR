#' @name display_curve
#' @title Display IRT curve
#' @author Nicolas Mangin
#' @description Function drawing the IRT curve associated to a selected question.
#' @param selected_model Tibble. Data produced by the function statistics_compute and stored in statistics$models for each question.
#' @return Plot a curve.
#' @import patchwork
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_histogram
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 ylim
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 scale_y_reverse
#' @export

display_curve <- function(selected_model){
  
  correct  <- NULL
  proficiency <- NULL
  prediction <- NULL
  
  above <- selected_model |>
    dplyr::filter(correct  == 1) |>
    ggplot2::ggplot(ggplot2::aes(x = proficiency)) +
    ggplot2::geom_histogram(binwidth = 0.1, na.rm = TRUE) +
    ggplot2::xlim(0,1) +
    ggplot2::labs(
      x = "",
      y = "Success"
    )
  middle <- selected_model |>
    dplyr::mutate(outcome = base::ifelse(prediction == "success", 1, 0)) |>
    ggplot2::ggplot(ggplot2::aes(proficiency, correct )) +
    ggplot2::geom_point(alpha = 0.2) +
    ggplot2::geom_smooth(
      method = "glm",
      method.args = base::list(family = "binomial"),
      formula = y ~ x,
      na.rm = TRUE
    ) +
    ggplot2::xlim(0,1) + ggplot2::ylim(0,1) +
    ggplot2::labs(
      x = "",
      y = "Probability of success"
    )
  below <- selected_model |>
    dplyr::filter(correct  == 0) |>
    ggplot2::ggplot(ggplot2::aes(x = proficiency)) +
    ggplot2::geom_histogram(binwidth = 0.1, na.rm = TRUE) +
    ggplot2::xlim(0,1) +
    ggplot2::labs(
      x = "Student's proficiency",
      y = "failure"
    ) +
    ggplot2::scale_y_reverse()
  above + middle + below +
    patchwork::plot_layout(widths = c(1), heights = c(1,4,1))
}
