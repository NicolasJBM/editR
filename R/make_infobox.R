#' @name make_infobox
#' @title Create infoboxes
#' @author Nicolas Mangin
#' @description Make infoboxes for ratings, views, results, or comments.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param selected_file Character. Name of the file for which statistics should be displayed.
#' @param infotype Character. Whether the infobox should display statistics about "ratings", "views", or "results" (comments are not implemented yet).
#' @return Shinydashboard infoboxes
#' @importFrom dplyr filter
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shinydashboard valueBox
#' @export


make_infobox <- function(course_data, selected_file, infotype){

  if (infotype == "ratings"){
    page_ratings <- course_data()$page_ratings |>
      dplyr::filter(file == selected_file) |>
      stats::na.omit()

    if (base::nrow(page_ratings) == 1){
      shiny::fluidRow(
        shiny::column(
          4,
          shinydashboard::valueBox(
            page_ratings$rates,
            "Rates",
            icon = shiny::icon("users"),
            color = page_ratings$rates_color,
            width = 12
          )
        ),
        shiny::column(
          4,
          shinydashboard::valueBox(
            page_ratings$average,
            "Average",
            icon = shiny::icon("star-half-stroke"),
            color = page_ratings$average_color,
            width = 12
          )
        ),
        shiny::column(
          4,
          shinydashboard::valueBox(
            page_ratings$dispersion,
            "Dispersion",
            icon = shiny::icon("left-right"),
            color = page_ratings$dispersion_color,
            width = 12
          )
        )
      )
    }

  } else if (infotype == "views"){

    video_views <- course_data()$video_views |>
      dplyr::filter(file == selected_file) |>
      stats::na.omit()

    if (base::nrow(video_views) == 1){
      shiny::fluidRow(
        shiny::column(
          4,
          shinydashboard::valueBox(
            video_views$views,
            "Views",
            icon = shiny::icon("eye"),
            color = video_views$views_color,
            width = 12
          ),
          shinydashboard::valueBox(
            video_views$viewers,
            "Viewers",
            icon = shiny::icon("users"),
            color = video_views$viewers_color,
            width = 12
          )
        ),
        shiny::column(
          4,
          shinydashboard::valueBox(
            video_views$watchtime,
            "Watchtime",
            icon = shiny::icon("clock"),
            color = video_views$watchtime_color,
            width = 12
          ),
          shinydashboard::valueBox(
            video_views$duration,
            "Duration",
            icon = shiny::icon("hourglass"),
            color = video_views$duration_color,
            width = 12
          )
        ),
        shiny::column(
          4,
          shinydashboard::valueBox(
            video_views$retention,
            "Retention",
            icon = shiny::icon("battery-half"),
            color = video_views$retention_color,
            width = 12
          ),
          shinydashboard::valueBox(
            video_views$repetition,
            "Repetition",
            icon = shiny::icon("rotate-right"),
            color = video_views$repetition_color,
            width = 12
          )
        )
      )
    }

  } else if (infotype == "results") {

    document_parameters <- course_data()$document_parameters |>
      dplyr::filter(file == selected_file) |>
      stats::na.omit()

    if (base::nrow(document_parameters)){
      shiny::fluidRow(
        shiny::column(
          4,
          shinydashboard::valueBox(
            document_parameters$answers,
            "Answers",
            icon = shiny::icon("users"),
            color = document_parameters$answers_color,
            width = 12
          ),
          shinydashboard::valueBox(
            document_parameters$success,
            "% Success",
            icon = shiny::icon("thumbs-up"),
            color = document_parameters$success_color,
            width = 12
          )
        ),
        shiny::column(
          4,
          shinydashboard::valueBox(
            document_parameters$difficulty,
            "Difficulty",
            icon = shiny::icon("graduation-cap"),
            color = document_parameters$difficulty_color,
            width = 12
          ),
          shinydashboard::valueBox(
            document_parameters$discrimination,
            "Discrimination",
            icon = shiny::icon("circle-half-stroke"),
            color = document_parameters$discrimination_color,
            width = 12
          )
        ),
        shiny::column(
          4,
          shinydashboard::valueBox(
            document_parameters$guess,
            "% Guess",
            icon = shiny::icon("dice"),
            color = document_parameters$guess_color,
            width = 12
          ),
          shinydashboard::valueBox(
            document_parameters$accuracy,
            "% Accuracy",
            icon = shiny::icon("bullseye"),
            color = document_parameters$accuracy_color,
            width = 12
          )
        )
      )
    }
  } else {
    #Comments
  }

}

