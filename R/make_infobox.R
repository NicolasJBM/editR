#' @name make_infobox
#' @title Create infoboxes
#' @author Nicolas Mangin
#' @description Make infoboxes for ratings, views, results, or comments.
#' @param course_data Reactive. Function containing all the course data loaded with the course.
#' @param selected_file Character. Nale of the file for which statistics should be displayed.
#' @param infotype Character. Whether the infobox should display statistics about ratings, views, results, or comments.
#' @return Shinydashboard infoboxes
#' @importFrom dplyr filter
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shinydashboard valueBox
#' @importFrom stats na.omit
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
            icon = shiny::icon("star-half-alt"),
            color = page_ratings$average_color,
            width = 12
          )
        ),
        shiny::column(
          4,
          shinydashboard::valueBox(
            page_ratings$dispersion,
            "Dispersion",
            icon = shiny::icon("arrows-alt-h"),
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
          2,
          shinydashboard::valueBox(
            video_views$views,
            "Views",
            icon = shiny::icon("eye"),
            color = video_views$views_color,
            width = 12
          )
        ),
        shiny::column(
          2,
          shinydashboard::valueBox(
            video_views$viewers,
            "Viewers",
            icon = shiny::icon("users"),
            color = video_views$viewers_color,
            width = 12
          )
        ),
        shiny::column(
          2,
          shinydashboard::valueBox(
            video_views$watchtime,
            "Watchtime",
            icon = shiny::icon("clock"),
            color = video_views$watchtime_color,
            width = 12
          )
        ),
        shiny::column(
          2,
          shinydashboard::valueBox(
            video_views$retention,
            "Retention",
            icon = shiny::icon("battery-half"),
            color = video_views$retention_color,
            width = 12
          )
        ),
        shiny::column(
          2,
          shinydashboard::valueBox(
            video_views$duration,
            "Duration",
            icon = shiny::icon("hourglass"),
            color = video_views$duration_color,
            width = 12
          )
        ),
        shiny::column(
          2,
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
          2,
          shinydashboard::valueBox(
            document_parameters$answers,
            "Answers",
            icon = shiny::icon("users"),
            color = document_parameters$answers_color,
            width = 12
          )
        ),
        shiny::column(
          2,
          shinydashboard::valueBox(
            document_parameters$success,
            "success",
            icon = shiny::icon("thumbs-up"),
            color = document_parameters$success_color,
            width = 12
          )
        ),
        shiny::column(
          2,
          shinydashboard::valueBox(
            document_parameters$difficulty,
            "difficulty",
            icon = shiny::icon("graduation-cap"),
            color = document_parameters$difficulty_color,
            width = 12
          )
        ),
        shiny::column(
          2,
          shinydashboard::valueBox(
            document_parameters$discrimination,
            "discrimination",
            icon = shiny::icon("scale-unbalanced"),
            color = document_parameters$discrimination_color,
            width = 12
          )
        ),
        shiny::column(
          2,
          shinydashboard::valueBox(
            document_parameters$guess,
            "guess",
            icon = shiny::icon("dice"),
            color = document_parameters$guess_color,
            width = 12
          )
        ),
        shiny::column(
          2,
          shinydashboard::valueBox(
            document_parameters$accuracy,
            "accuracy",
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

