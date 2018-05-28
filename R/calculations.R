#' Inputted data expected for each golf round:
#' Date
#' Course
#' Rating
#' Slope
#' Par
#' Duration
#' Transport
#' Gross score
#' Fairways in Reg
#' Fairways to Hit
#' Greens in Reg
#' Putts
#'
#' Methods to compute the following:
#' Over/Under (Gross)
#' Course Handicap
#' Net score
#' Over/Under (Net)
#' Resultant Handicap Index (Adjusts based on number of prior samples)
#' Handicap Differential (Used for Course Handicap and Handicap Index)
#'
#' Other code (scripts/shiny app) will handle:
#' Display of per round data
#' Display of summary data
#' Trending of performance over time, pivoting on various features
#'
#'
transform_inputs <- function(input_data) {
  input_data %>%
    dplyr::mutate("Over/Under" = compute_over_under(Score, Par)) %>%
    dplyr::mutate("Handicap Differential" = compute_handicap_differential(Score, Rating, Slope)) %>%
    dplyr::mutate("ID" = row_number())
    # dplyr::mutate(first_id = v_find_first_index(ID))
    # handicap index
    # course handicap
    # net score
}

compute_over_under <- function(score, par) {
  return(score - par)
}

compute_course_handicap <- function(handicap_index, course_slope) {
  return(handicap_index * (course_slope/113))
}

compute_net_score <- function(score, course_handicap) {
  return(score-course_handicap)
}

compute_handicap_differential <- function(score, course_rating, course_slope) {
  return((score-course_rating) * (113/course_slope))
}

compute_handicap_index <- function(index, handicap_differentials) {
  # TODO check if handicap_differentials is a vector
  avg_count <- pick_differential_count(index)

  if (avg_count > 0) {
    best_differentials <- handicap_differentials[1:index] %>%
      sort() %>%
      head(avg_count)

    result <- mean(best_differentials) * .96
  } else {result <- NA}

  result
}

v_compute_handicap_index <- Vectorize(compute_handicap_index)

find_first_index <- function(row_number) {
  return(max(1, row_number-19))
}

v_find_first_index <- Vectorize(find_first_index)

pick_differential_count <- function(count) {
  # TODO check if count has length >= 5
  if (count < 5) {
    result <- 0
  } else if (count < 7) {
    result <- 1
  } else if (count < 9) {
    result <- 2
  } else if (count < 11) {
    result <- 3
  } else if (count < 13) {
    result <- 4
  } else if (count < 15) {
    result <- 5
  } else if (count < 17) {
    result <- 6
  } else if (count < 18) {
    result <- 7
  } else if (count < 19) {
    result <- 8
  } else if (count < 20) {
    result <- 9
  } else result <- 10

  result
}
