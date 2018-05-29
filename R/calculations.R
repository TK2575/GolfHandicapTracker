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
  diff_count <- pick_differential_count(index)

  if (diff_count > 0) {
    result <- handicap_differentials[1:index] %>%
      sort() %>%
      head(diff_count) %>%
      mean() * .96
  } else {result <- NA}

  result
}

v_compute_handicap_index <- Vectorize(compute_handicap_index)

find_first_index <- function(row_number) {
  return(max(1, row_number-19))
}

v_find_first_index <- Vectorize(find_first_index)

pick_differential_count <- function(count) {
  if (count < 5) {
    result <- 0
  } else if (count %in% c(5,6)) {
    result <- 1
  } else if (count %in% c(7,8)) {
    result <- 2
  } else if (count %in% c(9,10)) {
    result <- 3
  } else if (count %in% c(11,12)) {
    result <- 4
  } else if (count %in% c(13,14)) {
    result <- 5
  } else if (count %in% c(15,16)) {
    result <- 6
  } else if (count == 17) {
    result <- 7
  } else if (count == 18) {
    result <- 8
  } else if (count == 19) {
    result <- 9
  } else result <- 10

  result
}
