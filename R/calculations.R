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
#' Percent FIR, GIR
#'
#' Other code (scripts/shiny app) will handle:
#' Display of per round data
#' Display of summary data
#' Trending of performance over time, pivoting on various features
#'
#'
transform_inputs <- function(input_data) {
  result <- input_data %>%
    dplyr::mutate("Over/Under" = compute_over_under(Score, Par),
                  hndcp_diff = compute_handicap_differential(Score, Rating, Slope))

    diffs <- result %>% select(hndcp_diff) %>% dplyr::pull()
    purrr::map(
     .x=c(1:length(diff)),
     .f=compute_handicap_index,
     handicap_differentials=diff)

    hndcp_indexes <-
      purrr::map(c(1:length(diffs)),compute_handicap_index,handicap_differentials = diffs) %>%
      unlist()

    result %>%
      tibble::add_column(hndcp_indexes) %>%
      dplyr::mutate("Course Handicap" = compute_course_handicap(hndcp_indexes, Slope)) %>%
      dplyr::rename("Handicap Index" = hndcp_indexes) %>%
      dplyr::mutate("Net Score" = compute_net_score(Score, `Course Handicap`)) %>%
      dplyr::mutate(dt = lubridate::mdy(`Date`)) %>%
      dplyr::select(-hndcp_diff, -`Date`) %>%
      dplyr::rename("Date" = dt) %>%
      dplyr::mutate("FIR" = `Fairways Hit`/`Fairways To Hit` * 100) %>%
      dplyr::mutate("GIR" = `Greens in Reg`/18 * 100)
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
  diff_count <- pick_sample_size(index)

  if (diff_count > 0) {
    first_index <- find_first_index(index)

    dt <- handicap_differentials[first_index:index] %>%
      sort() %>%
      head(diff_count)

    result <- mean(dt) * .96

  } else {result <- NA}

  result
}

find_first_index <- function(row_number) {
  return(max(1, row_number-19))
}

v_find_first_index <- Vectorize(find_first_index)

pick_sample_size <- function(count) {
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
