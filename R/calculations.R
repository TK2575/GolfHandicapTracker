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
    dplyr::mutate(over_under = compute_over_under(Score, Par),
                  hndcp_diff = compute_handicap_differential(Score, Rating, Slope))

    diffs <- result %>% select(hndcp_diff) %>% dplyr::pull()
    purrr::map(
     .x=c(1:length(diff)),
     .f=compute_handicap_index,
     handicap_differentials=diff)

    hndcp_index <-
      purrr::map(c(1:length(diffs)),compute_handicap_index,handicap_differentials = diffs) %>%
      unlist()

    result %>%
      tibble::add_column(hndcp_index) %>%
      dplyr::mutate(course_handicap = compute_course_handicap(hndcp_index, Slope)) %>%
      dplyr::mutate(net_score = compute_net_score(Score, course_handicap)) %>%
      dplyr::mutate(dt = lubridate::mdy(Date)) %>%
      dplyr::select(-hndcp_diff, -Date) %>%
      dplyr::rename(date = dt) %>%
      dplyr::mutate(quarter = lubridate::floor_date(date, "quarter")) %>%
      dplyr::mutate(fir = `Fairways Hit`/`Fairways To Hit` * 100) %>%
      dplyr::mutate(gir = `Greens in Reg`/18 * 100) %>%
      dplyr::mutate(pph = Putts/18) %>%
      dplyr::mutate(net_over_under = as.integer(round(net_score - Par)))
    # TODO group slopes into bins
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
