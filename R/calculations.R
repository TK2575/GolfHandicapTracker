# TODO support nine hole rounds
transform_inputs <- function(input_data) {
  result <- input_data %>%
    validate_inputs %>%
    unique %>%
    dplyr::mutate(dt = lubridate::mdy(date)) %>%
    dplyr::select(-date) %>%
    dplyr::rename(date = dt) %>%
    dplyr::mutate(quarter = lubridate::floor_date(date, "quarter")) %>%
    dplyr::arrange(date)

  if ("nine_hole_round" %in% names(result)) {
    nine_hole_rounds <- result %>%
      subset(nine_hole_round==T) %>%
      compute_nine_hole_rounds()

    eighteen_hole_rounds <- result %>%
      subset(nine_hole_round==F)

    result <- dplyr::bind_rows(eighteen_hole_rounds, nine_hole_rounds) %>%
      dplyr::arrange(date)
  }

  result %>%
    dplyr::mutate(over_under = compute_over_under(score, par),
                  hndcp_diff = compute_handicap_differential(score, rating, slope)) %>%
    dplyr::mutate(fir = fairways_hit/fairways * 100) %>%
    dplyr::mutate(gir = greens_in_reg/18 * 100) %>%
    dplyr::mutate(pph = putts/18)

  result <- add_handicap_indexes(result)

  result %>%
    dplyr::mutate(net_score = compute_net_score(score, course_handicap)) %>%
    dplyr::mutate(net_over_under = as.integer(round(net_score - par))) %>%
    select(-hndcp_diff)
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

validate_inputs <- function(input_data) {
  required_columns <- c("date",
                        "rating",
                        "slope",
                        "par",
                        "score",
                        "fairways_hit",
                        "fairways",
                        "greens_in_reg",
                        "putts")

  optional_columns <- c("course",
                        "tees",
                        "duration",
                        "transport",
                        "nine_hole_round")

  empty_columns <- colnames(input_data)[which(colSums(is.na(input_data)) == nrow(input_data))]

  result <- input_data %>% dplyr::select(-empty_columns)

  if (length(required_columns) > sum(required_columns %in% names(result))) {
    stop("Missing or Empty Required column(s)")
  }

  if (length(optional_columns) > sum(optional_columns %in% names(result))) {
    warning("Missing or Empty Optional column(s)")
  }

}

add_handicap_indexes <- function(df) {
  diffs <- df %>% select(hndcp_diff) %>% dplyr::pull()
  purrr::map(
    .x=c(1:length(diff)),
    .f=compute_handicap_index,
    handicap_differentials=diff)

  hndcp_index <-
    purrr::map(c(1:length(diffs)),compute_handicap_index,handicap_differentials = diffs) %>%
    unlist()

  df %>%
    tibble::add_column(hndcp_index) %>%
    dplyr::mutate(course_handicap = compute_course_handicap(hndcp_index, slope))
}

compute_nine_hole_rounds <- function(df) {
  if (nrow(df) %% 2 != 0) {
    df <- df[1:nrow(df)-1,]
  }

# TODO - from each odd numbered row, "sum" up it and the lead (next even numbered) row
}
