# TODO finish support nine hole rounds
# TODO update namespace and/or roxygen comments to identify public/private functions
transform_inputs <- function(input_data) {
  result <- input_data %>%
    validate_inputs() %>%
    initial_transformation()

  if (suppressWarnings(sum(result$nine_hole_round)) > 1) {
    nine_hole_rounds <- result %>%
      subset(nine_hole_round==T) %>%
      compute_nine_hole_rounds()

    eighteen_hole_rounds <- result %>%
      subset(nine_hole_round==F)

    result <- dplyr::bind_rows(eighteen_hole_rounds, nine_hole_rounds) %>%
      dplyr::arrange(date)
  }

  result <- result %>%
    second_transformation() %>%
    add_handicap_indexes

  result %>%
    dplyr::mutate(net_score = compute_net_score(score, course_handicap)) %>%
    dplyr::mutate(net_over_under = as.integer(round(net_score - par))) %>%
    dplyr::select(-hndcp_diff)

  result
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

find_first_index <- function(row_number) {
  return(max(1, row_number-19))
}

compute_nine_hole_rounds <- function(df) {
  if (nrow(df) %% 2 != 0) {
    df <- df[1:nrow(df)-1,]
  }

# TODO pass df in pairs of rows to merge_nine_hole_rounds(), store each result in new df

}

merge_nine_hole_rounds <- function(df) {
  if (nrow(df) != 2) {
    stop("Invalid nine_hole_round input")
  }
  # TODO check for equality in course/tees/transport and vary output as a result
  df %>% dplyr::summarize
  (
    course = "nine-hole aggregate",
    tees = "nine-hole aggregate",
    rating = sum(rating),
    slope = mean(slope),
    par = sum(par),
    transport = "nine-hole aggregate",
    score = sum(score),
    fairways_hit = sum(fairways_hit),
    fairways = sum(fairways),
    greens_in_reg = sum(greens_in_reg),
    putts = sum(putts),
    nine_hole_round = T,
    date = max(date),
    quarter = max(quarter)
  )
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

  all_columns <- c(required_columns, optional_columns)

  empty_columns <- colnames(input_data)[which(colSums(is.na(input_data)) == nrow(input_data))]

  result <- input_data %>% dplyr::select(-dplyr::one_of(empty_columns))

  if (length(required_columns) > sum(required_columns %in% names(result))) {
    stop("Missing or Empty Required column(s)")
  }

  if (length(optional_columns) > sum(optional_columns %in% names(result))) {
    warning("Missing or Empty Optional column(s)")
  }

  result[, names(result) %in% all_columns]
}

initial_transformation <- function(input_data) {
  input_data %>%
    unique() %>%
    dplyr::mutate(dt = lubridate::mdy(date)) %>%
    dplyr::select(-date) %>%
    dplyr::rename(date = dt) %>%
    dplyr::mutate(quarter = lubridate::floor_date(date, "quarter")) %>%
    dplyr::arrange(date)

}

second_transformation <- function(input_data) {
  result <- input_data %>%
    dplyr::mutate(over_under = compute_over_under(score, par),
                  hndcp_diff = compute_handicap_differential(score, rating, slope)) %>%
    dplyr::mutate(fir = fairways_hit/fairways * 100) %>%
    dplyr::mutate(gir = greens_in_reg/18 * 100) %>%
    dplyr::mutate(pph = putts/18)

  result
}

add_handicap_indexes <- function(df) {
  diffs <- df %>% dplyr::select(hndcp_diff) %>% dplyr::pull()
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

