#' @export
transform_inputs <- function(input_data) {
  result <- input_data %>%
    validate_inputs() %>%
    unique() %>%
    interpret_date()

  if (suppressWarnings(sum(result$nine_hole_round)) > 1) {
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
    dplyr::mutate(pph = putts/18) %>%
    add_rolling_average("hndcp_diff", "hndcp_index") %>% dplyr::rename(hndcp_index = name) %>%
    add_rolling_average("fir", "fir_avg") %>% dplyr::rename(fir_avg = name) %>%
    add_rolling_average("gir", "gir_avg") %>% dplyr::rename(gir_avg = name) %>%
    add_rolling_average("pph", "pph_avg") %>% dplyr::rename(pph_avg = name) %>%
    dplyr::mutate(course_handicap = compute_course_handicap(hndcp_index, slope)) %>%
    dplyr::mutate(net_score = compute_net_score(score, course_handicap)) %>%
    dplyr::mutate(net_over_under = as.integer(round(net_score - par))) %>%
    dplyr::select(-hndcp_diff)

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

compute_handicap_index <- function(index, samples) {
  diff_count <- pick_sample_size(index)
  result <- NA

  if (diff_count > 0) {
    first_index <- find_first_index(index)

    dt <- samples[first_index:index] %>%
      sort() %>%
      head(diff_count)

    result <- mean(dt) * .96

  } 
  
  result
}

compute_rolling_average <- function(index, samples) {
  diff_count <- pick_sample_size(index)
  result <- NA
  
  if (diff_count > 0) {
    result <- samples[index-diff_count:index] %>%
      mean()
  }
  
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
    df <- df[1:nrow(df) - 1,]
  }

  df <- df %>%
    dplyr::mutate(row_pair_index = 0:(nrow(df) - 1) %/% 2) %>%
    dplyr::group_by(row_pair_index) %>%
    dplyr::summarize(
      course = if (exists('course', where = .))
        dplyr::if_else(
          dplyr::n_distinct(course) > 1,
          paste(dplyr::first(course), dplyr::last(course), sep = " - "),
          dplyr::first(course)
        )
      else
        NA,
      tees = if (exists('tees', where = .))
        dplyr::if_else(
          dplyr::n_distinct(tees) > 1,
          paste(dplyr::first(tees), dplyr::last(tees), sep = " - "),
          dplyr::first(tees)
        )
      else
        NA,
      rating = sum(rating),
      slope = mean(slope),
      par = sum(par),
      transport = if (exists('transport', where = .))
        dplyr::if_else(
          dplyr::n_distinct(transport) > 1,
          "Various",
          dplyr::first(transport)
        )
      else
        NA,
      duration = if (exists('duration', where = .))
        sum(duration)
      else
        NA,
      score = sum(score),
      fairways_hit = sum(fairways_hit),
      fairways = sum(fairways),
      greens_in_reg = sum(greens_in_reg),
      putts = sum(putts),
      nine_hole_round = T,
      date = max(date),
      quarter = max(quarter)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-row_pair_index)

  df[, colSums(is.na(df)) != nrow(df)]
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

  # changes to optional columns need updates in compute_nine_hole_rounds too
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

interpret_date <- function(input_data) {
  input_data %>%
    unique() %>%
    dplyr::mutate(dt = lubridate::mdy(date)) %>%
    dplyr::select(-date) %>%
    dplyr::rename(date = dt) %>%
    dplyr::mutate(quarter = lubridate::floor_date(date, "quarter")) %>%
    dplyr::arrange(date)

}

add_rolling_average <- function(df, column, name) {
  samples <- df %>% dplyr::select(column) %>% dplyr::pull()
  
  name <-
    purrr::map(c(1:length(samples)),
               ifelse(column=="hndcp_diff",
                      compute_handicap_index,
                      compute_rolling_average),
               samples=samples) %>%
    unlist()
  
  df %>%
    tibble::add_column(name)
}

