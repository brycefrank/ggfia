#' The relative positions of subplots
#'
#' @keywords internal
split_pos <- data.frame(
  subgroup = c(1, 2, 3, 4),
  diff_x = c(0, 0, 60 * sqrt(3), -60 * sqrt(3)),
  diff_y = c(0, 120, -60, -60)
)

#' Move an arbitrary set of coordinates into their subplot locations
#'
#' @param data A data frame containing x and y coordinates, a group column and a
#' subgroup column indicating the subplot membership of the coordinate.
#' @keywords internal
split_coords <- function(data) {
  data %>%
    dplyr::left_join(split_pos, by = "subgroup") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      group = paste0(.data$group, ".", .data$subgroup),
      x = .data$x + .data$diff_x,
      y = .data$y + .data$diff_y
    ) %>%
    dplyr::mutate(group = as.numeric(.data$group)) %>%
    as.data.frame()
}

filter_subplots <- function(data) {
  if(is.null(data$subp)) {
    return(data)
  } else {
    data <- data %>%
      dplyr::rowwise() %>%
      dplyr::filter(.data$subgroup %in% subp) %>%
      dplyr::ungroup() %>%
      as.data.frame()
  }
}

#' Splits circles into the FIA arrangement
#'
#' This function ingests circle data from `setup_data()`, replicates each circle
#' four times and positions them relative to plot center.
#'
#' @param data A data frame of circle coordinates from the `setup_data()` step.
#' @return A data frame with replicated circle coordinates.
#' @keywords internal
split_circles <- function(data) {
  n_elements <- length(unique(data$group))
  n_vertices <- nrow(data) / n_elements

  data %>%
    dplyr::mutate(group = as.numeric(factor(.data$group))) %>%
    dplyr::group_by(.data$group) %>%
    dplyr::slice(rep(seq_len(dplyr::n()), 4)) %>%
    dplyr::mutate(subgroup = rep(1:4, each = n_vertices)) %>%
    split_coords() %>%
    filter_subplots()
}

#' Splits points into the FIA arrangement
#' 
#' This function ingests point data from `setup_data()`, replicates each point
#' four times and positions them relative to plot center.
#'
#' @keywords internal
split_points <- function(data) {
  data %>%
    dplyr::mutate(id = 1:nrow(.)) %>%
    dplyr::rename(x = .data$x0, y = .data$y0) %>%
    dplyr::group_by(.data$id) %>%
    dplyr::slice(rep(seq_len(dplyr::n()), 4)) %>%
    dplyr::mutate(subgroup = 1:4) %>%
    split_coords()
}


#' Splits points into lines connecting to the outer subplots
#'
#' This function ingests point data from `setup_data()` and creates three
#' lines originating from splot center to each of the outer subplots.
#'
#' @keywords internal
split_points_to_lines <- function(data) {
  split_subp_centers <- split_points(data)

  # Parition into the center and outer data
  center_data <- split_subp_centers %>%
    dplyr::filter(.data$subgroup == 1)

  outer_data <- split_subp_centers %>%
    dplyr::filter(.data$subgroup != 1) %>%
    dplyr::rename(xend = .data$x, yend = .data$y) %>%
    dplyr::select(-c(PANEL, group, subgroup, diff_x, diff_y))

  center_data %>%
    dplyr::left_join(outer_data, by = c("id"))
}