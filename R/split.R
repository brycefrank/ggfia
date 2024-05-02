#' Splits a set of coordinates into their subplot locations
split_subplots <- function(data) {
  n_elements <- length(unique(data$group))
  n_vertices <- nrow(data) / n_elements

  shift <- data.frame(
    subgroup = c(1, 2, 3, 4),
    diff_x = c(0, 0, 60 * sqrt(3), -60 * sqrt(3)),
    diff_y = c(0, 120, -60, -60)
  )

  replicated <- data %>%
    dplyr::mutate(group = as.numeric(factor(.data$group))) %>%
    dplyr::group_by(.data$group) %>%
    dplyr::slice(rep(seq(dplyr::n()), 4)) %>%
    dplyr::mutate(subgroup = rep(1:4, each = n_vertices)) %>%
    dplyr::left_join(shift, by = "subgroup") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      group = paste0(.data$group, ".", .data$subgroup),
      x = .data$x + .data$diff_x, y = .data$y + .data$diff_y
    ) %>%
    dplyr::mutate(group = as.numeric(.data$group)) %>%
    as.data.frame()

  replicated
}