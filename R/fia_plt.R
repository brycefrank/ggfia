
attach_subgroup_id <- function(expanded_data) {

  expanded_data %>%
    group_by(group)
}

#' Splits a set of coordinates into their subplot locations
split_subplots <- function(data) {
  n_elements <- length(unique(data$group))
  n_vertices <- nrow(data) / n_elements

  shift <- data.frame(
    subgroup = c(1, 2, 3, 4),
    diff_x = c(0, 0, 60 * sqrt(3), -60 * sqrt(3)),
    diff_y = c(0, 120, -60, -60)
  )


  #replicated <- data %>%
  #  slice(rep(seq(n()), 4)) %>%
  #  mutate(group = as.integer(rep(1:4, each = n))) %>%
  #  left_join(shift, by = "group") %>%
  #  mutate(x = x + diff_x, y = y + diff_y)

  replicated <- data %>%
    mutate(group = as.numeric(factor(group))) %>%
    group_by(group) %>%
    slice(rep(seq(n()), 4)) %>%
    mutate(subgroup = rep(1:4, each = n_vertices)) %>%
    left_join(shift, by = "subgroup") %>%
    ungroup() %>%
    mutate(group = paste0(group, ".", subgroup), x = x + diff_x, y = y + diff_y) %>%
    mutate(group = as.numeric(group))

  replicated
}

GeomFiaPlt <- ggplot2::ggproto(
  "GeomFiaPlt", ggforce::GeomCircle,
  default_aes = ggforce::GeomCircle$default_aes,
  draw_panel = function(data, panel_params, coord) {
    data <- split_subplots(data)

    munched <- coord_munch(coord, data, panel_params)
    munched <- munched[order(munched$group), ]

    if (!is.integer(munched$group)) {
      munched$group <- match(munched$group, ggforce:::unique0(munched$group))
    }


    first_idx <- !duplicated(munched$group)
    first_rows <- munched[first_idx, ]



    shapeGrob(munched$x, munched$y,
      default.units = 'native',
      id = munched$group,
      gp = grid::gpar(
        col = first_rows$colour,
        fill = alpha(first_rows$fill, first_rows$alpha),
        lwd = (first_rows$linewidth %||% first_rows$size) * .pt,
        lty = first_rows$linetype
      )
    )
  }
)

geom_fia_plt <- function(mapping = NULL, data = NULL, stat = "circle",
  position = "identity", n = 360, expand = 0, radius = 0, na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE, ...
) {

  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomFiaPlt,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = rlang::list2(n = n, na.rm = na.rm, ...)
  )
}

