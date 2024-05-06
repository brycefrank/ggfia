GeomPlotCircles <- ggplot2::ggproto(
  "GeomPlotCircles", ggforce::GeomCircle,
  required_aes = ggforce::GeomCircle$required_aes,
  default_aes = combine_aes(
    ggforce::GeomCircle$default_aes,
    aes(subp = list(c(1, 2, 3, 4)))
  ),
  setup_data = function(data, params) {
    split_circles(data)
  }
)

#' FIA subplot circles of arbitrary radius
#'
#' This function creates the four FIA subplot circles of a given radius,
#' and is typically used in the convenience wrappers `geom_macroplot()`,
#' `geom_midplot()`, and `geom_microplot()`, which set the radius to the
#' specified size for each subplot type.
#'
#' @export
geom_plot_circles <- function(mapping = NULL, data = NULL,
  stat = ggforce::StatCircle, position = "identity", n = 360,
  na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...
) {
  # TODO some aesthetics should not be possible (e.g., position_jitter does
  # not work)
  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomPlotCircles,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = rlang::list2(n = n, na.rm = na.rm, ...)
  )
}