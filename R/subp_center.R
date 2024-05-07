StatSubpCenter <- ggplot2::ggproto(
  "StatSubpCenter", ggplot2::Stat,
  compute_panel = function(data, scales) {
    data
  }
)

GeomSubpCenter <- ggplot2::ggproto(
  "GeomSubpCenter", ggplot2::GeomPoint,
  required_aes = c("x0", "y0"),
  default_aes = ggplot2::GeomPoint$default_aes,
  setup_data = function(data, params) {
    split_points(data)
  }
)

#' FIA subplot centers
#'
#' This geom creates splits a set of input points into the four FIA plot centers
#'
#' @export
geom_subp_center <- function(mapping = NULL, data = NULL, stat = "identity",
  position = "identity", na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE, ...
) {

  ggplot2::layer(
    data = data, mapping = mapping, stat = StatSubpCenter,
    geom = GeomSubpCenter, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes, params = rlang::list2(na.rm = na.rm, ...)
  )
}