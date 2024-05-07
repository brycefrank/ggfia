
StatSubpLine <- ggplot2::ggproto(
  "StatSubpLine", ggplot2::Stat,
  compute_panel = function(data, scales) {
    data
  }
)

GeomSubpLine <- ggplot2::ggproto(
  "GeomSubpLine", ggplot2::GeomSegment,
  required_aes = c("x0", "y0"),
  default_aes = ggplot2::GeomSegment$default_aes,
  setup_data = function(data, params) {
    split_points_to_lines(data)
  }
)

#' FIA subplot lines
#'
#' This geom creates the three lines that eminate from plot center to each of
#' the outliyng subplots.
#'
#' @export
geom_subp_line <- function(mapping = NULL, data = NULL, stat = "identity",
  position = "identity", na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE, ...
) {

  ggplot2::layer(
    data = data, mapping = mapping, stat = StatSubpLine,
    geom = GeomSubpLine, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes, params = rlang::list2(na.rm = na.rm, ...)
  )
}