GeomFiaCluster <- ggplot2::ggproto(
  "GeomFiaCluster", ggforce::GeomCircle,
  required_aes = ggforce::GeomCircle$required_aes,
  default_aes = ggforce::GeomCircle$default_aes,
  setup_data = function(data, params) {
    split_subplots(data)
  }
)

geom_fia_cluster <- function(mapping = NULL, data = NULL,
  stat = ggforce::StatCircle, position = "identity", n = 360, expand = 0,
  radius = 0, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...
) {
  # FIXME some aesthetics should not be possible (e.g., position_jitter does
  # not work)
  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomFiaCluster,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = rlang::list2(n = n, na.rm = na.rm, ...)
  )
}