GeomFiaCluster <- ggplot2::ggproto(
  "GeomFiaCluster", ggforce::GeomCircle,
  required_aes = ggforce::GeomCircle$required_aes,
  default_aes = ggforce::GeomCircle$default_aes,
  setup_data = function(data, params) {
    # FIXME this causes issues when sent to GeomShape, becuase the group
    # assignment has changed, resulting in warnings for first_rows$linewidth
    # Is there a simpler way to do this further "upstream"? e.g., with a stat?
    split_subplots(data)
  }
)

geom_fia_cluster <- function(mapping = NULL, data = NULL,
  stat = ggforce::StatCircle, position = "identity", n = 360, expand = 0,
  radius = 0, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...
) {

  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomFiaCluster,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = rlang::list2(n = n, na.rm = na.rm, ...)
  )
}