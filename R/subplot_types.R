GeomMacroplot <- ggplot2::ggproto(
  "GeomMacroplot", GeomFiaCluster,
  required_aes = c("x0", "y0"),
  default_aes = GeomFiaCluster$default_aes
)

geom_macroplot <- function(mapping = NULL, data = NULL,
  stat = ggforce::StatCircle, position = "identity", n = 360, expand = 0,
  radius = 0, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...
) {

  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomMacroplot,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = rlang::list2(n = n, na.rm = na.rm, ...)
  )
}