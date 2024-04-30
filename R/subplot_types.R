geom_macroplot <- function(mapping = NULL, data = NULL, stat = "circle",
  position = "identity", n = 360, expand = 0, radius = 0, na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE, ...) {
  geom_fia_cluster(..., r = 58.9)
}

geom_midplot <- function(...) {
  geom_fia_cluster(..., r = 24)
}