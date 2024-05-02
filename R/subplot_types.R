StatMacroplot <- ggplot2::ggproto(
  "StatMacroplot", ggplot2::Stat,
  compute_panel = function(data, scales, n = 360) {
    data$r <- 58.9 # TODO incorporate arbitrary units
    ggforce::StatCircle$compute_panel(data, scales, n = n)
  },
  required_aes = c("x0", "y0")
)

#' FIA macroplot
#'
#' A geom representing the four FIA macroplots
#'
#' @export
geom_macroplot <- function(mapping = NULL, data = NULL,
  stat = StatMacroplot, position = "identity", n = 360, expand = 0,
  radius = 0, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...
) {

  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomPlotCircles,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = rlang::list2(n = n, na.rm = na.rm, ...)
  )
}

StatMidplot <- ggplot2::ggproto(
  "StatMidplot", ggplot2::Stat,
  compute_panel = function(data, scales, n = 360) {
    data$r <- 24 # TODO incorporate arbitrary units
    ggforce::StatCircle$compute_panel(data, scales, n = n)
  },
  required_aes = c("x0", "y0")
)

#' FIA midplot
#'
#' A geom representing the four FIA midplots, sometimes referred to as
#' "subplots".
#'
#' @export
geom_midplot <- function(mapping = NULL, data = NULL,
  stat = StatMidplot, position = "identity", n = 360, expand = 0,
  radius = 0, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...
) {

  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomPlotCircles,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = rlang::list2(n = n, na.rm = na.rm, ...)
  )
}

StatMicroplot <- ggplot2::ggproto(
  "StatMicroplot", ggplot2::Stat,
  compute_panel = function(data, scales, n = 360) {
    data$r <- 6.8 # TODO incorporate arbitrary units
    data$x0 <- data$x0 + 12
    ggforce::StatCircle$compute_panel(data, scales, n = n)
  },
  required_aes = c("x0", "y0")
)

#' FIA microplot
#'
#' A geom representing the four FIA microplots
#'
#' @export
geom_microplot <- function(mapping = NULL, data = NULL,
  stat = StatMicroplot, position = "identity", n = 360, expand = 0,
  radius = 0, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...
) {

  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomPlotCircles,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = rlang::list2(n = n, na.rm = na.rm, ...)
  )
}