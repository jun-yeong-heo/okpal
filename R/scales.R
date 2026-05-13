#' Continuous sequential colour/fill scale using OKLAB interpolation
#'
#' Drop-in replacement for [ggplot2::scale_colour_gradient()] that
#' interpolates in OKLAB space instead of RGB.
#'
#' @param low Hex colour for the low end (default "#132B43").
#' @param high Hex colour for the high end (default "#56B1F7").
#' @param n Number of colours for the internal ramp (default 256).
#' @param space Interpolation space: "oklab" (default) or "oklch".
#' @param ... Additional arguments passed to
#'   [ggplot2::scale_colour_gradientn()].
#' @return A ggplot2 scale object.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_tile() +
#'   scale_fill_oklab(low = "#1B0A55", high = "#FDE725")
scale_colour_oklab <- function(low = "#132B43", high = "#56B1F7",
                               n = 256, space = c("oklab", "oklch"), ...) {
  space <- match.arg(space)
  pal <- if (space == "oklch") oklch_seq(low, high, n) else oklab_seq(low, high, n)
  ggplot2::scale_colour_gradientn(colours = pal, ...)
}

#' @rdname scale_colour_oklab
#' @export
scale_color_oklab <- scale_colour_oklab

#' @rdname scale_colour_oklab
#' @export
scale_fill_oklab <- function(low = "#132B43", high = "#56B1F7",
                             n = 256, space = c("oklab", "oklch"), ...) {
  space <- match.arg(space)
  pal <- if (space == "oklch") oklch_seq(low, high, n) else oklab_seq(low, high, n)
  ggplot2::scale_fill_gradientn(colours = pal, ...)
}

#' Continuous diverging colour/fill scale using OKLAB interpolation
#'
#' @param low Hex colour for the low end.
#' @param mid Hex colour for the midpoint (default white).
#' @param high Hex colour for the high end.
#' @param n Number of colours for the internal ramp (default 256).
#' @param space Interpolation space: "oklab" (default) or "oklch".
#' @param ... Additional arguments passed to
#'   [ggplot2::scale_colour_gradientn()].
#' @return A ggplot2 scale object.
#' @export
scale_colour_oklab_div <- function(low = "#2166AC", mid = "#F7F7F7",
                                   high = "#B2182B", n = 256,
                                   space = c("oklab", "oklch"), ...) {
  space <- match.arg(space)
  pal <- if (space == "oklch") {
    oklch_div(low, mid, high, n)
  } else {
    oklab_div(low, mid, high, n)
  }
  ggplot2::scale_colour_gradientn(colours = pal, ...)
}

#' @rdname scale_colour_oklab_div
#' @export
scale_color_oklab_div <- scale_colour_oklab_div

#' @rdname scale_colour_oklab_div
#' @export
scale_fill_oklab_div <- function(low = "#2166AC", mid = "#F7F7F7",
                                 high = "#B2182B", n = 256,
                                 space = c("oklab", "oklch"), ...) {
  space <- match.arg(space)
  pal <- if (space == "oklch") {
    oklch_div(low, mid, high, n)
  } else {
    oklab_div(low, mid, high, n)
  }
  ggplot2::scale_fill_gradientn(colours = pal, ...)
}

#' Discrete qualitative colour/fill scale using OKLCH
#'
#' Assigns evenly-spaced hues from OKLCH space with consistent
#' lightness and chroma.
#'
#' @param L Lightness (0-1, default 0.7).
#' @param C Chroma (default 0.15).
#' @param h_start Starting hue in degrees (default 0).
#' @param ... Additional arguments passed to
#'   [ggplot2::discrete_scale()].
#' @return A ggplot2 scale object.
#' @export
scale_colour_oklch_d <- function(L = 0.7, C = 0.15, h_start = 0, ...) {
  pal_fn <- function(n) oklch_qualitative(n, L = L, C = C, h_start = h_start)
  ggplot2::discrete_scale("colour", "oklch_d", palette = pal_fn, ...)
}

#' @rdname scale_colour_oklch_d
#' @export
scale_color_oklch_d <- scale_colour_oklch_d

#' @rdname scale_colour_oklch_d
#' @export
scale_fill_oklch_d <- function(L = 0.7, C = 0.15, h_start = 0, ...) {
  pal_fn <- function(n) oklch_qualitative(n, L = L, C = C, h_start = h_start)
  ggplot2::discrete_scale("fill", "oklch_d", palette = pal_fn, ...)
}
