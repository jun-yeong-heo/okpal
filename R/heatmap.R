#' Generate colour vector for pheatmap using OKLAB/OKLCH interpolation
#'
#' Returns a hex colour vector suitable for use as the `color` argument
#' in [pheatmap::pheatmap()].
#'
#' @param low Hex colour for the low end.
#' @param high Hex colour for the high end.
#' @param n Number of colours (default 256).
#' @param space "oklab" (default) or "oklch".
#' @return Character vector of hex colour strings.
#' @export
#' @examples
#' \dontrun{
#' library(pheatmap)
#' mat <- matrix(rnorm(100), 10, 10)
#' pheatmap(mat, color = pheatmap_oklab("#1B0A55", "#FDE725"))
#' }
pheatmap_oklab <- function(low = "#132B43", high = "#56B1F7",
                           n = 256, space = c("oklab", "oklch")) {
  space <- match.arg(space)
  if (space == "oklch") oklch_seq(low, high, n) else oklab_seq(low, high, n)
}

#' Generate diverging colour vector for pheatmap
#'
#' @param low Hex colour for the low end.
#' @param mid Hex colour for the midpoint.
#' @param high Hex colour for the high end.
#' @param n Number of colours (default 256).
#' @param space "oklab" (default) or "oklch".
#' @return Character vector of hex colour strings.
#' @export
pheatmap_oklab_div <- function(low = "#2166AC", mid = "#F7F7F7",
                               high = "#B2182B", n = 256,
                               space = c("oklab", "oklch")) {
  space <- match.arg(space)
  if (space == "oklch") {
    oklch_div(low, mid, high, n)
  } else {
    oklab_div(low, mid, high, n)
  }
}

#' Generate a colorRamp2-compatible function for ComplexHeatmap
#'
#' Returns a [circlize::colorRamp2()] object where the internal colour
#' mapping uses OKLAB/OKLCH interpolation instead of RGB.
#'
#' @param breaks Numeric vector of breakpoints.
#' @param low Hex colour for the lowest break.
#' @param high Hex colour for the highest break.
#' @param space "oklab" (default) or "oklch".
#' @return A function compatible with ComplexHeatmap's `col` argument.
#' @export
#' @examples
#' \dontrun{
#' library(ComplexHeatmap)
#' mat <- matrix(rnorm(100), 10, 10)
#' col_fn <- cheatmap_oklab(
#'   breaks = seq(-3, 3, length.out = 256),
#'   low = "#1B0A55", high = "#FDE725"
#' )
#' Heatmap(mat, col = col_fn)
#' }
cheatmap_oklab <- function(breaks, low = "#132B43", high = "#56B1F7",
                           space = c("oklab", "oklch")) {
  space <- match.arg(space)
  if (!requireNamespace("circlize", quietly = TRUE)) {
    stop("Package 'circlize' is required for cheatmap_oklab()", call. = FALSE)
  }
  n <- length(breaks)
  pal <- if (space == "oklch") oklch_seq(low, high, n) else oklab_seq(low, high, n)
  circlize::colorRamp2(breaks, pal)
}

#' Generate a diverging colorRamp2-compatible function for ComplexHeatmap
#'
#' @param breaks Numeric vector of breakpoints.
#' @param low Hex colour for the low end.
#' @param mid Hex colour for the midpoint.
#' @param high Hex colour for the high end.
#' @param space "oklab" (default) or "oklch".
#' @return A function compatible with ComplexHeatmap's `col` argument.
#' @export
cheatmap_oklab_div <- function(breaks, low = "#2166AC", mid = "#F7F7F7",
                               high = "#B2182B",
                               space = c("oklab", "oklch")) {
  space <- match.arg(space)
  if (!requireNamespace("circlize", quietly = TRUE)) {
    stop("Package 'circlize' is required for cheatmap_oklab_div()", call. = FALSE)
  }
  n <- length(breaks)
  pal <- if (space == "oklch") {
    oklch_div(low, mid, high, n)
  } else {
    oklab_div(low, mid, high, n)
  }
  circlize::colorRamp2(breaks, pal)
}
