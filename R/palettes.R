#' Generate a sequential palette via OKLAB interpolation
#'
#' Interpolates between two colours in OKLAB space, producing perceptually
#' uniform gradients without the muddy mid-tones of RGB interpolation.
#'
#' @param low Hex colour string for the low end.
#' @param high Hex colour string for the high end.
#' @param n Number of colours to generate (default 256).
#' @return Character vector of hex colour strings.
#' @export
#' @examples
#' oklab_seq("#1B0A55", "#FDE725", n = 10)
oklab_seq <- function(low, high, n = 256) {
  .check_hex(low, "low"); .check_hex(high, "high")
  interp_oklab(low, high, n)
}

#' Generate a diverging palette via OKLAB interpolation
#'
#' Interpolates low -> mid and mid -> high separately in OKLAB space,
#' then joins them.
#'
#' @param low Hex colour string for the low end.
#' @param mid Hex colour string for the midpoint.
#' @param high Hex colour string for the high end.
#' @param n Number of colours to generate (default 256).
#' @return Character vector of hex colour strings.
#' @export
#' @examples
#' oklab_div("#2166AC", "#F7F7F7", "#B2182B", n = 11)
oklab_div <- function(low, mid, high, n = 256) {
  .check_hex(low, "low"); .check_hex(mid, "mid"); .check_hex(high, "high")
  interp_multi(c(low, mid, high), n, space = "oklab")
}

#' Generate a multi-anchor palette via OKLAB interpolation
#'
#' Interpolates through an arbitrary number of anchor colours in OKLAB space.
#'
#' @param colours Character vector of hex colour strings (at least 2).
#' @param n Number of colours to generate (default 256).
#' @return Character vector of hex colour strings.
#' @export
#' @examples
#' oklab_multi(c("#440154", "#21918C", "#FDE725"), n = 15)
oklab_multi <- function(colours, n = 256) {
  .check_hex(colours, "colours")
  interp_multi(colours, n, space = "oklab")
}

#' Generate a sequential palette via OKLCH interpolation
#'
#' Interpolates between two colours in OKLCH space. Unlike OKLAB, this
#' preserves chroma through the interpolation path and rotates hue along
#' the shortest arc, producing more vivid transitions between different hues.
#'
#' @inheritParams oklab_seq
#' @return Character vector of hex colour strings.
#' @export
#' @examples
#' oklch_seq("#FF0000", "#0000FF", n = 10)
oklch_seq <- function(low, high, n = 256) {
  .check_hex(low, "low"); .check_hex(high, "high")
  interp_oklch(low, high, n)
}

#' Generate a diverging palette via OKLCH interpolation
#'
#' @inheritParams oklab_div
#' @return Character vector of hex colour strings.
#' @export
#' @examples
#' oklch_div("#2166AC", "#F7F7F7", "#B2182B", n = 11)
oklch_div <- function(low, mid, high, n = 256) {
  .check_hex(low, "low"); .check_hex(mid, "mid"); .check_hex(high, "high")
  interp_multi(c(low, mid, high), n, space = "oklch")
}

#' Generate a qualitative palette in OKLCH space
#'
#' Creates a set of evenly-spaced hues at fixed lightness and chroma
#' in OKLCH space. Because OKLCH is perceptually uniform, the resulting
#' colours have consistent visual weight.
#'
#' @param n Number of colours (default 8).
#' @param L Lightness (0-1 scale, default 0.7).
#' @param C Chroma (default 0.15).
#' @param h_start Starting hue in degrees (default 0).
#' @return Character vector of hex colour strings.
#' @export
#' @examples
#' oklch_qualitative(6)
oklch_qualitative <- function(n = 8, L = 0.7, C = 0.15, h_start = 0) {
  hues <- (h_start + seq(0, 360, length.out = n + 1)[seq_len(n)]) %% 360
  mat <- cbind(l = rep(L, n), c = rep(C, n), h = hues)
  farver::encode_colour(mat, from = "oklch")
}
