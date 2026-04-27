# Internal utilities for okpal
# These are not exported

#' Interpolate between two colours in OKLAB space
#' @noRd
interp_oklab <- function(col1, col2, n = 256) {
  lab1 <- farver::decode_colour(col1, to = "oklab")
  lab2 <- farver::decode_colour(col2, to = "oklab")

  t <- seq(0, 1, length.out = n)
  mat <- cbind(
    l = lab1[1, "l"] + t * (lab2[1, "l"] - lab1[1, "l"]),
    a = lab1[1, "a"] + t * (lab2[1, "a"] - lab1[1, "a"]),
    b = lab1[1, "b"] + t * (lab2[1, "b"] - lab1[1, "b"])
  )

  farver::encode_colour(mat, from = "oklab")
}

#' Interpolate between two colours in OKLCH space
#' Handles hue wrapping via shortest-path logic
#' @noRd
interp_oklch <- function(col1, col2, n = 256) {
  lch1 <- farver::decode_colour(col1, to = "oklch")
  lch2 <- farver::decode_colour(col2, to = "oklch")

  t <- seq(0, 1, length.out = n)

  # Shortest-path hue interpolation
  h1 <- lch1[1, "h"]
  h2 <- lch2[1, "h"]
  dh <- h2 - h1
  if (dh > 180) dh <- dh - 360
  if (dh < -180) dh <- dh + 360

  mat <- cbind(
    l = lch1[1, "l"] + t * (lch2[1, "l"] - lch1[1, "l"]),
    c = lch1[1, "c"] + t * (lch2[1, "c"] - lch1[1, "c"]),
    h = (h1 + t * dh) %% 360
  )

  farver::encode_colour(mat, from = "oklch")
}

#' Interpolate across multiple anchor colours
#' @param colours character vector of hex colours
#' @param n total number of output colours
#' @param space "oklab" or "oklch"
#' @noRd
interp_multi <- function(colours, n = 256, space = "oklab") {
  nc <- length(colours)
  if (nc < 2) stop("At least 2 colours required", call. = FALSE)
  if (nc == 2) {
    if (space == "oklab") return(interp_oklab(colours[1], colours[2], n))
    if (space == "oklch") return(interp_oklch(colours[1], colours[2], n))
  }

  # Distribute n across segments proportionally
  segments <- nc - 1
  per_seg <- rep(floor(n / segments), segments)
  remainder <- n - sum(per_seg)
  if (remainder > 0) {
    per_seg[seq_len(remainder)] <- per_seg[seq_len(remainder)] + 1L
  }

  interp_fn <- if (space == "oklab") interp_oklab else interp_oklch

  result <- character(0)
  for (i in seq_len(segments)) {
    seg <- interp_fn(colours[i], colours[i + 1], per_seg[i] + 1L)
    # Drop last colour of each segment except the final one to avoid duplicates
    if (i < segments) seg <- seg[-length(seg)]
    result <- c(result, seg)
  }

  result
}

#' Compute pairwise OKLAB distances for a set of hex colours
#' @noRd
oklab_dist_matrix <- function(colours) {
  lab <- farver::decode_colour(colours, to = "oklab")
  n <- nrow(lab)
  d <- matrix(0, n, n)
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      dd <- sqrt(sum((lab[i, ] - lab[j, ])^2))
      d[i, j] <- dd
      d[j, i] <- dd
    }
  }
  d
}
