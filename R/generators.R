#' Generate a palette from a single base colour
#'
#' Creates a sequential palette anchored on the given base colour by
#' varying lightness and (optionally) hue in OKLCH space. With
#' \code{hue_range = 0} (default) the hue is fixed, producing a
#' monochromatic light-to-dark series. Increasing \code{hue_range}
#' rotates the hue across the palette for a multi-hue effect.
#'
#' @param base Hex colour string to build the palette around.
#' @param n Number of colours (default 8).
#' @param hue_range Degrees of hue rotation across the palette
#'   (default 0 = monochromatic). Positive values rotate clockwise.
#' @param L_range Length-2 numeric vector giving the lightness range
#'   for the palette endpoints (default \code{c(0.25, 0.95)}).
#' @param space Interpolation space: \code{"oklch"} (default) or
#'   \code{"oklab"}.
#' @return Character vector of hex colour strings.
#' @export
#' @examples
#' # Monochromatic blue
#' okpal_from("#3366CC", n = 7)
#'
#' # Multi-hue starting from blue
#' okpal_from("#3366CC", n = 7, hue_range = 60)
#'
#' plot_palette(okpal_from("#3366CC", n = 9))
#' plot_palette(okpal_from("#3366CC", n = 9, hue_range = 45))
okpal_from <- function(base, n = 8, hue_range = 0,
                       L_range = c(0.25, 0.95), space = "oklch") {
  lch <- farver::decode_colour(base, to = "oklch")
  base_L <- lch[1, "l"]
  base_C <- lch[1, "c"]
  base_h <- lch[1, "h"]

  t <- seq(0, 1, length.out = n)

  # Lightness: sweep from L_range[1] to L_range[2]
  L_vals <- L_range[1] + t * (L_range[2] - L_range[1])

  # Chroma: peak at base colour's lightness, taper toward extremes
  # This avoids oversaturated colours at very light/dark ends
  C_vals <- base_C * (1 - 2 * abs(L_vals - base_L))
  C_vals <- pmax(C_vals, 0)

  # Hue: fixed or rotating
  h_vals <- (base_h - hue_range / 2 + t * hue_range) %% 360

  mat <- cbind(l = L_vals, c = C_vals, h = h_vals)

  if (space == "oklab") {
    # Convert OKLCH anchors to hex, then re-interpolate in OKLAB
    anchors <- farver::encode_colour(mat, from = "oklch")
    return(interp_multi(anchors, n, space = "oklab"))
  }

  farver::encode_colour(mat, from = "oklch")
}

#' Generate a colorblind-safe palette from a single base colour
#'
#' Same as [okpal_from()] but adjusts the result for colorblind safety
#' using CVD simulation. If the base colour itself sits on a
#' problematic hue (e.g. red-green boundary), it is shifted minimally.
#'
#' @inheritParams okpal_from
#' @param severity CVD severity (0-1, default 1 = full dichromacy).
#' @param min_dist Minimum OKLAB distance between any pair after CVD
#'   simulation (default 0.05).
#' @return Character vector of hex colour strings.
#' @export
#' @examples
#' cb_from("#00CC00", n = 6)
#' cb_from("#3366CC", n = 6, hue_range = 40)
cb_from <- function(base, n = 8, hue_range = 0,
                    L_range = c(0.25, 0.95), space = "oklch",
                    severity = 1, min_dist = 0.05) {
  pal <- okpal_from(base, n = n, hue_range = hue_range,
                    L_range = L_range, space = space)
  cb_adjust(pal, severity = severity, min_dist = min_dist)
}

#' Generate a palette distant from an existing palette
#'
#' Creates a new palette whose colours are maximally separated from
#' the colours in \code{existing} in OKLAB space. Lightness and chroma
#' are matched to the existing palette so the two sets look visually
#' cohesive while remaining distinguishable.
#'
#' @param existing Character vector of hex colours to avoid.
#' @param n Number of new colours to generate (default same as
#'   \code{length(existing)}).
#' @param L Lightness for new colours. If \code{NULL} (default), uses
#'   the median lightness of \code{existing}.
#' @param C Chroma for new colours. If \code{NULL} (default), uses the
#'   median chroma of \code{existing}.
#' @return Character vector of hex colour strings.
#' @export
#' @examples
#' pal_a <- oklch_qualitative(4)
#' pal_b <- okpal_contrast(pal_a, 4)
#' plot_palette(c(pal_a, pal_b), main = "Original + Contrast")
okpal_contrast <- function(existing, n = NULL, L = NULL, C = NULL) {
  if (is.null(n)) n <- length(existing)

  # Get L/C from existing palette if not specified
  exist_lch <- farver::decode_colour(existing, to = "oklch")
  if (is.null(L)) L <- stats::median(exist_lch[, "l"])
  if (is.null(C)) C <- stats::median(exist_lch[, "c"])

  # OKLAB coordinates of existing colours for distance computation
  exist_lab <- farver::decode_colour(existing, to = "oklab")

  # Generate all candidate hues
  all_hues <- seq(0, 359, by = 1)
  cand_mat <- cbind(l = rep(L, 360), c = rep(C, 360), h = all_hues)
  cand_hex <- farver::encode_colour(cand_mat, from = "oklch")
  cand_lab <- farver::decode_colour(cand_hex, to = "oklab")

  # Greedy selection: maximise minimum distance to existing + selected
  selected <- integer(0)
  available <- seq_len(360)

  for (step in seq_len(n)) {
    best_idx <- NA
    best_min_d <- -Inf

    for (idx in available) {
      # Distance to all existing colours
      d_exist <- apply(exist_lab, 1, function(row) {
        sqrt(sum((cand_lab[idx, ] - row)^2))
      })

      # Distance to already-selected new colours
      d_selected <- if (length(selected) > 0) {
        vapply(selected, function(s) {
          sqrt(sum((cand_lab[idx, ] - cand_lab[s, ])^2))
        }, numeric(1))
      } else {
        Inf
      }

      min_d <- min(c(d_exist, d_selected))

      if (min_d > best_min_d) {
        best_min_d <- min_d
        best_idx <- idx
      }
    }

    selected <- c(selected, best_idx)
    available <- available[available != best_idx]
  }

  cand_hex[selected]
}

#' Generate a colorblind-safe palette distant from an existing palette
#'
#' Same as [okpal_contrast()] but ensures the new palette is
#' distinguishable from both the existing palette and itself under
#' CVD simulation.
#'
#' @inheritParams okpal_contrast
#' @param severity CVD severity (0-1, default 1).
#' @param min_dist Minimum OKLAB distance after CVD simulation
#'   (default 0.05).
#' @return Character vector of hex colour strings.
#' @export
#' @examples
#' pal_a <- cb_safe_palette(4)
#' pal_b <- cb_contrast(pal_a, 4)
#' plot_palette(c(pal_a, pal_b), main = "Original + CB Contrast")
cb_contrast <- function(existing, n = NULL, L = NULL, C = NULL,
                        severity = 1, min_dist = 0.05) {
  if (is.null(n)) n <- length(existing)

  # Get L/C from existing palette if not specified
  exist_lch <- farver::decode_colour(existing, to = "oklch")
  if (is.null(L)) L <- stats::median(exist_lch[, "l"])
  if (is.null(C)) C <- stats::median(exist_lch[, "c"])

  # Existing colours under CVD simulation
  exist_deutan <- colorspace::deutan(existing, severity = severity)
  exist_protan <- colorspace::protan(existing, severity = severity)
  exist_deutan_lab <- farver::decode_colour(exist_deutan, to = "oklab")
  exist_protan_lab <- farver::decode_colour(exist_protan, to = "oklab")

  # Generate all candidates and their CVD versions
  all_hues <- seq(0, 359, by = 1)
  cand_mat <- cbind(l = rep(L, 360), c = rep(C, 360), h = all_hues)
  cand_hex <- farver::encode_colour(cand_mat, from = "oklch")

  cand_deutan <- colorspace::deutan(cand_hex, severity = severity)
  cand_protan <- colorspace::protan(cand_hex, severity = severity)
  cand_deutan_lab <- farver::decode_colour(cand_deutan, to = "oklab")
  cand_protan_lab <- farver::decode_colour(cand_protan, to = "oklab")

  # Greedy selection
  selected <- integer(0)
  available <- seq_len(360)

  for (step in seq_len(n)) {
    best_idx <- NA
    best_min_d <- -Inf

    for (idx in available) {
      # CVD distances to existing palette
      d_exist_d <- apply(exist_deutan_lab, 1, function(row) {
        sqrt(sum((cand_deutan_lab[idx, ] - row)^2))
      })
      d_exist_p <- apply(exist_protan_lab, 1, function(row) {
        sqrt(sum((cand_protan_lab[idx, ] - row)^2))
      })

      # CVD distances to already-selected
      if (length(selected) > 0) {
        d_sel_d <- vapply(selected, function(s) {
          sqrt(sum((cand_deutan_lab[idx, ] - cand_deutan_lab[s, ])^2))
        }, numeric(1))
        d_sel_p <- vapply(selected, function(s) {
          sqrt(sum((cand_protan_lab[idx, ] - cand_protan_lab[s, ])^2))
        }, numeric(1))
      } else {
        d_sel_d <- Inf
        d_sel_p <- Inf
      }

      min_d <- min(c(d_exist_d, d_exist_p, d_sel_d, d_sel_p))

      if (min_d > best_min_d) {
        best_min_d <- min_d
        best_idx <- idx
      }
    }

    if (best_min_d < min_dist) {
      warning(
        sprintf("Could only place %d colours above min_dist threshold (requested %d)",
                length(selected), n),
        call. = FALSE
      )
      break
    }

    selected <- c(selected, best_idx)
    available <- available[available != best_idx]
  }

  cand_hex[selected]
}
