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
                       L_range = c(0.25, 0.95),
                       space = c("oklch", "oklab")) {
  .check_hex(base, "base")
  space <- match.arg(space)
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
                    L_range = c(0.25, 0.95),
                    space = c("oklch", "oklab"),
                    severity = 1, min_dist = 0.05) {
  space <- match.arg(space)
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
#'
#' @section Limitations:
#' Candidates are drawn from a single ring at fixed lightness and
#' chroma (the medians of \code{existing}), so success depends on
#' how \code{existing} is shaped in colour space.
#'
#' Works well when \code{existing} is clustered in one region (e.g.
#' sequential / diverging palettes such as viridis, or any palette
#' whose colours span a wide range of lightness or chroma): the
#' candidate ring sits far from \code{existing} and most hues are
#' admissible.
#'
#' Cannot find well-separated candidates when \code{existing} is
#' already spread evenly around the candidate ring (e.g. Dark2 and
#' other qualitative palettes, palettes from
#' \code{\link{oklch_qualitative}}): there is no hue left that is far
#' enough from every existing colour. Use
#' \code{\link{okpal_contrast_relaxed}} to widen the search to nearby
#' L/C values.
#' @seealso [okpal_contrast_relaxed()]
#' @export
#' @examples
#' pal_a <- oklch_qualitative(4)
#' pal_b <- okpal_contrast(pal_a, 4)
#' plot_palette(c(pal_a, pal_b), main = "Original + Contrast")
okpal_contrast <- function(existing, n = NULL, L = NULL, C = NULL) {
  .check_hex(existing, "existing")
  if (is.null(n)) n <- length(existing)

  exist_lch <- farver::decode_colour(existing, to = "oklch")
  if (is.null(L)) L <- stats::median(exist_lch[, "l"])
  if (is.null(C)) C <- stats::median(exist_lch[, "c"])

  exist_lab <- farver::decode_colour(existing, to = "oklab")
  cand_hex <- .build_ring_grid(L, C, 0, 0)
  cand_lab <- farver::decode_colour(cand_hex, to = "oklab")

  selected <- .greedy_lab_contrast(cand_lab, exist_lab, n)
  cand_hex[selected]
}

#' Generate a contrast palette with relaxed L/C
#'
#' A variant of [okpal_contrast()] that widens the L/C search space
#' progressively when the default single ring cannot fit \code{n}
#' colours far enough from \code{existing}. Trades a small amount of
#' visual cohesion with \code{existing} for being able to satisfy
#' \code{n}. Step 1 is identical to \code{okpal_contrast}; later steps
#' search a wider grid.
#'
#' @inheritParams okpal_contrast
#' @param L_tol Maximum lightness deviation from the centre (default 0.1).
#' @param C_tol Maximum chroma deviation from the centre (default 0.05).
#' @param tol_steps Number of progressive widening steps (default 5).
#' @return Character vector of hex colour strings (length \code{n}).
#' @seealso [okpal_contrast()]
#' @export
okpal_contrast_relaxed <- function(existing, n = NULL, L = NULL, C = NULL,
                                   L_tol = 0.1, C_tol = 0.05,
                                   tol_steps = 5L) {
  .check_hex(existing, "existing")
  stopifnot(L_tol >= 0, C_tol >= 0, tol_steps >= 1L)
  if (is.null(n)) n <- length(existing)

  exist_lch <- farver::decode_colour(existing, to = "oklch")
  if (is.null(L)) L <- stats::median(exist_lch[, "l"])
  if (is.null(C)) C <- stats::median(exist_lch[, "c"])
  exist_lab <- farver::decode_colour(existing, to = "oklab")

  # okpal_contrast always returns n (no threshold). The progressive
  # widening here only affects *which* n colours are chosen — later
  # steps just provide a richer pool. We accept the first step's
  # result and stop, matching strict semantics for L_tol=C_tol=0.
  cand_hex <- .build_ring_grid(L, C,
                               .tol_offsets(L_tol, tol_steps, tol_steps),
                               .tol_offsets(C_tol, tol_steps, tol_steps))
  cand_lab <- farver::decode_colour(cand_hex, to = "oklab")
  selected <- .greedy_lab_contrast(cand_lab, exist_lab, n)
  cand_hex[selected]
}

#' Colorblind-safe contrast palette with relaxed L/C
#'
#' A variant of [cb_contrast()] that widens the L/C search space
#' progressively when the default single ring cannot satisfy
#' \code{min_dist} for all \code{n} colours. Returns the first step's
#' result that places \code{n} colours, or the best partial result
#' with a warning. Step 1 is identical to \code{cb_contrast}.
#'
#' @inheritParams cb_contrast
#' @param L_tol Maximum lightness deviation from the centre (default 0.1).
#' @param C_tol Maximum chroma deviation from the centre (default 0.05).
#' @param tol_steps Number of progressive widening steps (default 5).
#' @return Character vector of hex colour strings (length \code{<= n}).
#' @seealso [cb_contrast()]
#' @export
cb_contrast_relaxed <- function(existing, n = NULL, L = NULL, C = NULL,
                                severity = 1, min_dist = 0.05,
                                L_tol = 0.1, C_tol = 0.05,
                                tol_steps = 5L) {
  .check_hex(existing, "existing")
  stopifnot(L_tol >= 0, C_tol >= 0, tol_steps >= 1L)
  if (is.null(n)) n <- length(existing)

  exist_lch <- farver::decode_colour(existing, to = "oklch")
  if (is.null(L)) L <- stats::median(exist_lch[, "l"])
  if (is.null(C)) C <- stats::median(exist_lch[, "c"])

  exist_d_lab <- farver::decode_colour(
    colorspace::deutan(existing, severity = severity), to = "oklab"
  )
  exist_p_lab <- farver::decode_colour(
    colorspace::protan(existing, severity = severity), to = "oklab"
  )

  best_sel <- integer(0)
  best_hex <- character(0)
  reached <- 0L

  for (k in seq_len(tol_steps)) {
    cand_hex <- .build_ring_grid(L, C,
                                 .tol_offsets(L_tol, tol_steps, k),
                                 .tol_offsets(C_tol, tol_steps, k))
    cand_d_lab <- farver::decode_colour(
      colorspace::deutan(cand_hex, severity = severity), to = "oklab"
    )
    cand_p_lab <- farver::decode_colour(
      colorspace::protan(cand_hex, severity = severity), to = "oklab"
    )
    sel <- .greedy_cvd_contrast(cand_d_lab, cand_p_lab,
                                exist_d_lab, exist_p_lab,
                                n, min_dist)
    if (length(sel) > length(best_sel)) {
      best_sel <- sel
      best_hex <- cand_hex
      reached <- k
    }
    if (length(sel) >= n) break
  }

  if (length(best_sel) < n) {
    warning(
      sprintf(
        "Reached step %d/%d (L_tol=%g, C_tol=%g); placed %d / %d colours.",
        reached, tol_steps, L_tol, C_tol, length(best_sel), n
      ),
      call. = FALSE
    )
  }
  best_hex[best_sel]
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
#'
#' @section Limitations:
#' Inherits the same shape constraint as [okpal_contrast()]: candidates
#' are drawn from a single ring at fixed lightness and chroma. When
#' \code{existing} is already spread evenly around that ring (e.g.
#' Dark2-style qualitative palettes, palettes from
#' \code{\link{cb_safe_palette}} or \code{\link{oklch_qualitative}}),
#' \code{min_dist} cannot be satisfied for every requested colour. The
#' function then warns and returns fewer than \code{n} colours.
#' Sequential / diverging inputs (e.g. viridis) generally succeed.
#' Use \code{\link{cb_contrast_relaxed}} to widen the search.
#' @seealso [cb_contrast_relaxed()]
#' @export
#' @examples
#' pal_a <- cb_safe_palette(4)
#' pal_b <- cb_contrast(pal_a, 4)
#' plot_palette(c(pal_a, pal_b), main = "Original + CB Contrast")
cb_contrast <- function(existing, n = NULL, L = NULL, C = NULL,
                        severity = 1, min_dist = 0.05) {
  .check_hex(existing, "existing")
  if (is.null(n)) n <- length(existing)

  exist_lch <- farver::decode_colour(existing, to = "oklch")
  if (is.null(L)) L <- stats::median(exist_lch[, "l"])
  if (is.null(C)) C <- stats::median(exist_lch[, "c"])

  exist_d_lab <- farver::decode_colour(
    colorspace::deutan(existing, severity = severity), to = "oklab"
  )
  exist_p_lab <- farver::decode_colour(
    colorspace::protan(existing, severity = severity), to = "oklab"
  )

  cand_hex <- .build_ring_grid(L, C, 0, 0)
  cand_d_lab <- farver::decode_colour(
    colorspace::deutan(cand_hex, severity = severity), to = "oklab"
  )
  cand_p_lab <- farver::decode_colour(
    colorspace::protan(cand_hex, severity = severity), to = "oklab"
  )

  selected <- .greedy_cvd_contrast(cand_d_lab, cand_p_lab,
                                   exist_d_lab, exist_p_lab,
                                   n, min_dist)
  if (length(selected) < n) {
    warning(
      sprintf("Could only place %d colours above min_dist threshold (requested %d)",
              length(selected), n),
      call. = FALSE
    )
  }
  cand_hex[selected]
}
