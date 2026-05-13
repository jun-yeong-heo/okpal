#' Find the nearest colorblind-safe colour
#'
#' Takes an arbitrary colour and adjusts its OKLCH hue so that it shifts
#' as little as possible under deuteranopia and protanopia simulation
#' (i.e. the colour looks similar to a CVD viewer as to a normal viewer).
#' The adjustment minimises perceptual distance from the original colour
#' in OKLAB space among hues whose simulated versions stay within
#' \code{min_dist} of the candidate.
#'
#' This is a single-colour heuristic; for palettes use
#' \code{\link{cb_safe_palette}} or \code{\link{cb_adjust}}, which
#' enforce pairwise distinguishability.
#'
#' @param col A hex colour string.
#' @param severity CVD severity (0-1, default 1 = full dichromacy).
#' @param min_dist Maximum allowed OKLAB distance between the colour
#'   and its CVD-simulated versions (default 0.02). Colours already
#'   below this threshold are returned unchanged.
#' @return A hex colour string.
#' @export
#' @examples
#' cb_safe_nearest("#00CC00")  # green -> shifted to safer hue
cb_safe_nearest <- function(col, severity = 1, min_dist = 0.02) {
  .check_hex(col, "col")
  # Check if already safe
  if (.is_cb_safe_single(col, severity, min_dist)) return(col)

  # Get OKLCH coordinates
  lch <- farver::decode_colour(col, to = "oklch")
  L <- lch[1, "l"]
  C <- lch[1, "c"]
  orig_h <- lch[1, "h"]

  # Search hue space for closest safe colour
  candidate_hues <- (orig_h + seq(-180, 180, by = 1)) %% 360
  orig_lab <- farver::decode_colour(col, to = "oklab")
  best_col <- col
  best_dist <- Inf

  for (h in candidate_hues) {
    cand_mat <- matrix(c(L, C, h), nrow = 1,
                       dimnames = list(NULL, c("l", "c", "h")))
    cand_hex <- farver::encode_colour(cand_mat, from = "oklch")

    if (!.is_cb_safe_single(cand_hex, severity, min_dist)) next

    # OKLAB distance from original
    cand_lab <- farver::decode_colour(cand_hex, to = "oklab")
    d <- sqrt(sum((orig_lab - cand_lab)^2))

    if (d < best_dist) {
      best_dist <- d
      best_col <- cand_hex
    }
  }

  best_col
}

#' Generate a colorblind-safe qualitative palette
#'
#' Creates an n-colour palette in OKLCH space where all pairs remain
#' distinguishable under deuteranopia and protanopia simulation.
#' Colours are placed by maximising minimum pairwise OKLAB distance
#' after CVD simulation.
#'
#' @param n Number of colours (default 8, max ~12 for reliable results).
#' @param L Lightness (0-1, default 0.7).
#' @param C Chroma (default 0.15).
#' @param severity CVD severity (0-1, default 1).
#' @param min_dist Minimum OKLAB distance between any simulated pair
#'   (default 0.05).
#' @return Character vector of hex colour strings.
#'
#' @section Limitations:
#' Candidates are sampled from a single ring at fixed \code{L}/\code{C}
#' and only the hue is varied. As \code{n} grows the ring becomes
#' crowded under CVD simulation; beyond a certain point no hue is far
#' enough from every previously placed colour. The function then warns
#' and returns fewer than \code{n} colours. With defaults this typically
#' starts to fail around \code{n = 5}-\code{6}.
#' Use \code{\link{cb_safe_palette_relaxed}} to widen the L/C search
#' space and trade a small amount of cohesion for completeness.
#' @seealso [cb_safe_palette_relaxed()]
#' @export
#' @examples
#' cb_safe_palette(6)
#' plot_palette(cb_safe_palette(8))
cb_safe_palette <- function(n = 8, L = 0.7, C = 0.15,
                            severity = 1, min_dist = 0.05) {
  cand_hex <- .build_ring_grid(L, C, 0, 0)
  deutan_lab <- farver::decode_colour(
    colorspace::deutan(cand_hex, severity = severity), to = "oklab"
  )
  protan_lab <- farver::decode_colour(
    colorspace::protan(cand_hex, severity = severity), to = "oklab"
  )
  selected <- .greedy_cvd_palette(deutan_lab, protan_lab, n, min_dist)
  if (length(selected) < n) {
    warning(
      sprintf("Could only place %d colours above min_dist threshold (requested %d)",
              length(selected), n),
      call. = FALSE
    )
  }
  cand_hex[selected]
}

#' Generate a colorblind-safe qualitative palette with relaxed L/C
#'
#' A variant of [cb_safe_palette()] that progressively widens the
#' search space along lightness and chroma when the default single
#' OKLCH ring cannot accommodate \code{n} colours. Trades a small
#' amount of L/C cohesion for being able to satisfy \code{n} when the
#' strict ring is full.
#'
#' At step 1 the candidate set is identical to \code{cb_safe_palette}.
#' If that fails to place \code{n} colours, the search expands to a
#' grid of L/C points within \code{[L +- L_tol, C +- C_tol]} and tries
#' again. The grid grows linearly across \code{tol_steps} steps; the
#' function returns at the first step that satisfies \code{n}, or at
#' the final step with a warning.
#'
#' @inheritParams cb_safe_palette
#' @param L_tol Maximum lightness deviation from \code{L} the search
#'   may use (default 0.1, range \code{[0, 1]}).
#' @param C_tol Maximum chroma deviation from \code{C} (default 0.05).
#' @param tol_steps Number of progressive widening steps (default 5).
#' @return Character vector of hex colour strings (length \code{<= n}).
#' @seealso [cb_safe_palette()]
#' @export
#' @examples
#' # Strict variant fails to place all 8
#' suppressWarnings(length(cb_safe_palette(8)))
#'
#' # Relaxed variant trades cohesion for completeness
#' length(cb_safe_palette_relaxed(8, L_tol = 0.15, C_tol = 0.05))
cb_safe_palette_relaxed <- function(n = 8, L = 0.7, C = 0.15,
                                    severity = 1, min_dist = 0.05,
                                    L_tol = 0.1, C_tol = 0.05,
                                    tol_steps = 5L) {
  stopifnot(L_tol >= 0, C_tol >= 0, tol_steps >= 1L)

  best_sel <- integer(0)
  best_hex <- character(0)
  reached <- 0L

  for (k in seq_len(tol_steps)) {
    cand_hex <- .build_ring_grid(L, C,
                                 .tol_offsets(L_tol, tol_steps, k),
                                 .tol_offsets(C_tol, tol_steps, k))
    deutan_lab <- farver::decode_colour(
      colorspace::deutan(cand_hex, severity = severity), to = "oklab"
    )
    protan_lab <- farver::decode_colour(
      colorspace::protan(cand_hex, severity = severity), to = "oklab"
    )
    sel <- .greedy_cvd_palette(deutan_lab, protan_lab, n, min_dist)
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

#' Adjust an existing palette for colorblind safety
#'
#' Takes a vector of colours and adjusts any problematic pairs by shifting
#' OKLCH hue of the less-constrained colour until all pairs are
#' distinguishable under CVD simulation.
#'
#' @param colours Character vector of hex colours.
#' @param severity CVD severity (0-1, default 1).
#' @param min_dist Minimum OKLAB distance between simulated pairs
#'   (default 0.05).
#' @return Character vector of adjusted hex colours.
#' @export
#' @examples
#' cols <- c("#FF0000", "#00FF00", "#0000FF", "#FF8800")
#' cb_adjust(cols)
cb_adjust <- function(colours, severity = 1, min_dist = 0.05) {
  .check_hex(colours, "colours")
  adjusted <- colours
  n <- length(adjusted)
  max_iter <- 50

  for (iter in seq_len(max_iter)) {
    # Find worst pair
    deutan_hex <- colorspace::deutan(adjusted, severity = severity)
    protan_hex <- colorspace::protan(adjusted, severity = severity)
    deutan_lab <- farver::decode_colour(deutan_hex, to = "oklab")
    protan_lab <- farver::decode_colour(protan_hex, to = "oklab")

    worst_d <- Inf
    worst_i <- NA
    worst_j <- NA

    for (i in seq_len(n - 1)) {
      for (j in (i + 1):n) {
        dd <- sqrt(sum((deutan_lab[i, ] - deutan_lab[j, ])^2))
        dp <- sqrt(sum((protan_lab[i, ] - protan_lab[j, ])^2))
        d <- min(dd, dp)
        if (d < worst_d) {
          worst_d <- d
          worst_i <- i
          worst_j <- j
        }
      }
    }

    if (worst_d >= min_dist) break

    # Shift the second colour's hue. Try both directions and keep the
    # one that improves the worst pair more (reduces oscillation).
    lch <- farver::decode_colour(adjusted[worst_j], to = "oklch")
    best_hex <- adjusted[worst_j]
    best_pair_d <- worst_d
    for (delta in c(15, -15)) {
      cand_lch <- lch
      cand_lch[1, "h"] <- (cand_lch[1, "h"] + delta) %% 360
      cand_hex <- farver::encode_colour(cand_lch, from = "oklch")
      cand_d_lab <- farver::decode_colour(
        colorspace::deutan(cand_hex, severity = severity), to = "oklab"
      )
      cand_p_lab <- farver::decode_colour(
        colorspace::protan(cand_hex, severity = severity), to = "oklab"
      )
      dd <- sqrt(sum((cand_d_lab - deutan_lab[worst_i, ])^2))
      dp <- sqrt(sum((cand_p_lab - protan_lab[worst_i, ])^2))
      pair_d <- min(dd, dp)
      if (pair_d > best_pair_d) {
        best_pair_d <- pair_d
        best_hex <- cand_hex
      }
    }
    adjusted[worst_j] <- best_hex
  }

  if (iter == max_iter && worst_d < min_dist) {
    warning("Could not fully resolve all pairs within iteration limit", call. = FALSE)
  }

  adjusted
}

#' Check colorblind safety of a palette
#'
#' Diagnoses a palette by computing pairwise OKLAB distances after
#' deutan, protan, and tritan simulation. Returns a data.frame
#' flagging problematic pairs.
#'
#' @param colours Character vector of hex colours.
#' @param severity CVD severity (0-1, default 1).
#' @param min_dist Threshold below which a pair is flagged (default 0.05).
#' @return A data.frame with columns: col_i, col_j, dist_deutan, dist_protan,
#'   dist_tritan, min_dist, safe.
#' @export
#' @examples
#' cb_check(c("#FF0000", "#00FF00", "#0000FF"))
cb_check <- function(colours, severity = 1, min_dist = 0.05) {
  .check_hex(colours, "colours")
  deutan_hex <- colorspace::deutan(colours, severity = severity)
  protan_hex <- colorspace::protan(colours, severity = severity)
  tritan_hex <- colorspace::tritan(colours, severity = severity)

  deutan_lab <- farver::decode_colour(deutan_hex, to = "oklab")
  protan_lab <- farver::decode_colour(protan_hex, to = "oklab")
  tritan_lab <- farver::decode_colour(tritan_hex, to = "oklab")

  n <- length(colours)
  npairs <- n * (n - 1) / 2
  col_i <- character(npairs)
  col_j <- character(npairs)
  dist_deutan <- numeric(npairs)
  dist_protan <- numeric(npairs)
  dist_tritan <- numeric(npairs)

  k <- 0L
  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      k <- k + 1L
      col_i[k] <- colours[i]
      col_j[k] <- colours[j]
      dist_deutan[k] <- sqrt(sum((deutan_lab[i, ] - deutan_lab[j, ])^2))
      dist_protan[k] <- sqrt(sum((protan_lab[i, ] - protan_lab[j, ])^2))
      dist_tritan[k] <- sqrt(sum((tritan_lab[i, ] - tritan_lab[j, ])^2))
    }
  }

  md <- pmin(dist_deutan, dist_protan, dist_tritan)
  data.frame(
    col_i = col_i, col_j = col_j,
    dist_deutan = round(dist_deutan, 4),
    dist_protan = round(dist_protan, 4),
    dist_tritan = round(dist_tritan, 4),
    min_dist = round(md, 4),
    safe = md >= min_dist,
    stringsAsFactors = FALSE
  )
}

#' Check if a single colour is colorblind-safe
#' @noRd
.is_cb_safe_single <- function(col, severity = 1, min_dist = 0.02) {
  orig_lab <- farver::decode_colour(col, to = "oklab")
  deutan_lab <- farver::decode_colour(
    colorspace::deutan(col, severity = severity), to = "oklab"
  )
  protan_lab <- farver::decode_colour(
    colorspace::protan(col, severity = severity), to = "oklab"
  )

  dd <- sqrt(sum((orig_lab - deutan_lab)^2))
  dp <- sqrt(sum((orig_lab - protan_lab)^2))

  # A colour is "safe" if it doesn't shift too much under simulation
  # (i.e., what you see is roughly what a CVD person sees).
  # This is a heuristic; the real check is pairwise in cb_check().
  dd < min_dist && dp < min_dist
}
