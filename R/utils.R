# Internal utilities for okpal
# These are not exported

#' Validate that x is one or more 6-digit hex colour strings
#' @noRd
.check_hex <- function(x, arg) {
  if (length(x) == 0L || !is.character(x) || any(is.na(x)) ||
      any(!grepl("^#[0-9A-Fa-f]{6}$", x))) {
    stop(sprintf("`%s` must be 6-digit hex colour string(s) like \"#1B0A55\".", arg),
         call. = FALSE)
  }
  invisible(x)
}

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

  h1 <- lch1[1, "h"]
  h2 <- lch2[1, "h"]
  c1 <- lch1[1, "c"]
  c2 <- lch2[1, "c"]

  # farver returns an arbitrary hue for achromatic colours. Treat them
  # as inheriting the other endpoint's hue to avoid sweeping through
  # an unrelated hue arc when only chroma should change.
  achromatic <- 1e-3
  if (c1 < achromatic && c2 >= achromatic) h1 <- h2
  if (c2 < achromatic && c1 >= achromatic) h2 <- h1

  # Shortest-path hue interpolation
  dh <- h2 - h1
  if (dh > 180) dh <- dh - 360
  if (dh < -180) dh <- dh + 360

  mat <- cbind(
    l = lch1[1, "l"] + t * (lch2[1, "l"] - lch1[1, "l"]),
    c = c1 + t * (c2 - c1),
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
    # Non-final segments produce per_seg[i] + 1 points and drop the
    # trailing anchor (it reappears as the head of the next segment),
    # contributing per_seg[i]. The final segment keeps its endpoint
    # and is sized to exactly per_seg[i]. Total = sum(per_seg) = n.
    if (i < segments) {
      seg <- interp_fn(colours[i], colours[i + 1], per_seg[i] + 1L)
      seg <- seg[-length(seg)]
    } else {
      seg <- interp_fn(colours[i], colours[i + 1], per_seg[i])
    }
    result <- c(result, seg)
  }

  result
}

#' Greedy selection of n indices that maximise min pairwise distance
#' under both deutan and protan CVD simulation.
#' Stops early if the next pick would fall below \code{min_dist}.
#' Always seeds with index 1L.
#' @return integer vector of selected indices (length <= n)
#' @noRd
.greedy_cvd_palette <- function(deutan_lab, protan_lab, n, min_dist) {
  ncand <- nrow(deutan_lab)
  selected <- 1L
  available <- seq_len(ncand)[-1]
  for (step in seq_len(n - 1)) {
    best_idx <- NA_integer_
    best_min_d <- -Inf
    for (idx in available) {
      min_d <- Inf
      for (s in selected) {
        dd <- sqrt(sum((deutan_lab[idx, ] - deutan_lab[s, ])^2))
        dp <- sqrt(sum((protan_lab[idx, ] - protan_lab[s, ])^2))
        min_d <- min(min_d, dd, dp)
      }
      if (min_d > best_min_d) {
        best_min_d <- min_d
        best_idx <- idx
      }
    }
    if (is.na(best_idx) || best_min_d < min_dist) break
    selected <- c(selected, best_idx)
    available <- available[available != best_idx]
  }
  selected
}

#' Greedy selection of n indices that maximise min OKLAB distance to
#' both existing colours and previously selected candidates.
#' Always picks n (does not enforce a threshold).
#' @return integer vector of selected indices (length n)
#' @noRd
.greedy_lab_contrast <- function(cand_lab, exist_lab, n) {
  ncand <- nrow(cand_lab)
  selected <- integer(0)
  available <- seq_len(ncand)
  for (step in seq_len(n)) {
    best_idx <- NA_integer_
    best_min_d <- -Inf
    for (idx in available) {
      d_exist <- apply(exist_lab, 1, function(row) {
        sqrt(sum((cand_lab[idx, ] - row)^2))
      })
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
  selected
}

#' Greedy selection that maximises min OKLAB distance to existing and
#' selected candidates under both deutan and protan CVD simulation.
#' Stops early if the next pick would fall below \code{min_dist}.
#' @return integer vector of selected indices (length <= n)
#' @noRd
.greedy_cvd_contrast <- function(cand_d_lab, cand_p_lab,
                                 exist_d_lab, exist_p_lab,
                                 n, min_dist) {
  ncand <- nrow(cand_d_lab)
  selected <- integer(0)
  available <- seq_len(ncand)
  for (step in seq_len(n)) {
    best_idx <- NA_integer_
    best_min_d <- -Inf
    for (idx in available) {
      d_exist_d <- apply(exist_d_lab, 1, function(row) {
        sqrt(sum((cand_d_lab[idx, ] - row)^2))
      })
      d_exist_p <- apply(exist_p_lab, 1, function(row) {
        sqrt(sum((cand_p_lab[idx, ] - row)^2))
      })
      if (length(selected) > 0) {
        d_sel_d <- vapply(selected, function(s) {
          sqrt(sum((cand_d_lab[idx, ] - cand_d_lab[s, ])^2))
        }, numeric(1))
        d_sel_p <- vapply(selected, function(s) {
          sqrt(sum((cand_p_lab[idx, ] - cand_p_lab[s, ])^2))
        }, numeric(1))
      } else {
        d_sel_d <- Inf; d_sel_p <- Inf
      }
      min_d <- min(c(d_exist_d, d_exist_p, d_sel_d, d_sel_p))
      if (min_d > best_min_d) {
        best_min_d <- min_d
        best_idx <- idx
      }
    }
    if (is.na(best_idx) || best_min_d < min_dist) break
    selected <- c(selected, best_idx)
    available <- available[available != best_idx]
  }
  selected
}

#' Build an OKLCH ring grid: outer product of L/C offsets and 360 hues.
#' Returns a hex character vector. Used by *_relaxed variants.
#' @noRd
.build_ring_grid <- function(L_center, C_center, L_offsets, C_offsets) {
  L_vals <- pmax(0, pmin(1, L_center + L_offsets))
  C_vals <- pmax(0, C_center + C_offsets)
  all_hues <- seq(0, 359, by = 1)
  grid <- expand.grid(l = L_vals, c = C_vals, h = all_hues,
                      KEEP.OUT.ATTRS = FALSE)
  farver::encode_colour(as.matrix(grid), from = "oklch")
}

#' Linearly scaled offsets for tol step k of tol_steps.
#' Step 1 is always a single 0 (= single ring). Later steps expand.
#' @noRd
.tol_offsets <- function(tol, tol_steps, k) {
  if (tol <= 0 || k <= 1) return(0)
  sf <- (k - 1) / max(1L, tol_steps - 1)
  seq(-tol * sf, tol * sf, length.out = 2 * k - 1)
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
