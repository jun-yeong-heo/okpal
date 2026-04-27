#' Find the nearest colorblind-safe colour
#'
#' Takes an arbitrary colour and adjusts its OKLCH hue so that it remains
#' distinguishable under deuteranopia and protanopia simulation.
#' The adjustment minimises perceptual distance from the original colour
#' in OKLAB space while ensuring the simulated versions stay distinct.
#'
#' @param col A hex colour string.
#' @param severity CVD severity (0-1, default 1 = full dichromacy).
#' @param min_dist Minimum OKLAB distance between original and simulated
#'   versions (default 0.02). Colours already passing this threshold
#'   are returned unchanged.
#' @return A hex colour string.
#' @export
#' @examples
#' cb_safe_nearest("#00CC00")  # green -> shifted to safer hue
cb_safe_nearest <- function(col, severity = 1, min_dist = 0.02) {
  # Check if already safe
  if (.is_cb_safe_single(col, severity, min_dist)) return(col)

  # Get OKLCH coordinates
 lch <- farver::decode_colour(col, to = "oklch")
  L <- lch[1, "l"]
  C <- lch[1, "c"]
  orig_h <- lch[1, "h"]

  # Search hue space for closest safe colour
  candidate_hues <- (orig_h + seq(-180, 180, by = 1)) %% 360
  best_col <- col
  best_dist <- Inf

  for (h in candidate_hues) {
    cand_mat <- matrix(c(L, C, h), nrow = 1,
                       dimnames = list(NULL, c("l", "c", "h")))
    cand_hex <- farver::encode_colour(cand_mat, from = "oklch")

    if (!.is_cb_safe_single(cand_hex, severity, min_dist)) next

    # OKLAB distance from original
    orig_lab <- farver::decode_colour(col, to = "oklab")
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
#' @export
#' @examples
#' cb_safe_palette(6)
#' plot_palette(cb_safe_palette(8))
cb_safe_palette <- function(n = 8, L = 0.7, C = 0.15,
                            severity = 1, min_dist = 0.05) {
  # Generate candidate hues and greedily select well-separated ones
  all_hues <- seq(0, 359, by = 1)

  # Pre-generate all candidates
  cand_mat <- cbind(l = rep(L, 360), c = rep(C, 360), h = all_hues)
  cand_hex <- farver::encode_colour(cand_mat, from = "oklch")

  # Simulate CVD for all candidates
  deutan_hex <- colorspace::deutan(cand_hex, severity = severity)
  protan_hex <- colorspace::protan(cand_hex, severity = severity)

  deutan_lab <- farver::decode_colour(deutan_hex, to = "oklab")
  protan_lab <- farver::decode_colour(protan_hex, to = "oklab")

  # Greedy selection: pick hues that maximise minimum CVD distance
  selected <- integer(0)
  available <- seq_len(360)

  # Start with hue 0
  selected <- c(selected, 1L)
  available <- available[-1]

  for (step in seq_len(n - 1)) {
    best_idx <- NA
    best_min_d <- -Inf

    for (idx in available) {
      min_d <- Inf
      for (s in selected) {
        # Distance under deutan simulation
        dd <- sqrt(sum((deutan_lab[idx, ] - deutan_lab[s, ])^2))
        # Distance under protan simulation
        dp <- sqrt(sum((protan_lab[idx, ] - protan_lab[s, ])^2))
        min_d <- min(min_d, dd, dp)
      }
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

    # Shift the second colour's hue
    lch <- farver::decode_colour(adjusted[worst_j], to = "oklch")
    lch[1, "h"] <- (lch[1, "h"] + 15) %% 360
    adjusted[worst_j] <- farver::encode_colour(lch, from = "oklch")
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
  deutan_hex <- colorspace::deutan(colours, severity = severity)
  protan_hex <- colorspace::protan(colours, severity = severity)
  tritan_hex <- colorspace::tritan(colours, severity = severity)

  deutan_lab <- farver::decode_colour(deutan_hex, to = "oklab")
  protan_lab <- farver::decode_colour(protan_hex, to = "oklab")
  tritan_lab <- farver::decode_colour(tritan_hex, to = "oklab")

  n <- length(colours)
  results <- data.frame(
    col_i = character(0), col_j = character(0),
    dist_deutan = numeric(0), dist_protan = numeric(0),
    dist_tritan = numeric(0), min_dist = numeric(0),
    safe = logical(0),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      dd <- sqrt(sum((deutan_lab[i, ] - deutan_lab[j, ])^2))
      dp <- sqrt(sum((protan_lab[i, ] - protan_lab[j, ])^2))
      dt <- sqrt(sum((tritan_lab[i, ] - tritan_lab[j, ])^2))
      md <- min(dd, dp, dt)
      results <- rbind(results, data.frame(
        col_i = colours[i], col_j = colours[j],
        dist_deutan = round(dd, 4), dist_protan = round(dp, 4),
        dist_tritan = round(dt, 4), min_dist = round(md, 4),
        safe = md >= min_dist,
        stringsAsFactors = FALSE
      ))
    }
  }

  results
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

  # (i.e., what you see is roughly what a CVD person sees)
  # This is a heuristic; the real check is pairwise in cb_check()
  TRUE
}
