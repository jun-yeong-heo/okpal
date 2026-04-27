#' okpal: OKLAB/OKLCH-Based Perceptual Color Interpolation and
#' Colorblind-Safe Palette Generation
#'
#' @description
#' Attempt to provide perceptually uniform color gradients by interpolating
#' in OKLAB/OKLCH color spaces via the farver package. Includes tools for
#' colorblind-safe palette generation, adjustment, and checking via
#' colorspace CVD simulation. Provides scale functions for ggplot2,
#' pheatmap, and ComplexHeatmap.
#'
#' @section Palette generation:
#' \itemize{
#'   \item \code{\link{oklab_seq}}: Sequential (OKLAB)
#'   \item \code{\link{oklab_div}}: Diverging (OKLAB)
#'   \item \code{\link{oklab_multi}}: Multi-anchor (OKLAB)
#'   \item \code{\link{oklch_seq}}: Sequential (OKLCH)
#'   \item \code{\link{oklch_div}}: Diverging (OKLCH)
#'   \item \code{\link{oklch_qualitative}}: Qualitative (OKLCH)
#' }
#'
#' @section Colorblind tools:
#' \itemize{
#'   \item \code{\link{cb_safe_nearest}}: Adjust single colour
#'   \item \code{\link{cb_safe_palette}}: Generate safe palette
#'   \item \code{\link{cb_adjust}}: Adjust existing palette
#'   \item \code{\link{cb_check}}: Diagnose palette safety
#' }
#'
#' @docType package
#' @name okpal-package
"_PACKAGE"
