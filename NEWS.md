# okpal 0.2.0

## Bug fixes

* `interp_multi()` (and therefore `oklab_div()`, `oklch_div()`,
  `oklab_multi()`) now returns exactly `n` colours instead of `n + 1`
  for palettes built from three or more anchors.
* `cb_safe_nearest()`'s internal safety check no longer always returns
  `TRUE`. The function now actually searches for a safer hue.
* `interp_oklch()` no longer sweeps through an unrelated hue arc when
  one endpoint is achromatic (e.g. grey → red); it now inherits the
  chromatic endpoint's hue.

## New functions

* `cb_safe_palette_relaxed()`, `cb_contrast_relaxed()`,
  `okpal_contrast_relaxed()` widen the OKLCH search progressively
  along lightness and chroma when the single-ring strict variants
  cannot place `n` colours. With `L_tol = 0`, `C_tol = 0` they are
  identical to the strict variants (backward compatible).

## Behaviour changes

* All exported functions that take hex colour input now validate the
  input via an internal helper and raise a clear error on malformed
  hex strings instead of silently propagating a `farver` error.
* The `space` argument of `scale_*_oklab*()`, `pheatmap_oklab*()`,
  `cheatmap_oklab*()`, `okpal_from()`, and `cb_from()` is now
  validated with `match.arg()`. Invalid values raise an explicit
  error rather than silently falling back to `"oklab"`.
* `cb_check()` no longer accumulates rows via `rbind()` inside a
  loop; the result is otherwise identical.
* `cb_adjust()` now tries both ±hue directions when nudging a
  problematic colour, reducing oscillation.

## Documentation and infrastructure

* `okpal_contrast()`, `cb_contrast()`, and `cb_safe_palette()` now
  document the geometric limitation that prevents them from filling
  large `n` when the input is already spread evenly across a single
  L/C ring, and cross-reference the `*_relaxed` variants.
* `DESCRIPTION` Imports is cleaned up: `grDevices` removed (unused),
  `graphics` and `stats` added (used by `plot.R` and
  `generators.R`).
* GitHub Actions `R-CMD-check` workflow added.
* README gains a "When the strict ring is full" subsection
  illustrating the strict / relaxed trade-off.

# okpal 0.1.0

* Initial release.
