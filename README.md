# okpal

OKLAB/OKLCH-based perceptual color interpolation and colorblind-safe palette generation for R.

Built on [farver](https://github.com/thomasp85/farver) (color space conversion) and [colorspace](https://colorspace.R-Forge.R-project.org/) (CVD simulation).

## Why?

Standard RGB interpolation produces muddy, desaturated mid-tones when blending between two colours. This happens because RGB is not perceptually uniform — equal numeric steps don't correspond to equal visual steps. OKLAB (Björn Ottosson, 2020) solves this by providing a colour space where Euclidean distance closely tracks human perception.

## Installation

```r
# install.packages("devtools")
devtools::install_github("jun-yeong-heo/okpal")
```

## Quick start

```r
library(okpal)
library(ggplot2)

# Sequential gradient (OKLAB interpolation)
ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
  geom_tile() +
  scale_fill_oklab(low = "#1B0A55", high = "#FDE725")

# Diverging gradient
ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
  geom_tile() +
  scale_fill_oklab_div(low = "#2166AC", mid = "#F7F7F7", high = "#B2182B")

# Discrete qualitative (OKLCH, even hue spacing)
ggplot(mpg, aes(displ, hwy, colour = factor(class))) +
  geom_point() +
  scale_colour_oklch_d()

# Preview any palette
plot_palette(oklab_seq("#1B0A55", "#FDE725", n = 12))
```

## OKLAB vs OKLCH interpolation

Both spaces are perceptually uniform, but they interpolate differently:

- **OKLAB** interpolates in Cartesian coordinates (L, a, b). This traces a straight line through the colour space. Best for sequential palettes within a similar hue family. Can lose chroma (saturation) when blending distant hues because the straight path may pass near the neutral axis.
- **OKLCH** interpolates in polar coordinates (L, Chroma, Hue). Chroma is maintained as an independent channel, and hue rotates along the shortest arc. Best for blending across different hues (e.g. red → blue) without the desaturation dip. Produces more vivid mid-tones.

```r
# Compare: red to blue
plot_palette(oklab_seq("#FF0000", "#0000FF", 15), main = "OKLAB: red → blue")
plot_palette(oklch_seq("#FF0000", "#0000FF", 15), main = "OKLCH: red → blue")

# For similar hues, both are similar
plot_palette(oklab_seq("#1B0A55", "#FDE725", 15), main = "OKLAB: purple → yellow")
plot_palette(oklch_seq("#1B0A55", "#FDE725", 15), main = "OKLCH: purple → yellow")
```

## Heatmap support

```r
# pheatmap
library(pheatmap)
mat <- matrix(rnorm(200), 20, 10)
pheatmap(mat, color = pheatmap_oklab("#1B0A55", "#FDE725"))
pheatmap(mat, color = pheatmap_oklab_div())

# ComplexHeatmap
library(ComplexHeatmap)
col_fn <- cheatmap_oklab(
  breaks = seq(-3, 3, length.out = 256),
  low = "#1B0A55", high = "#FDE725"
)
Heatmap(mat, col = col_fn)
```

## Colorblind-safe tools

```r
# Generate a colorblind-safe qualitative palette
safe_pal <- cb_safe_palette(4)
plot_palette(safe_pal)

# Check if an existing palette is safe
cb_check(c("#FF0000", "#00FF00", "#0000FF"))

# Adjust an unsafe palette
original <- c("#FF0000", "#00FF00", "#0000FF", "#FF8800")
adjusted <- cb_adjust(original)
plot_palette(original, main = "Original")
plot_palette(adjusted, main = "Adjusted")

# Find nearest safe colour for a single colour
cb_safe_nearest("#00CC00")
```

### When the strict ring is full

`cb_safe_palette()` and `cb_contrast()` search a single OKLCH ring at
fixed lightness and chroma. For large `n` (typically `n >= 6` with
default `min_dist`) that ring runs out of room and the function warns
and returns fewer colours. The `*_relaxed` variants progressively
widen the search along `L` and `C` to fill the request, trading a
small amount of visual cohesion for completeness:

```r
# Strict: 5 / 8 with a warning
cb_safe_palette(8)

# Relaxed: full 8, slightly less uniform in L/C
cb_safe_palette_relaxed(8, L_tol = 0.15, C_tol = 0.05)

# Same trade-off available for contrast palettes
existing <- cb_safe_palette(3)
cb_contrast_relaxed(existing, 3, L_tol = 0.2, C_tol = 0.07)
```

With `L_tol = 0, C_tol = 0` the relaxed variants are identical to the
strict ones — backward compatible.

## Function reference

| Function | Description |
|---|---|
| `oklab_seq()` | Sequential palette (OKLAB) |
| `oklab_div()` | Diverging palette (OKLAB) |
| `oklab_multi()` | Multi-anchor palette (OKLAB) |
| `oklch_seq()` | Sequential palette (OKLCH) |
| `oklch_div()` | Diverging palette (OKLCH) |
| `oklch_qualitative()` | Qualitative palette (OKLCH) |
| `okpal_from()` | Palette built around a single base colour |
| `okpal_contrast()` | New palette maximally distant from an existing one |
| `scale_colour_oklab()` | ggplot2 continuous sequential scale |
| `scale_fill_oklab()` | ggplot2 continuous sequential fill |
| `scale_colour_oklab_div()` | ggplot2 continuous diverging scale |
| `scale_fill_oklab_div()` | ggplot2 continuous diverging fill |
| `scale_colour_oklch_d()` | ggplot2 discrete qualitative scale |
| `scale_fill_oklch_d()` | ggplot2 discrete qualitative fill |
| `pheatmap_oklab()` | Colour vector for pheatmap (sequential) |
| `pheatmap_oklab_div()` | Colour vector for pheatmap (diverging) |
| `cheatmap_oklab()` | colorRamp2 object for ComplexHeatmap (sequential) |
| `cheatmap_oklab_div()` | colorRamp2 object for ComplexHeatmap (diverging) |
| `cb_safe_palette()` | Generate colorblind-safe palette |
| `cb_safe_palette_relaxed()` | Same, with progressive L/C widening for larger `n` |
| `cb_safe_nearest()` | Find nearest colorblind-safe colour |
| `cb_adjust()` | Adjust palette for colorblind safety |
| `cb_check()` | Diagnose palette colorblind safety |
| `cb_from()` | Colorblind-safe variant of `okpal_from()` |
| `cb_contrast()` | Colorblind-safe variant of `okpal_contrast()` |
| `okpal_contrast_relaxed()` / `cb_contrast_relaxed()` | Variants with progressive L/C widening |
| `plot_palette()` | Preview palette strip |

## Dependencies

- [farver](https://cran.r-project.org/package=farver) (>= 2.1.0) — OKLAB/OKLCH conversion
- [colorspace](https://cran.r-project.org/package=colorspace) — CVD simulation
- [ggplot2](https://cran.r-project.org/package=ggplot2) — scale functions
- [pheatmap](https://cran.r-project.org/package=pheatmap) — pheatmap helpers (suggested)
- [ComplexHeatmap](https://bioconductor.org/packages/ComplexHeatmap/) — ComplexHeatmap helpers (suggested)
- [circlize](https://cran.r-project.org/package=circlize) — required when using `cheatmap_*()` (suggested)

## License

MIT
