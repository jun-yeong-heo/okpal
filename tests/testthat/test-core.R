test_that("oklab_seq returns correct length", {
  pal <- oklab_seq("#000000", "#FFFFFF", n = 10)
  expect_length(pal, 10)
  expect_true(all(grepl("^#[0-9A-F]{6}$", pal, ignore.case = TRUE)))
})

test_that("oklch_seq returns correct length", {
  pal <- oklch_seq("#FF0000", "#0000FF", n = 10)
  expect_length(pal, 10)
})

test_that("oklab_div returns correct length", {
  pal <- oklab_div("#2166AC", "#F7F7F7", "#B2182B", n = 11)
  expect_length(pal, 11)
})

test_that("oklab_multi handles multiple anchors", {
  pal <- oklab_multi(c("#FF0000", "#00FF00", "#0000FF"), n = 15)
  expect_length(pal, 15)
})

test_that("oklch_qualitative generates n colours", {
  pal <- oklch_qualitative(6)
  expect_length(pal, 6)
  expect_true(length(unique(pal)) == 6)
})

test_that("cb_check returns data.frame with correct structure", {
  result <- cb_check(c("#FF0000", "#00FF00", "#0000FF"))
  expect_s3_class(result, "data.frame")
  expect_true("safe" %in% names(result))
  expect_equal(nrow(result), 3)  # 3 choose 2 = 3 pairs
})

test_that("cb_safe_palette returns requested n", {
  pal <- cb_safe_palette(5)
  expect_length(pal, 5)
})

test_that("cb_adjust returns same length", {
  cols <- c("#FF0000", "#00FF00", "#0000FF")
  adj <- cb_adjust(cols)
  expect_length(adj, 3)
})

test_that("okpal_from returns correct length (mono)", {
  pal <- okpal_from("#3366CC", n = 7)
  expect_length(pal, 7)
  expect_true(all(grepl("^#[0-9A-F]{6}$", pal, ignore.case = TRUE)))
})

test_that("okpal_from with hue_range produces distinct hues", {
  mono <- okpal_from("#3366CC", n = 5, hue_range = 0)
  multi <- okpal_from("#3366CC", n = 5, hue_range = 60)
  # multi-hue should produce more hue variation
  mono_lch <- farver::decode_colour(mono, to = "oklch")
  multi_lch <- farver::decode_colour(multi, to = "oklch")
  mono_hue_range <- diff(range(mono_lch[, "h"]))
  multi_hue_range <- diff(range(multi_lch[, "h"]))
  expect_true(multi_hue_range > mono_hue_range)
})

test_that("cb_from returns adjusted palette", {
  pal <- cb_from("#00CC00", n = 5)
  expect_length(pal, 5)
})

test_that("okpal_contrast avoids existing colours", {
  existing <- oklch_qualitative(4)
  contrast <- okpal_contrast(existing, 4)
  expect_length(contrast, 4)
  # No overlap
  expect_equal(length(intersect(existing, contrast)), 0)
})

test_that("cb_contrast warns and returns fewer when ring is full", {
  # cb_safe_palette spreads colours evenly around a single L/C ring,
  # so cb_contrast cannot satisfy min_dist for every requested colour.
  # This pins the documented limitation.
  existing <- cb_safe_palette(3)
  expect_warning(contrast <- cb_contrast(existing, 3))
  expect_lte(length(contrast), 3)
})

test_that("endpoints are preserved in oklab_seq", {
  low <- "#1B0A55"
  high <- "#FDE725"
  pal <- oklab_seq(low, high, n = 100)
  expect_equal(pal[1], toupper(low))
  expect_equal(pal[100], toupper(high))
})

test_that("endpoints are preserved in oklch_seq", {
  low <- "#1B0A55"
  high <- "#FDE725"
  pal <- oklch_seq(low, high, n = 100)
  expect_equal(pal[1], toupper(low))
  expect_equal(pal[100], toupper(high))
})

test_that("oklch_seq grey -> colour does not sweep hue", {
  # When one endpoint is achromatic, hue should not rotate through
  # an unrelated arc; intermediate colours should stay near the
  # chromatic endpoint's hue.
  pal <- oklch_seq("#888888", "#FF0000", n = 5)
  lch <- farver::decode_colour(pal, to = "oklch")
  red_h <- farver::decode_colour("#FF0000", to = "oklch")[1, "h"]
  # Skip the first row (grey) - its hue is undefined.
  hue_dev <- abs(((lch[-1, "h"] - red_h + 180) %% 360) - 180)
  expect_true(all(hue_dev < 5))
})

test_that("hex inputs are validated", {
  expect_error(oklab_seq("not a hex", "#FFFFFF"), "low")
  expect_error(oklab_seq("#FFFFFF", "#GGGGGG"), "high")
  expect_error(oklab_div("#000000", "bad", "#FFFFFF"), "mid")
  expect_error(oklab_multi(c("#000000", "#XYZ")), "colours")
  expect_error(oklab_multi(character(0)), "colours")
  expect_error(cb_check(c("#000000", NA_character_)), "colours")
  expect_error(cb_safe_nearest("blue"), "col")
  expect_error(okpal_from("#1234"), "base")
  expect_error(plot_palette("#GGG"), "colours")
})

test_that("space argument is validated", {
  expect_error(oklab_multi(c("#FF0000", "#00FF00")), NA)  # baseline ok
  expect_error(okpal_from("#3366CC", space = "rgb"))
  expect_error(scale_colour_oklab(space = "xyz"))
})

test_that("oklab_multi n=2 collapses to oklab_seq", {
  a <- oklab_seq("#FF0000", "#0000FF", n = 5)
  b <- oklab_multi(c("#FF0000", "#0000FF"), n = 5)
  expect_equal(a, b)
})

test_that("cb_check row count equals n choose 2", {
  expect_equal(nrow(cb_check(c("#FF0000", "#00FF00", "#0000FF", "#FF8800"))), 6)
  expect_equal(nrow(cb_check(c("#FF0000", "#00FF00"))), 1)
})

test_that("oklch_qualitative produces evenly spaced hues", {
  pal <- oklch_qualitative(4, h_start = 0)
  h <- farver::decode_colour(pal, to = "oklch")[, "h"]
  # Spacing of consecutive hues should be ~90 deg, allowing for
  # roundtrip loss through sRGB encoding/decoding via farver.
  dh <- diff(sort(h %% 360))
  expect_true(all(abs(dh - 90) < 5))
})

test_that("cb_check on a single colour returns an empty result frame", {
  r <- cb_check(c("#FF0000"))
  expect_s3_class(r, "data.frame")
  expect_equal(nrow(r), 0)
  expect_named(r, c("col_i", "col_j", "dist_deutan", "dist_protan",
                    "dist_tritan", "min_dist", "safe"))
})

test_that("seq with n=1 collapses to the low endpoint", {
  # farver may carry over a name attribute from the encoded matrix;
  # ignore attributes for this comparison.
  expect_equal(oklab_seq("#1B0A55", "#FDE725", n = 1), "#1B0A55",
               ignore_attr = TRUE)
  expect_equal(oklch_seq("#1B0A55", "#FDE725", n = 1), "#1B0A55",
               ignore_attr = TRUE)
})

test_that("cb_safe_palette_relaxed fills n where strict cannot", {
  # Strict variant stalls around 5 with defaults
  n <- 8
  suppressWarnings(strict <- cb_safe_palette(n))
  relaxed <- cb_safe_palette_relaxed(n, L_tol = 0.2, C_tol = 0.07)
  expect_gte(length(relaxed), length(strict))
  expect_length(relaxed, n)
})

test_that("cb_safe_palette_relaxed with zero tol equals strict at n=4", {
  # At small n both succeed at step 1, which is identical to strict.
  expect_equal(
    cb_safe_palette_relaxed(4, L_tol = 0, C_tol = 0),
    cb_safe_palette(4)
  )
})

test_that("cb_contrast_relaxed handles a saturated ring better", {
  existing <- cb_safe_palette(3)
  relaxed <- cb_contrast_relaxed(existing, 3, L_tol = 0.2, C_tol = 0.07)
  expect_length(relaxed, 3)
})

test_that("okpal_contrast_relaxed always returns n", {
  existing <- oklch_qualitative(4)
  expect_length(okpal_contrast_relaxed(existing, 5), 5)
})

test_that("relaxed variants reject invalid tol", {
  expect_error(cb_safe_palette_relaxed(4, L_tol = -1))
  expect_error(cb_safe_palette_relaxed(4, tol_steps = 0))
})
