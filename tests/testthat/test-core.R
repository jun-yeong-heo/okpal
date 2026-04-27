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

test_that("cb_contrast returns requested n", {
  existing <- cb_safe_palette(3)
  contrast <- cb_contrast(existing, 3)
  expect_length(contrast, 3)
})

test_that("endpoints are preserved in oklab_seq", {
  low <- "#1B0A55"
  high <- "#FDE725"
  pal <- oklab_seq(low, high, n = 100)
  expect_equal(pal[1], toupper(low))
  expect_equal(pal[100], toupper(high))
})
