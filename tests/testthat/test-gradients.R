test_that("gradients are graded good, flat or muddy", {
  m <- haishoku_metrics(c(232, 240, 252))
  expect_equal(m$seq_quality, c("good", "muddy", "flat"))
  expect_equal(m$seq_ends_quality[m$combo == 240], "good")
  expect_true(all(haishoku_metrics()$seq_quality %in% c("good", "usable", "muddy", "flat")))
})

test_that("anchors = 'ends' keeps only the lightest and darkest", {
  all <- haishoku_palette(252, type = "sequential")
  ends <- haishoku_palette(252, type = "sequential", anchors = "ends")
  expect_equal(as.character(ends), as.character(all)[c(1, length(all))])
  expect_equal(attr(ends, "quality"), "usable")
  expect_length(haishoku_palette(252, type = "sequential", anchors = "ends", n = 5), 5)
})

test_that("diverging palettes pick a light or dark middle", {
  light <- haishoku_palette(226, type = "diverging")
  expect_equal(colour_names(light)[2], "Cream Yellow")
  expect_equal(attr(light, "quality"), "light")
  dark <- haishoku_palette(255, type = "diverging", middle = "dark")
  expect_equal(colour_names(dark)[2], "Black")
  l <- lightness(as.character(dark))
  expect_lt(l[2], min(l[c(1, 3)]))
  expect_equal(attr(haishoku_palette(255, type = "diverging", middle = "light"), "quality"), "none")
  expect_equal(attr(haishoku_palette(42, type = "diverging"), "quality"), "none")
})

test_that("find filters on gradient quality", {
  good <- haishoku_find(sequential = "good")
  expect_true(all(good$seq_quality == "good"))
  ends <- haishoku_find(sequential = c("good", "usable"), anchors = "ends", size = 4)
  expect_true(all(ends$seq_ends_quality %in% c("good", "usable")))
  expect_true(all(haishoku_find(diverging = "dark")$div_dark))
  any <- haishoku_find(diverging = "any")
  expect_true(all(any$div_light | any$div_dark))
})

test_that("scales warn about poor gradients and stay quiet otherwise", {
  expect_warning(scale_fill_haishoku_c(240), "muddy")
  expect_warning(scale_fill_haishoku_c(252), "anchors = \"ends\"")
  expect_warning(scale_colour_haishoku_b(252), "flat")
  expect_warning(scale_fill_haishoku_c(42, type = "diverging"), "no colourblind-safe")
  expect_no_warning(scale_fill_haishoku_c(232))
  expect_no_warning(scale_fill_haishoku_c(252, anchors = "ends"))
  expect_no_warning(scale_fill_haishoku_b(226, type = "diverging"))
  p <- ggplot2::ggplot(ggplot2::faithfuld, ggplot2::aes(waiting, eruptions, fill = density - 0.01)) +
    ggplot2::geom_raster()
  expect_no_error(ggplot2::ggplot_build(p + scale_fill_haishoku_c(255, type = "diverging", middle = "dark")))
})
