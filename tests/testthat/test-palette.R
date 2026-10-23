test_that("discrete palettes return the combination", {
  pal <- haishoku_palette(42)
  expect_s3_class(pal, "haishoku_palette")
  expect_equal(unname(as.character(pal)), haishoku_combo(42)$hex)
  expect_equal(colour_names(pal), haishoku_combo(42)$name)
  expect_null(names(pal))
  expect_length(haishoku_palette(286, n = 2), 2)
  expect_error(haishoku_palette(42, n = 5), "extend = TRUE")
  expect_length(haishoku_palette(42, n = 5, extend = TRUE), 5)
})

test_that("custom palettes accept names, hex and R colours", {
  pal <- haishoku_palette(c("Coral Red", "#000000", "steelblue"))
  expect_length(pal, 3)
  expect_equal(attr(pal, "combo"), "custom")
})

test_that("sequential palettes run light to dark", {
  pal <- haishoku_palette(286, type = "sequential", n = 9)
  expect_length(pal, 9)
  expect_true(all(diff(lightness(as.character(pal))) < 1))
})

test_that("diverging palettes have a light middle", {
  pal <- haishoku_palette(286, type = "diverging", n = 5)
  l <- lightness(as.character(pal))
  expect_equal(unname(which.max(l)), 3)
  expect_equal(as.character(haishoku_palette(42, type = "diverging"))[2], "#FFFFFF")
})

test_that("order and reverse", {
  pal <- haishoku_palette(286)
  expect_equal(rev(as.character(pal)), as.character(haishoku_palette(286, reverse = TRUE)))
  light <- haishoku_palette(286, order = "lightness")
  expect_true(all(diff(lightness(as.character(light))) <= 0))
})

test_that("extend adds distinct dictionary colours", {
  ext <- haishoku_extend(286, 7)
  expect_length(ext, 7)
  expect_equal(as.character(ext)[1:4], as.character(haishoku_palette(286)))
  expect_true(all(as.character(ext) %in% wada_colours$hex))
  expect_false(anyDuplicated(as.character(ext)) > 0)
  expect_gt(min_pairwise(delta_e_matrix(as.character(ext))), 5)
})

test_that("haishoku_pal returns a palette function", {
  f <- haishoku_pal(286)
  expect_length(f(3), 3)
  expect_type(f(3), "character")
})

test_that("printing works", {
  expect_output(print(haishoku_palette(42)), "combination 42")
})

test_that("extend respects a background", {
  ext <- haishoku_extend(c("Deep Slate Olive", "Peach Red"), 6, background = "Naples Yellow")
  expect_true(all(haishoku_contrast(as.character(ext)[-(1:2)], "Naples Yellow") >= 3))
})

test_that("base plot method draws", {
  path <- tempfile(fileext = ".png")
  grDevices::png(path)
  on.exit(grDevices::dev.off())
  expect_no_error(plot(haishoku_palette(250)))
})
