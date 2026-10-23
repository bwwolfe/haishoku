test_that("volume 1 has 159 colours and 348 combinations of 2-4", {
  expect_equal(nrow(colours_in(1)), 159)
  combos <- combos_in(1)
  expect_equal(length(unique(combos$combo)), 348)
  sizes <- tapply(combos$size, combos$combo, unique)
  expect_true(all(sizes[1:120] == 2))
  expect_true(all(sizes[121:240] == 3))
  expect_true(all(sizes[241:348] == 4))
})

test_that("every combination colour exists and hex codes are valid", {
  expect_true(all(wada_combos$colour_id %in% wada_colours$id))
  expect_true(all(is_hex(wada_colours$hex)))
  expect_false(anyDuplicated(tolower(wada_colours$name)) > 0)
})

test_that("print_delta_e flags the out-of-gamut colours", {
  de <- wada_colours$print_delta_e
  expect_false(anyNA(de))
  expect_gt(mean(de < 1), 0.9)
  expect_gt(de[wada_colours$name == "Peacock Blue"], 5)
  expect_lt(de[wada_colours$name == "Hermosa Pink"], 1)
  expect_true("print_delta_e" %in% names(haishoku_cmyk(haishoku_palette(286))))
})
