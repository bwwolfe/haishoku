test_that("colours are found by forgiving names and by number", {
  expect_equal(unname(haishoku_colour("hermosa pink")), "#F9C1CE")
  expect_equal(haishoku_colour("Grayish Lavender - A"), haishoku_colour("greyish lavender a"))
  expect_equal(names(haishoku_colour(1)), "Hermosa Pink")
  expect_equal(haishoku_colour("1"), haishoku_colour(1))
})

test_that("misspelt names suggest alternatives", {
  expect_error(haishoku_colour("Cerulean Blue"), "Did you mean: Cerulian Blue")
})

test_that("CMYK recipes come from the book data", {
  cmyk <- haishoku_cmyk("Hermosa Pink")
  expect_equal(cmyk$recipe, "C0 M30 Y6 K0")
  expect_equal(nrow(haishoku_cmyk(haishoku_palette(286))), 4)
  expect_error(haishoku_cmyk(haishoku_palette(286, type = "sequential", n = 7)), "interpolated")
})

test_that("nearest colour of a dictionary colour is itself", {
  near <- haishoku_nearest(c("#F9C1CE", "firebrick"), n = 2)
  expect_equal(nrow(near), 4)
  expect_equal(near$name[1], "Hermosa Pink")
  expect_equal(near$delta_e[1], 0)
})

test_that("the dictionary finds combinations containing colours", {
  found <- haishoku_combos_with("Hermosa Pink")
  expect_equal(found$combo, c(176, 227, 273))
  both <- haishoku_combos_with(c("Hermosa Pink", "Neutral Gray"))
  expect_equal(both$combo, 273)
  expect_equal(nrow(haishoku_combos_with("Hermosa Pink", size = 2)), 0)
  expect_error(haishoku_combo(999), "No combination")
})

test_that("names and numbers can be mixed", {
  mixed <- haishoku_colour(c("hermosa pink", "58"))
  expect_equal(names(mixed), c("Hermosa Pink", wada_colours$name[58]))
  expect_equal(as.character(haishoku_palette(c("Black", "1"))), c("#111314", "#F9C1CE"))
})

test_that("hs_ aliases point at the haishoku_ functions", {
  expect_identical(hs_palette, haishoku_palette)
  expect_identical(hs_colour, haishoku_colour)
  expect_identical(hs_show_colour, haishoku_show_colour)
  expect_equal(as.character(hs_palette(42)), as.character(haishoku_palette(42)))
})

test_that("book corrections are in the data", {
  dvb <- haishoku_cmyk("Dull Violet Black")
  expect_equal(dvb$recipe, "C100 M90 Y38 K50")
  expect_equal(dvb$hex, "#0C1C43")
  expect_equal(names(haishoku_colour(39)), "Sulphur Yellow")
})

test_that("Artemisia Green is spelt as in the book", {
  expect_equal(names(haishoku_colour(95)), "Artemisia Green")
})
