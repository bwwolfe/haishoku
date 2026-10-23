test_that("contrast matches WCAG reference values", {
  expect_equal(haishoku_contrast("#000000", "#FFFFFF"), 21)
  expect_equal(haishoku_contrast("#FFFFFF", "#FFFFFF"), 1)
})

test_that("metrics cover every combination", {
  m <- haishoku_metrics()
  expect_equal(nrow(m), 348)
  expect_true(all(m$min_de_cvd <= m$min_de_deutan))
  expect_equal(haishoku_metrics(c("#000000", "#FFFFFF"))$max_contrast, 21)
})

test_that("find filters and sorts", {
  f <- haishoku_find(size = 4, min_de = 15, cvd_safe = TRUE)
  expect_true(all(f$size == 4))
  expect_true(all(f$min_de_cvd >= 15))
  expect_false(is.unsorted(rev(f$min_de_cvd)))
  expect_true(all(haishoku_find(contains = "Coral Red")$combo %in% haishoku_combos_with("Coral Red")$combo))
  expect_equal(nrow(haishoku_find(contains = "Hermosa Pink", size = 2)), 0)
})

test_that("roles put the lightest colour behind the highest contrast ink", {
  r <- haishoku_roles(302)
  expect_equal(r$role[1:2], c("background", "ink"))
  expect_equal(r$name[1], "Nile Blue")
  expect_true(all(diff(r$contrast[-1]) <= 0))
  w <- haishoku_roles(286)
  expect_equal(w$hex[1], "#FFFFFF")
  expect_equal(nrow(w), 5)
})
