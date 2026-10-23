library(ggplot2)

test_that("discrete scales map levels to combination colours", {
  p <- ggplot(mpg, aes(displ, hwy, colour = drv)) + geom_point() + scale_colour_haishoku(286)
  b <- ggplot_build(p)
  expect_setequal(unique(b$data[[1]]$colour), as.character(haishoku_palette(286, n = 3)))
  expect_s3_class(scale_fill_haishoku(), "ScaleDiscrete")
  expect_s3_class(scale_color_haishoku_d(42), "ScaleDiscrete")
})

test_that("too many levels error helpfully unless extended", {
  p <- ggplot(mpg, aes(class, fill = class)) + geom_bar()
  expect_error(ggplot_build(p + scale_fill_haishoku(42)), "extend")
  b <- ggplot_build(p + scale_fill_haishoku(42, extend = TRUE))
  expect_length(unique(b$data[[1]]$fill), 7)
})

test_that("continuous and binned scales build", {
  p <- ggplot(faithfuld, aes(waiting, eruptions, fill = density)) + geom_raster()
  expect_s3_class(scale_fill_haishoku(232, discrete = FALSE), "ScaleContinuous")
  expect_no_error(ggplot_build(p + scale_fill_haishoku_c(232)))
  expect_no_error(ggplot_build(p + scale_fill_haishoku_b(232)))
  expect_no_error(ggplot_build(p + scale_colour_haishoku_c(286, type = "diverging",
                                                           midpoint = 0.02, aesthetics = "fill")))
})

test_that("show, show_colour and theme return ggplots", {
  expect_s3_class(haishoku_show(286, cvd = "all"), "ggplot")
  expect_s3_class(haishoku_show_colour("Hermosa Pink"), "ggplot")
  expect_s3_class(theme_haishoku(286), "theme")
})

test_that("palettes work directly as manual scale values", {
  p <- ggplot(mpg, aes(displ, hwy, colour = drv)) + geom_point() +
    scale_colour_manual(values = haishoku_palette(286))
  b <- ggplot_build(p)
  expect_false(anyNA(b$data[[1]]$colour))
  expect_setequal(unique(b$data[[1]]$colour), as.character(haishoku_palette(286))[1:3])
})
