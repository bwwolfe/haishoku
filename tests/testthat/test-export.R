test_that("exports produce the expected formats", {
  css <- haishoku_export(42, "css")
  expect_match(css, "--hs-yellow-ocher: #E2B540;", fixed = TRUE, all = FALSE)
  gpl <- haishoku_export(42, "gpl")
  expect_equal(gpl[1], "GIMP Palette")
  tex <- haishoku_export(42, "tex")
  expect_match(tex, "\\definecolor{hsYellowOcher}{cmyk}{0.12,0.28,0.88,0.00}", fixed = TRUE, all = FALSE)
  skip_if_not_installed("jsonlite")
  json <- jsonlite::fromJSON(paste(haishoku_export(42, "json"), collapse = "\n"))
  expect_equal(json$colours$name, c("Yellow Ocher", "Violet"))
})

test_that("export writes files", {
  path <- tempfile(fileext = ".gpl")
  haishoku_export(286, "gpl", file = path)
  expect_true(file.exists(path))
})
