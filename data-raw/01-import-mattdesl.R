# One-off import of the community transcription of Volume 1 into the two
# editable source-of-truth CSVs. Run once; after that, correct the CSVs by hand
# against the printed book and re-run 02-build-data.R.
#
# Source: https://github.com/mattdesl/dictionary-of-colour-combinations (MIT),
# itself a corrected fork of Dain M. Blodorn Kim's sanzo-wada data. Hex values
# there were converted from the book's CMYK with the SWOP v2 -> sRGB ICC
# profiles (relative colorimetric, black point compensation).

src <- jsonlite::fromJSON("data-raw/mattdesl-colors.json", simplifyVector = FALSE)

colours <- data.frame(
  volume  = 1L,
  id      = seq_along(src),
  name    = vapply(src, `[[`, "", "name"),
  section = vapply(src, function(e) as.integer(e$swatch) + 1L, 1L),
  cyan    = vapply(src, function(e) as.integer(e$cmyk[[1]]), 1L),
  magenta = vapply(src, function(e) as.integer(e$cmyk[[2]]), 1L),
  yellow  = vapply(src, function(e) as.integer(e$cmyk[[3]]), 1L),
  black   = vapply(src, function(e) as.integer(e$cmyk[[4]]), 1L),
  hex     = toupper(vapply(src, `[[`, "", "hex")),
  # Lab (D50) of the CMYK recipe through the SWOP profile: the colour as printed
  print_lab_l = vapply(src, function(e) round(e$lab[[1]], 2), 1),
  print_lab_a = vapply(src, function(e) round(e$lab[[2]], 2), 1),
  print_lab_b = vapply(src, function(e) round(e$lab[[3]], 2), 1),
  name_ja = NA_character_,
  verified = FALSE
)

combinations <- do.call(rbind, lapply(seq_along(src), function(i) {
  data.frame(volume = 1L, combo = as.integer(unlist(src[[i]]$combinations)), colour_id = i)
}))
combinations <- combinations[order(combinations$combo, combinations$colour_id), ]
# Position within a combination follows the colour index. Edit to match the
# printed arrangement if you want palettes to come out in book order.
combinations$position <- stats::ave(combinations$colour_id, combinations$combo, FUN = seq_along)

utils::write.csv(colours, "data-raw/colours.csv", row.names = FALSE, na = "")
utils::write.csv(combinations, "data-raw/combinations.csv", row.names = FALSE, na = "")
