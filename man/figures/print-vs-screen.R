# Writes man/figures/print-vs-screen.svg: tiles whose left half is the sRGB hex
# and right half the printed colour (the CMYK recipe's Lab through the SWOP
# profile) in Display P3, with the hex as fallback. On a wide-gamut screen the
# right half shows what sRGB loses; on a standard screen the halves match.
# Run from the package root after data-raw/02-build-data.R.

load("data/wada_colours.rda")
cols <- read.csv("data-raw/colours.csv", fileEncoding = "UTF-8-BOM")
cols <- cols[match(wada_colours$id, cols$id), ]

lab_to_p3 <- function(lab) {
  fy <- (lab[, 1] + 16) / 116
  fx <- fy + lab[, 2] / 500
  fz <- fy - lab[, 3] / 200
  finv <- function(t) ifelse(t > 6 / 29, t^3, 3 * (6 / 29)^2 * (t - 4 / 29))
  xyz50 <- cbind(0.9642 * finv(fx), finv(fy), 0.8249 * finv(fz))
  bradford <- matrix(c(0.9555766, -0.0230393, 0.0631636,
                       -0.0282895, 1.0099416, 0.0210077,
                       0.0122982, -0.0204830, 1.3299098), 3, byrow = TRUE)
  to_p3 <- matrix(c(2.4934969, -0.9313836, -0.4027108,
                    -0.8294890, 1.7626641, 0.0236247,
                    0.0358458, -0.0761724, 0.9568845), 3, byrow = TRUE)
  lin <- xyz50 %*% t(bradford) %*% t(to_p3)
  clipped <- apply(lin < -0.001 | lin > 1.001, 1, any)
  lin <- pmin(pmax(lin, 0), 1)
  enc <- ifelse(lin <= 0.0031308, 12.92 * lin, 1.055 * lin^(1 / 2.4) - 0.055)
  # Lab (D50) of the clipped P3 colour, to measure what P3 still misses
  xyz_back <- lin %*% t(solve(to_p3)) %*% t(solve(bradford))
  f <- function(t) ifelse(t > (6 / 29)^3, t^(1 / 3), t / (3 * (6 / 29)^2) + 4 / 29)
  gx <- f(xyz_back[, 1] / 0.9642); gy <- f(xyz_back[, 2]); gz <- f(xyz_back[, 3] / 0.8249)
  lab_back <- cbind(116 * gy - 16, 500 * (gx - gy), 200 * (gy - gz))
  de <- diag(matrix(farver::compare_colour(lab, lab_back, "lab", method = "cie2000",
                                           white_from = "D50", white_to = "D50"), nrow(lab)))
  list(p3 = enc, clipped = clipped, p3_delta_e = de)
}

conv <- lab_to_p3(as.matrix(cols[c("print_lab_l", "print_lab_a", "print_lab_b")]))
wada_colours$p3 <- sprintf("color(display-p3 %.4f %.4f %.4f)", conv$p3[, 1], conv$p3[, 2], conv$p3[, 3])
wada_colours$p3_clipped <- conv$clipped
wada_colours$p3_delta_e <- round(conv$p3_delta_e, 2)

worst <- wada_colours[order(-wada_colours$print_delta_e), ][1:7, ]
best_names <- c("Hermosa Pink", "Burnt Sienna", "Orange Yellow", "Violet Blue")
best <- wada_colours[match(best_names, wada_colours$name), ]

tile_w <- 120; tile_h <- 76; gap <- 14; label_h <- 56; left <- 16
row_svg <- function(d, y) {
  vapply(seq_len(nrow(d)), function(i) {
    x <- left + (i - 1) * (tile_w + gap)
    r <- d[i, ]
    recipe <- sprintf("C%d M%d Y%d K%d", r$cyan, r$magenta, r$yellow, r$black)
    paste0(
      sprintf('<rect x="%d" y="%d" width="%d" height="%d" fill="%s"/>', x, y, tile_w / 2, tile_h, r$hex),
      sprintf('<rect x="%d" y="%d" width="%d" height="%d" fill="%s" style="fill: %s"/>',
              x + tile_w / 2, y, tile_w / 2, tile_h, r$hex, r$p3),
      sprintf('<text x="%d" y="%d" class="name">%s</text>', x, y + tile_h + 16, r$name),
      sprintf('<text x="%d" y="%d" class="meta">%s</text>', x, y + tile_h + 31, recipe),
      sprintf('<text x="%d" y="%d" class="meta">sRGB ΔE %.1f%s</text>', x, y + tile_h + 45,
              r$print_delta_e, if (r$print_delta_e >= 1) sprintf(" · P3 %.1f", r$p3_delta_e) else "")
    )
  }, "")
}

ncol <- max(nrow(worst), nrow(best))
width <- left * 2 + ncol * tile_w + (ncol - 1) * gap
y1 <- 58; y2 <- y1 + tile_h + label_h + 48
height <- y2 + tile_h + label_h + 58
svg <- c(
  sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d" font-family="Helvetica, Arial, sans-serif">',
          width, height, width, height),
  '<style>.h{font-size:15px;font-weight:bold;fill:#222}.s{font-size:12px;fill:#555}.name{font-size:12px;fill:#222}.meta{font-size:10.5px;fill:#666}</style>',
  sprintf('<rect width="%d" height="%d" fill="#ffffff"/>', width, height),
  sprintf('<text x="%d" y="24" class="h">Screen (sRGB hex, left) vs print (CMYK recipe, right)</text>', left),
  sprintf('<text x="%d" y="42" class="s">Most lost on screen: full-cyan teals and blues, and pure yellow</text>', left),
  row_svg(worst, y1),
  sprintf('<text x="%d" y="%d" class="s">Typical: %d of the %d colours differ by less than 1 ΔE, which is imperceptible</text>',
          left, y2 - 12, sum(wada_colours$print_delta_e < 1), nrow(wada_colours)),
  row_svg(best, y2),
  sprintf('<text x="%d" y="%d" class="s">Right halves use Display P3. On a wide-gamut screen (recent Macs, iPhones, many laptops) they show the printed colour, or as close as P3 gets;</text>',
          left, height - 36),
  sprintf('<text x="%d" y="%d" class="s">on a standard sRGB screen both halves look the same, because that is all the screen can show. ΔE is CIEDE2000 from the printed colour.</text>',
          left, height - 20),
  "</svg>"
)
writeLines(enc2utf8(svg), "man/figures/print-vs-screen.svg", useBytes = TRUE)

cat("Clipped to the P3 boundary:", paste(wada_colours$name[wada_colours$p3_clipped], collapse = ", "), "\n")
print(worst[c("name", "hex", "p3", "print_delta_e", "p3_delta_e", "p3_clipped")], row.names = FALSE)
