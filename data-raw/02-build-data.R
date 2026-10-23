# Builds the package datasets from the editable CSVs in data-raw/.
#
#   colours.csv       one row per colour      (volume, id, name, section, CMYK, hex, ...)
#   combinations.csv  one row per colour in a combination (volume, combo, colour_id, position)
#
# Volume 2 (or any later volume) is picked up automatically: add rows with
# volume = 2 to both files, or drop vol2-colours.csv / vol2-combinations.csv
# (same columns as the templates) next to them.
#
# After correcting a CMYK recipe, blank its hex and print_lab_* cells and run
#   powershell -File data-raw/cmyk-convert.ps1 -Csv data-raw/colours.csv
# to convert it through the same SWOP ICC profile as the other colours. Any hex
# still blank here is derived with the naive formula, a rough approximation;
# blank print Lab leaves print_delta_e as NA.

read_csv <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "", fileEncoding = "UTF-8-BOM")
}

colours <- read_csv("data-raw/colours.csv")
combinations <- read_csv("data-raw/combinations.csv")
for (v in c("vol2-colours.csv", "vol2-combinations.csv")) {
  path <- file.path("data-raw", v)
  if (file.exists(path)) {
    extra <- read_csv(path)
    if (startsWith(v, "vol2-colours")) colours <- rbind(colours, extra[names(colours)])
    else combinations <- rbind(combinations, extra[names(combinations)])
  }
}

# --- checks -----------------------------------------------------------------
cmyk <- c("cyan", "magenta", "yellow", "black")
stopifnot(
  !anyDuplicated(colours[c("volume", "id")]),
  !anyDuplicated(tolower(paste(colours$volume, colours$name))),
  all(paste(combinations$volume, combinations$colour_id) %in% paste(colours$volume, colours$id)),
  !anyDuplicated(combinations[c("volume", "combo", "colour_id")])
)
# Out-of-range inks are transcription errors to check against the book; flag
# rather than guess.
bad_ink <- apply(as.matrix(colours[cmyk]), 1, function(r) any(r < 0 | r > 100))
if (any(bad_ink)) {
  warning("CMYK outside 0-100, check against the book: ",
          paste(colours$name[bad_ink], collapse = ", "), call. = FALSE)
}

naive_hex <- function(c, m, y, k) {
  c <- c / 100; m <- m / 100; y <- y / 100; k <- k / 100
  grDevices::rgb((1 - c) * (1 - k), (1 - m) * (1 - k), (1 - y) * (1 - k))
}
missing_hex <- is.na(colours$hex) | colours$hex == ""
if (any(missing_hex)) {
  message("Deriving hex from CMYK (naive formula) for: ",
          paste(colours$name[missing_hex], collapse = ", "))
  colours$hex[missing_hex] <- with(colours[missing_hex, ], naive_hex(cyan, magenta, yellow, black))
}
colours$hex <- toupper(colours$hex)

# --- derived colour coordinates ---------------------------------------------
rgb <- farver::decode_colour(colours$hex)
lab <- farver::convert_colour(rgb, "rgb", "lab")
hcl <- farver::convert_colour(rgb, "rgb", "hcl")

# How far the screen colour falls short of the print: CIEDE2000 between the
# CMYK recipe's Lab (D50, through the SWOP profile) and the hex's Lab, adapted
# to D50 with Bradford as ICC colour management does.
srgb_to_lab_d50 <- function(rgb) {
  v <- rgb / 255
  v <- ifelse(v <= 0.04045, v / 12.92, ((v + 0.055) / 1.055)^2.4)
  m <- matrix(c(0.4360747, 0.3850649, 0.1430804,
                0.2225045, 0.7168786, 0.0606169,
                0.0139322, 0.0971045, 0.7141733), 3, byrow = TRUE)
  xyz <- v %*% t(m)
  f <- function(t) ifelse(t > (6 / 29)^3, t^(1 / 3), t / (3 * (6 / 29)^2) + 4 / 29)
  fx <- f(xyz[, 1] / 0.9642); fy <- f(xyz[, 2]); fz <- f(xyz[, 3] / 0.8249)
  cbind(l = 116 * fy - 16, a = 500 * (fx - fy), b = 200 * (fy - fz))
}
print_lab <- as.matrix(colours[c("print_lab_l", "print_lab_a", "print_lab_b")])
screen_lab <- srgb_to_lab_d50(rgb)
print_delta_e <- rep(NA_real_, nrow(colours))
has_print <- stats::complete.cases(print_lab)
if (any(has_print)) {
  print_delta_e[has_print] <- diag(matrix(
    farver::compare_colour(print_lab[has_print, , drop = FALSE], screen_lab[has_print, , drop = FALSE],
                           from_space = "lab", method = "cie2000", white_from = "D50", white_to = "D50"),
    sum(has_print)))
}
if (!all(has_print)) {
  message("No print Lab (print_delta_e is NA) for: ", paste(colours$name[!has_print], collapse = ", "))
}

key <- paste(combinations$volume, combinations$colour_id)
n_combos <- table(factor(key, levels = paste(colours$volume, colours$id)))

wada_colours <- data.frame(
  volume   = as.integer(colours$volume),
  id       = as.integer(colours$id),
  name     = colours$name,
  name_ja  = colours$name_ja,
  section  = as.integer(colours$section),
  hex      = colours$hex,
  cyan     = as.integer(colours$cyan),
  magenta  = as.integer(colours$magenta),
  yellow   = as.integer(colours$yellow),
  black    = as.integer(colours$black),
  red      = as.integer(rgb[, "r"]),
  green    = as.integer(rgb[, "g"]),
  blue     = as.integer(rgb[, "b"]),
  lab_l    = round(lab[, "l"], 2),
  lab_a    = round(lab[, "a"], 2),
  lab_b    = round(lab[, "b"], 2),
  hue      = round(ifelse(is.na(hcl[, "h"]), 0, hcl[, "h"]), 1),
  chroma   = round(hcl[, "c"], 2),
  print_delta_e = round(print_delta_e, 2),
  n_combos = as.integer(n_combos),
  verified = as.logical(colours$verified),
  stringsAsFactors = FALSE
)
wada_colours <- wada_colours[order(wada_colours$volume, wada_colours$id), ]
rownames(wada_colours) <- NULL

combinations <- combinations[order(combinations$volume, combinations$combo, combinations$position), ]
idx <- match(paste(combinations$volume, combinations$colour_id),
             paste(wada_colours$volume, wada_colours$id))
size <- stats::ave(combinations$colour_id, combinations$volume, combinations$combo, FUN = length)
wada_combos <- data.frame(
  volume    = as.integer(combinations$volume),
  combo     = as.integer(combinations$combo),
  size      = as.integer(size),
  position  = as.integer(combinations$position),
  colour_id = as.integer(combinations$colour_id),
  name      = wada_colours$name[idx],
  hex       = wada_colours$hex[idx],
  stringsAsFactors = FALSE
)
rownames(wada_combos) <- NULL

dir.create("data", showWarnings = FALSE)
save(wada_colours, file = "data/wada_colours.rda", compress = "xz", version = 2)
save(wada_combos, file = "data/wada_combos.rda", compress = "xz", version = 2)
