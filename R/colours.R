# Internal helpers ------------------------------------------------------------

colours_in <- function(volume = 1) {
  cols <- haishoku::wada_colours
  cols <- cols[cols$volume %in% volume, , drop = FALSE]
  if (nrow(cols) == 0) stop("No colours for volume ", volume, ".", call. = FALSE)
  cols
}

combos_in <- function(volume = 1) {
  combos <- haishoku::wada_combos
  combos[combos$volume %in% volume, , drop = FALSE]
}

# Case, spacing and punctuation insensitive, and grey/colour spellings accepted.
normalise_name <- function(x) {
  x <- tolower(x)
  x <- gsub("grey", "gray", x, fixed = TRUE)
  x <- gsub("colour", "color", x, fixed = TRUE)
  gsub("[^a-z0-9]", "", x)
}

is_hex <- function(x) is.character(x) & grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", x)

# Rows of colours_in(volume) for colour numbers or names; errors on misses.
match_colours <- function(x, volume = 1) {
  cols <- colours_in(volume)
  idx <- lookup_colours(x, cols)
  if (anyNA(idx)) {
    bad <- x[is.na(idx)]
    hint <- if (is.character(bad)) suggest_names(bad[1], cols$name) else ""
    stop("Not a colour in volume ", paste(volume, collapse = "/"), ": ",
         paste(bad, collapse = ", "), hint, call. = FALSE)
  }
  idx
}

# Row indices for colour numbers, digit strings or names; NA for misses.
lookup_colours <- function(x, cols) {
  if (is.numeric(x)) return(match(x, cols$id))
  idx <- match(normalise_name(x), normalise_name(cols$name))
  digits <- grepl("^[0-9]+$", x)
  idx[digits] <- match(as.integer(x[digits]), cols$id)
  idx
}

suggest_names <- function(x, choices) {
  near <- choices[agrep(x, choices, ignore.case = TRUE, max.distance = 0.2)]
  if (length(near) == 0) {
    near <- choices[grepl(strsplit(x, "\\s+")[[1]][1], choices, ignore.case = TRUE)]
  }
  if (length(near) == 0) return("")
  paste0(". Did you mean: ", paste(utils::head(near, 5), collapse = ", "), "?")
}

# Any colour spec -> upper-case hex. Dictionary names and numbers win over R's
# colour names, so "White" is Wada's white.
as_hex <- function(x, volume = 1) {
  if (inherits(x, "haishoku_palette")) return(toupper(as.character(x)))
  if (is.numeric(x)) return(colours_in(volume)$hex[match_colours(x, volume)])
  out <- character(length(x))
  hex <- is_hex(x)
  out[hex] <- toupper(substr(x[hex], 1, 7))
  rest <- which(!hex)
  if (length(rest)) {
    cols <- colours_in(volume)
    idx <- lookup_colours(x[rest], cols)
    out[rest[!is.na(idx)]] <- cols$hex[idx[!is.na(idx)]]
    r_col <- rest[is.na(idx)]
    if (length(r_col)) {
      known <- x[r_col] %in% grDevices::colors()
      if (!all(known)) match_colours(x[r_col][!known], volume)  # errors with hints
      rgb <- grDevices::col2rgb(x[r_col])
      out[r_col] <- grDevices::rgb(rgb[1, ], rgb[2, ], rgb[3, ], maxColorValue = 255)
    }
  }
  out
}

# Names for hex codes that are dictionary colours, "" otherwise.
dictionary_names <- function(hex, volume = 1) {
  cols <- colours_in(volume)
  out <- cols$name[match(toupper(hex), cols$hex)]
  out[is.na(out)] <- ""
  out
}

# Exported -------------------------------------------------------------------

#' Look up dictionary colours
#'
#' Get the hex codes of dictionary colours by name or number. Names are
#' matched ignoring case, spaces and punctuation, and accept "grey" and
#' "colour" spellings. Misspelt names produce suggestions.
#'
#' @param x Colour names (e.g. `"Hermosa Pink"`) or colour numbers.
#' @param volume Which volume of the dictionary to look in.
#' @return A named character vector of hex codes.
#' @export
#' @examples
#' haishoku_colour(c("hermosa pink", "Cerulian Blue", "58"))
#' haishoku_colour(1:5)
haishoku_colour <- function(x, volume = 1) {
  if (is.character(x) && all(grepl("^[0-9]+$", x))) x <- as.integer(x)
  cols <- colours_in(volume)
  idx <- match_colours(x, volume)
  stats::setNames(cols$hex[idx], cols$name[idx])
}

#' @rdname haishoku_colour
#' @export
haishoku_color <- haishoku_colour

#' Search the dictionary's colours
#'
#' @param pattern Regular expression matched against colour names, ignoring
#'   case. `NULL` returns every colour.
#' @param section Optionally restrict to sections of the book (1-6).
#' @param volume Which volume(s) to search.
#' @return Rows of [wada_colours].
#' @export
#' @examples
#' haishoku_search("pink")
#' haishoku_search(section = 6)
haishoku_search <- function(pattern = NULL, section = NULL, volume = 1) {
  cols <- colours_in(volume)
  if (!is.null(pattern)) cols <- cols[grepl(pattern, cols$name, ignore.case = TRUE), ]
  if (!is.null(section)) cols <- cols[cols$section %in% section, ]
  rownames(cols) <- NULL
  cols
}

#' CMYK printing recipes
#'
#' The book gives each colour as a CMYK ink recipe. Use these when the work
#' will be printed: they are the source values, whereas the hex codes are a
#' conversion for screens.
#'
#' @param x Colour names or numbers, a combination from [haishoku_palette()], or hex
#'   codes of dictionary colours.
#' @param volume Which volume of the dictionary to look in.
#' @return A data frame with the colour name, hex code, the four ink
#'   percentages, a `recipe` string such as `"C0 M30 Y6 K0"`, and
#'   `print_delta_e`: how far the hex falls short of the printed colour (see
#'   [wada_colours]).
#' @export
#' @examples
#' haishoku_cmyk(haishoku_palette(42))
#' haishoku_cmyk("Burnt Sienna")
haishoku_cmyk <- function(x, volume = 1) {
  cols <- colours_in(volume)
  if (inherits(x, "haishoku_palette") || all(is_hex(x))) {
    idx <- match(as_hex(x, volume), cols$hex)
    if (anyNA(idx)) {
      stop("Only dictionary colours have CMYK recipes; ",
           "interpolated or extended colours do not.", call. = FALSE)
    }
  } else {
    idx <- match_colours(x, volume)
  }
  out <- cols[idx, c("id", "name", "hex", "cyan", "magenta", "yellow", "black")]
  out$recipe <- sprintf("C%d M%d Y%d K%d", out$cyan, out$magenta, out$yellow, out$black)
  out$print_delta_e <- cols$print_delta_e[idx]
  rownames(out) <- NULL
  out
}

#' Nearest dictionary colours
#'
#' Find the dictionary colours closest to any colour, measured by CIEDE2000
#' colour difference. Useful for translating a brand or existing palette into
#' Wada's colours.
#'
#' @param x Colours: hex codes or R colour names.
#' @param n Number of matches to return per colour.
#' @param volume Which volume(s) to match against.
#' @return A data frame with the input colour, rank, matched dictionary colour
#'   and `delta_e` (CIEDE2000; below about 2 is barely noticeable, above about
#'   10 is clearly a different colour).
#' @export
#' @examples
#' haishoku_nearest(c("#1F77B4", "firebrick"), n = 2)
haishoku_nearest <- function(x, n = 1, volume = 1) {
  cols <- colours_in(volume)
  hex <- vapply(x, function(col) {
    if (is_hex(col)) toupper(substr(col, 1, 7)) else as_hex(col, volume)
  }, "")
  de <- farver::compare_colour(farver::decode_colour(hex), farver::decode_colour(cols$hex),
                               from_space = "rgb", method = "cie2000")
  de <- matrix(de, nrow = length(hex))
  n <- min(n, nrow(cols))
  out <- do.call(rbind, lapply(seq_along(hex), function(i) {
    best <- order(de[i, ])[seq_len(n)]
    data.frame(input = unname(x[i]), rank = seq_len(n), id = cols$id[best],
               name = cols$name[best], hex = cols$hex[best],
               delta_e = round(de[i, best], 2), stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}

#' Combinations and the colour dictionary
#'
#' `haishoku_combo()` returns the colours of one or more combinations with their
#' CMYK recipes. `haishoku_combos_with()` is the book's dictionary: the
#' combinations a colour (or every one of several colours) appears in.
#'
#' @param combo Combination numbers.
#' @param colours Colour names or numbers. Combinations must contain all of
#'   them.
#' @param size Optionally restrict to combinations of this many colours.
#' @param volume Which volume of the dictionary to use.
#' @return `haishoku_combo()`: one row per colour. `haishoku_combos_with()`: one row per
#'   combination with its size, colour names and hex codes.
#' @export
#' @examples
#' haishoku_combo(42)
#' haishoku_combos_with("Hermosa Pink")
#' haishoku_combos_with(c("Black", "Coral Red"), size = 3)
haishoku_combo <- function(combo, volume = 1) {
  combos <- combos_in(volume)
  missing <- setdiff(combo, combos$combo)
  if (length(missing)) {
    stop("No combination ", paste(missing, collapse = ", "), " in volume ", volume,
         " (combinations run 1-", max(combos$combo), ").", call. = FALSE)
  }
  out <- combos[combos$combo %in% combo, c("combo", "size", "position", "colour_id", "name", "hex")]
  out <- out[order(match(out$combo, combo), out$position), ]
  cmyk <- haishoku_cmyk(out$colour_id, volume)
  out <- cbind(out, cmyk[c("cyan", "magenta", "yellow", "black")])
  rownames(out) <- NULL
  out
}

#' @rdname haishoku_combo
#' @export
haishoku_combos_with <- function(colours, size = NULL, volume = 1) {
  ids <- colours_in(volume)$id[match_colours(colours, volume)]
  combos <- combos_in(volume)
  has_all <- tapply(combos$colour_id, combos$combo, function(x) all(ids %in% x))
  keep <- as.integer(names(has_all)[has_all])
  if (!is.null(size)) keep <- intersect(keep, unique(combos$combo[combos$size %in% size]))
  rows <- combos[combos$combo %in% keep, ]
  if (nrow(rows) == 0) {
    return(data.frame(combo = integer(), size = integer(), colours = character(),
                      hex = character()))
  }
  out <- do.call(rbind, lapply(split(rows, rows$combo), function(d) {
    d <- d[order(d$position), ]
    data.frame(combo = d$combo[1], size = d$size[1],
               colours = paste(d$name, collapse = " / "),
               hex = paste(d$hex, collapse = " "), stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}
