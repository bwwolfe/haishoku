#' Palettes from the dictionary's combinations
#'
#' `haishoku_palette()` returns the colours of a combination as a palette for base
#' graphics, 'ggplot2' (via `scale_*_manual()` or the [scale_fill_haishoku()]
#' family) or anything else that takes hex codes. `haishoku_pal()` returns the
#' palette as a function of `n`, the form `scales` and 'ggplot2' (e.g.
#' `theme(palette.colour.discrete = haishoku_pal(42))` in 'ggplot2' 4.0) expect.
#'
#' @param combo A combination number, or a character vector of colour names,
#'   numbers or hex codes to build a custom palette.
#' @param n Number of colours. For discrete palettes, at most the size of the
#'   combination unless `extend = TRUE`. For sequential and diverging
#'   palettes, colours are interpolated in CIELAB; `NULL` returns the anchors.
#' @param type `"discrete"` keeps the combination's colours; `"sequential"`
#'   orders them by lightness (light to dark) to make a gradient;
#'   `"diverging"` arranges them as end, middle, end (see Details).
#' @param anchors For sequential palettes, `"all"` interpolates through every
#'   colour, light to dark; `"ends"` uses only the lightest and darkest, which
#'   avoids flat or muddy steps through the middle colours. See
#'   [haishoku_metrics()] (`seq_quality`, `seq_ends_quality`) for which suits a
#'   combination.
#' @param middle For diverging palettes: `"light"` for a light middle between
#'   two darker ends, `"dark"` for a dark middle between two lighter ends
#'   (suits dark backgrounds, or a neutral point that should stand out), or
#'   `"auto"` for whichever arrangement scores best.
#' @param order Order of colours: `"book"` (combination order), `"lightness"`
#'   (light to dark), `"hue"` or `"chroma"` (most saturated first). Defaults to
#'   `"book"` for discrete and `"lightness"` for sequential palettes.
#' @param reverse Reverse the palette.
#' @param extend If `TRUE` and `n` is larger than the combination, add
#'   dictionary colours with [haishoku_extend()].
#' @param mid For diverging palettes, a colour to force as the midpoint. The
#'   ends are then the most distinct pair of the remaining colours.
#' @param volume Which volume of the dictionary to use.
#' @details
#' A diverging palette needs two ends of clearly different hue but similar
#' lightness, set well apart in lightness from the middle, and still distinct
#' to readers with red-green colour vision deficiency. `haishoku_palette()`
#' tries every arrangement of the combination's colours and uses the best one
#' that meets all of those, falling back to Wada's White or Black as the middle
#' for pairs or when none of the combination's own colours works. If nothing
#' works it uses the most distinct pair around the lightest colour (or White)
#' and records that in the palette's `"quality"` attribute; the 'ggplot2'
#' scales warn in that case. `haishoku_find(diverging = "light")` (or
#' `"dark"`) lists combinations that work.
#' @return An `haishoku_palette`: a character vector of hex codes that prints as
#'   named swatches. Colour names are kept in the `"colour_names"` attribute
#'   rather than as `names()`, so the palette can go straight into
#'   `scale_colour_manual(values = )`. Sequential and diverging palettes also
#'   carry a `"quality"` attribute (e.g. `"good"`, `"muddy"`, `"flat"`, or the
#'   diverging middle used). `haishoku_pal()` returns a function.
#' @export
#' @examples
#' haishoku_palette(42)
#' haishoku_palette(232, type = "sequential", n = 7)
#' haishoku_palette(252, type = "sequential", anchors = "ends", n = 7)
#' haishoku_palette(226, type = "diverging", n = 7)
#' haishoku_palette(255, type = "diverging", middle = "dark")
#' haishoku_palette(c("Coral Red", "Black", "White"))
#' plot(haishoku_palette(250))
#'
#' barplot(1:4, col = haishoku_palette(302))
haishoku_palette <- function(combo = default_combo(), n = NULL,
                       type = c("discrete", "sequential", "diverging"),
                       order = NULL, reverse = FALSE, extend = FALSE,
                       anchors = c("all", "ends"), middle = c("auto", "light", "dark"),
                       mid = NULL, volume = 1) {
  type <- match.arg(type)
  anchors <- match.arg(anchors)
  middle <- match.arg(middle)
  if (inherits(combo, "haishoku_palette")) {
    hex <- as_hex(combo)
    label <- attr(combo, "combo")
  } else if (is.numeric(combo) && length(combo) == 1) {
    hex <- haishoku_combo(combo, volume)$hex
    label <- as.character(combo)
  } else {
    if (is.character(combo) && all(grepl("^[0-9]+$", combo))) combo <- as.integer(combo)
    hex <- as_hex(combo, volume)
    label <- "custom"
  }

  order <- match.arg(order %||% if (type == "discrete") "book" else "lightness",
                     c("book", "lightness", "hue", "chroma"))
  book <- hex
  hex <- hex[order_colours(hex, order)]

  quality <- NULL
  if (type == "sequential") {
    hex <- sequential_anchors(hex, anchors)
    if (order == "lightness") quality <- gradient_check(hex)$verdict
  }
  if (type == "diverging") {
    hex <- diverging_anchors(book, mid, middle, volume)
    quality <- attr(hex, "middle")
  }
  hex <- as.character(hex)
  if (reverse) hex <- rev(hex)

  if (!is.null(n)) {
    if (type == "discrete") {
      if (n > length(hex)) {
        if (!extend) {
          stop("Combination ", label, " has ", length(hex), " colours but ", n,
               " were requested. Use `extend = TRUE` to add dictionary colours, ",
               "or `haishoku_find(size = 4)` for a larger combination.", call. = FALSE)
        }
        hex <- as.character(haishoku_extend(hex, n, volume = volume))
      }
      hex <- hex[seq_len(n)]
    } else {
      hex <- scales::pal_gradient_n(hex)(seq(0, 1, length.out = n))
    }
  }
  pal <- new_haishoku_palette(toupper(hex), combo = label, type = type, volume = volume)
  attr(pal, "quality") <- quality
  pal
}

#' @rdname haishoku_palette
#' @param ... Passed on to `haishoku_palette()`.
#' @export
haishoku_pal <- function(combo = default_combo(), ...) {
  force(combo)
  args <- list(...)
  function(n) as.character(do.call(haishoku_palette, c(list(combo, n = n), args)))
}

`%||%` <- function(x, y) if (is.null(x)) y else x

new_haishoku_palette <- function(hex, combo, type = "discrete", volume = 1) {
  structure(unname(hex), class = c("haishoku_palette", "character"), combo = combo, type = type,
            colour_names = dictionary_names(hex, volume))
}

# Colour names live in an attribute rather than names() so palettes can go
# straight into scale_*_manual(values = ) without being matched to levels.
colour_names <- function(x) attr(x, "colour_names") %||% rep("", length(x))

#' @export
`[.haishoku_palette` <- function(x, i) {
  structure(as.character(x)[i], class = class(x), combo = attr(x, "combo"),
            type = attr(x, "type"), colour_names = colour_names(x)[i])
}

order_colours <- function(hex, order) {
  hcl <- farver::convert_colour(farver::decode_colour(hex), "rgb", "hcl")
  switch(order,
    book = seq_along(hex),
    lightness = base::order(-hcl[, "l"]),
    hue = base::order(ifelse(is.na(hcl[, "h"]), -1, hcl[, "h"])),
    chroma = base::order(-hcl[, "c"])
  )
}

lightness <- function(hex) {
  farver::convert_colour(farver::decode_colour(hex), "rgb", "lab")[, "l"]
}

delta_e_matrix <- function(hex) {
  rgb <- farver::decode_colour(hex)
  farver::compare_colour(rgb, rgb, from_space = "rgb", method = "cie2000")
}

# End, middle, end. Uses the best arrangement that passes diverging_options()
# unless `mid` is forced; otherwise falls back to the most distinct pair around
# the lightest (or darkest) colour, or White (Black), and marks it "none".
diverging_anchors <- function(hex, mid = NULL, middle = "auto", volume = 1) {
  if (is.null(mid)) {
    pick <- diverging_pick(hex, middle)
    if (!is.null(pick)) return(pick)
    l <- lightness(hex)
    if (middle == "dark") {
      if (length(hex) >= 3 && min(l) <= 35) {
        mid <- hex[which.min(l)]
      } else {
        mid <- "#111314"
      }
    } else if (length(hex) >= 3 && max(l) >= 75) {
      mid <- hex[which.max(l)]
    } else {
      mid <- "#FFFFFF"
    }
    forced <- FALSE
  } else {
    mid <- as_hex(mid, volume)
    forced <- TRUE
  }
  hex <- setdiff(hex, mid)
  if (length(hex) < 2) stop("A diverging palette needs two colours besides the midpoint.", call. = FALSE)
  if (length(hex) > 2) {
    de <- delta_e_matrix(hex)
    ends <- which(de == max(de), arr.ind = TRUE)[1, ]
    hex <- hex[sort(ends)]
  }
  structure(unname(c(hex[1], mid, hex[2])), middle = if (forced) "forced" else "none")
}

#' Extend a palette with further dictionary colours
#'
#' Wada's combinations have at most four colours. `haishoku_extend()` adds
#' dictionary colours one at a time, each time choosing the colour that is
#' most distinct (largest minimum CIEDE2000 difference) from those already
#' chosen. With `cvd = TRUE` distinctness is the worst case over normal,
#' deuteranope and protanope vision. With `prefer = "wada"`, candidates are
#' first limited to colours Wada combined with the palette's colours anywhere
#' in the book, so additions stay in the spirit of the original.
#'
#' @param x A palette: an [haishoku_palette()], combination number, or colours.
#' @param n Total number of colours wanted.
#' @param cvd Account for colour vision deficiency when judging distinctness.
#' @param prefer `"wada"` to favour colours the book pairs with the palette,
#'   or `"distinct"` to consider every colour.
#' @param lightness Allowed range of CIELAB lightness for added colours. The
#'   default avoids near-white and near-black colours that vanish against
#'   backgrounds and text.
#' @param background Optional background colour. Only colours with a WCAG
#'   contrast of at least 3 against it (enough for lines, points and other
#'   graphics) are added.
#' @param volume Which volume of the dictionary to draw from.
#' @return An `haishoku_palette` of `n` colours, starting with `x`.
#' @export
#' @examples
#' haishoku_extend(42, 6)
#' haishoku_extend(haishoku_palette(302), 8, prefer = "distinct")
#' haishoku_extend(c("Deep Slate Olive", "Peach Red"), 5, background = "Naples Yellow")
haishoku_extend <- function(x, n, cvd = TRUE, prefer = c("wada", "distinct"),
                      lightness = c(25, 85), background = NULL, volume = 1) {
  prefer <- match.arg(prefer)
  hex <- if (is.numeric(x) && length(x) == 1) haishoku_combo(x, volume)$hex else as_hex(x, volume)
  if (n <= length(hex)) return(new_haishoku_palette(hex[seq_len(n)], combo = "extended", volume = volume))

  cols <- colours_in(volume)
  pool <- cols[!cols$hex %in% hex & cols$lab_l >= lightness[1] & cols$lab_l <= lightness[2], ]
  if (!is.null(background)) pool <- pool[haishoku_contrast(pool$hex, background, volume) >= 3, ]
  conditions <- if (cvd) c("none", "deutan", "protan") else "none"
  sim <- lapply(conditions, function(cond) farver::decode_colour(simulate_cvd(pool$hex, cond)))
  chosen <- lapply(conditions, function(cond) farver::decode_colour(simulate_cvd(hex, cond)))

  partners <- function(current) {
    ids <- cols$id[match(current, cols$hex)]
    combos <- combos_in(volume)
    in_combo <- combos$combo[combos$colour_id %in% ids]
    unique(combos$colour_id[combos$combo %in% in_combo])
  }

  while (length(hex) < n && nrow(pool) > 0) {
    candidates <- seq_len(nrow(pool))
    if (prefer == "wada") {
      wada <- which(pool$id %in% partners(hex))
      if (length(wada)) candidates <- wada
    }
    score <- Reduce(pmin, lapply(seq_along(conditions), function(k) {
      de <- farver::compare_colour(sim[[k]][candidates, , drop = FALSE], chosen[[k]],
                                   from_space = "rgb", method = "cie2000")
      apply(matrix(de, nrow = length(candidates)), 1, min)
    }))
    best <- candidates[which.max(score)]
    hex <- c(hex, pool$hex[best])
    chosen <- lapply(seq_along(conditions), function(k) rbind(chosen[[k]], sim[[k]][best, , drop = FALSE]))
    sim <- lapply(sim, function(m) m[-best, , drop = FALSE])
    pool <- pool[-best, ]
  }
  if (length(hex) < n) warning("Ran out of dictionary colours at ", length(hex), ".", call. = FALSE)
  new_haishoku_palette(hex, combo = "extended", volume = volume)
}

#' @export
print.haishoku_palette <- function(x, ...) {
  combo <- attr(x, "combo")
  title <- if (identical(combo, "custom") || identical(combo, "extended")) {
    paste0("<haishoku_palette: ", combo, ">")
  } else {
    paste0("<haishoku_palette: combination ", combo, ">")
  }
  cat(title, "\n", sep = "")
  hex <- as.character(x)
  nms <- colour_names(x)
  ansi <- requireNamespace("cli", quietly = TRUE) && cli::num_ansi_colors() >= 256
  for (i in seq_along(hex)) {
    swatch <- if (ansi) cli::make_ansi_style(hex[i], bg = TRUE)("      ") else ""
    cat(swatch, " ", hex[i], "  ", nms[i], "\n", sep = "")
  }
  invisible(x)
}

#' @export
plot.haishoku_palette <- function(x, ...) {
  hex <- as.character(x)
  n <- length(hex)
  old <- graphics::par(mar = c(0.5, 0.5, 2, 0.5))
  on.exit(graphics::par(old))
  graphics::plot(NULL, xlim = c(0, n), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "",
                 main = paste("Combination", attr(x, "combo")), ...)
  graphics::rect(seq_len(n) - 1, 0, seq_len(n), 1, col = hex, border = NA)
  labels <- ifelse(colour_names(x) == "", hex, paste(colour_names(x), hex, sep = "\n"))
  graphics::text(seq_len(n) - 0.5, 0.5, labels, col = ink_for(hex), cex = 0.8)
  invisible(x)
}
