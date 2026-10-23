#' 'ggplot2' scales from the dictionary's combinations
#'
#' Colour and fill scales built from a combination. The `_d` scales are
#' discrete (one colour per level), `_c` continuous and `_b` binned.
#' `scale_*_haishoku()` is discrete by default; set `discrete = FALSE` for a
#' continuous scale.
#'
#' Discrete scales use the combination's colours in order, so a combination
#' of four colours serves up to four levels. For more, set `extend = TRUE` to
#' add dictionary colours with [haishoku_extend()], or look for a better fitting
#' combination with [haishoku_find()].
#'
#' Continuous and binned scales interpolate in CIELAB. Sequential scales run
#' light to dark through the combination's colours (`anchors = "all"`) or
#' between just its lightest and darkest (`anchors = "ends"`). Diverging scales
#' use the best end-middle-end arrangement of the combination (see
#' [haishoku_palette()]), with a light or dark middle.
#'
#' Wada's combinations were chosen for harmony, not as gradients, and many
#' make poor ones. The scales warn when a sequential scale would be flat
#' (neighbouring colours too close in lightness) or muddy (greying between
#' opposing hues), or when no arrangement makes a colourblind-safe diverging
#' scale. Use `haishoku_find(sequential = "good")` or
#' `haishoku_find(diverging = "light")` to find combinations that work, or wrap
#' the scale in [suppressWarnings()] if the look is intended.
#'
#' @param combo A combination number, or colours, as for [haishoku_palette()].
#'   Defaults to `getOption("haishoku.combo")`, else combination 286.
#' @param ... Passed to [ggplot2::discrete_scale()],
#'   [ggplot2::continuous_scale()] or [ggplot2::binned_scale()], e.g. `name`,
#'   `labels`, `limits` or `na.value`.
#' @param discrete Use a discrete scale (`TRUE`) or a continuous one.
#' @param type For continuous and binned scales, `"sequential"` or
#'   `"diverging"`.
#' @param order,reverse,extend,anchors,middle,mid,volume See [haishoku_palette()].
#' @param midpoint For diverging scales, the data value at the midpoint
#'   colour.
#' @param aesthetics The aesthetics the scale applies to.
#' @param guide Legend type.
#' @return A 'ggplot2' scale.
#' @name scale_haishoku
#' @examples
#' library(ggplot2)
#' ggplot(mpg, aes(displ, hwy, colour = drv)) +
#'   geom_point() +
#'   scale_colour_haishoku(302)
#'
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_raster() +
#'   scale_fill_haishoku_c(232)
#'
#' # Lightest to darkest only, for a combination whose middle colours clash
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_raster() +
#'   scale_fill_haishoku_c(252, anchors = "ends")
#'
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density - 0.01)) +
#'   geom_raster() +
#'   scale_fill_haishoku_c(226, type = "diverging")
#'
#' ggplot(mpg, aes(class, fill = class)) +
#'   geom_bar() +
#'   scale_fill_haishoku_d(42, extend = TRUE)
NULL

haishoku_d <- function(aesthetics, combo, order, reverse, extend, volume, ...) {
  force(combo)
  pal <- function(n) {
    as.character(haishoku_palette(combo, n = n, type = "discrete", order = order,
                              reverse = reverse, extend = extend, volume = volume))
  }
  ggplot2::discrete_scale(aesthetics, palette = pal, ...)
}

# Anchors for a continuous or binned scale, warning if they make a poor one.
gradient_anchors <- function(combo, type, order, reverse, anchors, middle, mid, volume) {
  type <- match.arg(type, c("sequential", "diverging"))
  pal <- haishoku_palette(combo, type = type, order = order, reverse = reverse,
                          anchors = anchors, middle = middle, mid = mid, volume = volume)
  warn_gradient(pal, combo, type, order, anchors, volume)
  pal
}

warn_gradient <- function(pal, combo, type, order, anchors, volume) {
  quality <- attr(pal, "quality")
  if (is.null(quality)) return(invisible())
  label <- attr(pal, "combo")
  what <- if (label %in% c("custom", "extended")) "This palette" else paste("Combination", label)
  if (type == "sequential" && quality %in% c("flat", "muddy")) {
    why <- if (quality == "flat") {
      "neighbouring colours are too close in lightness, so it changes hue without getting darker"
    } else {
      "it greys out between opposing hues"
    }
    ends <- if (anchors == "all") {
      attr(haishoku_palette(combo, type = "sequential", order = order, anchors = "ends",
                            volume = volume), "quality")
    }
    hint <- if (!is.null(ends) && gradient_ok(ends)) {
      " `anchors = \"ends\"` (lightest to darkest only) makes a cleaner one."
    } else {
      " See `haishoku_find(sequential = \"good\")` for combinations that work."
    }
    warning(what, " makes a ", quality, " sequential scale: ", why, ".", hint, call. = FALSE)
  }
  if (type == "diverging" && quality == "none") {
    warning(what, " has no colourblind-safe diverging arrangement (two distinct ends of ",
            "similar lightness around a clearly lighter or darker middle); using ",
            paste(colour_names(pal), collapse = " / "), ". See ",
            "`haishoku_find(diverging = \"light\")` or `\"dark\"` for combinations that work.",
            call. = FALSE)
  }
  invisible()
}

haishoku_c <- function(aesthetics, combo, type, order, reverse, anchors, middle, mid, midpoint,
                       volume, guide, ...) {
  pal <- gradient_anchors(combo, type, order, reverse, anchors, middle, mid, volume)
  rescaler <- if (identical(attr(pal, "type"), "diverging")) {
    function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
      scales::rescale_mid(x, to, from, midpoint)
    }
  } else {
    scales::rescale
  }
  ggplot2::continuous_scale(aesthetics, palette = scales::pal_gradient_n(as.character(pal)),
                            rescaler = rescaler, guide = guide, ...)
}

haishoku_b <- function(aesthetics, combo, type, order, reverse, anchors, middle, mid, volume,
                       guide, ...) {
  pal <- scales::pal_gradient_n(as.character(
    gradient_anchors(combo, type, order, reverse, anchors, middle, mid, volume)
  ))
  ggplot2::binned_scale(aesthetics, palette = function(x) pal(seq(0, 1, length.out = length(x))),
                        guide = guide, ...)
}

#' @rdname scale_haishoku
#' @export
scale_colour_haishoku <- function(combo = default_combo(), ..., discrete = TRUE,
                                  aesthetics = "colour") {
  if (discrete) {
    scale_colour_haishoku_d(combo, ..., aesthetics = aesthetics)
  } else {
    scale_colour_haishoku_c(combo, ..., aesthetics = aesthetics)
  }
}

#' @rdname scale_haishoku
#' @export
scale_fill_haishoku <- function(combo = default_combo(), ..., discrete = TRUE,
                                aesthetics = "fill") {
  if (discrete) {
    scale_fill_haishoku_d(combo, ..., aesthetics = aesthetics)
  } else {
    scale_fill_haishoku_c(combo, ..., aesthetics = aesthetics)
  }
}

#' @rdname scale_haishoku
#' @export
scale_colour_haishoku_d <- function(combo = default_combo(), ..., order = "book",
                                    reverse = FALSE, extend = FALSE, volume = 1,
                                    aesthetics = "colour") {
  haishoku_d(aesthetics, combo, order, reverse, extend, volume, ...)
}

#' @rdname scale_haishoku
#' @export
scale_fill_haishoku_d <- function(combo = default_combo(), ..., order = "book",
                                  reverse = FALSE, extend = FALSE, volume = 1,
                                  aesthetics = "fill") {
  haishoku_d(aesthetics, combo, order, reverse, extend, volume, ...)
}

#' @rdname scale_haishoku
#' @export
scale_colour_haishoku_c <- function(combo = default_combo(), ..., type = "sequential",
                                    order = NULL, reverse = FALSE, anchors = "all",
                                    middle = "auto", mid = NULL, midpoint = 0, volume = 1,
                                    aesthetics = "colour", guide = "colourbar") {
  haishoku_c(aesthetics, combo, type, order, reverse, anchors, middle, mid, midpoint,
             volume, guide, ...)
}

#' @rdname scale_haishoku
#' @export
scale_fill_haishoku_c <- function(combo = default_combo(), ..., type = "sequential",
                                  order = NULL, reverse = FALSE, anchors = "all",
                                  middle = "auto", mid = NULL, midpoint = 0, volume = 1,
                                  aesthetics = "fill", guide = "colourbar") {
  haishoku_c(aesthetics, combo, type, order, reverse, anchors, middle, mid, midpoint,
             volume, guide, ...)
}

#' @rdname scale_haishoku
#' @export
scale_colour_haishoku_b <- function(combo = default_combo(), ..., type = "sequential",
                                    order = NULL, reverse = FALSE, anchors = "all",
                                    middle = "auto", mid = NULL, volume = 1,
                                    aesthetics = "colour", guide = "coloursteps") {
  haishoku_b(aesthetics, combo, type, order, reverse, anchors, middle, mid, volume, guide, ...)
}

#' @rdname scale_haishoku
#' @export
scale_fill_haishoku_b <- function(combo = default_combo(), ..., type = "sequential",
                                  order = NULL, reverse = FALSE, anchors = "all",
                                  middle = "auto", mid = NULL, volume = 1,
                                  aesthetics = "fill", guide = "coloursteps") {
  haishoku_b(aesthetics, combo, type, order, reverse, anchors, middle, mid, volume, guide, ...)
}

#' @rdname scale_haishoku
#' @export
scale_color_haishoku <- scale_colour_haishoku

#' @rdname scale_haishoku
#' @export
scale_color_haishoku_d <- scale_colour_haishoku_d

#' @rdname scale_haishoku
#' @export
scale_color_haishoku_c <- scale_colour_haishoku_c

#' @rdname scale_haishoku
#' @export
scale_color_haishoku_b <- scale_colour_haishoku_b
