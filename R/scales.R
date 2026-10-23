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
#' combination with [haishoku_find()]. Continuous and binned scales interpolate in
#' CIELAB between the colours ordered light to dark (`type = "sequential"`),
#' or between two distinct colours through a light midpoint
#' (`type = "diverging"`).
#'
#' @param combo A combination number, or colours, as for [haishoku_palette()].
#'   Defaults to `getOption("haishoku.combo")`, else combination 286.
#' @param ... Passed to [ggplot2::discrete_scale()],
#'   [ggplot2::continuous_scale()] or [ggplot2::binned_scale()], e.g. `name`,
#'   `labels`, `limits` or `na.value`.
#' @param discrete Use a discrete scale (`TRUE`) or a continuous one.
#' @param type For continuous and binned scales, `"sequential"` or
#'   `"diverging"`.
#' @param order,reverse,extend,mid,volume See [haishoku_palette()].
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

haishoku_c <- function(aesthetics, combo, type, order, reverse, mid, midpoint, volume,
                       guide, ...) {
  type <- match.arg(type, c("sequential", "diverging"))
  anchors <- haishoku_palette(combo, type = type, order = order, reverse = reverse,
                        mid = mid, volume = volume)
  rescaler <- if (type == "diverging") {
    function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
      scales::rescale_mid(x, to, from, midpoint)
    }
  } else {
    scales::rescale
  }
  ggplot2::continuous_scale(aesthetics, palette = scales::pal_gradient_n(as.character(anchors)),
                            rescaler = rescaler, guide = guide, ...)
}

haishoku_b <- function(aesthetics, combo, type, order, reverse, mid, volume, guide, ...) {
  type <- match.arg(type, c("sequential", "diverging"))
  pal <- function(n) {
    as.character(haishoku_palette(combo, n = n, type = type, order = order, reverse = reverse,
                              mid = mid, volume = volume))
  }
  ggplot2::binned_scale(aesthetics, palette = function(x) pal(length(x)), guide = guide, ...)
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
                                    order = NULL, reverse = FALSE, mid = NULL, midpoint = 0,
                                    volume = 1, aesthetics = "colour", guide = "colourbar") {
  haishoku_c(aesthetics, combo, type, order, reverse, mid, midpoint, volume, guide, ...)
}

#' @rdname scale_haishoku
#' @export
scale_fill_haishoku_c <- function(combo = default_combo(), ..., type = "sequential",
                                  order = NULL, reverse = FALSE, mid = NULL, midpoint = 0,
                                  volume = 1, aesthetics = "fill", guide = "colourbar") {
  haishoku_c(aesthetics, combo, type, order, reverse, mid, midpoint, volume, guide, ...)
}

#' @rdname scale_haishoku
#' @export
scale_colour_haishoku_b <- function(combo = default_combo(), ..., type = "sequential",
                                    order = NULL, reverse = FALSE, mid = NULL, volume = 1,
                                    aesthetics = "colour", guide = "coloursteps") {
  haishoku_b(aesthetics, combo, type, order, reverse, mid, volume, guide, ...)
}

#' @rdname scale_haishoku
#' @export
scale_fill_haishoku_b <- function(combo = default_combo(), ..., type = "sequential",
                                  order = NULL, reverse = FALSE, mid = NULL, volume = 1,
                                  aesthetics = "fill", guide = "coloursteps") {
  haishoku_b(aesthetics, combo, type, order, reverse, mid, volume, guide, ...)
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
