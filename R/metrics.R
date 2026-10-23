simulate_cvd <- function(hex, type = c("none", "deutan", "protan", "tritan")) {
  type <- match.arg(type)
  out <- switch(type,
    none = hex,
    deutan = colorspace::deutan(hex),
    protan = colorspace::protan(hex),
    tritan = colorspace::tritan(hex)
  )
  toupper(out)
}

# WCAG 2 relative luminance.
rel_luminance <- function(hex) {
  v <- farver::decode_colour(hex) / 255
  v <- ifelse(v <= 0.04045, v / 12.92, ((v + 0.055) / 1.055)^2.4)
  drop(v %*% c(0.2126, 0.7152, 0.0722))
}

# Black or white, whichever reads better on each colour.
ink_for <- function(hex) {
  ifelse(haishoku_contrast("#000000", hex) >= haishoku_contrast("#FFFFFF", hex), "#000000", "#FFFFFF")
}

min_pairwise <- function(m) if (nrow(m) < 2) NA_real_ else min(m[upper.tri(m)])

#' Contrast ratio between colours
#'
#' The WCAG 2 contrast ratio, from 1 (no contrast) to 21 (black on white).
#' Text needs at least 4.5 (3 for large text); non-text graphics such as lines
#' and points need at least 3 against their background.
#'
#' @param fg,bg Foreground and background colours: dictionary names or
#'   numbers, hex codes or R colour names. Recycled to a common length.
#' @param volume Which volume of the dictionary names refer to.
#' @return A numeric vector of contrast ratios.
#' @export
#' @examples
#' haishoku_contrast("Black", haishoku_palette(42))
haishoku_contrast <- function(fg, bg, volume = 1) {
  l1 <- rel_luminance(as_hex(fg, volume))
  l2 <- rel_luminance(as_hex(bg, volume))
  round((pmax(l1, l2) + 0.05) / (pmin(l1, l2) + 0.05), 2)
}

#' Distinctness, colour vision deficiency and contrast metrics
#'
#' For each combination (or a custom palette), the smallest CIEDE2000
#' difference between any two of its colours under normal vision and
#' simulated deuteranopia, protanopia and tritanopia, along with lightness
#' spread and the largest contrast ratio between two of its colours.
#'
#' As a rough guide, a minimum difference of 10 or more keeps categories
#' distinguishable in small marks such as points and lines; below 5 colours
#' are easily confused. Simulations are from [colorspace::simulate_cvd()] at
#' full severity.
#'
#' The gradient columns grade the combination as a continuous scale.
#' `seq_quality` interpolates light to dark through every colour;
#' `seq_ends_quality` through only the lightest and darkest (`anchors =
#' "ends"`). Each is `"good"` (every lightness step at least 15, no greying),
#' `"usable"`, `"muddy"` (chroma drops below half of a straight blend between
#' two colours, i.e. it greys out between opposing hues) or `"flat"` (two
#' neighbouring colours less than 8 apart in lightness, so the scale changes
#' hue without getting darker). `div_light` and `div_dark` say whether some
#' arrangement makes a diverging scale with a light or dark middle: two ends
#' of distinct hue (CIEDE2000 >= 20, also >= 15 under deutan and protan
#' simulation) within 12 of each other in lightness, and at least 25 lighter
#' or darker in the middle. Pairs may use Wada's White or Black as the middle.
#'
#' @param x Combination numbers (default: all), or a single palette given as
#'   an [haishoku_palette()] or a character vector of colours.
#' @param volume Which volume of the dictionary to use.
#' @return A data frame with one row per combination: `combo`, `size`,
#'   `colours`, `min_de`, `min_de_deutan`, `min_de_protan`, `min_de_tritan`,
#'   `min_de_cvd` (the worse of deutan and protan, the common forms),
#'   `l_range` (lightness spread), `l_step` (smallest lightness gap between
#'   neighbouring colours), `max_contrast`, `seq_quality`,
#'   `seq_ends_quality`, `div_light` and `div_dark`.
#' @export
#' @examples
#' haishoku_metrics(c(42, 232, 302))
#' haishoku_metrics(c("Black", "Coral Red", "White"))
haishoku_metrics <- function(x = NULL, volume = 1) {
  if (is.null(x) || (is.numeric(x) && !inherits(x, "haishoku_palette"))) {
    key <- paste(volume, collapse = "-")
    out <- metrics_cache[[key]]
    if (is.null(out)) {
      combos <- combos_in(volume)
      groups <- split(combos, combos$combo)
      out <- compute_metrics(
        palettes = lapply(groups, function(d) d$hex[order(d$position)]),
        names_ = vapply(groups, function(d) paste(d$name[order(d$position)], collapse = " / "), ""),
        ids = as.integer(names(groups))
      )
      assign(key, out, envir = metrics_cache)
    }
    if (!is.null(x)) out <- out[out$combo %in% x, ]
    rownames(out) <- NULL
    return(out)
  }
  pal <- haishoku_palette(x, volume = volume)
  compute_metrics(
    palettes = list(as.character(pal)),
    names_ = paste(ifelse(colour_names(pal) == "", as.character(pal), colour_names(pal)),
                   collapse = " / "),
    ids = NA_integer_
  )
}

# Metrics for every combination are computed once per session.
metrics_cache <- new.env(parent = emptyenv())

compute_metrics <- function(palettes, names_, ids) {
  all_hex <- unique(unlist(palettes))
  sims <- lapply(c(deutan = "deutan", protan = "protan", tritan = "tritan"),
                 function(type) stats::setNames(simulate_cvd(all_hex, type), all_hex))
  min_de <- function(hex) min_pairwise(delta_e_matrix(hex))

  out <- data.frame(
    combo = ids,
    size = lengths(palettes),
    colours = unname(names_),
    min_de = vapply(palettes, min_de, 1),
    min_de_deutan = vapply(palettes, function(p) min_de(sims$deutan[p]), 1),
    min_de_protan = vapply(palettes, function(p) min_de(sims$protan[p]), 1),
    min_de_tritan = vapply(palettes, function(p) min_de(sims$tritan[p]), 1),
    l_range = vapply(palettes, function(p) diff(range(lightness(p))), 1),
    l_step = vapply(palettes, function(p) min(-diff(sort(lightness(p), decreasing = TRUE))), 1),
    max_contrast = vapply(palettes, function(p) {
      lum <- rel_luminance(p)
      (max(lum) + 0.05) / (min(lum) + 0.05)
    }, 1),
    stringsAsFactors = FALSE
  )
  out$min_de_cvd <- pmin(out$min_de_deutan, out$min_de_protan)
  num <- vapply(out, is.double, TRUE)
  out[num] <- lapply(out[num], round, 1)
  out <- out[c("combo", "size", "colours", "min_de", "min_de_deutan", "min_de_protan",
               "min_de_tritan", "min_de_cvd", "l_range", "l_step", "max_contrast")]
  out <- cbind(out, do.call(rbind, lapply(palettes, gradient_summary)))
  rownames(out) <- NULL
  out
}

#' Find combinations that suit a task
#'
#' Filter and rank the dictionary's combinations, for example to find
#' four-colour combinations that stay distinguishable for readers with colour
#' vision deficiency.
#'
#' @param size Number of colours wanted (2, 3 or 4), or `NULL` for any.
#' @param contains Colours (names or numbers) that must be in the
#'   combination.
#' @param min_de Minimum CIEDE2000 difference between any two colours under
#'   normal vision.
#' @param cvd_safe If `TRUE`, also require `min_de` under simulated
#'   deuteranopia and protanopia.
#' @param min_contrast Minimum contrast ratio between the combination's most
#'   and least luminous colours (see [haishoku_contrast()]).
#' @param sequential Gradient grades to accept for a sequential scale, e.g.
#'   `"good"` or `c("good", "usable")`. See [haishoku_metrics()].
#' @param anchors Which sequential grade `sequential` filters on: `"all"`
#'   (`seq_quality`) or `"ends"` (`seq_ends_quality`).
#' @param diverging `"light"`, `"dark"` or `"any"` to keep combinations that
#'   make a diverging scale with that kind of middle.
#' @param sort_by Numeric metric to sort by, largest first: see
#'   [haishoku_metrics()].
#' @param volume Which volume of the dictionary to search.
#' @return Rows of [haishoku_metrics()] that pass the filters.
#' @export
#' @examples
#' haishoku_find(size = 4, min_de = 15, cvd_safe = TRUE)
#' haishoku_find(contains = "Coral Red", size = 3)
#' haishoku_find(size = 3, sequential = "good", sort_by = "l_step")
#' haishoku_find(sequential = "good", anchors = "ends", size = 4)
#' haishoku_find(diverging = "dark")
haishoku_find <- function(size = NULL, contains = NULL, min_de = 0, cvd_safe = FALSE,
                    min_contrast = 1, sequential = NULL, anchors = c("all", "ends"),
                    diverging = NULL, sort_by = "min_de_cvd", volume = 1) {
  anchors <- match.arg(anchors)
  if (!is.null(sequential)) sequential <- match.arg(sequential, c("good", "usable", "muddy", "flat"),
                                                    several.ok = TRUE)
  if (!is.null(diverging)) diverging <- match.arg(diverging, c("light", "dark", "any"))
  candidates <- NULL
  if (!is.null(contains)) {
    candidates <- haishoku_combos_with(contains, size, volume)$combo
    if (length(candidates) == 0) return(haishoku_metrics(volume = volume)[0, ])
  }
  m <- haishoku_metrics(candidates, volume)
  if (!is.null(size)) m <- m[m$size %in% size, ]
  keep <- m$min_de >= min_de & m$max_contrast >= min_contrast
  if (cvd_safe) keep <- keep & m$min_de_cvd >= min_de
  if (!is.null(sequential)) {
    grade <- if (anchors == "ends") m$seq_ends_quality else m$seq_quality
    keep <- keep & grade %in% sequential
  }
  if (!is.null(diverging)) {
    keep <- keep & switch(diverging, light = m$div_light, dark = m$div_dark,
                          any = m$div_light | m$div_dark)
  }
  m <- m[keep, ]
  sort_by <- match.arg(sort_by, names(m)[vapply(m, is.numeric, TRUE) & names(m) != "combo"])
  m <- m[order(-m[[sort_by]], m$combo), ]
  rownames(m) <- NULL
  m
}

#' Assign information-design roles to a palette
#'
#' Splits a palette into a background, an ink for text and lines (the colour
#' with the most contrast against the background) and accents, and says what
#' each accent can safely be used for against that background under WCAG 2:
#' body text (contrast >= 4.5), large text and graphics (>= 3), or
#' decoration and fills only.
#'
#' The background is the palette's lightest colour when it is pale enough to
#' work as paper (CIELAB lightness of at least `paper_lightness`); otherwise
#' it is Wada's White and every palette colour is ink or accent.
#'
#' @param x A palette: an [haishoku_palette()], combination number, or colours.
#' @param background Optional background colour to use instead (e.g.
#'   `"#FFFFFF"` for a white page, or a dark colour for a dark theme).
#' @param paper_lightness Minimum lightness for a palette colour to be used
#'   as the background.
#' @param volume Which volume of the dictionary to use.
#' @return A data frame with `hex`, `name`, `role`, `contrast` (against the
#'   background) and `use`.
#' @export
#' @examples
#' haishoku_roles(302)
#' haishoku_roles(286)
#' haishoku_roles(286, background = "Black")
haishoku_roles <- function(x = default_combo(), background = NULL, paper_lightness = 85,
                     volume = 1) {
  pal <- haishoku_palette(x, volume = volume)
  hex <- as.character(pal)
  if (is.null(background)) {
    lightest <- hex[which.max(lightness(hex))]
    bg <- if (max(lightness(hex)) >= paper_lightness) lightest else "#FFFFFF"
  } else {
    bg <- as_hex(background, volume)
  }
  rest <- setdiff(hex, bg)
  contrast <- haishoku_contrast(rest, bg)
  rest <- rest[order(-contrast)]
  contrast <- sort(contrast, decreasing = TRUE)
  use <- ifelse(contrast >= 4.5, "text and graphics",
                ifelse(contrast >= 3, "large text and graphics", "fills and decoration only"))
  out <- data.frame(
    hex = c(bg, rest),
    name = dictionary_names(c(bg, rest), volume),
    role = c("background", if (length(rest)) c("ink", rep("accent", length(rest) - 1))),
    contrast = c(1, contrast),
    use = c("background", use),
    stringsAsFactors = FALSE
  )
  out
}
