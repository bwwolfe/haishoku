#' haishoku: colour palettes from Sanzo Wada's 'A Dictionary of Color Combinations'
#'
#' Sanzo Wada published *Haishoku Soukan*, six volumes of colour
#' combinations, in 1933-34. Seigensha's 2010 re-edition, *A Dictionary of
#' Color Combinations* (Japanese title *Haishoku Jiten*), presents 348
#' combinations of two, three and four colours drawn from 159 named colours,
#' each with a CMYK printing recipe. *Haishoku* means "colour arrangement".
#'
#' Where to start:
#' * [haishoku_palette()] and [haishoku_show()] to get and look at a combination.
#' * [scale_fill_haishoku()] and friends for 'ggplot2'.
#' * [haishoku_combos_with()] for the book's dictionary: which combinations a colour
#'   appears in. [haishoku_show_colour()] draws them.
#' * [haishoku_find()] to rank combinations by distinctness and colour vision
#'   deficiency (CVD) safety; [haishoku_extend()] when you need more colours.
#' * [haishoku_cmyk()] and [haishoku_export()] for print and other tools.
#'
#' Every `haishoku_` function also has a short `hs_` alias, e.g.
#' `hs_palette()` for [haishoku_palette()].
#'
#' Set `options(haishoku.combo = <number>)` to change the default combination
#' used by the scales and [theme_haishoku()].
#'
#' @keywords internal
#' @importFrom ggplot2 .data
"_PACKAGE"

# Burnt Sienna / Orange Yellow / Peacock Blue / Violet Blue: distinct under
# deutan and protan simulation (min CIEDE2000 > 20) and all readable on white.
default_combo_id <- 286L

default_combo <- function() getOption("haishoku.combo", default_combo_id)
