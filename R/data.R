#' The colours of 'A Dictionary of Color Combinations'
#'
#' One row per colour, in the book's order. The CMYK columns are the book's
#' printing recipes; the sRGB hex values were converted from them with the
#' SWOP v2 and sRGB ICC profiles, so they approximate the printed swatches and
#' will not match them exactly.
#'
#' Most colours survive the conversion: `print_delta_e` is below 1
#' (imperceptible) for 148 of them. The exceptions are mostly full-cyan teals
#' and blues (Peacock Blue, Benzol Green, Dusky Green, Antwarp Blue) and pure
#' Yellow, which printing ink can reach but standard sRGB screens cannot; on
#' screen they show as the same hue, but duller.
#'
#' @format A data frame with one row per colour and 21 columns:
#' \describe{
#'   \item{volume}{Volume of the dictionary (1; later volumes can be added, see
#'     `data-raw/` in the source package).}
#'   \item{id}{Colour number within the volume, in book order.}
#'   \item{name}{English colour name as printed.}
#'   \item{name_ja}{Japanese colour name, where transcribed (`NA` otherwise).}
#'   \item{section}{Section of the book the colour is printed in (1-6).}
#'   \item{hex}{sRGB hex code.}
#'   \item{cyan, magenta, yellow, black}{CMYK recipe, percent ink.}
#'   \item{red, green, blue}{sRGB channels, 0-255.}
#'   \item{lab_l, lab_a, lab_b}{CIELAB coordinates (D65).}
#'   \item{hue, chroma}{HCL hue (degrees) and chroma.}
#'   \item{print_delta_e}{CIEDE2000 difference between the colour as printed
#'     (the CMYK recipe through the SWOP profile) and its sRGB hex. Below 1 is
#'     imperceptible, 2-5 noticeable side by side, above 5 clearly duller on
#'     screen than in print.}
#'   \item{n_combos}{Number of combinations the colour appears in.}
#'   \item{verified}{`TRUE` once the row has been checked against a printed
#'     copy.}
#' }
#' @source Sanzo Wada (1933-34) *Haishoku Soukan*; Seigensha (2010) *A
#'   Dictionary of Color Combinations*. Transcription from
#'   <https://github.com/mattdesl/dictionary-of-colour-combinations> (MIT).
#' @seealso [wada_combos], [haishoku_colour()], [haishoku_search()]
"wada_colours"

#' The combinations of 'A Dictionary of Color Combinations'
#'
#' One row per colour per combination. In volume 1, combinations 1-120 are
#' pairs, 121-240 are trios and 241-348 are quads.
#'
#' @format A data frame with seven columns:
#' \describe{
#'   \item{volume}{Volume of the dictionary.}
#'   \item{combo}{Combination number.}
#'   \item{size}{Number of colours in the combination.}
#'   \item{position}{Position of the colour within the combination. This
#'     follows colour number unless corrected to the printed arrangement.}
#'   \item{colour_id}{Colour number, matching `wada_colours$id`.}
#'   \item{name, hex}{Colour name and sRGB hex code.}
#' }
#' @source As for [wada_colours].
#' @seealso [haishoku_combo()], [haishoku_combos_with()]
"wada_combos"
