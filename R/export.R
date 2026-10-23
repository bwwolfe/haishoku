#' Export a palette for other tools
#'
#' Writes a palette in a format other software can read:
#' * `"css"`: custom properties (`--hs-coral-red: #E9546B;`).
#' * `"gpl"`: GIMP palette, also read by Inkscape, Krita and Scribus.
#' * `"tex"`: `xcolor` definitions using the book's CMYK recipes, for print
#'   documents in LaTeX.
#' * `"json"`: name, hex and CMYK for each colour.
#'
#' @param x A combination number, [haishoku_palette()] or colours.
#' @param format One of `"css"`, `"gpl"`, `"tex"` or `"json"`.
#' @param file Path to write to. If `NULL`, the text is returned.
#' @param prefix Prefix for CSS and LaTeX colour names.
#' @param volume Which volume of the dictionary to use.
#' @return The exported text, invisibly when written to `file`.
#' @export
#' @examples
#' cat(haishoku_export(42, "css"), sep = "\n")
#' cat(haishoku_export(42, "tex"), sep = "\n")
haishoku_export <- function(x = default_combo(), format = c("css", "gpl", "tex", "json"),
                      file = NULL, prefix = "hs", volume = 1) {
  format <- match.arg(format)
  pal <- if (inherits(x, "haishoku_palette")) x else haishoku_palette(x, volume = volume)
  hex <- as.character(pal)
  nm <- colour_names(pal)
  nm[nm == ""] <- paste0("colour-", which(nm == ""))
  cols <- colours_in(volume)
  idx <- match(hex, cols$hex)
  cmyk <- cols[idx, c("cyan", "magenta", "yellow", "black")]
  slug <- gsub("^-|-$", "", gsub("[^a-z0-9]+", "-", tolower(nm)))
  title <- if (attr(pal, "combo") %in% c("custom", "extended")) "haishoku palette" else
    paste("haishoku combination", attr(pal, "combo"))

  out <- switch(format,
    css = c(paste0("/* ", title, " */"), ":root {",
            sprintf("  --%s-%s: %s;", prefix, slug, hex), "}"),
    gpl = {
      rgb <- farver::decode_colour(hex)
      c("GIMP Palette", paste("Name:", title), "Columns: 0", "#",
        sprintf("%3d %3d %3d\t%s", rgb[, 1], rgb[, 2], rgb[, 3], nm))
    },
    tex = {
      if (anyNA(idx)) stop("LaTeX export uses CMYK recipes, so needs dictionary colours.", call. = FALSE)
      camel <- gsub("[^A-Za-z0-9]", "", nm)
      c(paste0("% ", title, " (CMYK recipes from the book; needs \\usepackage{xcolor})"),
        sprintf("\\definecolor{%s%s}{cmyk}{%s}", prefix, camel,
                apply(cmyk / 100, 1, function(v) paste(format(v, nsmall = 2, trim = TRUE), collapse = ","))))
    },
    json = {
      entry <- sprintf('    {"name": "%s", "hex": "%s", "cmyk": %s}', nm, hex,
                       ifelse(is.na(idx), "null",
                              sprintf("[%s]", apply(cmyk, 1, paste, collapse = ", "))))
      c("{", sprintf('  "name": "%s",', title), '  "colours": [',
        paste0(entry, c(rep(",", length(entry) - 1), "")), "  ]", "}")
    }
  )
  if (is.null(file)) return(out)
  writeLines(out, file)
  invisible(out)
}
