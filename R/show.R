#' Show a palette as labelled swatches
#'
#' Draws a combination or palette as swatches labelled with colour name, hex
#' code and CMYK recipe, optionally alongside how it appears with simulated
#' colour vision deficiency.
#'
#' @param x A combination number, [haishoku_palette()] or colours.
#' @param labels Label swatches with name, hex and CMYK recipe.
#' @param cvd `"none"`, one of `"deutan"`, `"protan"`, `"tritan"`, or `"all"`
#'   to show normal vision and all three simulations.
#' @param volume Which volume of the dictionary to use.
#' @return A ggplot.
#' @export
#' @examples
#' haishoku_show(302)
#' haishoku_show(302, cvd = "all")
haishoku_show <- function(x = default_combo(), labels = TRUE,
                    cvd = c("none", "deutan", "protan", "tritan", "all"), volume = 1) {
  cvd <- match.arg(cvd)
  pal <- if (inherits(x, "haishoku_palette")) x else haishoku_palette(x, volume = volume)
  hex <- as.character(pal)
  label <- colour_names(pal)
  cols <- colours_in(volume)
  idx <- match(hex, cols$hex)
  recipe <- ifelse(is.na(idx), "",
                   sprintf("C%d M%d Y%d K%d", cols$cyan[idx], cols$magenta[idx],
                           cols$yellow[idx], cols$black[idx]))
  label <- trimws(paste(label, hex, recipe, sep = "\n"))

  conditions <- switch(cvd, none = "none", all = c("none", "deutan", "protan", "tritan"), cvd)
  titles <- c(none = "Normal vision", deutan = "Deuteranopia", protan = "Protanopia",
              tritan = "Tritanopia")
  df <- do.call(rbind, lapply(conditions, function(cond) {
    shown <- simulate_cvd(hex, cond)
    data.frame(x = seq_along(hex), fill = shown, ink = ink_for(shown),
               label = if (labels) label else "",
               condition = factor(titles[[cond]], levels = titles[conditions]),
               stringsAsFactors = FALSE)
  }))

  combo <- attr(pal, "combo")
  title <- if (combo %in% c("custom", "extended")) NULL else paste("Combination", combo)
  p <- ggplot2::ggplot(df, ggplot2::aes(.data$x, 1)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$fill), width = 1, height = 1) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label, colour = .data$ink),
                       size = 3, lineheight = 0.9) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_colour_identity() +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(6, 6, 6, 6))
  if (length(conditions) > 1) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$condition), ncol = 1) +
      ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(4, 0, 2, 0)))
  }
  p
}

#' Show every combination a colour appears in
#'
#' The visual form of the book's dictionary: all combinations containing a
#' colour, with that colour marked.
#'
#' @param colour A colour name or number.
#' @param size Optionally restrict to combinations of this many colours.
#' @param volume Which volume of the dictionary to use.
#' @return A ggplot.
#' @export
#' @examples
#' haishoku_show_colour("Hermosa Pink")
haishoku_show_colour <- function(colour, size = NULL, volume = 1) {
  target <- haishoku_colour(colour, volume)
  found <- haishoku_combos_with(colour, size, volume)
  if (nrow(found) == 0) stop(names(target), " appears in no combinations of that size.", call. = FALSE)
  df <- haishoku_combo(found$combo, volume)
  df$panel <- factor(paste("No.", df$combo), levels = paste("No.", found$combo))
  df$is_target <- df$hex == target
  df$ink <- ink_for(df$hex)

  ggplot2::ggplot(df, ggplot2::aes(.data$position, 1)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$hex), width = 1, height = 1) +
    ggplot2::geom_point(data = df[df$is_target, ], ggplot2::aes(colour = .data$ink),
                        size = 1.5) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_colour_identity() +
    ggplot2::facet_wrap(ggplot2::vars(.data$panel), scales = "free_x") +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::labs(title = names(target),
                  subtitle = sprintf("%s  |  appears in %d combination%s", target,
                                     nrow(found), if (nrow(found) == 1) "" else "s")) +
    ggplot2::theme_void() +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 8, margin = ggplot2::margin(2, 0, 2, 0)),
                   panel.spacing = ggplot2::unit(6, "pt"),
                   plot.margin = ggplot2::margin(6, 6, 6, 6))
}
