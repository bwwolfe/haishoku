#' A 'ggplot2' theme on a combination's paper and ink
#'
#' A minimal theme that takes its background and text colours from a
#' combination via [haishoku_roles()]: the lightest colour as paper and the
#' highest-contrast colour as ink. Pair it with a scale from the same
#' combination, keeping in mind that the paper colour is then not available
#' for data.
#'
#' @param x A combination number, [haishoku_palette()] or colours.
#' @param base_size,base_family Base font size and family.
#' @param paper Override the background colour, e.g. `"#FFFFFF"`.
#' @param volume Which volume of the dictionary to use.
#' @return A 'ggplot2' theme.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mpg, aes(displ, hwy, colour = drv)) +
#'   geom_point() +
#'   scale_colour_haishoku(haishoku_extend(c("Deep Slate Olive", "Peach Red"), 3,
#'                                   background = "Naples Yellow")) +
#'   theme_haishoku(303)
theme_haishoku <- function(x = default_combo(), base_size = 11, base_family = "",
                           paper = NULL, volume = 1) {
  roles <- haishoku_roles(x, background = paper, volume = volume)
  bg <- roles$hex[roles$role == "background"]
  ink <- roles$hex[roles$role == "ink"]
  if (!is.null(paper) && !length(ink)) ink <- ink_for(bg)
  if (haishoku_contrast(ink, bg) < 4.5) ink <- ink_for(bg)
  grid <- grDevices::colorRampPalette(c(bg, ink))(9)[2]
  muted <- grDevices::colorRampPalette(c(bg, ink))(9)[6]

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = bg, colour = NA),
      panel.background = ggplot2::element_rect(fill = bg, colour = NA),
      text = ggplot2::element_text(colour = ink),
      axis.text = ggplot2::element_text(colour = muted),
      panel.grid = ggplot2::element_line(colour = grid),
      panel.grid.minor = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(fill = bg, colour = NA),
      strip.text = ggplot2::element_text(colour = ink, hjust = 0),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.title.position = "plot"
    )
}
