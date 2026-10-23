# Regenerates the README figures. Run from the package root.
devtools::load_all(quiet = TRUE)
library(ggplot2)

save <- function(plot, file, width, height) {
  ggsave(file.path("man/figures", file), plot, width = width, height = height, dpi = 150, bg = "white")
}

save(haishoku_show(286, cvd = "all"), "show-cvd.png", 7, 5)
save(haishoku_show_colour("Hermosa Pink"), "show-colour.png", 7, 2.2)

save(
  ggplot(mpg, aes(displ, hwy, colour = drv)) +
    geom_point(size = 2) +
    scale_colour_haishoku(286) +
    labs(title = "scale_colour_haishoku(286)") +
    theme_minimal(),
  "scale-discrete.png", 6, 4
)
save(
  ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
    geom_raster() +
    scale_fill_haishoku_c(232) +
    labs(title = "scale_fill_haishoku_c(232)") +
    theme_minimal(),
  "scale-continuous.png", 6, 4
)
save(
  ggplot(mpg, aes(class, fill = class)) +
    geom_bar() +
    scale_fill_haishoku_d(42, extend = TRUE) +
    labs(title = "scale_fill_haishoku_d(42, extend = TRUE)") +
    theme_minimal() +
    theme(legend.position = "none"),
  "scale-extend.png", 6, 4
)
save(
  ggplot(subset(economics_long, variable != "uempmed"), aes(date, value01, colour = variable)) +
    geom_line(linewidth = 0.7) +
    scale_colour_manual(values = haishoku_extend(c("Deep Slate Olive", "Peach Red"), 4,
                                           background = "Naples Yellow")) +
    labs(title = "theme_haishoku(303)", x = NULL, y = NULL) +
    theme_haishoku(303),
  "theme.png", 6, 4
)
