# Judging and choosing gradient anchors ---------------------------------------
#
# Wada's combinations are qualitative, so interpolating through them can fail
# in two ways: "flat" (neighbouring anchors too close in lightness, so the
# ramp changes hue without getting darker) and "muddy" (a segment between
# opposing hues passes through grey). These helpers grade a set of anchors
# and pick diverging arrangements that avoid both.

hcl_of <- function(hex) farver::convert_colour(farver::decode_colour(hex), "rgb", "hcl")

# Anchors ordered light to dark -> "good", "usable", "muddy" or "flat".
gradient_check <- function(anchors, n = 64) {
  a <- hcl_of(anchors)
  steps <- -diff(a[, "l"])
  ramp <- hcl_of(scales::pal_gradient_n(anchors)(seq(0, 1, length.out = n)))
  seg <- pmin(floor(seq(0, 1, length.out = n) * length(steps)) + 1, length(steps))
  # Chroma along each segment against the duller of its two ends. A segment
  # with a near-grey end (White, Black, Neutral Gray) can't grey out further.
  dip <- min(vapply(seq_along(steps), function(k) {
    ref <- min(a[k, "c"], a[k + 1, "c"])
    if (ref < 10) return(1)
    min(ramp[seg == k, "c"]) / ref
  }, 1))
  verdict <- if (min(steps) < 8) "flat" else if (dip < 0.5) "muddy" else if (min(steps) >= 15) "good" else "usable"
  list(verdict = verdict, min_step = min(steps), chroma_dip = dip)
}

gradient_ok <- function(verdict) verdict %in% c("good", "usable")

# Hex already ordered light to dark.
sequential_anchors <- function(hex, anchors = "all") {
  if (anchors == "ends" && length(hex) > 2) hex[c(1, length(hex))] else hex
}

hue_gap <- function(h1, h2) abs(((h1 - h2) + 180) %% 360 - 180)

# Every (end, middle, end) arrangement of a combination's colours, scored.
# Pairs, and combinations with no arrangement of their own that works, also
# try Wada's White (light middle) and Black (dark middle).
diverging_options <- function(hex) {
  score_mid <- function(mid, pool) {
    rest <- setdiff(pool, mid)
    if (length(rest) < 2) return(NULL)
    do.call(rbind, lapply(utils::combn(seq_along(rest), 2, simplify = FALSE), function(ij) {
      ends <- rest[ij]
      l <- lightness(c(ends, mid))
      h <- hcl_of(ends)
      sims <- lapply(c("deutan", "protan"), function(type) farver::decode_colour(simulate_cvd(ends, type)))
      de_cvd <- min(vapply(sims, function(s) {
        farver::compare_colour(s[1, , drop = FALSE], s[2, , drop = FALSE], "rgb", method = "cie2000")[1, 1]
      }, 1))
      data.frame(end1 = ends[1], mid = mid, end2 = ends[2],
                 light_mid = l[3] > max(l[1:2]),
                 end_l_diff = abs(l[1] - l[2]),
                 mid_gap = min(abs(l[3] - l[1:2])),
                 ends_de = delta_e_matrix(ends)[1, 2],
                 ends_de_cvd = de_cvd,
                 hue_gap = hue_gap(h[1, "h"], h[2, "h"]),
                 end_chroma = min(h[, "c"]),
                 stringsAsFactors = FALSE)
    }))
  }
  rate <- function(opts) {
    if (is.null(opts) || nrow(opts) == 0) return(opts)
    opts$works <- with(opts, end_l_diff <= 12 & mid_gap >= 25 & ends_de >= 20 & hue_gap >= 60 &
                         end_chroma >= 15 & ends_de_cvd >= 15)
    opts$score <- with(opts, mid_gap - end_l_diff + 0.5 * pmin(ends_de_cvd, 40))
    opts
  }
  own <- if (length(hex) >= 3) rate(do.call(rbind, lapply(hex, score_mid, pool = hex))) else NULL
  white <- "#FFFFFF"
  black <- "#111314"
  if (is.null(own) || !any(own$works)) {
    extra <- rate(rbind(score_mid(white, c(hex, white)), score_mid(black, c(hex, black))))
    own <- rbind(own, extra)
  }
  own
}

# Best working arrangement for `middle` ("auto", "light" or "dark"), or NULL.
diverging_pick <- function(hex, middle = "auto") {
  opts <- diverging_options(hex)
  if (is.null(opts)) return(NULL)
  opts <- opts[opts$works, , drop = FALSE]
  if (middle == "light") opts <- opts[opts$light_mid, , drop = FALSE]
  if (middle == "dark") opts <- opts[!opts$light_mid, , drop = FALSE]
  if (nrow(opts) == 0) return(NULL)
  best <- opts[which.max(opts$score), ]
  structure(unname(c(best$end1, best$mid, best$end2)),
            middle = if (best$light_mid) "light" else "dark")
}

# Quality summary for one palette (used by haishoku_metrics()).
gradient_summary <- function(hex) {
  light <- hex[order_colours(hex, "lightness")]
  opts <- diverging_options(hex)
  works <- if (is.null(opts)) opts else opts[opts$works, , drop = FALSE]
  data.frame(
    seq_quality = gradient_check(light)$verdict,
    seq_ends_quality = gradient_check(sequential_anchors(light, "ends"))$verdict,
    div_light = !is.null(works) && any(works$light_mid),
    div_dark = !is.null(works) && any(!works$light_mid),
    stringsAsFactors = FALSE
  )
}
