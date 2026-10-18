#' Summary measures of a stream
#'
#' @description
#' Computes course-level measures of a stream, or of each group of a stream.
#'
#' * `distance_2d`, `distance_3d`: horizontal and 3D length (m).
#' * `elevation_gain`, `elevation_loss`: cumulative positive and negative
#'   elevation change (m, both positive).
#' * `ele_min`, `ele_max`: elevation range (m).
#' * `mean_slope`, `iqr_slope`: mean and inter-quartile range of the slope of
#'   moving readings (%).
#' * `sinuosity`: path length over straight-line distance between the first
#'   and last readings. `NA` for closed paths (first and last readings closer
#'   than `closed_tol`), where the ratio is not defined.
#' * `sinuosity_local`: mean ratio between the path length of a window of
#'   `window` metres and the straight-line distance (chord) between its ends.
#'   Windows start every `window / 10` metres along the path and both ends are
#'   interpolated, so the measure does not depend on the sampling rate of the
#'   device. It is defined for open and closed paths alike, equals 1 for a
#'   straight line and grows with the curvature at the scale of `window`.
#'   Windows do not cross segments; `NA` when no segment is longer than
#'   `window`.
#' * `mean_turn`: mean heading change (degrees).
#' * `resolution`: mean horizontal distance between consecutive readings (m).
#' * `km_effort`: kilometre-effort, horizontal km plus elevation gain / 100.
#' * `pct_idle`: share of readings without horizontal displacement (%), and
#'   `idle_gain`, `idle_loss`: elevation gained and lost while idle.
#' * `duration`: elapsed time (s), when `time` is available.
#' * `fractal_dim`: fractal dimension of the horizontal path (divider method,
#'   see [fractal_stream()]), when `fractal = TRUE`.
#'
#' @param data A stream with `lon` and `lat` (and optionally `ele`, `time`).
#' @param by Optional name(s) of grouping columns (e.g. `"round"`).
#' @param idle Horizontal distance (m) below which a reading is idle.
#' @param fractal If `TRUE`, adds the fractal dimension (`NA` when the path is
#'   too short for its resolution).
#' @param window Length (m) of the windows of `sinuosity_local`. It should be
#'   several times the resolution of the stream.
#' @param closed_tol Distance (m) between the first and last readings below
#'   which the path is closed and `sinuosity` is `NA`.
#'
#' @return A tibble with one row per group.
#'
#' @references
#' Sanchez, R., Egli, P. E., Jornet, K., Duggan, M., & Besomi, M. (2025). How
#' fractal complexity distorts distance and elevation gain in trail and
#' mountain running: The case for course measurement standardization.
#' *Proceedings of the Institution of Mechanical Engineers, Part P: Journal of
#' Sports Engineering and Technology*. \doi{10.1177/17543371251341660}
#'
#' Sanchez, R., Nieto-Jimenez, C., & Riveros-Matthey, C. (2025). High-precision
#' mapping of mountain terrain for human locomotion research: A multimodal
#' geospatial framework. *2025 15th IEEE International Conference on Pattern
#' Recognition Systems (ICPRS)*. \doi{10.1109/ICPRS66293.2025.11302827}
#'
#' @export
#'
#' @examples
#' f = system.file("extdata", "gpx_activity.gpx", package = "metrictrails")
#' summarise_stream(read_stream(f) %>% uniform_stream())
summarise_stream = function(data, by = NULL, idle = 0.1, fractal = FALSE,
                            window = 100, closed_tol = 50) {
  check_columns(data, c("lon", "lat", by), "summarise_stream")
  data = tibble::as_tibble(data)
  if (is.null(by)) {
    group = rep(1L, nrow(data))
  } else {
    key = do.call(paste, c(lapply(by, function(b) data[[b]]), sep = "\r"))
    group = match(key, unique(key))
  }

  out = NULL
  for (g in unique(group)) {
    sub = data[group == g, ]
    d = differential_stream(sub, idle = idle)
    is_idle = d$delta_d < idle
    is_idle[1] = FALSE
    xy = to_metric(sub$lon, sub$lat, utm_epsg(sub$lon, sub$lat))
    straight = sqrt((xy[nrow(xy), 1] - xy[1, 1])^2 + (xy[nrow(xy), 2] - xy[1, 2])^2)

    row = tibble::tibble(
      n = nrow(sub),
      distance_2d = sum(d$delta_d),
      resolution = mean(d$delta_d[-1]),
      sinuosity = if (straight < closed_tol) NA_real_ else sum(d$delta_d) / straight,
      sinuosity_local = local_sinuosity(xy[, 1], xy[, 2], segment_id(sub), window),
      mean_turn = mean(d$turn_angle, na.rm = TRUE),
      pct_idle = 100 * mean(is_idle[-1])
    )
    if ("ele" %in% names(sub)) {
      row$distance_3d = sum(d$delta_d3d, na.rm = TRUE)
      row$elevation_gain = sum(d$dplus, na.rm = TRUE)
      row$elevation_loss = sum(d$dminus, na.rm = TRUE)
      row$ele_min = min(sub$ele, na.rm = TRUE)
      row$ele_max = max(sub$ele, na.rm = TRUE)
      row$mean_slope = 100 * mean(d$slope, na.rm = TRUE)
      row$iqr_slope = 100 * stats::IQR(d$slope, na.rm = TRUE)
      row$km_effort = row$distance_2d / 1000 + row$elevation_gain / 100
      row$idle_gain = sum(d$dplus[is_idle], na.rm = TRUE)
      row$idle_loss = sum(d$dminus[is_idle], na.rm = TRUE)
    }
    if ("time" %in% names(sub)) {
      row$duration = diff(range(sub$time, na.rm = TRUE))
    }
    if (fractal) {
      fd = tryCatch(fractal_stream(sub)$dimension, error = function(e) NA_real_)
      row$fractal_dim = fd
    }
    if (!is.null(by)) {
      row = cbind(sub[1, by, drop = FALSE], row)
    }
    out = rbind(out, row)
  }
  tibble::as_tibble(out)
}

# Mean ratio of path length to chord over windows of fixed arc length `w`
# (x, y in metres). Windows start every w / 10 m along the path and both ends
# are interpolated, so the result does not depend on the sampling rate.
local_sinuosity = function(x, y, seg, w) {
  ratios = NULL
  for (s in unique(seg)) {
    rows = which(seg == s)
    if (length(rows) < 2) next
    px = x[rows]
    py = y[rows]
    arc = c(0, cumsum(sqrt(diff(px)^2 + diff(py)^2)))
    if (max(arc) <= w) next
    keep = !duplicated(arc)
    s0 = seq(0, max(arc) - w, by = w / 10)
    sx = stats::approx(arc[keep], px[keep], xout = s0)$y
    sy = stats::approx(arc[keep], py[keep], xout = s0)$y
    ex = stats::approx(arc[keep], px[keep], xout = s0 + w)$y
    ey = stats::approx(arc[keep], py[keep], xout = s0 + w)$y
    chord = sqrt((ex - sx)^2 + (ey - sy)^2)
    ratios = c(ratios, w / chord[chord > 0])
  }
  if (is.null(ratios)) {
    return(NA_real_)
  }
  mean(ratios)
}
