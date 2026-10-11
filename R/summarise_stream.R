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
#'   and last readings (`Inf` for closed loops).
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
summarise_stream = function(data, by = NULL, idle = 0.1, fractal = FALSE) {
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
      sinuosity = sum(d$delta_d) / straight,
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
