#' Spatial resolution of a stream
#'
#' @description
#' Describes the distribution of the horizontal distance between consecutive
#' readings (`delta_d`, 2D, in metres) and derives the Nyquist resolution used
#' by [resample_stream()].
#'
#' A GNSS device that records at a fixed rate (e.g. 1 Hz) produces a spatial
#' resolution that is a random variable: `delta_d` is the speed times the
#' sampling interval. Readings with `delta_d < idle` (stops) are excluded.
#'
#' The spacing `res` is a high quantile (`q`) of `delta_d`. Spatial features
#' with wavelength of at least `2 * res` are resolved almost everywhere along
#' the track, and a uniform grid with spacing `res` represents exactly that
#' band (its Nyquist wavelength is `2 * res`). A finer grid would add
#' interpolated readings between sparse fixes; a coarser grid would discard
#' resolved information.
#'
#' Only horizontal geodesic distance is used: raw elevations come from sensor
#' fusion or are rounded to the metre, and are replaced from a DEM at the end
#' of the pipeline.
#'
#' @param data A stream with `lon` and `lat`, or a list of streams.
#' @param q Quantile of `delta_d` that defines the resolution.
#' @param idle Distance (m) below which a reading is considered idle.
#'
#' @return A tibble with one row per stream: number of readings, share of idle
#'   readings, mean, standard deviation and quantiles of `delta_d`, and `res`,
#'   the `q`-quantile. When `data` is a list, use `max(result$res)` as the
#'   common resolution to make the streams comparable.
#'
#' @references
#' Sanchez, R., Egli, P. E., Jornet, K., Duggan, M., & Besomi, M. (2025). How
#' fractal complexity distorts distance and elevation gain in trail and
#' mountain running: The case for course measurement standardization.
#' *Proceedings of the Institution of Mechanical Engineers, Part P: Journal of
#' Sports Engineering and Technology*. \doi{10.1177/17543371251341660}
#'
#' @export
#'
#' @examples
#' f = system.file("extdata", "gpx_activity.gpx", package = "metrictrails")
#' resolution_stream(read_stream(f))
resolution_stream = function(data, q = 0.9, idle = 0.1) {
  if (!is.data.frame(data) && is.list(data)) {
    out = NULL
    for (i in seq_along(data)) {
      row = resolution_stream(data[[i]], q = q, idle = idle)
      row = cbind(stream = if (is.null(names(data))) i else names(data)[i], row)
      out = rbind(out, row)
    }
    return(tibble::as_tibble(out))
  }

  check_columns(data, c("lon", "lat"), "resolution_stream")
  epsg = utm_epsg(data$lon, data$lat)
  xy = to_metric(data$lon, data$lat, epsg)
  seg = segment_id(data)
  is_first = c(TRUE, seg[-1] != seg[-length(seg)])
  delta_d = sqrt(diff(xy[, 1])^2 + diff(xy[, 2])^2)
  delta_d = delta_d[!is_first[-1]]
  moving = delta_d[delta_d >= idle & !is.na(delta_d)]
  if (length(moving) == 0) {
    stop("The stream does not move.", call. = FALSE)
  }

  qs = stats::quantile(moving, c(0.05, 0.25, 0.5, 0.75, 0.95), names = FALSE)
  tibble::tibble(
    n = nrow(data),
    pct_idle = 100 * mean(delta_d < idle, na.rm = TRUE),
    mean = mean(moving),
    sd = stats::sd(moving),
    q05 = qs[1], q25 = qs[2], q50 = qs[3], q75 = qs[4], q95 = qs[5],
    max = max(moving),
    q = q,
    res = stats::quantile(moving, q, names = FALSE)
  )
}

#' Resample a stream to a uniform spatial resolution
#'
#' @description
#' Resamples each segment of a stream along its horizontal 2D path, so that the
#' result does not depend on how fast the athlete moved or on the recording
#' rate of the device (the "defractalizer").
#'
#' By default the resolution is derived from the stream itself with
#' [resolution_stream()] (`res = "nyquist"`): the `q`-quantile of the distance
#' between consecutive moving readings. Streams resampled at different
#' resolutions are not directly comparable; to compare several streams, pass
#' the same numeric `res` to all of them (at least the largest of their Nyquist
#' resolutions).
#'
#' Methods:
#'
#' * `"arclength"` (default): points are placed every `L / floor(L / res)`
#'   metres along the 2D path of length `L` (a spacing equal to or slightly
#'   larger than `res`, keeping both ends), as in `defractalize()` of Sanchez,
#'   Nieto-Jimenez & Riveros-Matthey (2025).
#' * `"simplify"`: the path is densified to 0.2 m and then simplified with the
#'   Douglas-Peucker algorithm using `res` as tolerance, as in Sanchez et al.
#'   (2025). The spacing of the result is not uniform.
#'
#' Resampling decimates the stream wherever the original readings are closer
#' than `res`; the stream should be low-pass filtered before (see
#' [sanitize_stream()]) to avoid aliasing. `time`, `timestamp`, `ele` and
#' every other numeric column are interpolated linearly along the 2D distance;
#' other columns take the value of the preceding original reading. Elevation is
#' expected to be replaced from a DEM afterwards ([ele_correction()]).
#'
#' @param data A stream with `lon` and `lat`.
#' @param res Target resolution in metres, or `"nyquist"`.
#' @param q Quantile used when `res = "nyquist"`.
#' @param method `"arclength"` or `"simplify"`.
#' @param idle Distance (m) below which a reading is idle (see
#'   [resolution_stream()]).
#'
#' @return The resampled stream; the resolution used is stored in
#'   `attr(x, "res")`.
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
#' stream = read_stream(f) %>% uniform_stream() %>% sanitize_stream()
#' resampled = resample_stream(stream)
#' attr(resampled, "res")
resample_stream = function(data, res = "nyquist", q = 0.9,
                           method = c("arclength", "simplify"), idle = 0.1) {
  method = match.arg(method)
  check_columns(data, c("lon", "lat"), "resample_stream")
  data = tibble::as_tibble(data)
  if (identical(res, "nyquist")) {
    res = resolution_stream(data, q = q, idle = idle)$res
  }
  if (!is.numeric(res) || length(res) != 1 || res <= 0) {
    stop("'res' must be a positive number or \"nyquist\".", call. = FALSE)
  }

  epsg = utm_epsg(data$lon, data$lat)
  xy = to_metric(data$lon, data$lat, epsg)
  seg = segment_id(data)

  # columns to carry over
  numeric_cols = setdiff(names(data)[vapply(data, is.numeric, logical(1))],
                         c("lon", "lat", "segment"))
  is_time = vapply(data, function(v) inherits(v, "POSIXct"), logical(1))
  time_cols = names(data)[is_time]
  other_cols = setdiff(names(data), c("lon", "lat", numeric_cols, time_cols))

  pieces = list()
  for (s in unique(seg)) {
    rows = which(seg == s)
    x = xy[rows, 1]
    y = xy[rows, 2]
    s_orig = c(0, cumsum(sqrt(diff(x)^2 + diff(y)^2)))
    total = s_orig[length(s_orig)]
    if (length(rows) < 2 || total < res) next

    if (method == "arclength") {
      n_step = floor(total / res)
      s_new = seq(0, total, length.out = n_step + 1)
    } else {
      s_dense = seq(0, total, length.out = ceiling(total / 0.2) + 1)
      x_dense = stats::approx(s_orig, x, xout = s_dense, ties = mean)$y
      y_dense = stats::approx(s_orig, y, xout = s_dense, ties = mean)$y
      line = sf::st_sfc(sf::st_linestring(cbind(x_dense, y_dense)), crs = epsg)
      kept = sf::st_coordinates(sf::st_simplify(line, preserveTopology = FALSE, dTolerance = res))
      # Douglas-Peucker keeps a subset of the dense vertices: find them in order
      idx = integer(nrow(kept))
      pointer = 1
      for (k in seq_len(nrow(kept))) {
        while (pointer < length(x_dense) &&
               (x_dense[pointer] != kept[k, "X"] || y_dense[pointer] != kept[k, "Y"])) {
          pointer = pointer + 1
        }
        idx[k] = pointer
      }
      s_new = s_dense[idx]
    }

    piece = tibble::tibble(
      x = stats::approx(s_orig, x, xout = s_new, ties = mean)$y,
      y = stats::approx(s_orig, y, xout = s_new, ties = mean)$y
    )
    for (col in numeric_cols) {
      v = data[[col]][rows]
      ok = !is.na(v)
      piece[[col]] = if (sum(ok) >= 2) {
        stats::approx(s_orig[ok], v[ok], xout = s_new, ties = mean, rule = 2)$y
      } else {
        NA_real_
      }
    }
    for (col in time_cols) {
      v = as.numeric(data[[col]][rows])
      ok = !is.na(v)
      value = if (sum(ok) >= 2) stats::approx(s_orig[ok], v[ok], xout = s_new, ties = mean, rule = 2)$y else NA_real_
      piece[[col]] = as.POSIXct(value, origin = "1970-01-01", tz = "UTC")
    }
    for (col in other_cols) {
      before = findInterval(s_new, s_orig, rightmost.closed = TRUE)
      piece[[col]] = data[[col]][rows][pmax(before, 1)]
    }
    if ("segment" %in% names(data)) {
      piece$segment = data$segment[rows[1]]
    }
    pieces[[length(pieces) + 1]] = piece
  }
  out = do.call(rbind, pieces)
  if (is.null(out)) {
    stop("No segment is longer than 'res'.", call. = FALSE)
  }

  ll = to_lonlat(out$x, out$y, epsg)
  out = tibble::tibble(lon = ll[, "lon"], lat = ll[, "lat"], out[, setdiff(names(out), c("x", "y"))])
  first = intersect(c("lon", "lat", "ele", "timestamp", "time"), names(out))
  out = out[, c(first, setdiff(names(out), first))]
  attr(out, "res") = res
  out
}
