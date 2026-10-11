#' Project a stream to metric coordinates
#'
#' @description
#' Adds the coordinates of every reading in a metric CRS, by default the
#' WGS 84 / UTM zone that contains the centre of the stream. This is the
#' projection used internally by every function of the package that measures
#' distances.
#'
#' @param data A stream with `lon` and `lat`.
#' @param epsg Optional EPSG code of the target CRS. When `NULL`, the UTM zone
#'   of the stream is used (326xx north, 327xx south).
#'
#' @return The stream with columns `x` and `y` (metres); the EPSG code is
#'   stored in `attr(x, "epsg")`.
#' @seealso [locate_stream()]
#' @export
#'
#' @examples
#' f = system.file("extdata", "gpx_route.gpx", package = "metrictrails")
#' route = project_stream(read_stream(f))
#' attr(route, "epsg")
#' head(route[, c("lon", "lat", "x", "y")])
project_stream = function(data, epsg = NULL) {
  check_columns(data, c("lon", "lat"), "project_stream")
  data = tibble::as_tibble(data)
  if (is.null(epsg)) {
    epsg = utm_epsg(data$lon, data$lat)
  }
  xy = to_metric(data$lon, data$lat, epsg)
  data$x = xy[, "x"]
  data$y = xy[, "y"]
  attr(data, "epsg") = epsg
  data
}

#' Locate a stream along a reference
#'
#' @description
#' Linear referencing: every reading of `data` is projected orthogonally onto
#' the polyline of `reference`, giving its position along the reference and its
#' signed distance to it. Unlike [match_stream()], each reading is located
#' independently, without monotonicity or speed constraints, so noisy readings
#' can jump back and forth along the reference.
#'
#' Each reading is projected on its nearest segment of the reference.
#' Distances are measured in the UTM zone of the reference.
#'
#' @param data A stream with `lon` and `lat`.
#' @param reference The reference, with `lon` and `lat`, ordered along its
#'   path.
#'
#' @return The stream with:
#' * `s_ref`: distance along the reference of the projected point (m);
#' * `offset_cross`: signed distance to the reference (m, positive to the left
#'   of the reference direction);
#' * `lon_ref`, `lat_ref`: coordinates of the projected point.
#' @seealso [match_stream()], [detect_stream()]
#' @export
#'
#' @examples
#' act = read_stream(system.file("extdata", "gpx_activity.gpx", package = "metrictrails"))
#' route = read_stream(system.file("extdata", "gpx_route.gpx", package = "metrictrails"))
#' located = locate_stream(act, route)
#' summary(abs(located$offset_cross))
locate_stream = function(data, reference) {
  check_columns(data, c("lon", "lat"), "locate_stream")
  check_columns(reference, c("lon", "lat"), "locate_stream")
  data = tibble::as_tibble(data)
  epsg = utm_epsg(reference$lon, reference$lat)
  rxy = to_metric(reference$lon, reference$lat, epsg)
  if (any(is.na(rxy))) {
    stop("The reference has missing coordinates.", call. = FALSE)
  }
  if (nrow(rxy) < 2) {
    stop("The reference needs at least 2 readings.", call. = FALSE)
  }
  rs = c(0, cumsum(sqrt(diff(rxy[, 1])^2 + diff(rxy[, 2])^2)))
  oxy = to_metric(data$lon, data$lat, epsg)
  ok = stats::complete.cases(oxy)

  s = rep(NA_real_, nrow(data))
  cross = rep(NA_real_, nrow(data))
  proj = project_on_line(oxy[ok, 1], oxy[ok, 2], rxy[, 1], rxy[, 2], rs)
  s[ok] = proj$s
  cross[ok] = proj$cross

  # coordinates of the projected points, interpolated along the reference
  px = stats::approx(rs, rxy[, 1], xout = s, ties = "ordered")$y
  py = stats::approx(rs, rxy[, 2], xout = s, ties = "ordered")$y
  ll = to_lonlat(px, py, epsg)
  data$s_ref = s
  data$offset_cross = cross
  data$lon_ref = ll[, "lon"]
  data$lat_ref = ll[, "lat"]
  data
}
