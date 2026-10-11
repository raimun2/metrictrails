#' Spatial and temporal derivatives of a stream
#'
#' @description
#' Adds, for every reading, the differences with the previous reading of the
#' same segment and the quantities derived from them.
#'
#' Always computed:
#'
#' * `delta_d`: horizontal (2D) distance (m), and `distance`, its cumulative
#'   sum along the stream (distance between segments is not counted).
#' * `turn_angle`: absolute change of heading between the previous and the
#'   current step (degrees, 0 to 180).
#'
#' With `ele`: `delta_ele`, `dplus` and `dminus` (elevation gained and lost,
#' both positive), `delta_d3d` (3D distance) and `slope` (`delta_ele / delta_d`,
#' rise over run; `NA` when the reading is idle).
#'
#' With `time`: `delta_time`, `speed` (horizontal, m/s), `accel` (change of
#' `speed` over time, m/s^2), `pace` (min/km); with `ele` also `vert_speed`,
#' `vert_accel` and `speed_3d`.
#'
#' The first reading of every segment gets 0 for differences and `NA` for
#' rates.
#'
#' @param data A stream with `lon` and `lat`.
#' @param distance How horizontal distance is computed: `"utm"` (Euclidean
#'   distance in the UTM zone of the stream) or `"haversine"` (great circle on
#'   a sphere of radius 6371008.8 m).
#' @param idle Horizontal distance (m) below which slope and heading are not
#'   defined.
#'
#' @return The stream with the new columns.
#'
#' @references
#' Sanchez, R., Nieto-Jimenez, C., & Riveros-Matthey, C. (2025). High-precision
#' mapping of mountain terrain for human locomotion research: A multimodal
#' geospatial framework. *2025 15th IEEE International Conference on Pattern
#' Recognition Systems (ICPRS)*. \doi{10.1109/ICPRS66293.2025.11302827}
#'
#' @export
#'
#' @examples
#' f = system.file("extdata", "gpx_activity.gpx", package = "metrictrails")
#' stream = read_stream(f) %>% uniform_stream() %>% differential_stream()
#' summary(stream$speed)
differential_stream = function(data, distance = c("utm", "haversine"), idle = 0.1) {
  distance = match.arg(distance)
  check_columns(data, c("lon", "lat"), "differential_stream")
  data = tibble::as_tibble(data)
  seg = segment_id(data)
  n = nrow(data)
  is_first = c(TRUE, seg[-1] != seg[-n])

  epsg = utm_epsg(data$lon, data$lat)
  xy = to_metric(data$lon, data$lat, epsg)
  dx = diff_by_segment(xy[, 1], seg)
  dy = diff_by_segment(xy[, 2], seg)

  if (distance == "utm") {
    delta_d = sqrt(dx^2 + dy^2)
  } else {
    rad = pi / 180
    lat1 = c(NA, data$lat[-n]) * rad
    lat2 = data$lat * rad
    dlat = lat2 - lat1
    dlon = (data$lon - c(NA, data$lon[-n])) * rad
    a = sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
    delta_d = 2 * 6371008.8 * asin(pmin(1, sqrt(a)))
    delta_d[is_first] = 0
  }
  data$delta_d = delta_d
  data$distance = cumsum(delta_d)

  # heading change between consecutive steps (planar, in UTM)
  moving = delta_d >= idle
  heading = ifelse(moving & !is_first, atan2(dy, dx), NA)
  turn = (heading - c(NA, heading[-n]) + pi) %% (2 * pi) - pi
  turn[is_first] = NA
  data$turn_angle = abs(turn) * 180 / pi

  has_ele = "ele" %in% names(data)
  if (has_ele) {
    delta_ele = diff_by_segment(data$ele, seg)
    data$delta_ele = delta_ele
    data$dplus = pmax(delta_ele, 0)
    data$dminus = pmax(-delta_ele, 0)
    data$delta_d3d = sqrt(delta_d^2 + delta_ele^2)
    data$slope = ifelse(moving & !is_first, delta_ele / delta_d, NA)
  }

  if ("time" %in% names(data)) {
    delta_time = diff_by_segment(data$time, seg)
    data$delta_time = delta_time
    ok = !is_first & delta_time > 0
    speed = ifelse(ok, delta_d / delta_time, NA)
    data$speed = speed
    data$accel = ifelse(ok, diff_by_segment(speed, seg, first = NA) / delta_time, NA)
    data$pace = ifelse(speed > 0, 1000 / 60 / speed, NA)
    if (has_ele) {
      vert = ifelse(ok, data$delta_ele / delta_time, NA)
      data$vert_speed = vert
      data$vert_accel = ifelse(ok, diff_by_segment(vert, seg, first = NA) / delta_time, NA)
      data$speed_3d = ifelse(ok, data$delta_d3d / delta_time, NA)
    }
  }
  data
}
