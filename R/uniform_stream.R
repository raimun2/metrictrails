#' Uniform naming convention for a stream
#'
#' @description
#' Renames the columns of a stream to the convention used by every function of
#' the package:
#'
#' * `lon`: longitude (WGS 84 degrees)
#' * `lat`: latitude (WGS 84 degrees)
#' * `ele`: elevation or altitude (metres)
#' * `timestamp`: time signature of the reading (POSIXct, UTC)
#' * `time`: seconds since the first reading
#'
#' Columns are matched against a dictionary of exact names (case-insensitive),
#' e.g. `lng`, `longitude` or `position_long` for longitude and `altitude` or
#' `enhanced_altitude` for elevation. Partial matches are not used, so columns
#' such as `moving_time` or `platform` are never renamed. Other variables
#' (heart rate, cadence, power, ...) are kept unchanged.
#'
#' A column called `time` holding date-times (or ISO 8601 text) is treated as a
#' timestamp. A numeric `time` column (e.g. Strava API streams) is kept as
#' seconds. When time increases in less than half of the readings (e.g. routes
#' exported by planners with a constant time), `time` and `timestamp` are
#' removed with a warning.
#'
#' @param data A data frame or tibble with a GNSS stream.
#'
#' @return A tibble with `lon`, `lat` (and `ele`, `timestamp`, `time` when
#'   available) as first columns.
#' @export
#'
#' @examples
#' data(strava_streams)
#' head(uniform_stream(strava_streams))
#'
#' df = data.frame(Latitude = c(-33.40, -33.41), Longitude = c(-70.50, -70.51),
#'                 Altitude = c(800, 805),
#'                 Time = c("2024-01-01T10:00:00Z", "2024-01-01T10:00:05Z"))
#' uniform_stream(df)
uniform_stream = function(data) {
  data = tibble::as_tibble(data)
  dictionary = list(
    lon = c("lon", "lng", "long", "longitude", "position_long", "position_lon"),
    lat = c("lat", "latitude", "position_lat"),
    ele = c("ele", "elevation", "altitude", "alt", "enhanced_altitude", "elev"),
    timestamp = c("timestamp", "datetime", "date_time")
  )

  lower = tolower(names(data))
  for (target in names(dictionary)) {
    hits = which(lower %in% dictionary[[target]])
    if (length(hits) > 1) {
      stop("Several columns could be '", target, "': ",
           paste(names(data)[hits], collapse = ", "), ". Remove or rename all but one.",
           call. = FALSE)
    }
    if (length(hits) == 1) {
      names(data)[hits] = target
    }
  }

  # a 'time' column can be a timestamp or elapsed seconds
  hit = which(tolower(names(data)) == "time")
  if (length(hit) == 1) {
    names(data)[hit] = "time"
    if (inherits(data$time, c("POSIXct", "POSIXlt", "Date")) || is.character(data$time)) {
      if ("timestamp" %in% names(data)) {
        stop("Both 'timestamp' and a date-time 'time' column are present.", call. = FALSE)
      }
      names(data)[hit] = "timestamp"
    }
  }

  check_columns(data, c("lon", "lat"), "uniform_stream")
  data$lon = as.numeric(data$lon)
  data$lat = as.numeric(data$lat)
  if ("ele" %in% names(data)) {
    data$ele = as.numeric(data$ele)
  }

  if ("timestamp" %in% names(data)) {
    ts = data$timestamp
    if (is.character(ts)) {
      ts = as.POSIXct(gsub("T", " ", sub("Z$", "", ts)), tz = "UTC")
    }
    ts = as.POSIXct(ts, tz = "UTC")
    attr(ts, "tzone") = "UTC"
    data$timestamp = ts
    data$time = as.numeric(difftime(ts, min(ts, na.rm = TRUE), units = "secs"))
  } else if ("time" %in% names(data)) {
    data$time = as.numeric(data$time)
  }

  # route planners often write the same (or no meaningful) time on every point
  if ("time" %in% names(data) && nrow(data) > 2) {
    increasing = mean(diff(data$time) > 0, na.rm = TRUE)
    if (is.na(increasing) || increasing < 0.5) {
      warning("Time increases in less than half of the readings; it is removed and the ",
              "stream is treated as a route without time.", call. = FALSE)
      data$time = NULL
      data$timestamp = NULL
    }
  }

  first = intersect(c("lon", "lat", "ele", "timestamp", "time"), names(data))
  data[, c(first, setdiff(names(data), first))]
}
