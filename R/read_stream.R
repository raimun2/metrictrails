#' Read a GNSS stream from a GPX file
#'
#' @description
#' Reads the track points (or route points) of a GPX file with [sf::st_read()]
#' and returns them as a tibble with one row per reading.
#'
#' Coordinates are returned as `lon` and `lat` (WGS 84 degrees), elevation as
#' `ele` and the time signature as `timestamp` (POSIXct, UTC). Each GPX track
#' segment (`trkseg`) becomes a value of the column `segment`, so that later
#' processing never interpolates across a recording gap. Point extensions such
#' as the Garmin `TrackPointExtension` (heart rate, cadence, temperature, power)
#' are returned as numeric columns with their original tag names (e.g. `hr`,
#' `cad`, `atemp`).
#'
#' Only GPX is supported. Other formats can be converted to GPX beforehand.
#'
#' @param file Path to a `.gpx` file.
#' @param type Which GPX geometry to read: `"auto"` (track points if present,
#'   otherwise route points), `"track"` or `"route"`.
#'
#' @return A tibble with columns `lon`, `lat`, and, when available, `ele`,
#'   `timestamp`, `segment` and extension variables.
#' @seealso [uniform_stream()], [write_stream()]
#' @export
#'
#' @examples
#' f = system.file("extdata", "gpx_activity.gpx", package = "metrictrails")
#' stream = read_stream(f)
#' head(stream)
read_stream = function(file, type = c("auto", "track", "route")) {
  type = match.arg(type)
  if (!file.exists(file)) {
    stop("File not found: ", file, call. = FALSE)
  }
  if (!grepl("\\.gpx$", file, ignore.case = TRUE)) {
    stop("read_stream() only reads GPX files.", call. = FALSE)
  }

  layers = sf::st_layers(file)
  n_features = stats::setNames(layers$features, layers$name)
  n_track = if ("track_points" %in% names(n_features)) n_features[["track_points"]] else 0
  n_route = if ("route_points" %in% names(n_features)) n_features[["route_points"]] else 0

  if (type == "auto") {
    layer = if (n_track > 0) "track_points" else "route_points"
  } else {
    layer = paste0(type, "_points")
  }
  if ((layer == "track_points" && n_track == 0) || (layer == "route_points" && n_route == 0)) {
    stop("No ", sub("_points", "", layer), " points found in ", basename(file),
         " (waypoint-only files are not streams).", call. = FALSE)
  }

  points = sf::st_read(file, layer = layer, quiet = TRUE)

  coords = sf::st_coordinates(points)
  attrs = sf::st_drop_geometry(points)
  stream = tibble::tibble(lon = coords[, "X"], lat = coords[, "Y"])

  if ("ele" %in% names(attrs) && any(!is.na(attrs$ele))) {
    stream$ele = as.numeric(attrs$ele)
  }
  if ("time" %in% names(attrs) && any(!is.na(attrs$time))) {
    stream$timestamp = as.POSIXct(attrs$time, tz = "UTC")
    attr(stream$timestamp, "tzone") = "UTC"
  }

  # one segment per track (or route) and track segment
  if (layer == "track_points") {
    seg_key = paste(attrs$track_fid, attrs$track_seg_id)
  } else {
    seg_key = as.character(attrs$route_fid)
  }
  stream$segment = match(seg_key, unique(seg_key))

  # Point extensions (e.g. Garmin TrackPointExtension) are nested XML that the
  # GDAL GPX driver does not expose, so they are parsed from the file text. The
  # point blocks appear in the same order as the features read by sf.
  text = paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (grepl("<extensions>", text, fixed = TRUE)) {
    tag = if (layer == "track_points") "trkpt" else "rtept"
    blocks = regmatches(text, gregexpr(paste0("(?s)<", tag, "[ >].*?(</", tag, ">|/>)"), text, perl = TRUE))[[1]]
    if (length(blocks) == nrow(stream)) {
      ext = sub("(?s)^.*?<extensions>(.*)</extensions>.*$", "\\1", blocks, perl = TRUE)
      ext[!grepl("<extensions>", blocks, fixed = TRUE)] = ""
      leaves = regmatches(ext, gregexpr("<([A-Za-z0-9_]+:)?[A-Za-z0-9_]+>[^<]*</", ext, perl = TRUE))
      leaf_names = unique(gsub("^<([A-Za-z0-9_]+:)?([A-Za-z0-9_]+)>.*$", "\\2", unlist(leaves)))
      for (leaf in leaf_names) {
        pattern = paste0("<([A-Za-z0-9_]+:)?", leaf, ">([^<]*)</")
        value = rep(NA_character_, length(ext))
        has_leaf = grepl(pattern, ext, perl = TRUE)
        value[has_leaf] = sub(paste0("(?s)^.*?", pattern, ".*$"), "\\2", ext[has_leaf], perl = TRUE)
        value = suppressWarnings(as.numeric(value))
        # extension tags must not collide with the naming convention
        name = leaf
        reserved = c("lon", "lat", "ele", "timestamp", "time", "segment", "lng", "longitude",
                     "latitude", "elevation", "altitude", "alt", "datetime")
        if (tolower(name) %in% reserved || name %in% names(stream)) {
          name = paste0("ext_", leaf)
        }
        if (any(!is.na(value)) && !name %in% names(stream)) {
          stream[[name]] = value
        }
      }
    } else {
      warning("Point extensions could not be aligned with the points and were skipped.", call. = FALSE)
    }
  }

  stream
}
