#' Write a GNSS stream to a GPX file
#'
#' @description
#' Writes a stream as a GPX 1.1 track. Each value of `segment` becomes a
#' `<trkseg>`. Elevation (`ele`) and time (`timestamp`) are written when
#' present.
#'
#' @param data A stream with at least `lon` and `lat` (see [uniform_stream()]).
#' @param file Path of the GPX file to create.
#' @param name Optional track name.
#'
#' @return `file`, invisibly.
#' @seealso [read_stream()]
#' @export
#'
#' @examples
#' f = system.file("extdata", "gpx_route.gpx", package = "metrictrails")
#' route = read_stream(f)
#' out = tempfile(fileext = ".gpx")
#' write_stream(route, out)
#' nrow(read_stream(out)) == nrow(route)
write_stream = function(data, file, name = NULL) {
  check_columns(data, c("lon", "lat"), "write_stream")
  seg = segment_id(data)

  has_ele = "ele" %in% names(data)
  has_time = "timestamp" %in% names(data)

  # one <trkpt> line per reading
  pt = paste0('<trkpt lat="', sprintf("%.8f", data$lat), '" lon="', sprintf("%.8f", data$lon), '">')
  if (has_ele) {
    ele = ifelse(is.na(data$ele), "", paste0("<ele>", sprintf("%.2f", data$ele), "</ele>"))
    pt = paste0(pt, ele)
  }
  if (has_time) {
    time = format(as.POSIXct(data$timestamp), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    time = ifelse(is.na(data$timestamp), "", paste0("<time>", time, "</time>"))
    pt = paste0(pt, time)
  }
  pt = paste0("      ", pt, "</trkpt>")

  out = c('<?xml version="1.0" encoding="UTF-8"?>',
          '<gpx version="1.1" creator="metrictrails" xmlns="http://www.topografix.com/GPX/1/1">',
          "  <trk>")
  if (!is.null(name)) {
    out = c(out, paste0("    <name>", name, "</name>"))
  }
  for (s in unique(seg)) {
    out = c(out, "    <trkseg>", pt[seg == s], "    </trkseg>")
  }
  out = c(out, "  </trk>", "</gpx>")

  writeLines(out, file, useBytes = TRUE)
  invisible(file)
}
