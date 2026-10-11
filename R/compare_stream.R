#' Compare two streams
#'
#' @description
#' Spatial agreement between two streams, based on the distance from every
#' reading of one stream to the nearest reading of the other (in the UTM zone
#' of `a`). Directed measures are reported in both directions (`_ab`: from `a`
#' to `b`; `_ba`: from `b` to `a`):
#'
#' * `hausdorff`: maximum nearest-neighbour distance (m); the symmetric value
#'   is the larger of both directions.
#' * `separation`: median nearest-neighbour distance (m).
#' * `metric_score`: share of readings whose nearest-neighbour distance is at
#'   most `threshold` (%).
#'
#' Separation follows the implementation used in Sanchez, Nieto-Jimenez &
#' Riveros-Matthey (2025): the median of the directed nearest-neighbour
#' distances. Streams should be resampled to the same resolution before
#' comparing them (see [resample_stream()]).
#'
#' @param a,b Streams with `lon` and `lat` (and `ele` when `dims = 3`).
#' @param threshold Distance (m) used by the metric score.
#' @param dims `2` for horizontal distance, `3` to include elevation.
#'
#' @return A one-row tibble.
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
#' act = read_stream(system.file("extdata", "gpx_activity.gpx", package = "metrictrails"))
#' route = read_stream(system.file("extdata", "gpx_route.gpx", package = "metrictrails"))
#' compare_stream(resample_stream(act, res = 2), resample_stream(route, res = 2))
compare_stream = function(a, b, threshold = 1, dims = 2) {
  cols = if (dims == 3) c("lon", "lat", "ele") else c("lon", "lat")
  check_columns(a, cols, "compare_stream")
  check_columns(b, cols, "compare_stream")
  epsg = utm_epsg(a$lon, a$lat)
  pa = to_metric(a$lon, a$lat, epsg)
  pb = to_metric(b$lon, b$lat, epsg)
  if (dims == 3) {
    pa = cbind(pa, a$ele)
    pb = cbind(pb, b$ele)
  }
  pa = pa[stats::complete.cases(pa), , drop = FALSE]
  pb = pb[stats::complete.cases(pb), , drop = FALSE]

  d_ab = nearest_neighbour(pa, pb)$dist
  d_ba = nearest_neighbour(pb, pa)$dist

  tibble::tibble(
    dims = dims,
    hausdorff = max(max(d_ab), max(d_ba)),
    hausdorff_ab = max(d_ab),
    hausdorff_ba = max(d_ba),
    separation_ab = stats::median(d_ab),
    separation_ba = stats::median(d_ba),
    metric_score_ab = 100 * mean(d_ab <= threshold),
    metric_score_ba = 100 * mean(d_ba <= threshold)
  )
}
