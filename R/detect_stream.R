#' Detect the passes of a stream along a route
#'
#' @description
#' Predicate and locator: does a stream follow a route, and where? The stream
#' is not transformed (compare with [match_stream()], which aligns a stream
#' known to follow a route).
#'
#' The procedure follows, in metres:
#'
#' 1. the stream must come within `tolerance` of the route;
#' 2. at least `min_completion` of the route readings must lie within
#'    `tolerance` of the stream;
#' 3. every time the stream comes within `tolerance` of the start (or of the
#'    end) of the route a candidate pass begins; it ends at the first arrival
#'    within `tolerance` of the opposite end of the route after covering half
#'    of the route length (or, if there is none, after the route length plus
#'    `tolerance`);
#' 4. the coverage of a pass is the share of route readings within `tolerance`
#'    of the pass; passes with coverage of at least `min_coverage` are
#'    `"match"`, the others `"partial"`;
#' 5. the direction of a pass is `"forward"` when the order of the readings
#'    agrees with the order of their nearest route readings (positive Spearman
#'    correlation), otherwise `"reverse"`.
#'
#' Distances are measured between readings, so both streams should have a
#' resolution finer than `tolerance` (see [resample_stream()]).
#'
#' @param data A stream with `lon` and `lat`.
#' @param route The route, with `lon` and `lat`, ordered from start to end.
#' @param tolerance Distance (m) within which the stream is on the route.
#' @param min_completion Share of the route that must be covered by the whole
#'   stream.
#' @param min_coverage Share of the route that a pass must cover to be a
#'   `"match"`.
#' @param min_partial Passes covering less than this share of the route are
#'   not reported. Overlapping passes are reduced to the one with the highest
#'   coverage.
#'
#' @return A tibble with one row per pass: `pass`, `start_row`, `end_row`
#'   (rows of `data`), `direction`, `coverage` and `status`. It has no rows
#'   when the stream does not follow the route; the reason is given in
#'   `attr(x, "diagnosis")`.
#'
#' @seealso [match_stream()], [compare_stream()]
#' @export
#'
#' @examples
#' act = read_stream(system.file("extdata", "gpx_activity.gpx", package = "metrictrails"))
#' route = read_stream(system.file("extdata", "gpx_route.gpx", package = "metrictrails"))
#' detect_stream(act, route)
detect_stream = function(data, route, tolerance = 50, min_completion = 1, min_coverage = 0.97,
                         min_partial = 0.25) {
  check_columns(data, c("lon", "lat"), "detect_stream")
  check_columns(route, c("lon", "lat"), "detect_stream")
  epsg = utm_epsg(route$lon, route$lat)
  rxy = to_metric(route$lon, route$lat, epsg)
  dxy = to_metric(data$lon, data$lat, epsg)
  rxy = rxy[stats::complete.cases(rxy), , drop = FALSE]
  n = nrow(dxy)
  m = nrow(rxy)

  empty = tibble::tibble(pass = integer(0), start_row = integer(0), end_row = integer(0),
                         direction = character(0), coverage = numeric(0), status = character(0))

  d_data = nearest_neighbour(dxy, rxy)
  d_route = nearest_neighbour(rxy, dxy)$dist
  if (min(d_data$dist, na.rm = TRUE) > tolerance) {
    attr(empty, "diagnosis") = "streams do not intersect"
    return(empty)
  }
  completion = mean(d_route <= tolerance)
  if (completion < min_completion) {
    attr(empty, "diagnosis") = paste0("streams intersect but the stream covers only ",
                                      round(100 * completion, 1), "% of the route")
    return(empty)
  }

  route_length = sum(sqrt(diff(rxy[, 1])^2 + diff(rxy[, 2])^2))
  step = c(0, sqrt(diff(dxy[, 1])^2 + diff(dxy[, 2])^2))
  step[is.na(step)] = 0
  cum = cumsum(step)
  nearest = d_data$index

  # candidate starts: first reading of every visit to the route start or end;
  # each candidate runs until the stream reaches the opposite end of the route
  ends = list(rxy[1, ], rxy[m, ])
  near_end = list()
  for (e in 1:2) {
    close = sqrt((dxy[, 1] - ends[[e]][1])^2 + (dxy[, 2] - ends[[e]][2])^2) <= tolerance
    close[is.na(close)] = FALSE
    near_end[[e]] = close
  }

  passes = NULL
  for (e in 1:2) {
    close = near_end[[e]]
    starts = which(close & !c(FALSE, close[-n]))
    target = near_end[[3 - e]]
    for (start in starts) {
      # first arrival at the opposite end after covering half the route,
      # otherwise the route length plus the tolerance
      arrival = which(target & seq_len(n) > start & cum - cum[start] >= 0.5 * route_length)
      end = if (length(arrival) > 0) arrival[1] else max(which(cum <= cum[start] + route_length + tolerance))
      if (end - start < 2) next
      sub = dxy[start:end, , drop = FALSE]
      covered = nearest_neighbour(rxy, sub)$dist <= tolerance
      rho = suppressWarnings(stats::cor(seq_len(end - start + 1), nearest[start:end], method = "spearman"))
      passes = rbind(passes, data.frame(
        start_row = start,
        end_row = end,
        direction = if (!is.na(rho) && rho < 0) "reverse" else "forward",
        coverage = mean(covered)
      ))
    }
  }
  if (!is.null(passes)) {
    # keep partial passes covering at least min_partial, and drop passes that
    # overlap a pass with higher coverage
    passes = passes[passes$coverage >= min_partial, , drop = FALSE]
    passes = passes[order(-passes$coverage, passes$start_row), , drop = FALSE]
    keep = rep(TRUE, nrow(passes))
    for (i in seq_len(nrow(passes))) {
      if (!keep[i]) next
      overlap = passes$start_row <= passes$end_row[i] & passes$end_row >= passes$start_row[i]
      overlap[i] = FALSE
      keep[overlap & seq_len(nrow(passes)) > i] = FALSE
    }
    passes = passes[keep, , drop = FALSE]
    passes = passes[order(passes$start_row), , drop = FALSE]
    if (nrow(passes) == 0) passes = NULL
  }
  if (is.null(passes)) {
    attr(empty, "diagnosis") = "the stream never passes through the start or end of the route"
    return(empty)
  }
  passes$status = ifelse(passes$coverage >= min_coverage, "match", "partial")
  out = tibble::as_tibble(cbind(pass = seq_len(nrow(passes)), passes))
  attr(out, "diagnosis") = if (any(out$status == "match")) "match" else "partial match only"
  out
}
