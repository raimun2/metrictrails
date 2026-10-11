#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

# Internal helpers shared by several functions. Kept to the minimum needed:
# projection to metres and segment handling.

# EPSG code of the WGS 84 / UTM zone containing the centroid of the stream
utm_epsg = function(lon, lat) {
  lon0 = mean(range(lon, na.rm = TRUE))
  lat0 = mean(range(lat, na.rm = TRUE))
  zone = floor((lon0 + 180) / 6) + 1
  zone = min(max(zone, 1), 60)
  if (lat0 < 0) {
    return(32700 + zone)
  }
  32600 + zone
}

# Project lon/lat (degrees) to a metric CRS. Returns a 2-column matrix x, y.
# Rows with missing coordinates stay NA.
to_metric = function(lon, lat, epsg) {
  xy = matrix(NA_real_, nrow = length(lon), ncol = 2)
  ok = !is.na(lon) & !is.na(lat)
  if (any(ok)) {
    xy[ok, ] = sf::sf_project(from = "EPSG:4326", to = paste0("EPSG:", epsg),
                              pts = cbind(lon[ok], lat[ok]), keep = TRUE)
  }
  colnames(xy) = c("x", "y")
  xy
}

# Inverse of to_metric(). Returns a 2-column matrix lon, lat.
to_lonlat = function(x, y, epsg) {
  ll = matrix(NA_real_, nrow = length(x), ncol = 2)
  ok = !is.na(x) & !is.na(y)
  if (any(ok)) {
    ll[ok, ] = sf::sf_project(from = paste0("EPSG:", epsg), to = "EPSG:4326",
                              pts = cbind(x[ok], y[ok]), keep = TRUE)
  }
  colnames(ll) = c("lon", "lat")
  ll
}

# Integer segment id for every row. A stream without a `segment` column is a
# single segment. Segments are numbered in order of appearance.
segment_id = function(data) {
  if (!"segment" %in% names(data)) {
    return(rep(1L, nrow(data)))
  }
  seg = data$segment
  match(seg, unique(seg))
}

# Stop with an informative message when required columns are missing
check_columns = function(data, columns, fun) {
  missing = setdiff(columns, names(data))
  if (length(missing) > 0) {
    stop(fun, "() needs column(s): ", paste(missing, collapse = ", "),
         ". See ?uniform_stream for the naming convention.", call. = FALSE)
  }
  invisible(TRUE)
}

# Differences inside each segment; the first row of every segment gets `first`
diff_by_segment = function(x, seg, first = 0) {
  d = c(first, diff(x))
  d[c(TRUE, seg[-1] != seg[-length(seg)])] = first
  d
}

# Nearest neighbour of every row of `query` among the rows of `target`
# (2 or 3 columns, metric coordinates). Returns the index and the distance.
# In 2D the search uses GEOS (sf); in 3D it is a brute-force search in blocks.
nearest_neighbour = function(query, target) {
  target = target[stats::complete.cases(target), , drop = FALSE]
  ok = stats::complete.cases(query)
  index = rep(NA_integer_, nrow(query))
  q = query[ok, , drop = FALSE]
  if (ncol(query) == 2) {
    q_sf = sf::st_sfc(lapply(seq_len(nrow(q)), function(i) sf::st_point(q[i, ])))
    t_sf = sf::st_sfc(lapply(seq_len(nrow(target)), function(i) sf::st_point(target[i, ])))
    index[ok] = sf::st_nearest_feature(q_sf, t_sf)
  } else {
    found = integer(nrow(q))
    block = max(1, floor(1e6 / nrow(target)))
    for (start in seq(1, nrow(q), by = block)) {
      rows = start:min(nrow(q), start + block - 1)
      d2 = 0
      for (j in seq_len(ncol(q))) {
        d2 = d2 + outer(q[rows, j], target[, j], "-")^2
      }
      found[rows] = max.col(-d2, ties.method = "first")
    }
    index[ok] = found
  }
  dist = rep(NA_real_, nrow(query))
  dist[ok] = sqrt(rowSums((q - target[index[ok], , drop = FALSE])^2))
  list(index = index, dist = dist)
}
