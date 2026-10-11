#' Match a stream to a known reference trail
#'
#' @description
#' Temporal alignment of a stream along a reference trail that the athlete is
#' known to have followed (for example, rounds on a trail measured with a
#' high-precision protocol). For every reading the function estimates where
#' the athlete was along the reference, under the constraints that the
#' athlete never moves backwards and never exceeds a physiologically plausible
#' speed for the local slope.
#'
#' The unknown is the sequence of reference vertices \eqn{S = (j_1, \dots, j_n)},
#' non-decreasing along the direction of travel, that minimises
#'
#' \deqn{J(S) = \sum_i d^2(obs_i, ref_{j_i}) + \lambda^{-2} \sum_i pen(j_{i-1}, j_i, \Delta t_i)}
#'
#' where \eqn{d} is the horizontal (or 3D) distance and \eqn{pen} the squared
#' excess of the horizontal, vertical and total speeds along the reference over
#' the Tobler ceiling (`tobler_speed() * tol_factor`). Transitions whose total
#' speed exceeds the ceiling (with slope clipped to `slope_clip`) are not
#' allowed; staying on the same vertex is always allowed, so a solution always
#' exists. Because \eqn{J} is a sum of terms that depend on one reading or on
#' two consecutive readings, the global minimum is found exactly with dynamic
#' programming (Viterbi algorithm).
#'
#' The reference should come out of the full pipeline (sanitized, resampled at
#' a fine resolution, with DEM elevation), because the matched stream takes its
#' coordinates and elevation from it.
#'
#' @param data A stream with `lon`, `lat` and `time` (and `ele` when
#'   `dims = 3`).
#' @param reference The reference trail, with `lon`, `lat` and preferably
#'   `ele`, ordered along the trail.
#' @param params Tobler parameters, output of [tobler_stream()] or a vector
#'   accepted by [tobler_speed()].
#' @param direction Direction of travel along the reference: `"auto"` (from the
#'   order of the projected readings), `"forward"` or `"reverse"`.
#' @param by Optional name of a column splitting `data` into independent rounds;
#'   each round is matched separately (and its direction detected separately).
#' @param dims `2` (horizontal distance) or `3` (include elevation) in the
#'   distance term.
#' @param lambda Weight of the speed penalty (larger values weaken it).
#' @param tol_factor Tolerance factor applied to the Tobler ceiling.
#' @param slope_clip Slope clipping used by the hard speed constraint.
#' @param max_cells Maximum size of the dynamic programming table
#'   (readings x vertices) for one round.
#'
#' @return The stream with:
#' * `lon`, `lat`, `ele`: position of the matched reference vertex (the
#'   original values are kept in `lon_obs`, `lat_obs`, `ele_obs`);
#' * `ref_index`, `s_ref`: matched vertex and its distance along the reference;
#' * `offset_d`: horizontal distance between the original reading and the
#'   matched vertex (m);
#' * `offset_along`, `offset_cross`: the same offset decomposed along the
#'   local direction of travel of the reference (positive when the reading is
#'   ahead) and across it (positive to the left), so that
#'   `offset_d^2 = offset_along^2 + offset_cross^2`;
#' * `speed_h`, `speed_v`, `speed_total`, `slope_ref`: kinematics along the
#'   reference after matching.
#'
#' A quality-control report is stored in `attr(x, "match_report")`: readings
#' per reference vertex (maximum and number of vertices above
#' `max(2, ceiling(n / m))`), share of transitions at the speed ceiling,
#' objective value and offset summaries, per round.
#'
#' @references
#' Sanchez, R. (2025). Human trajectory mapping in ecological contexts: a
#' framework for reliable space-time localization. *ANZSCPB Conference 2025*,
#' Sippy Downs, Australia.
#'
#' Forney, G. D. (1973). The Viterbi algorithm. *Proceedings of the IEEE*,
#' 61(3), 268-278. \doi{10.1109/PROC.1973.9030}
#'
#' @seealso [tobler_stream()], [detect_stream()]
#' @export
#'
#' @examples
#' act = read_stream(system.file("extdata", "gpx_activity.gpx", package = "metrictrails"))
#' route = read_stream(system.file("extdata", "gpx_route.gpx", package = "metrictrails"))
#' dem = system.file("extdata", "test_dem.tif", package = "metrictrails")
#' ref = route %>% resample_stream(res = 5) %>% ele_correction(dem = dem)
#' act = act[1:300, ] %>% uniform_stream() %>% sanitize_stream() %>%
#'   ele_correction(dem = dem)
#' params = c(v1 = 3.5, m1 = 0.1, c_plus = 3.2, c_minus = -0.5)
#' matched = match_stream(act, ref, params)
#' summary(matched$offset_d)
match_stream = function(data, reference, params, direction = c("auto", "forward", "reverse"),
                        by = NULL, dims = 2, lambda = 1, tol_factor = 1.1, slope_clip = 0.6,
                        max_cells = 5e7) {
  direction = match.arg(direction)
  check_columns(data, c("lon", "lat", "time", if (dims == 3) "ele", by), "match_stream")
  check_columns(reference, c("lon", "lat"), "match_stream")
  data = tibble::as_tibble(data)

  # reference arrays (metres)
  epsg = utm_epsg(reference$lon, reference$lat)
  rxy = to_metric(reference$lon, reference$lat, epsg)
  rz = if ("ele" %in% names(reference)) reference$ele else rep(0, nrow(reference))
  if (any(is.na(rxy)) || any(is.na(rz))) {
    stop("The reference has missing coordinates or elevations.", call. = FALSE)
  }
  rs = c(0, cumsum(sqrt(diff(rxy[, 1])^2 + diff(rxy[, 2])^2)))
  m = nrow(rxy)

  if (is.null(by)) {
    group = rep(1L, nrow(data))
  } else {
    group = match(data[[by]], unique(data[[by]]))
  }

  out = data
  out$lon_obs = data$lon
  out$lat_obs = data$lat
  if ("ele" %in% names(data)) out$ele_obs = data$ele
  new_cols = c("ref_index", "s_ref", "offset_d", "offset_cross", "offset_along",
               "speed_h", "speed_v", "speed_total", "slope_ref")
  for (col in new_cols) out[[col]] = NA_real_
  out$ele = NA_real_
  report = NULL

  for (g in unique(group)) {
    rows = which(group == g)
    oxy = to_metric(data$lon[rows], data$lat[rows], epsg)
    t = data$time[rows]
    n = length(rows)
    if (n * m > max_cells) {
      stop("Round with ", n, " readings and a reference of ", m, " vertices exceeds 'max_cells'; ",
           "split the stream into rounds or use a coarser reference.", call. = FALSE)
    }

    # orthogonal projection of every reading on the reference line
    proj = project_on_line(oxy[, 1], oxy[, 2], rxy[, 1], rxy[, 2], rs)

    dir_g = direction
    if (dir_g == "auto") {
      inner = seq(ceiling(0.1 * n), max(ceiling(0.1 * n), floor(0.9 * n)))
      rho = suppressWarnings(stats::cor(seq_along(inner), proj$s[inner], method = "spearman"))
      dir_g = if (!is.na(rho) && rho < 0) "reverse" else "forward"
    }

    # work along the direction of travel
    ord = if (dir_g == "forward") 1:m else m:1
    gx = rxy[ord, 1]
    gy = rxy[ord, 2]
    gz = rz[ord]
    gs = if (dir_g == "forward") rs else rs[m] - rs[ord]
    oz = if (dims == 3) data$ele[rows] else NULL

    path = viterbi_match(oxy[, 1], oxy[, 2], oz, t, gx, gy, gz, gs, params,
                         lambda, tol_factor, slope_clip)
    j = path$j
    j_ref = ord[j]

    out$lon[rows] = reference$lon[j_ref]
    out$lat[rows] = reference$lat[j_ref]
    out$ele[rows] = rz[j_ref]
    out$ref_index[rows] = j_ref
    out$s_ref[rows] = rs[j_ref]
    out$offset_d[rows] = sqrt((oxy[, 1] - rxy[j_ref, 1])^2 + (oxy[, 2] - rxy[j_ref, 2])^2)
    # decompose the offset along and across the local direction of travel
    prev_j = pmax(j - 1, 1)
    next_j = pmin(j + 1, m)
    tx = gx[next_j] - gx[prev_j]
    ty = gy[next_j] - gy[prev_j]
    norm = sqrt(tx^2 + ty^2)
    norm[norm == 0] = NA
    tx = tx / norm
    ty = ty / norm
    ex = oxy[, 1] - gx[j]
    ey = oxy[, 2] - gy[j]
    out$offset_along[rows] = ex * tx + ey * ty
    out$offset_cross[rows] = tx * ey - ty * ex

    ds = c(NA, diff(gs[j]))
    dz = c(NA, diff(gz[j]))
    dt = c(NA, diff(t))
    ok = !is.na(dt) & dt > 0
    out$speed_h[rows] = ifelse(ok, ds / dt, NA)
    out$speed_v[rows] = ifelse(ok, abs(dz) / dt, NA)
    out$speed_total[rows] = ifelse(ok, sqrt(ds^2 + dz^2) / dt, NA)
    out$slope_ref[rows] = ifelse(!is.na(ds) & ds > 0, dz / ds, NA)

    load = tabulate(j, nbins = m)
    cap = max(2, ceiling(n / m))
    slope_c = pmin(pmax(ifelse(!is.na(ds) & ds > 0, dz / ds, 0), -slope_clip), slope_clip)
    ceiling_h = tobler_speed(slope_c, params) * tol_factor
    at_ceiling = ok & ds > 0 & (ds / dt) >= 0.98 * ceiling_h
    report = rbind(report, tibble::tibble(
      group = if (is.null(by)) NA else data[[by]][rows[1]],
      direction = dir_g,
      n = n,
      vertices_used = sum(load > 0),
      max_per_vertex = max(load),
      vertices_over_cap = sum(load > cap),
      pct_at_ceiling = 100 * mean(at_ceiling[ok]),
      objective = path$objective,
      offset_median = stats::median(out$offset_d[rows]),
      offset_q95 = stats::quantile(out$offset_d[rows], 0.95, names = FALSE)
    ))
  }

  attr(out, "match_report") = report
  out
}

# Orthogonal projection of points on a polyline: distance along the line (s)
# and signed perpendicular distance (positive to the left of the line). The
# nearest segment of every point is found exactly with GEOS (sf).
project_on_line = function(px, py, rx, ry, rs) {
  m = length(rx)
  segments = sf::st_sfc(lapply(seq_len(m - 1), function(i) {
    sf::st_linestring(rbind(c(rx[i], ry[i]), c(rx[i + 1], ry[i + 1])))
  }))
  points = sf::st_sfc(lapply(seq_along(px), function(i) sf::st_point(c(px[i], py[i]))))
  a = sf::st_nearest_feature(points, segments)
  b = a + 1
  dx = rx[b] - rx[a]
  dy = ry[b] - ry[a]
  len2 = dx^2 + dy^2
  u = ifelse(len2 > 0, ((px - rx[a]) * dx + (py - ry[a]) * dy) / len2, 0)
  u = pmin(pmax(u, 0), 1)
  qx = rx[a] + u * dx
  qy = ry[a] + u * dy
  d = sqrt((px - qx)^2 + (py - qy)^2)
  side = sign(dx * (py - qy) - dy * (px - qx))
  list(s = rs[a] + u * sqrt(len2), cross = side * d)
}

# Exact minimisation of the matching objective by dynamic programming
viterbi_match = function(ox, oy, oz, t, gx, gy, gz, gs, params, lambda, tol_factor, slope_clip) {
  n = length(ox)
  m = length(gx)
  slopes = seq(-slope_clip, slope_clip, by = 0.01)
  v_top = max(tobler_speed(slopes, params) * tol_factor * sqrt(1 + slopes^2))

  unary = function(i) {
    u = (ox[i] - gx)^2 + (oy[i] - gy)^2
    if (!is.null(oz)) u = u + (oz[i] - gz)^2
    u
  }

  D = unary(1)
  back = matrix(0L, nrow = n, ncol = m)
  for (i in 2:n) {
    dt = t[i] - t[i - 1]
    best = D
    arg = seq_len(m)
    if (!is.na(dt) && dt > 0) {
      reach = findInterval(gs + v_top * dt, gs)
      k_max = max(reach - seq_len(m))
      for (k in seq_len(k_max)) {
        from = 1:(m - k)
        to = from + k
        ds = gs[to] - gs[from]
        dz = gz[to] - gz[from]
        slope = ifelse(ds > 0, dz / ds, 0)
        slope_c = pmin(pmax(slope, -slope_clip), slope_clip)
        vh_c = tobler_speed(slope_c, params) * tol_factor
        allowed = ds == 0 | sqrt(ds^2 + dz^2) / dt <= vh_c * sqrt(1 + slope_c^2) + 1e-9
        vh = tobler_speed(slope, params) * tol_factor
        excess = pmax(ds / dt - vh, 0)
        pen = excess^2 * (1 + slope^2 + (1 + slope^2)) / lambda^2
        pen[ds == 0] = 0
        cand = D[from] + pen
        cand[!allowed] = Inf
        update = cand < best[to]
        best[to[update]] = cand[update]
        arg[to[update]] = from[update]
      }
    }
    D = best + unary(i)
    back[i, ] = arg
  }

  j = integer(n)
  j[n] = which.min(D)
  for (i in n:2) {
    j[i - 1] = back[i, j[i]]
  }
  list(j = j, objective = min(D))
}
