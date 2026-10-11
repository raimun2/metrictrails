#' Sanitize the coordinates of a stream and flag implausible readings
#'
#' @description
#' Cleans the horizontal position (`lon`, `lat`) of a GNSS stream and adds red
#' flags for readings that are physically implausible. This is the third step
#' of the pipeline, after [read_stream()] and [uniform_stream()] and before
#' [resample_stream()].
#'
#' **Red flags** are computed on the raw values, before smoothing, and returned
#' as logical columns:
#'
#' | flag | rule | acts on |
#' |---|---|---|
#' | `flag_coord` | missing coordinate, \|lat\| > 90, \|lon\| > 180 or (0, 0) | position |
#' | `flag_time` | time does not increase (`delta_t <= 0`) | position |
#' | `flag_speed` | horizontal speed from the previous reading > `v_max`, or above the Tobler ceiling when `tobler` is given | position |
#' | `flag_accel` | \|acceleration\| > `accel_max` | position |
#' | `flag_jump` | displacement > `edge_jump` among the first or last 3 readings | position |
#' | `flag_slope` | \|slope\| > `slope_max` | elevation |
#' | `flag_vert_speed` | \|vertical speed\| > `vert_speed_max` | elevation |
#' | `flag_idle_ele` | elevation changes while the horizontal position does not | elevation |
#' | `flag_gap` | `delta_t > gap` | information only |
#' | `flag_low_res` | distance from the previous reading > `low_res` | information only |
#'
#' Rules that need time are skipped when the stream has no `time` column, or
#' when more than half of the readings exceed `v_max` (time is then not
#' physical, e.g. routes exported with fake timestamps; a warning is given).
#' Elevation flags are diagnostics of the device: elevation is replaced from a
#' DEM at the end of the pipeline (see [ele_correction()]).
#'
#' `action` decides what happens to flagged readings before smoothing:
#' `"flag"` only adds the columns; `"drop"` removes readings with a position
#' flag and sets `ele` to `NA` where an elevation flag is raised; `"interpolate"`
#' replaces flagged positions (and flagged elevations) by linear interpolation
#' between the nearest valid readings of the same segment. Readings with a
#' `flag_coord` cannot be smoothed and are always removed.
#'
#' **Smoothing methods** (per segment, in metric UTM coordinates):
#'
#' * `"kalman"` (default): constant-velocity Kalman filter followed by a
#'   Rauch-Tung-Striebel smoother (`rts = TRUE`). Process noise is a white
#'   acceleration with standard deviation `sigma_acc` (m/s^2); measurement noise
#'   has standard deviation `sigma_obs` (m). The filter restarts after time gaps
#'   longer than `gap` and where time is missing or decreases. Without time,
#'   every method except `"gaussian"` runs along the cumulative 2D distance
#'   instead of time (arc-length parametrisation; `sigma_acc` then bounds the
#'   curvature, in 1/m).
#' * `"loess"`: local polynomial regression of each coordinate against time (or
#'   cumulative distance when there is no time), with span `alpha`. This is the
#'   method of the former GPStream `smooth_stream()`.
#' * `"gaussian"`: Gaussian kernel regression against time with standard
#'   deviation `bandwidth` seconds (Sanchez & Villena, 2020).
#' * `"savgol"`: Savitzky-Golay filter with window `sg_window` and polynomial
#'   order `sg_order`.
#' * `"pipeline"`: the post-processing sequence of Martinez, Sanchez & Diaz
#'   (2026): Kalman filter, outlier detection on derived speed, acceleration and
#'   slope, linear interpolation of outliers, Savitzky-Golay filter and edge-jump
#'   correction. See Details.
#' * `"none"`: flags only.
#'
#' @details
#' In `"pipeline"`, a reading is an outlier when at least two of three criteria
#' hold for any of the derived variables: (1) extreme value, outside
#' `[Q1 - 1.5 IQR, Q3 + 1.5 IQR]`; (2) abrupt jump, absolute change from the
#' previous reading larger than `jump_k` standard deviations of the changes;
#' (3) local deviation, distance to the moving mean (window `window`) larger than
#' `local_k` moving standard deviations. The paper reports the thresholds
#' (window 25, jump 1.5, local deviation 2.0, Savitzky-Golay 9/2, edge jump
#' 30 m) but not their exact formulation; the formulation above is the one
#' implemented here. The uniform temporal interpolation of the paper is not
#' applied, because spatial resampling ([resample_stream()]) follows in the
#' metrictrails pipeline.
#'
#' @param data A stream with `lon` and `lat`; `time` and `ele` are optional.
#' @param method Smoothing method, see Description.
#' @param action What to do with flagged readings: `"flag"`, `"drop"` or
#'   `"interpolate"`.
#' @param smooth_ele If `TRUE`, `ele` is smoothed with the same method.
#' @param v_max Maximum plausible horizontal speed (m/s).
#' @param accel_max Maximum plausible absolute acceleration (m/s^2).
#' @param vert_speed_max Maximum plausible absolute vertical speed (m/s).
#' @param slope_max Maximum plausible absolute slope (rise over run); 1.19 is
#'   about 50 degrees.
#' @param gap Time gap (s) reported by `flag_gap`.
#' @param low_res Distance (m) between readings reported by `flag_low_res`.
#' @param edge_jump Displacement (m) that defines a jump at the edges.
#' @param idle Horizontal displacement (m) below which a reading is idle.
#' @param tobler Optional Tobler parameters (output of [tobler_stream()]); when
#'   given, `flag_speed` uses the slope-dependent ceiling times `tol_factor`.
#' @param tol_factor Tolerance factor applied to the Tobler ceiling.
#' @param sigma_obs,sigma_acc,rts Kalman filter parameters.
#' @param alpha Span of `"loess"`.
#' @param bandwidth Standard deviation (s) of the `"gaussian"` kernel.
#' @param sg_window,sg_order Savitzky-Golay window (odd) and polynomial order.
#' @param window,jump_k,local_k Outlier detection parameters of `"pipeline"`.
#'
#' @return The stream with smoothed `lon`, `lat` (and `ele` if `smooth_ele`),
#'   the `flag_*` columns, and a summary in `attr(x, "sanitize_report")`.
#'
#' @references
#' Martinez, C., Sanchez, R., & Diaz, C. (2026). A post-processing methodology
#' for improving kinematic consistency in GNSS-based running trajectories.
#' *2026 6th International Conference on Electrical, Computer and Energy
#' Technologies (ICECET)*. \doi{10.1109/ICECET65726.2026.11632531}
#'
#' Sanchez, R., & Villena, M. (2020). Comparative evaluation of wearable
#' devices for measuring elevation gain in mountain physical activities.
#' *Proceedings of the Institution of Mechanical Engineers, Part P: Journal of
#' Sports Engineering and Technology*, 234(4), 312-319.
#' \doi{10.1177/1754337120918975}
#'
#' @export
#'
#' @examples
#' f = system.file("extdata", "gpx_activity.gpx", package = "metrictrails")
#' stream = read_stream(f) %>% uniform_stream() %>% sanitize_stream()
#' attr(stream, "sanitize_report")
sanitize_stream = function(data,
                           method = c("kalman", "loess", "gaussian", "savgol", "pipeline", "none"),
                           action = c("flag", "drop", "interpolate"),
                           smooth_ele = FALSE,
                           v_max = 10, accel_max = 6, vert_speed_max = 1, slope_max = 1.19,
                           gap = 30, low_res = 20, edge_jump = 30, idle = 0.1,
                           tobler = NULL, tol_factor = 1.1,
                           sigma_obs = 5, sigma_acc = 1, rts = TRUE,
                           alpha = 0.05, bandwidth = 10,
                           sg_window = 9, sg_order = 2,
                           window = 25, jump_k = 1.5, local_k = 2) {
  method = match.arg(method)
  action = match.arg(action)
  check_columns(data, c("lon", "lat"), "sanitize_stream")
  data = tibble::as_tibble(data)
  has_time = "time" %in% names(data)
  has_ele = "ele" %in% names(data)

  # ---- 1. flags on raw values ------------------------------------------------
  flag_coord = is.na(data$lon) | is.na(data$lat) | abs(data$lat) > 90 | abs(data$lon) > 180 |
    (data$lon == 0 & data$lat == 0)
  flag_coord[is.na(flag_coord)] = TRUE
  n_coord = sum(flag_coord)
  data = data[!flag_coord, ]
  if (nrow(data) < 3) {
    stop("Fewer than 3 readings with valid coordinates.", call. = FALSE)
  }

  epsg = utm_epsg(data$lon, data$lat)
  xy = to_metric(data$lon, data$lat, epsg)
  seg = segment_id(data)
  n = nrow(data)
  is_first = c(TRUE, seg[-1] != seg[-n])

  delta_d = sqrt(diff_by_segment(xy[, 1], seg)^2 + diff_by_segment(xy[, 2], seg)^2)
  flag_low_res = delta_d > low_res
  flag_time = rep(FALSE, n)
  flag_gap = rep(FALSE, n)
  flag_speed = rep(FALSE, n)
  flag_accel = rep(FALSE, n)
  flag_jump = rep(FALSE, n)
  flag_slope = rep(FALSE, n)
  flag_vert_speed = rep(FALSE, n)
  speed = rep(NA_real_, n)
  flag_idle_ele = rep(FALSE, n)

  delta_e = if (has_ele) diff_by_segment(data$ele, seg) else rep(0, n)
  if (has_ele) {
    moving = delta_d >= idle
    flag_slope = moving & abs(delta_e / delta_d) > slope_max
    flag_idle_ele = !moving & !is_first & abs(delta_e) > 0
    flag_slope[is.na(flag_slope)] = FALSE
    flag_idle_ele[is.na(flag_idle_ele)] = FALSE
  }

  if (has_time) {
    delta_t = diff_by_segment(data$time, seg)
    flag_time = !is_first & (is.na(delta_t) | delta_t <= 0)
    flag_gap = !is_first & !is.na(delta_t) & delta_t > gap
    ok_t = !is_first & !is.na(delta_t) & delta_t > 0
    speed[ok_t] = delta_d[ok_t] / delta_t[ok_t]
    if (is.null(tobler)) {
      speed_max = rep(v_max, n)
    } else {
      slope = rep(0, n)
      if (has_ele) {
        slope[ok_t] = ifelse(delta_d[ok_t] > idle, delta_e[ok_t] / delta_d[ok_t], 0)
      }
      speed_max = tobler_speed(pmin(pmax(slope, -0.6), 0.6), tobler) * tol_factor
    }
    flag_speed = !is.na(speed) & speed > speed_max
    accel = diff_by_segment(speed, seg, first = NA) / delta_t
    accel[!ok_t] = NA
    flag_accel = !is.na(accel) & abs(accel) > accel_max
    if (has_ele) {
      vert = rep(NA_real_, n)
      vert[ok_t] = delta_e[ok_t] / delta_t[ok_t]
      flag_vert_speed = !is.na(vert) & abs(vert) > vert_speed_max
    }
    # jumps among the first and last readings of each segment
    for (s in unique(seg)) {
      rows = which(seg == s)
      k = length(rows)
      if (k < 7) next
      for (i in 1:3) {
        if (delta_d[rows[i + 1]] > edge_jump) flag_jump[rows[i]] = TRUE
        if (delta_d[rows[k - i + 1]] > edge_jump) flag_jump[rows[k - i + 1]] = TRUE
      }
    }
  }

  # timestamps that imply impossible speeds for most readings are not physical
  # (e.g. route planners writing 1 ms between points)
  if (has_time && mean(speed[!is_first] > v_max, na.rm = TRUE) > 0.5) {
    warning("More than half of the readings exceed the speed limit: time is not physical ",
            "and the stream is smoothed as a route without time.", call. = FALSE)
    has_time = FALSE
    flag_speed[] = FALSE
    flag_accel[] = FALSE
    flag_vert_speed[] = FALSE
    flag_gap[] = FALSE
    flag_jump[] = FALSE
  }

  flags = data.frame(flag_time, flag_speed, flag_accel, flag_jump,
                     flag_slope, flag_vert_speed, flag_idle_ele, flag_gap, flag_low_res)
  position_flag = flag_time | flag_speed | flag_accel | flag_jump
  ele_flag = flag_slope | flag_vert_speed | flag_idle_ele

  report = list(
    n_readings = n + n_coord,
    flag_coord = n_coord,
    counts = colSums(flags),
    share_integer_ele = if (has_ele) mean(data$ele == round(data$ele), na.rm = TRUE) else NA_real_,
    share_low_res = mean(flag_low_res),
    idle_gain = if (has_ele) sum(pmax(delta_e[flag_idle_ele], 0)) else NA_real_,
    idle_loss = if (has_ele) sum(pmin(delta_e[flag_idle_ele], 0)) else NA_real_
  )
  report$flag_ele_rounding = isTRUE(report$share_integer_ele > 0.9)

  # ---- 2. act on flagged readings --------------------------------------------
  if (action == "drop") {
    keep = !position_flag
    if (has_ele) data$ele[ele_flag] = NA
    data = data[keep, ]
    flags = flags[keep, ]
    xy = xy[keep, , drop = FALSE]
    seg = seg[keep]
  } else if (action == "interpolate") {
    xy[position_flag, ] = NA
    if (has_ele) data$ele[ele_flag | position_flag] = NA
    for (s in unique(seg)) {
      rows = which(seg == s)
      base = if (has_time) data$time[rows] else seq_along(rows)
      for (j in 1:2) {
        xy[rows, j] = fill_linear(base, xy[rows, j])
      }
      if (has_ele) data$ele[rows] = fill_linear(base, data$ele[rows])
    }
  }

  # ---- 3. smoothing ----------------------------------------------------------
  if (method != "none") {
    for (s in unique(seg)) {
      rows = which(seg == s)
      if (length(rows) < 5) next
      # without time, smoothing runs along the cumulative 2D distance
      t = if (has_time) data$time[rows] else c(0, cumsum(sqrt(diff(xy[rows, 1])^2 + diff(xy[rows, 2])^2)))
      z = if (has_ele && smooth_ele) data$ele[rows] else NULL
      out = smooth_segment(xy[rows, 1], xy[rows, 2], z, t, method, has_time,
                           sigma_obs, sigma_acc, rts, alpha, bandwidth, sg_window, sg_order,
                           window, jump_k, local_k, edge_jump, gap)
      xy[rows, 1] = out$x
      xy[rows, 2] = out$y
      if (!is.null(z)) data$ele[rows] = out$z
    }
  }

  ll = to_lonlat(xy[, 1], xy[, 2], epsg)
  data$lon = ll[, "lon"]
  data$lat = ll[, "lat"]
  for (f in names(flags)) {
    data[[f]] = flags[[f]]
  }
  attr(data, "sanitize_report") = report
  data
}

# Linear interpolation of the NA values of y along x (ends take the nearest value)
fill_linear = function(x, y) {
  ok = !is.na(y) & !is.na(x)
  if (sum(ok) < 2 || all(ok)) {
    return(y)
  }
  y[!ok] = stats::approx(x[ok], y[ok], xout = x[!ok], rule = 2, ties = mean)$y
  y
}

# Smooth one segment; x, y in metres, z optional, t in seconds or reading index
smooth_segment = function(x, y, z, t, method, has_time,
                          sigma_obs, sigma_acc, rts, alpha, bandwidth, sg_window, sg_order,
                          window, jump_k, local_k, edge_jump, gap) {
  # without time the Kalman filter runs on the reading index
  reset = if (has_time) gap else Inf
  if (method == "kalman") {
    k = kalman_cv(cbind(x, y), t, sigma_obs, sigma_acc, rts, reset)
    zz = if (is.null(z)) NULL else kalman_cv(cbind(z), t, sigma_obs, sigma_acc, rts, reset)[, 1]
    return(list(x = k[, 1], y = k[, 2], z = zz))
  }
  if (method == "loess") {
    fit = function(v) {
      ok = !is.na(v)
      m = stats::loess(v[ok] ~ t[ok], span = alpha, family = "gaussian",
                       control = stats::loess.control(surface = "direct"))
      stats::predict(m, newdata = t)
    }
    return(list(x = fit(x), y = fit(y), z = if (is.null(z)) NULL else fit(z)))
  }
  if (method == "gaussian") {
    if (!has_time) {
      stop("method = \"gaussian\" needs a 'time' column.", call. = FALSE)
    }
    return(list(x = gaussian_kernel(t, x, bandwidth), y = gaussian_kernel(t, y, bandwidth),
                z = if (is.null(z)) NULL else gaussian_kernel(t, z, bandwidth)))
  }
  if (method == "savgol") {
    return(list(x = savgol(x, sg_window, sg_order), y = savgol(y, sg_window, sg_order),
                z = if (is.null(z)) NULL else savgol(z, sg_window, sg_order)))
  }

  # method == "pipeline" (Martinez, Sanchez & Diaz, 2026)
  k = kalman_cv(cbind(x, y), t, sigma_obs, sigma_acc, FALSE, reset)
  x = k[, 1]
  y = k[, 2]
  if (!is.null(z)) z = kalman_cv(cbind(z), t, sigma_obs, sigma_acc, FALSE, reset)[, 1]

  # derived variables and outliers (at least 2 of 3 criteria in any variable)
  dt = c(NA, diff(t))
  dd = c(NA, sqrt(diff(x)^2 + diff(y)^2))
  speed = dd / dt
  derived = list(speed = speed, accel = c(NA, diff(speed)) / dt)
  if (!is.null(z)) derived$slope = c(NA, diff(z)) / dd
  outlier = rep(FALSE, length(x))
  for (v in derived) {
    v[!is.finite(v)] = NA
    q = stats::quantile(v, c(0.25, 0.75), na.rm = TRUE)
    c1 = v < q[1] - 1.5 * diff(q) | v > q[2] + 1.5 * diff(q)
    dv = c(NA, diff(v))
    c2 = abs(dv) > jump_k * stats::sd(dv, na.rm = TRUE)
    local_mean = stats::filter(v, rep(1 / window, window), sides = 2)
    local_sd = sqrt(pmax(stats::filter(v^2, rep(1 / window, window), sides = 2) - local_mean^2, 0))
    c3 = abs(v - local_mean) > local_k * local_sd
    votes = (c1 %in% TRUE) + (c2 %in% TRUE) + (c3 %in% TRUE)
    outlier = outlier | votes >= 2
  }
  x[outlier] = NA
  y[outlier] = NA
  x = fill_linear(t, x)
  y = fill_linear(t, y)
  if (!is.null(z)) {
    z[outlier] = NA
    z = fill_linear(t, z)
  }

  x = savgol(x, sg_window, sg_order)
  y = savgol(y, sg_window, sg_order)
  if (!is.null(z)) z = savgol(z, sg_window, sg_order)

  # edge jumps: replace the first/last reading by its neighbour
  n = length(x)
  for (i in 1:3) {
    if (n > 7 && sqrt((x[i + 1] - x[i])^2 + (y[i + 1] - y[i])^2) > edge_jump) {
      x[1:i] = x[i + 1]
      y[1:i] = y[i + 1]
    }
    j = n - i + 1
    if (n > 7 && sqrt((x[j] - x[j - 1])^2 + (y[j] - y[j - 1])^2) > edge_jump) {
      x[j:n] = x[j - 1]
      y[j:n] = y[j - 1]
    }
  }
  list(x = x, y = y, z = z)
}

# Constant-velocity Kalman filter (and RTS smoother) for each column of obs.
# Columns are independent axes; NA observations are skipped (prediction only).
# The filter restarts after a time gap longer than `reset`, and where time is
# missing or goes backwards.
kalman_cv = function(obs, t, sigma_obs, sigma_acc, rts = TRUE, reset = Inf) {
  dt_all = c(0, diff(t))
  breaks = c(TRUE, is.na(dt_all[-1]) | dt_all[-1] < 0 | dt_all[-1] > reset)
  block = cumsum(breaks)
  out = obs
  for (b in unique(block)) {
    rows = which(block == b)
    out[rows, ] = kalman_block(obs[rows, , drop = FALSE], t[rows], sigma_obs, sigma_acc, rts)
  }
  out
}

kalman_block = function(obs, t, sigma_obs, sigma_acc, rts) {
  n = nrow(obs)
  out = obs
  R = sigma_obs^2
  for (axis in seq_len(ncol(obs))) {
    z = obs[, axis]
    first = which(!is.na(z))[1]
    if (is.na(first) || n < 2) next
    m = c(z[first], 0)
    P = diag(c(R, 100))
    m_filt = matrix(NA_real_, n, 2)
    P_filt = array(NA_real_, c(2, 2, n))
    m_pred = matrix(NA_real_, n, 2)
    P_pred = array(NA_real_, c(2, 2, n))
    for (i in 1:n) {
      dt = if (i == 1) 0 else max(t[i] - t[i - 1], 0)
      Fm = matrix(c(1, 0, dt, 1), 2, 2)
      Q = sigma_acc^2 * matrix(c(dt^4 / 4, dt^3 / 2, dt^3 / 2, dt^2), 2, 2)
      m = as.vector(Fm %*% m)
      P = Fm %*% P %*% t(Fm) + Q
      m_pred[i, ] = m
      P_pred[, , i] = P
      if (!is.na(z[i])) {
        K = P[, 1] / (P[1, 1] + R)
        m = m + K * (z[i] - m[1])
        P = P - K %*% P[1, , drop = FALSE]
      }
      m_filt[i, ] = m
      P_filt[, , i] = P
    }
    if (rts) {
      m_s = m_filt
      P_s = P_filt
      for (i in (n - 1):1) {
        dt = max(t[i + 1] - t[i], 0)
        Fm = matrix(c(1, 0, dt, 1), 2, 2)
        C = P_filt[, , i] %*% t(Fm) %*% pseudo_inverse(P_pred[, , i + 1])
        m_s[i, ] = m_filt[i, ] + as.vector(C %*% (m_s[i + 1, ] - m_pred[i + 1, ]))
        P_s[, , i] = P_filt[, , i] + C %*% (P_s[, , i + 1] - P_pred[, , i + 1]) %*% t(C)
      }
      m_filt = m_s
    }
    out[, axis] = m_filt[, 1]
  }
  out
}

# Moore-Penrose inverse of a small symmetric matrix (robust to singularity)
pseudo_inverse = function(A) {
  s = svd(A)
  keep = s$d > max(dim(A)) * max(s$d) * .Machine$double.eps
  if (!any(keep)) return(matrix(0, ncol(A), nrow(A)))
  s$v[, keep, drop = FALSE] %*% (t(s$u[, keep, drop = FALSE]) / s$d[keep])
}

# Nadaraya-Watson regression with a Gaussian kernel of standard deviation bw
gaussian_kernel = function(t, v, bw) {
  out = v
  ok = !is.na(v)
  tt = t[ok]
  vv = v[ok]
  for (i in seq_along(t)) {
    near = abs(tt - t[i]) <= 4 * bw
    w = stats::dnorm(tt[near] - t[i], sd = bw)
    out[i] = sum(w * vv[near]) / sum(w)
  }
  out
}

# Savitzky-Golay smoothing; the edges use the polynomial fitted to the first
# and last full windows.
savgol = function(v, window, order) {
  n = length(v)
  if (window %% 2 == 0) window = window + 1
  if (n < window) return(v)
  half = (window - 1) / 2
  X = outer(-half:half, 0:order, "^")
  proj = X %*% solve(t(X) %*% X) %*% t(X)
  out = v
  coef = proj[half + 1, ]
  for (i in (half + 1):(n - half)) {
    out[i] = sum(coef * v[(i - half):(i + half)])
  }
  out[1:half] = as.vector(proj[1:half, ] %*% v[1:window])
  out[(n - half + 1):n] = as.vector(proj[(half + 2):window, ] %*% v[(n - window + 1):n])
  out
}
