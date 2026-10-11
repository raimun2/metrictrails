#' Tobler parameters of a stream
#'
#' @description
#' Fits an asymmetric Tobler-type hiking function to the relation between
#' horizontal speed and slope of a stream:
#'
#' \deqn{v(m) = v_1 \exp(-c_+ (m - m_1)) \quad \textrm{if } m \ge m_1}
#' \deqn{v(m) = v_1 \exp(-c_- (m - m_1)) \quad \textrm{if } m < m_1}
#'
#' where `m` is the slope (rise over run), `m1` the slope of maximum speed,
#' `v1` the speed at `m1` (m/s), `c_plus > 0` the decay uphill and
#' `c_minus < 0` the decay downhill.
#'
#' The model describes the frontier of human performance, so it is fitted to
#' the `tau` quantile of speed, not to its mean. Two estimators are available,
#' both using only base R:
#'
#' * `"pinball"` (default): minimises the quantile (pinball) loss
#'   \eqn{\sum \rho_\tau(v_i - v(m_i))} with [stats::optim()] (Nelder-Mead),
#'   starting from the `"binned"` solution and a few perturbations of it. This
#'   is the same estimator as nonlinear quantile regression (`quantreg::nlrq`).
#' * `"binned"`: groups the readings in slope bins of width `bin_width`, takes
#'   the `tau` quantile of speed in each bin with at least `min_bin` readings,
#'   and fits `log(speed)` with a weighted piecewise-linear [stats::lm()]; `m1`
#'   is chosen with [stats::optimize()].
#'
#' The stream should have gone through the pipeline (sanitized, resampled and
#' with DEM elevation) so that slope and speed are reliable.
#'
#' @param data A stream with `lon`, `lat`, `ele` and `time`.
#' @param tau Quantile of speed to model.
#' @param method `"pinball"` or `"binned"`.
#' @param slope_range Slopes outside this range are ignored.
#' @param min_speed Readings slower than this (m/s) are ignored (stops).
#' @param bin_width Width of the slope bins of `"binned"`.
#' @param min_bin Minimum readings per bin.
#'
#' @return A one-row tibble with `v1`, `m1`, `c_plus`, `c_minus`, `tau`, `n`
#'   (readings used), `r1` (pseudo R-squared for quantile regression,
#'   1 - loss / loss of the intercept-only model) and `method`.
#' @seealso [tobler_speed()]
#'
#' @references
#' Tobler, W. (1993). Three presentations on geographical analysis and
#' modeling. *National Center for Geographic Information and Analysis,
#' Technical Report 93-1*.
#'
#' Koenker, R., & Machado, J. A. F. (1999). Goodness of fit and related
#' inference processes for quantile regression. *Journal of the American
#' Statistical Association*, 94(448), 1296-1310.
#'
#' @export
#'
#' @examples
#' f = system.file("extdata", "gpx_activity.gpx", package = "metrictrails")
#' dem = system.file("extdata", "test_dem.tif", package = "metrictrails")
#' stream = read_stream(f) %>% uniform_stream() %>% sanitize_stream() %>%
#'   resample_stream(res = 5) %>% ele_correction(dem = dem)
#' tobler_stream(stream)
tobler_stream = function(data, tau = 0.9, method = c("pinball", "binned"),
                         slope_range = c(-0.6, 0.6), min_speed = 0.1,
                         bin_width = 0.02, min_bin = 10) {
  method = match.arg(method)
  check_columns(data, c("lon", "lat", "ele", "time"), "tobler_stream")
  d = differential_stream(data)
  keep = is.finite(d$speed) & is.finite(d$slope) & d$speed > min_speed &
    d$slope >= slope_range[1] & d$slope <= slope_range[2]
  v = d$speed[keep]
  m = d$slope[keep]
  if (length(v) < 30) {
    stop("Fewer than 30 usable readings to fit the Tobler function.", call. = FALSE)
  }

  # binned quantiles and a piecewise log-linear lm
  breaks = seq(floor(min(m) / bin_width) * bin_width, ceiling(max(m) / bin_width) * bin_width + bin_width, by = bin_width)
  bin = cut(m, breaks, include.lowest = TRUE)
  bins = NULL
  for (b in levels(bin)) {
    in_bin = which(bin == b)
    if (length(in_bin) >= min_bin) {
      bins = rbind(bins, data.frame(m = stats::median(m[in_bin]),
                                    v = stats::quantile(v[in_bin], tau, names = FALSE),
                                    n = length(in_bin)))
    }
  }
  if (is.null(bins) || nrow(bins) < 4) {
    stop("Not enough populated slope bins; lower 'min_bin' or widen 'bin_width'.", call. = FALSE)
  }
  w = bins$n
  fit_at = function(m1) {
    stats::lm(log(bins$v) ~ pmax(bins$m - m1, 0) + pmin(bins$m - m1, 0), weights = w)
  }
  rss = function(m1) sum(stats::weighted.residuals(fit_at(m1))^2)
  inner = stats::quantile(bins$m, c(0.1, 0.9), names = FALSE)
  m1 = stats::optimize(rss, interval = inner)$minimum
  coef = stats::coef(fit_at(m1))
  theta = c(v1 = exp(coef[[1]]), m1 = m1, c_plus = -coef[[2]], c_minus = -coef[[3]])

  pinball = function(r) sum(r * (tau - (r < 0)))
  if (method == "pinball") {
    v_cap = 2 * max(v)
    loss = function(p) {
      if (p[1] <= 0 || p[1] > v_cap || p[2] < slope_range[1] || p[2] > slope_range[2]) return(Inf)
      pinball(v - tobler_speed(m, p))
    }
    starts = list(theta, theta * c(1.1, 1, 1.2, 1.2), theta * c(0.9, 1, 0.8, 0.8),
                  theta + c(0, 0.03, 0, 0), theta - c(0, 0.03, 0, 0))
    best = NULL
    for (st in starts) {
      opt = stats::optim(st, loss, method = "Nelder-Mead", control = list(maxit = 4000, reltol = 1e-10))
      if (is.null(best) || opt$value < best$value) best = opt
    }
    theta = best$par
  }

  if (theta[[1]] > 10) {
    warning("v1 = ", round(theta[[1]], 1), " m/s is implausible for human locomotion; ",
            "check the time of the stream.", call. = FALSE)
  }
  r1 = 1 - pinball(v - tobler_speed(m, theta)) / pinball(v - stats::quantile(v, tau, names = FALSE))
  tibble::tibble(v1 = theta[[1]], m1 = theta[[2]], c_plus = theta[[3]], c_minus = theta[[4]],
                 tau = tau, n = length(v), r1 = r1, method = method)
}

#' Speed predicted by the Tobler function
#'
#' @description
#' Evaluates the asymmetric Tobler function fitted by [tobler_stream()].
#'
#' @param slope Slope (rise over run).
#' @param params Output of [tobler_stream()], or a numeric vector with
#'   `v1`, `m1`, `c_plus`, `c_minus` (in this order or named).
#'
#' @return Horizontal speed (m/s).
#' @export
#'
#' @examples
#' tobler_speed(c(-0.2, 0, 0.2), c(v1 = 3, m1 = -0.05, c_plus = 3.5, c_minus = -3.5))
tobler_speed = function(slope, params) {
  p = unlist(params[c("v1", "m1", "c_plus", "c_minus")])
  if (any(is.na(p))) {
    p = as.numeric(params)[1:4]
  }
  p = unname(p)
  ifelse(slope >= p[2],
         p[1] * exp(-p[3] * (slope - p[2])),
         p[1] * exp(-p[4] * (slope - p[2])))
}
