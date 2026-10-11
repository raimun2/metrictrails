#' Fractal dimension of a stream
#'
#' @description
#' Estimates the fractal dimension of the horizontal path of a stream with the
#' divider (Richardson) method. A divider of size `r` is walked along the path:
#' from the current point, the next point is the first point of the path at a
#' straight-line distance `r`. The measured length is
#' \eqn{L(r) = N(r) \cdot r + \textrm{remainder}}, and for a fractal curve
#' \eqn{L(r) \propto r^{1 - D}}, so the dimension is
#' \eqn{D = 1 - b}, where `b` is the slope of the least-squares regression of
#' \eqn{\log L(r)} on \eqn{\log r}.
#'
#' A straight line has `D = 1`; values above 1 measure how much length is
#' gained when the path is measured with finer steps, i.e. the geometric
#' complexity that makes distance and elevation gain depend on the spatial
#' resolution (Sanchez et al., 2025). Only horizontal 2D distance (UTM) is
#' used. Each segment is walked separately and their lengths are added.
#'
#' By default the divider sizes are `n_scales` values evenly spaced in log
#' scale between twice the median distance between moving readings (finer
#' dividers would only measure the straight segments between readings) and one
#' tenth of the path length.
#'
#' @param data A stream with `lon` and `lat`.
#' @param scales Optional vector of divider sizes in metres.
#' @param n_scales Number of divider sizes when `scales` is `NULL`.
#'
#' @return A list with `dimension` (the estimate), `r_squared` of the
#'   log-log regression, and `scaling`, a tibble with the divider size `r`,
#'   the number of steps `n` and the length `length` (m).
#'
#' @references
#' Mandelbrot, B. (1967). How long is the coast of Britain? Statistical
#' self-similarity and fractional dimension. *Science*, 156(3775), 636-638.
#' \doi{10.1126/science.156.3775.636}
#'
#' Sanchez, R., Egli, P. E., Jornet, K., Duggan, M., & Besomi, M. (2025). How
#' fractal complexity distorts distance and elevation gain in trail and
#' mountain running: The case for course measurement standardization.
#' *Proceedings of the Institution of Mechanical Engineers, Part P: Journal of
#' Sports Engineering and Technology*. \doi{10.1177/17543371251341660}
#'
#' @seealso [summarise_stream()], [resample_stream()]
#' @export
#'
#' @examples
#' f = system.file("extdata", "gpx_route.gpx", package = "metrictrails")
#' fd = fractal_stream(read_stream(f))
#' fd$dimension
#' fd$scaling
fractal_stream = function(data, scales = NULL, n_scales = 10) {
  check_columns(data, c("lon", "lat"), "fractal_stream")
  ok = !is.na(data$lon) & !is.na(data$lat)
  data = data[ok, ]
  epsg = utm_epsg(data$lon, data$lat)
  xy = to_metric(data$lon, data$lat, epsg)
  seg = segment_id(data)

  if (is.null(scales)) {
    step = sqrt(diff(xy[, 1])^2 + diff(xy[, 2])^2)
    step = step[seg[-1] == seg[-length(seg)]]
    total = sum(step)
    moving = step[step >= 0.1]
    if (length(moving) < 3) {
      stop("The stream does not move.", call. = FALSE)
    }
    r_min = 2 * stats::median(moving)
    r_max = total / 10
    if (r_max <= r_min) {
      stop("The path is too short for its resolution to estimate a fractal dimension.", call. = FALSE)
    }
    scales = exp(seq(log(r_min), log(r_max), length.out = n_scales))
  }

  n_steps = numeric(length(scales))
  lengths = numeric(length(scales))
  for (k in seq_along(scales)) {
    for (s in unique(seg)) {
      rows = which(seg == s)
      if (length(rows) < 2) next
      walk = divider_walk(xy[rows, 1], xy[rows, 2], scales[k])
      n_steps[k] = n_steps[k] + walk$n
      lengths[k] = lengths[k] + walk$length
    }
  }

  fit = stats::lm(log(lengths) ~ log(scales))
  list(
    dimension = 1 - unname(stats::coef(fit)[2]),
    r_squared = stats::cor(log(scales), log(lengths))^2,
    scaling = tibble::tibble(r = scales, n = n_steps, length = lengths)
  )
}

# Walk a divider of size r along the polyline (x, y). Returns the number of
# full steps and the measured length (steps * r + distance to the last point).
divider_walk = function(x, y, r) {
  n_pts = length(x)
  cx = x[1]
  cy = y[1]
  i = 1        # current segment: from point i to point i + 1
  u0 = 0       # position of the current point within segment i (0 to 1)
  n = 0
  repeat {
    # first segment whose end lies at least r away from the current point;
    # the path leaves the circle of radius r on that segment
    k = i
    while (k < n_pts && (x[k + 1] - cx)^2 + (y[k + 1] - cy)^2 < r^2 * (1 - 1e-9)) {
      k = k + 1
    }
    if (k >= n_pts) {
      return(list(n = n, length = n * r + sqrt((x[n_pts] - cx)^2 + (y[n_pts] - cy)^2)))
    }
    # solve |P_k + u (P_k+1 - P_k) - C| = r for the exit root
    dx = x[k + 1] - x[k]
    dy = y[k + 1] - y[k]
    fx = x[k] - cx
    fy = y[k] - cy
    a = dx^2 + dy^2
    b = 2 * (fx * dx + fy * dy)
    c = fx^2 + fy^2 - r^2
    u = (-b + sqrt(max(b^2 - 4 * a * c, 0))) / (2 * a)
    lower = if (k == i) u0 else 0
    u = min(max(u, lower), 1)
    cx = x[k] + u * dx
    cy = y[k] + u * dy
    i = k
    u0 = u
    n = n + 1
  }
}
