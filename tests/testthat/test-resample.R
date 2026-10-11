act = sanitize_stream(uniform_stream(read_stream(activity_file)))

test_that("resolution_stream reports the Nyquist resolution", {
  r = resolution_stream(act)
  expect_equal(nrow(r), 1)
  expect_equal(r$res, unname(stats::quantile(r$res, 1)))
  expect_gt(r$res, r$q50)
  both = resolution_stream(list(a = act, b = read_stream(route_file)))
  expect_equal(nrow(both), 2)
})

test_that("arclength resampling gives uniform spacing and keeps the ends", {
  # on a straight line the chord equals the arc length
  line = tibble::tibble(lon = seq(-70.60, -70.59, length.out = 50), lat = -33.4,
                        time = seq(0, 490, by = 10))
  out = resample_stream(line, res = 5)
  xy = sf::sf_project("EPSG:4326", "EPSG:32719", cbind(out$lon, out$lat))
  step = sqrt(diff(xy[, 1])^2 + diff(xy[, 2])^2)
  expect_lt(max(step) - min(step), 1e-6)
  expect_gte(min(step), 5 - 1e-6)
  expect_lt(max(step), 5.2)
  expect_equal(out$lon[c(1, nrow(out))], line$lon[c(1, 50)], tolerance = 1e-9)
  expect_equal(out$time[c(1, nrow(out))], c(0, 490), tolerance = 1e-6)

  r = resample_stream(act, res = 5)
  expect_true(all(diff(r$time) >= 0))
  expect_equal(r$time[nrow(r)], act$time[nrow(act)], tolerance = 1e-6)
  expect_equal(attr(r, "res"), 5)
})

test_that("coarser resolution shortens the path (fractal effect)", {
  l1 = path_length(resample_stream(act, res = 1))
  l20 = path_length(resample_stream(act, res = 20))
  expect_lt(l20, l1)
  nyq = resample_stream(act)
  expect_equal(attr(nyq, "res"), resolution_stream(act)$res)
})

test_that("simplify method and segments", {
  s = resample_stream(act, res = 5, method = "simplify")
  expect_lt(nrow(s), nrow(act))
  two = act
  two$segment[1201:nrow(two)] = 2L
  r = resample_stream(two, res = 5)
  expect_equal(sort(unique(r$segment)), c(1, 2))
  expect_error(resample_stream(act, res = -1), "positive")
})
