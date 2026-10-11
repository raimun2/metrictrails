act = uniform_stream(read_stream(activity_file))

test_that("differential_stream computes consistent derivatives", {
  d = differential_stream(act)
  expect_equal(d$distance[nrow(d)], sum(d$delta_d))
  expect_equal(sum(d$dplus) - sum(d$dminus), act$ele[nrow(act)] - act$ele[1])
  expect_true(all(d$dplus >= 0 & d$dminus >= 0))
  expect_true(is.na(d$speed[1]))
  i = 100
  expect_equal(d$speed[i], d$delta_d[i] / d$delta_time[i])
  expect_equal(d$accel[i], (d$speed[i] - d$speed[i - 1]) / d$delta_time[i])
  expect_true(all(d$turn_angle >= 0 & d$turn_angle <= 180, na.rm = TRUE))
  h = differential_stream(act, distance = "haversine")
  expect_lt(abs(sum(h$delta_d) / sum(d$delta_d) - 1), 0.005)
})

test_that("summarise_stream aggregates per group", {
  s = summarise_stream(act)
  expect_equal(nrow(s), 1)
  expect_equal(s$km_effort, s$distance_2d / 1000 + s$elevation_gain / 100)
  act$half = ifelse(seq_len(nrow(act)) <= 1255, "a", "b")
  g = summarise_stream(act, by = "half")
  expect_equal(nrow(g), 2)
  expect_equal(g$half, c("a", "b"))
})

test_that("tobler_stream fits a frontier with both estimators", {
  s = act %>% sanitize_stream() %>% resample_stream(res = 2) %>% ele_correction(dem = dem_file)
  for (m in c("binned", "pinball")) {
    p = tobler_stream(s, method = m)
    expect_gt(p$v1, 1)
    expect_lt(p$v1, 8)
    expect_gt(p$c_plus, 0)
    expect_gt(p$r1, 0)
  }
  pb = tobler_stream(s, method = "pinball")
  # the fitted curve should leave about 1 - tau of the readings above it
  d = differential_stream(s)
  ok = is.finite(d$speed) & is.finite(d$slope) & d$speed > 0.1 & abs(d$slope) <= 0.6
  above = mean(d$speed[ok] > tobler_speed(d$slope[ok], pb))
  expect_lt(abs(above - 0.1), 0.05)
  expect_error(tobler_stream(read_stream(route_file)), "needs column")
})

test_that("tobler_speed accepts named and unnamed parameters", {
  p = c(v1 = 3, m1 = -0.05, c_plus = 3.5, c_minus = -3.5)
  expect_equal(tobler_speed(-0.05, p), 3)
  expect_equal(tobler_speed(0.2, unname(p)), tobler_speed(0.2, p))
  expect_lt(tobler_speed(0.3, p), tobler_speed(0, p))
  expect_lt(tobler_speed(-0.4, p), tobler_speed(-0.1, p))
})

test_that("fractal_stream recovers known dimensions", {
  line = tibble::tibble(lon = seq(-70.60, -70.50, length.out = 2000), lat = -33.4)
  expect_equal(fractal_stream(line)$dimension, 1, tolerance = 1e-3)

  # Koch curve: exact lengths at its self-similar scales, D = log(4) / log(3)
  koch = rbind(c(0, 0), c(9000, 0))
  for (depth in 1:5) {
    out = koch[1, , drop = FALSE]
    for (i in 1:(nrow(koch) - 1)) {
      a = koch[i, ]
      d = (koch[i + 1, ] - a) / 3
      peak = a + d + c(d[1] * cos(pi / 3) - d[2] * sin(pi / 3), d[1] * sin(pi / 3) + d[2] * cos(pi / 3))
      out = rbind(out, a + d, peak, a + 2 * d, koch[i + 1, ])
    }
    koch = out
  }
  ll = sf::sf_project("EPSG:32719", "EPSG:4326", cbind(koch[, 1] + 350000, koch[, 2] + 6300000))
  kc = tibble::tibble(lon = ll[, 1], lat = ll[, 2])
  fd = fractal_stream(kc, scales = 9000 / 3^(1:4))
  expect_equal(fd$scaling$length, 9000 * (4 / 3)^(1:4), tolerance = 1e-4)
  expect_equal(fd$dimension, log(4) / log(3), tolerance = 1e-4)

  s = summarise_stream(read_stream(activity_file), fractal = TRUE)
  expect_gt(s$fractal_dim, 1)
  expect_lt(s$fractal_dim, 1.5)
})
