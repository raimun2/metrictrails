act = uniform_stream(read_stream(activity_file))

test_that("every smoothing method keeps the readings and shortens the noisy path", {
  raw_length = path_length(act)
  for (m in c("kalman", "loess", "gaussian", "savgol", "pipeline")) {
    s = sanitize_stream(act, method = m)
    expect_equal(nrow(s), nrow(act))
    expect_false(anyNA(s$lon))
    expect_lt(path_length(s), raw_length)
  }
  expect_equal(sanitize_stream(act, method = "none")$lon, act$lon)
})

test_that("red flags detect injected errors", {
  bad = act
  bad$lat[500] = bad$lat[500] + 0.01   # ~1.1 km jump in a few seconds
  bad$lat[700] = 95                    # impossible latitude
  bad$ele[900] = bad$ele[900] + 80     # elevation spike
  bad$time[1200] = bad$time[1199]      # repeated time
  s = sanitize_stream(bad, method = "none")
  expect_equal(nrow(s), nrow(bad) - 1)
  rep = attr(s, "sanitize_report")
  expect_equal(rep$flag_coord, 1)
  expect_true(s$flag_speed[500])
  expect_true(s$flag_accel[500])
  expect_true(s$flag_vert_speed[899])
  expect_true(s$flag_time[1199])
  expect_false(rep$flag_ele_rounding)

  dropped = sanitize_stream(bad, method = "none", action = "drop")
  expect_lt(nrow(dropped), nrow(s))
  interp = sanitize_stream(bad, method = "none", action = "interpolate")
  expect_equal(nrow(interp), nrow(s))
  expect_lt(abs(interp$lat[500] - act$lat[500]), 0.001)
})

test_that("sanitize_stream works without time and with a Tobler ceiling", {
  route = read_stream(route_file)
  s = sanitize_stream(route, method = "savgol")
  expect_equal(nrow(s), nrow(route))
  expect_error(sanitize_stream(route, method = "gaussian"), "time")
  tob = c(v1 = 3, m1 = -0.05, c_plus = 3.5, c_minus = -3.5)
  s2 = sanitize_stream(act, method = "none", tobler = tob)
  expect_gte(sum(s2$flag_speed), sum(sanitize_stream(act, method = "none")$flag_speed))
})
