act = uniform_stream(read_stream(activity_file))
route = read_stream(route_file)

test_that("compare_stream is zero for identical streams and grows with a shift", {
  a = resample_stream(route, res = 2)
  same = compare_stream(a, a)
  expect_equal(same$hausdorff, 0)
  expect_equal(same$metric_score_ab, 100)
  shifted = a
  shifted$lat = shifted$lat + 10 / 111320
  cmp = compare_stream(a, shifted)
  expect_lt(abs(cmp$hausdorff - 10), 0.1)
  expect_lte(cmp$separation_ab, cmp$hausdorff)
  expect_lt(cmp$metric_score_ab, 20)
  expect_equal(compare_stream(a, a, dims = 3)$hausdorff, 0)
})

test_that("detect_stream finds forward, reverse and absent passes", {
  fwd = detect_stream(act, route)
  expect_equal(nrow(fwd), 1)
  expect_equal(fwd$direction, "forward")
  expect_equal(fwd$status, "match")
  rev = detect_stream(act[nrow(act):1, ], route)
  expect_equal(rev$direction, "reverse")
  far = act
  far$lat = far$lat + 1
  none = detect_stream(far, route)
  expect_equal(nrow(none), 0)
  expect_equal(attr(none, "diagnosis"), "streams do not intersect")
})

test_that("match_stream aligns a stream monotonically along the reference", {
  ref = route %>% resample_stream(res = 2) %>% ele_correction(dem = dem_file)
  obs = act[1:600, ] %>% sanitize_stream() %>% ele_correction(dem = dem_file)
  params = act %>% sanitize_stream() %>% resample_stream(res = 2) %>%
    ele_correction(dem = dem_file) %>% tobler_stream()
  m = match_stream(obs, ref, params)
  expect_equal(nrow(m), nrow(obs))
  expect_true(all(diff(m$ref_index) >= 0))
  expect_equal(m$offset_d^2, m$offset_along^2 + m$offset_cross^2, tolerance = 1e-6)
  expect_lt(stats::median(m$offset_d), 15)
  ceiling = tobler_speed(pmin(pmax(m$slope_ref, -0.6), 0.6), params) * 1.1 *
    sqrt(1 + pmin(pmax(m$slope_ref, -0.6), 0.6)^2)
  expect_true(all(m$speed_total <= ceiling + 1e-6, na.rm = TRUE))
  rep = attr(m, "match_report")
  expect_equal(rep$direction, "forward")

  # a stream travelled backwards is matched in reverse
  back = obs[nrow(obs):1, ]
  back$time = rev(max(back$time) - back$time)
  mb = match_stream(back, ref, params)
  expect_equal(attr(mb, "match_report")$direction, "reverse")
  expect_true(all(diff(mb$ref_index) <= 0))

  # several rounds at once
  two = rbind(transform(obs, round = 1), transform(obs, round = 2))
  mr = match_stream(two, ref, params, by = "round")
  expect_equal(nrow(attr(mr, "match_report")), 2)
})

test_that("project_stream adds UTM coordinates", {
  p = project_stream(route)
  expect_equal(attr(p, "epsg"), 32719)
  expect_true(all(c("x", "y") %in% names(p)))
  expect_equal(p$x[1], sf::sf_project("EPSG:4326", "EPSG:32719", cbind(route$lon[1], route$lat[1]))[1, 1])
  expect_equal(attr(project_stream(route, epsg = 3857), "epsg"), 3857)
})

test_that("locate_stream projects readings on the reference", {
  ref = resample_stream(route, res = 2)
  self = locate_stream(ref, ref)
  expect_lt(max(abs(self$offset_cross)), 1e-6)
  expect_equal(self$s_ref, c(0, cumsum(sqrt(diff(project_stream(ref)$x)^2 + diff(project_stream(ref)$y)^2))),
               tolerance = 1e-6)

  # a copy shifted 10 m to the north is about 10 m away, on both sides of the loop
  shifted = ref
  shifted$lat = shifted$lat + 10 / 111320
  loc = locate_stream(shifted, ref)
  expect_lte(max(abs(loc$offset_cross)), 10)
  expect_true(any(loc$offset_cross > 5) && any(loc$offset_cross < -5))
  expect_false(anyNA(loc$lon_ref))

  # the sign follows the side: a point east of a northbound reference is on the right
  north = tibble::tibble(lon = -70.6, lat = seq(-33.40, -33.39, length.out = 20))
  east = tibble::tibble(lon = -70.6 + 20 / 93000, lat = -33.395)
  expect_lt(locate_stream(east, north)$offset_cross, 0)
})
