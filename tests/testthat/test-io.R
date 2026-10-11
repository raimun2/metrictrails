test_that("read_stream reads tracks, routes and extensions", {
  act = read_stream(activity_file)
  expect_s3_class(act, "tbl_df")
  expect_equal(nrow(act), 2510)
  expect_true(all(c("lon", "lat", "ele", "timestamp", "segment", "hr", "cad", "atemp") %in% names(act)))
  expect_equal(act$hr[1], 106)
  expect_s3_class(act$timestamp, "POSIXct")

  route = read_stream(route_file)
  expect_equal(nrow(route), 410)
  expect_false("timestamp" %in% names(route))
})

test_that("read_stream rejects other formats and waypoint-only files", {
  expect_error(read_stream("missing.gpx"), "not found")
  tmp = tempfile(fileext = ".kml")
  writeLines("<kml/>", tmp)
  expect_error(read_stream(tmp), "only reads GPX")

  wpt = tempfile(fileext = ".gpx")
  writeLines(c('<?xml version="1.0"?>',
               '<gpx version="1.1" creator="test" xmlns="http://www.topografix.com/GPX/1/1">',
               '<wpt lat="-33.4" lon="-70.5"><name>a</name></wpt>', '</gpx>'), wpt)
  expect_error(read_stream(wpt), "waypoint")
})

test_that("write_stream round-trips coordinates, elevation, time and segments", {
  act = read_stream(activity_file)
  act$segment[1001:2510] = 2L
  out = tempfile(fileext = ".gpx")
  write_stream(act, out, name = "test")
  back = read_stream(out)
  expect_equal(nrow(back), nrow(act))
  expect_equal(back$lon, act$lon, tolerance = 1e-7)
  expect_equal(back$ele, act$ele, tolerance = 0.01)
  expect_equal(as.numeric(back$timestamp), as.numeric(act$timestamp))
  expect_equal(back$segment, act$segment)
})

test_that("uniform_stream applies the naming convention", {
  df = data.frame(Latitude = c(-33.40, -33.41), Longitude = c(-70.50, -70.51),
                  Altitude = c(800, 805), moving_time = c(0, 5), platform = c("a", "b"),
                  Time = c("2024-01-01T10:00:00Z", "2024-01-01T10:00:05Z"))
  u = uniform_stream(df)
  expect_equal(names(u)[1:5], c("lon", "lat", "ele", "timestamp", "time"))
  expect_true(all(c("moving_time", "platform") %in% names(u)))
  expect_equal(u$time, c(0, 5))

  s = uniform_stream(strava_streams)
  expect_equal(names(s)[1:4], c("lon", "lat", "ele", "time"))
  expect_error(uniform_stream(data.frame(lat = 1, latitude = 2, lon = 3)), "Several columns")
  expect_error(uniform_stream(data.frame(a = 1)), "needs column")
})

test_that("uniform_stream drops a constant time", {
  df = data.frame(lat = c(-33.40, -33.41, -33.42), lon = c(-70.50, -70.51, -70.52),
                  time = rep("2024-01-01T10:00:00Z", 3))
  expect_warning(u <- uniform_stream(df), "route without time")
  expect_false(any(c("time", "timestamp") %in% names(u)))
})
