act = uniform_stream(read_stream(activity_file))

test_that("ele_correction extracts DEM elevation", {
  s = ele_correction(act, dem = dem_file, replace = FALSE)
  expect_false(anyNA(s$ele_dem))
  expect_lt(stats::median(abs(s$ele - s$ele_dem)), 30)
  s2 = ele_correction(act, dem = terra::rast(dem_file), extract = "simple")
  expect_false(anyNA(s2$ele))
})

test_that("resample_dem refines and aggregates", {
  dem = terra::rast(dem_file)
  fine = resample_dem(dem, 2)
  expect_equal(terra::res(fine), c(2, 2))
  coarse = resample_dem(dem, 40, method = "mean")
  expect_gt(terra::res(coarse)[1], 30)
  expect_error(resample_dem(dem, 1, method = "max"), "twice")
  expect_error(resample_dem(terra::project(dem, "EPSG:4326"), 2), "metric")
})

test_that("get_dem downloads terrain tiles", {
  skip_on_cran()
  skip_if_not_installed("curl")
  skip_if_offline("s3.amazonaws.com")
  dem = get_dem(act[1:50, ], z = 10, buffer = 100)
  expect_s4_class(dem, "SpatRaster")
  expect_false(terra::is.lonlat(dem))
})
