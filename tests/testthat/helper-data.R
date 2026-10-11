activity_file = system.file("extdata", "gpx_activity.gpx", package = "metrictrails")
route_file = system.file("extdata", "gpx_route.gpx", package = "metrictrails")
dem_file = system.file("extdata", "test_dem.tif", package = "metrictrails")

# planar distance helper used in the expectations (metres, UTM 19S)
path_length = function(stream) {
  xy = sf::sf_project("EPSG:4326", "EPSG:32719", cbind(stream$lon, stream$lat))
  sum(sqrt(diff(xy[, 1])^2 + diff(xy[, 2])^2))
}
