#' Download a Digital Elevation Model for a stream
#'
#' @description
#' Downloads the Terrain Tiles (GeoTIFF) hosted on Amazon Web Services that
#' intersect a corridor of `buffer` metres around the stream, mosaics them and
#' projects the result to the UTM zone of the stream. Only the tiles along the
#' path are downloaded, not every tile of its bounding box; cells of the
#' bounding box outside those tiles are `NA`. The tiles combine SRTM and other sources; their
#' resolution depends on the zoom level `z`. The default, `z = 14`, is the
#' finest level (about 4 m at the latitude of Santiago, Chile, and 4.8 m at the
#' equator) and is used for every stream regardless of its extent, so that
#' elevations are always comparable; large areas simply need more tiles.
#'
#' When `res` is given, the DEM is refined (or coarsened) with
#' [resample_dem()]. Refining a public DEM with bilinear interpolation to
#' sub-metre resolution reduces elevation gain errors to levels comparable to
#' a LiDAR model (Sanchez et al., 2024).
#'
#' @param data A stream with `lon` and `lat`.
#' @param z Zoom level of the tiles, from 1 to 14 (default 14).
#' @param res Optional target resolution in metres, passed to [resample_dem()].
#' @param method Resampling method passed to [resample_dem()].
#' @param buffer Width (m) of the corridor around the stream.
#' @param max_tiles Optional maximum number of tiles to download (`Inf` by
#'   default). A message reports the number of tiles when it exceeds 50.
#'
#' @return A `terra::SpatRaster` in the UTM zone of the stream.
#'
#' @references
#' Sanchez, R., Egli, P., Besomi, M., & Truffello, R. (2024). Assessing the
#' impact of Digital Elevation Model resolution on elevation gain estimations
#' in trail running. *2024 International Conference on Electrical, Computer
#' and Energy Technologies (ICECET)*. \doi{10.1109/ICECET61485.2024.10698606}
#'
#' Terrain Tiles on AWS: <https://registry.opendata.aws/terrain-tiles/>
#'
#' @export
#'
#' @examples
#' \donttest{
#' f = system.file("extdata", "gpx_route.gpx", package = "metrictrails")
#' dem = get_dem(read_stream(f))
#' dem
#' }
get_dem = function(data, z = 14, res = NULL, method = "bilinear", buffer = 500, max_tiles = Inf) {
  check_columns(data, c("lon", "lat"), "get_dem")
  if (z < 1 || z > 14) {
    stop("'z' must be between 1 and 14.", call. = FALSE)
  }
  epsg = utm_epsg(data$lon, data$lat)
  xy = to_metric(data$lon, data$lat, epsg)
  box = c(min(xy[, 1], na.rm = TRUE) - buffer, min(xy[, 2], na.rm = TRUE) - buffer,
          max(xy[, 1], na.rm = TRUE) + buffer, max(xy[, 2], na.rm = TRUE) + buffer)
  corners = to_lonlat(box[c(1, 1, 3, 3)], box[c(2, 4, 2, 4)], epsg)

  # slippy map tiles that intersect a corridor of `buffer` metres around the
  # path (not the whole bounding box): the path is densified, and around every
  # point a grid of offsets finer than half a tile is sampled
  tile_x = function(lon) floor((lon + 180) / 360 * 2^z)
  tile_y = function(lat) {
    r = lat * pi / 180
    floor((1 - log(tan(r) + 1 / cos(r)) / pi) / 2 * 2^z)
  }
  ok = stats::complete.cases(xy)
  seg = segment_id(data)[ok]
  xy = xy[ok, , drop = FALSE]
  max_lat = max(abs(data$lat), na.rm = TRUE)
  tile_m = 40075016.7 * cos(max_lat * pi / 180) / 2^z
  spacing = min(buffer, tile_m / 2)
  px = NULL
  py = NULL
  for (sg in unique(seg)) {
    rows = which(seg == sg)
    s_along = c(0, cumsum(sqrt(diff(xy[rows, 1])^2 + diff(xy[rows, 2])^2)))
    s_new = unique(c(seq(0, max(s_along), by = max(spacing, 1)), max(s_along)))
    if (length(rows) == 1) {
      px = c(px, xy[rows, 1])
      py = c(py, xy[rows, 2])
    } else {
      px = c(px, stats::approx(s_along, xy[rows, 1], xout = s_new, ties = mean)$y)
      py = c(py, stats::approx(s_along, xy[rows, 2], xout = s_new, ties = mean)$y)
    }
  }
  offsets = seq(-buffer, buffer, length.out = max(2, ceiling(2 * buffer / (tile_m / 2)) + 1))
  grid = expand.grid(dx = offsets, dy = offsets)
  tiles = NULL
  for (k in seq_len(nrow(grid))) {
    ll = to_lonlat(px + grid$dx[k], py + grid$dy[k], epsg)
    tiles = unique(rbind(tiles, cbind(tile_x(ll[, "lon"]), tile_y(ll[, "lat"]))))
  }
  n_tiles = nrow(tiles)
  if (n_tiles > max_tiles) {
    stop(n_tiles, " tiles needed, more than 'max_tiles'.", call. = FALSE)
  }
  if (n_tiles > 50) {
    message("Downloading ", n_tiles, " terrain tiles at zoom ", z, ".")
  }

  old_timeout = options(timeout = max(300, getOption("timeout")))
  on.exit(options(old_timeout), add = TRUE)
  files = character(0)
  for (k in seq_len(n_tiles)) {
    tx = tiles[k, 1]
    ty = tiles[k, 2]
    url = sprintf("https://s3.amazonaws.com/elevation-tiles-prod/geotiff/%d/%d/%d.tif", z, tx, ty)
    dest = file.path(tempdir(), sprintf("metrictrails_dem_%d_%d_%d.tif", z, tx, ty))
    # a tile enters the cache only after it has been read completely, so an
    # interrupted transfer is never reused (truncated GeoTIFFs can crash GDAL)
    attempt = 0
    while (!file.exists(dest)) {
      attempt = attempt + 1
      if (attempt > 3) {
        stop("Could not download a valid tile from ", url, call. = FALSE)
      }
      part = paste0(dest, ".part")
      status = tryCatch(utils::download.file(url, part, mode = "wb", quiet = TRUE),
                        error = function(e) 1)
      valid = status == 0 && tryCatch({
        tile = terra::rast(part)
        length(terra::values(tile, mat = FALSE)) == terra::ncell(tile)
      }, error = function(e) FALSE)
      if (valid) {
        file.rename(part, dest)
      } else {
        unlink(part)
      }
    }
    files = c(files, dest)
  }

  tiles = lapply(files, terra::rast)
  mosaic = if (length(tiles) == 1) tiles[[1]] else do.call(terra::merge, tiles)
  dem = terra::project(mosaic, paste0("EPSG:", epsg), method = "bilinear")
  dem = terra::crop(dem, terra::ext(box[1], box[3], box[2], box[4]))
  names(dem) = "ele"
  if (!is.null(res)) {
    dem = resample_dem(dem, res, method = method)
  }
  dem
}

#' Change the resolution of a DEM
#'
#' @description
#' Refines or coarsens a Digital Elevation Model.
#'
#' * `"bilinear"` and `"near"` resample the DEM to a grid of `res` metres.
#'   Bilinear interpolation is recommended to refine a coarse public DEM;
#'   nearest neighbour performs poorly at fine resolutions (Sanchez et al.,
#'   2024).
#' * `"mean"`, `"median"`, `"min"` and `"max"` aggregate blocks of cells to a
#'   coarser resolution (factor `round(res / current resolution)`).
#'
#' @param dem A `terra::SpatRaster` (or a path to a raster file) in a metric
#'   CRS, such as the output of [get_dem()].
#' @param res Target resolution in metres.
#' @param method One of `"bilinear"`, `"near"`, `"mean"`, `"median"`, `"min"`,
#'   `"max"`.
#'
#' @return A `terra::SpatRaster`.
#'
#' @references
#' Sanchez, R., Egli, P., Besomi, M., & Truffello, R. (2024). Assessing the
#' impact of Digital Elevation Model resolution on elevation gain estimations
#' in trail running. *2024 International Conference on Electrical, Computer
#' and Energy Technologies (ICECET)*. \doi{10.1109/ICECET61485.2024.10698606}
#'
#' @export
#'
#' @examples
#' dem = terra::rast(system.file("extdata", "test_dem.tif", package = "metrictrails"))
#' dem_utm = terra::project(dem, "EPSG:32719")
#' resample_dem(dem_utm, res = 5)
resample_dem = function(dem, res, method = c("bilinear", "near", "mean", "median", "min", "max")) {
  method = match.arg(method)
  if (is.character(dem)) {
    dem = terra::rast(dem)
  }
  if (terra::is.lonlat(dem)) {
    stop("The DEM must be in a metric CRS; project it first (e.g. terra::project()).", call. = FALSE)
  }
  if (method %in% c("bilinear", "near")) {
    template = terra::rast(terra::ext(dem), resolution = res, crs = terra::crs(dem))
    return(terra::resample(dem, template, method = method))
  }
  fact = round(res / terra::res(dem)[1])
  if (fact < 2) {
    stop("Aggregation needs 'res' to be at least twice the current resolution (",
         signif(terra::res(dem)[1], 3), " m).", call. = FALSE)
  }
  terra::aggregate(dem, fact = fact, fun = method, na.rm = TRUE)
}

#' Replace the elevation of a stream with values from a DEM
#'
#' @description
#' Extracts the elevation of every reading from a Digital Elevation Model and
#' replaces the raw elevation (or adds it as `ele_dem`). Elevation from an open
#' DEM gives consistent elevation gain across devices, independently of the
#' altitude sensor (Sanchez & Villena, 2020). This is the last step of the
#' pipeline, after [resample_stream()].
#'
#' @param data A stream with `lon` and `lat`.
#' @param dem A `terra::SpatRaster` or a path to a raster file. When `NULL`,
#'   a DEM is downloaded with [get_dem()] at zoom `z`.
#' @param z Zoom level used when `dem` is `NULL` (see [get_dem()]).
#' @param extract `"bilinear"` (interpolate between the four nearest cells) or
#'   `"simple"` (value of the cell).
#' @param replace If `TRUE`, `ele` is replaced; otherwise the column `ele_dem`
#'   is added.
#'
#' @return The stream with the new elevation.
#'
#' @references
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
#' dem = system.file("extdata", "test_dem.tif", package = "metrictrails")
#' stream = ele_correction(read_stream(f), dem = dem, replace = FALSE)
#' summary(stream$ele - stream$ele_dem)
ele_correction = function(data, dem = NULL, z = 14, extract = c("bilinear", "simple"), replace = TRUE) {
  extract = match.arg(extract)
  check_columns(data, c("lon", "lat"), "ele_correction")
  if (is.null(dem)) {
    dem = get_dem(data, z = z)
  }
  if (is.character(dem)) {
    dem = terra::rast(dem)
  }
  pts = sf::sf_project(from = "EPSG:4326", to = terra::crs(dem),
                       pts = cbind(data$lon, data$lat), keep = TRUE)
  ele = terra::extract(dem[[1]], pts, method = extract)[, 1]
  if (any(is.na(ele))) {
    warning(sum(is.na(ele)), " readings fall outside the DEM and get NA elevation.", call. = FALSE)
  }
  if (replace) {
    data$ele = ele
  } else {
    data$ele_dem = ele
  }
  data
}
