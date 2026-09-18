#' Obtain elevation values from external source for stream dataframe
#'
#' @description
#' Uses SRTM data for obtaining elevation for each reading. Also supports external DEM in raster format.
#' If number of readings exceeds 200 points downloads a raster, else downloads each point at a time.
#' If local raster available, extract elevations directly
#'
#' @param data dataframe or tibble containing a GNSS stream
#' @param z zoom of Digital Elevation Model, between 6 and 14
#' @param raster raster object with DEM values
#' @param replace binary variable. Default value = TRUE. Defines whether to create a new column or replace the raw elevation
#'
#' @return GNSS stream dataframe with new variable 'ele_DEM'
#' @export
#'

ele_correction <- function(data, z = 13, raster=NULL, replace=TRUE){

  if(exists("lat", data) & exists("lon", data)){
   
    puntos <- sf::st_as_sf(data, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
       
    if(is.null(raster)){
      if(nrow(data)<200){
        ele_DEM <- elevatr::get_elev_point(locations = puntos, units = "meters", src="aws", z=z)$elevation
      } else {
        raster <- elevatr::get_elev_raster(locations = puntos, units = "meters", src="aws", z=z)
        ele_DEM <- raster::extract(raster, puntos)
      }
    } else {
      ele_DEM <- raster::extract(raster, puntos)
    }

    if(replace){
      data$ele <- ele_DEM
    } else {
      data$ele_DEM <- ele_DEM
    }
    return(data)
  } else{
    message("data must have lon and lat coordinates")
  }

}
