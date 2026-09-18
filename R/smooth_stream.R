#' Smoothed stream coordinates
#'
#' @description
#' Generate model for lon, lat and ele in function of time. For stream without time, uses the GNSS reading count.
#' Applies local polynomial regression fitting
#'
#' @param data dataframe or tibble containing a GNSS stream with lat, lon and ele columns
#' @param alpha smoothing parameter as a percentage of readings used for smoothing.
#' @param interpolate binary variable, if true generates an interpolated stream with 1 second difference between readins
#' @param replace binary variable, if true replaces original fields for smoothed, else, generate new columns. only applies when interpolate is false
#'
#' @return dataframe with smoothed coordinates and elevation if available
#' @export
#'
smooth_stream <- function(data, alpha = 0.05, interpolate = FALSE, replace = TRUE){

  # generate model for lon, lat and ele in function of time (or nrow in absence of time)
  # for routes without time uses the GPS readings cumulative count
  if(is.null(data$time)){
    data$reading <- 1:nrow(data)
    lonmodel <- loess(data$lon~data$reading,span = alpha,family = "gaussian",control=loess.control(surface="direct"))
    latmodel <- loess(data$lat~data$reading,span = alpha,family = "gaussian",control=loess.control(surface="direct"))
    if(exists("ele",data)) elemodel <- loess(data$ele~data$reading,span = alpha,family = "gaussian",control=loess.control(surface="direct"))
    data$reading <- NULL

  } else{
    lonmodel <- loess(data$lon~data$time,span = alpha,family = "gaussian",control=loess.control(surface="direct"))
    latmodel <- loess(data$lat~data$time,span = alpha,family = "gaussian",control=loess.control(surface="direct"))
    if(exists("ele",data)) elemodel <- loess(data$ele~data$time,span = alpha,family = "gaussian",control=loess.control(surface="direct"))
  }

  # if interpolate is false smooth recorded coordinates
  if(!interpolate){
    if(replace){
      data$lon <- predict(lonmodel)
      data$lat <- predict(latmodel)
      if(exists("ele",data)) data$ele <- predict(elemodel)
    } else{
      data$lon_smth <- predict(lonmodel)
      data$lat_smth <- predict(latmodel)
      if(exists("ele",data)) data$ele_smth <- predict(elemodel)
    }
    return(data) #return enhanced dataframe

  } else if(exists("time", data)){

    smoothActivity <- data.frame("time"=1:(max(data$time)))
    smoothActivity$lon <- stats::predict(lonmodel,smoothActivity$time)
    smoothActivity$lat <- stats::predict(latmodel,smoothActivity$time)
    if(exists("ele",data)) smoothActivity$ele <- stats::predict(elemodel,smoothActivity$time)
    return(smoothActivity) # return interpolated dataframe

  }

}
