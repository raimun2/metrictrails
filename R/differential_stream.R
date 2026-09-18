#' Spatial and temporal derivatives
#'
#' @description
#' Calculates spatial derivatives and if time fiel available, temporal derivatives. This includes
#' distance, speed, acceleration in vertical, horizontal and total dimension, azimut, slope and pace
#'
#' @param data dataframe or tibble containing a GNSS stream with at least lon and lat columns.
#'
#' @return dataframe with additional columns
#' @export
#'
differential_stream <- function(data){

  if(exists("lon",data) & exists("lat", data)){
    # previous lon / lat
    data$lat_p2 <- c(0,data$lat[-nrow(data)])
    data$lon_p2 <- c(0,data$lon[-nrow(data)])

    # 2 point previous
    data$lat_p3 <- c(0,data$lat_p2[-nrow(data)])
    data$lon_p3 <- c(0,data$lon_p2[-nrow(data)])

    # haversise distance between each point and previous one
    data$delta_distance <- geosphere::distHaversine(cbind(data$lon,data$lat),
                                                    cbind(data$lon_p2,data$lat_p2))
    data$delta_distance[1] <- 0

    # calculate cumulative distance
    data$distance <- cumsum(data$delta_distance)

    # calculate azimuthal angle
    vector1 <- cbind(data$lon-data$lon_p2, data$lat-data$lat_p2)
    vector2 <- cbind(data$lon_p3-data$lon_p2, data$lat_p3-data$lat_p2)
    num <- vector1[,1]*vector2[,1] + vector1[,2]*vector2[,2]
    den <- sqrt(vector1[,1]^2 + vector1[,2]^2)*sqrt(vector2[,1]^2 + vector2[,2]^2)
    data$azimuth <- suppressWarnings(ifelse(den == 0, 180, (360*acos(num/den))/(2*pi)))
    data$azimuth[is.na(data$azimuth)] <- 180

    data$lat_p2 <- NULL
    data$lon_p2 <- NULL
    data$lat_p3 <- NULL
    data$lon_p3 <- NULL

    ## 3d distance tambien
    if(exists("ele", data)){
      data$delta_ele <- c(0,diff(data$ele,lag=1))
      data$delta_ele[1] <- 0
      data$dplus <- ifelse(data$delta_ele>0, data$delta_ele, 0)
      data$dminus <- ifelse(data$delta_ele<0, data$delta_ele, 0)

      data$slope <- ( data$delta_ele / data$delta_distance )
      data$slope[1] <- 0
      data[data$delta_distance == 0,]$slope <- 0

    }
    if(exists("time", data)){
      # calculate difference in time and elevation between readings
      data$delta_time <- c(0,diff(data$time,lag=1))
      data$delta_time[1] <- 0
      data$hz_velocity <- data$delta_distance / data$delta_time
      data$hz_velocity[1] <- 0
      data$hz_accel <- data$hz_velocity / data$delta_time
      data$hz_accel[data$delta_time == 0] <- 0
      # calculate pace in minutes per kilometer
      data$pace <- c(0,1000/60/data$hz_velocity[-1])

      if(exists("ele",data)){
        # calculate derivatives: vertical and horizontal velocity, in meters per second
        data$vert_velocity <- data$delta_ele / data$delta_time
        data$vert_velocity[1] <- 0
        data$vert_accel <- data$vert_velocity / data$delta_time
        data$vert_accel[data$delta_time == 0] <- 0

        #3d velocity
        data$velocity <- sqrt(data$vert_velocity^2 + data$hz_velocity^2)
        data$accel <- sqrt(data$vert_accel^2 + data$hz_accel^2)
      }
    }
    return(data) # return enriched dataframe
  } else {
    message("lat and lon are needed to compute differences. see ? uniform_stream()")
  }
}
