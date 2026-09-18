#' Compare 2 streams
#'
#' @description
#' Evaluates if 2 streams belongs to the same route. Checks for distance to start and end of route, for completion of track
#' and for staying on the right track
#'
#' @param data dataframe or tibble containing a GNSS stream
#' @param route another stream / route to be validated
#' @param tolerance threshold. Default setting on 50 meters
#'
#' @return either coordinates of match section(s) of data or character with explanation of mismatch
#' @export
#'
match_stream <- function(data, route, tolerance = 50){
  matchs <- NULL
  tolerance_completion <- 0
  tolerance_direction <- .03

  if(exists("lat",data) & exists("lon", data) & exists("lat", route) & exists("lon", route)){

    data <- data %>% select(lon,lat) %>% differential_stream()
    route <- route %>% select(lon,lat) %>% differential_stream()
    route$coverage <- route$distance / max(route$distance)

    dist_to_degrees <- tolerance * sqrt(abs(data$lon[1] - route$lon[nrow(route)/2])^2 + abs(data$lat[1] - route$lat[nrow(route)/2])^2) /
      geosphere::distm(cbind(data$lon[1],data$lat[1]),
                       cbind(route$lon[nrow(route)/2],route$lat[nrow(route)/2]),
                       fun = geosphere::distHaversine)

    data_sf <- data %>%
      sf::st_as_sf(coords = c("lon", "lat"), agr = "constant") %>%
      summarise(do_union = FALSE) %>%
      sf::st_cast("LINESTRING") %>%
      sf::st_buffer(dist = dist_to_degrees)

    route_sf <- route %>%
      sf::st_as_sf(coords = c("lon", "lat"), agr = "constant") %>%
      summarise(do_union = FALSE) %>%
      sf::st_cast("LINESTRING") %>%
      sf::st_buffer(dist = dist_to_degrees)

    if(sf::st_intersects(data_sf, route_sf, sparse=FALSE)){
      data_sf_pts <- data %>%
        sf::st_as_sf(coords = c("lon", "lat"), agr = "constant") %>%
        summarise(do_union = FALSE) %>%
        sf::st_cast("POINT") %>%
        sf::st_buffer(dist = dist_to_degrees)

      route_sf_pts <- route %>%
        sf::st_as_sf(coords = c("lon", "lat"), agr = "constant") %>%
        summarise(do_union = FALSE) %>%
        sf::st_cast("POINT") %>%
        sf::st_buffer(dist = dist_to_degrees)

      inter_pts_rt <- sf::st_intersects(route_sf_pts, data_sf, sparse=FALSE)

      if(sum(inter_pts_rt)>=nrow(route)*(1-tolerance_completion)){
        start <- route_sf_pts[1,]

        inter_pts_start <- sf::st_intersects(data_sf_pts, start, sparse=FALSE)

        pass_start <- which(inter_pts_start == TRUE)
        pass_start <- c(1,pass_start[c(0,diff(pass_start))>1])

        for(i in 1:length(pass_start)){
          sub_data <- data[pass_start[i]:nrow(data_sf_pts),]
          sub_data$distance <- cumsum(sub_data$delta_distance)
          sub_data <- sub_data %>% filter(distance <= max(route$distance)+tolerance)

          if(nrow(sub_data)> 1){
            sub_data$coverage <- sub_data$distance / max(sub_data$distance)

            sub_data_sf <- sub_data %>%
              sf::st_as_sf(coords = c("lon", "lat"), agr = "constant") %>%
              summarise(do_union = FALSE) %>%
              sf::st_cast("LINESTRING") %>%
              sf::st_buffer(dist = dist_to_degrees)

            inter_subpts_rt <- sf::st_intersects(route_sf_pts, sub_data_sf, sparse=FALSE)

            order_data <- sub_data[round((1:10)*nrow(sub_data)/10),]
            order_route <- route[round((1:10)*nrow(route)/10),]

            dist_ord <- (mean(diag(geosphere::distm(cbind(order_data$lon,order_data$lat),
                             cbind(order_route$lon,order_route$lat),
                             fun = geosphere::distHaversine))))
            if(mean(inter_subpts_rt)>=(1-tolerance_direction)){
              if(dist_ord < 500){

                matchs <- rbind(matchs,
                                c(pass_start[i], nrow(sub_data))) #returns position and length of match within data

              } else {
                matchs <- paste0("stream match in reverse order")
              }
            }
            if(is.null(matchs)) {
              matchs <- paste0("stream match partially with route: ", round(mean(inter_subpts_rt),2)*100,"% overlap")
            }
          }
        }
      } else {
        matchs <- "streams intersect but dont match"
      }
    } else {
      matchs <- "streams do not intersect"
    }
  } else {
    matchs <- "data and route must contain lon and lat columns"
  }
  return(matchs)
}
