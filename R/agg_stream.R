#' Summarise segment
#'
#' @param data dataframe or tibble containing a stream segment
#'
#' @return
#' @export
#'
summarise_stream <- function(data){
  if(exists("delta_distance",data) &
     exists("dplus",data) &
     exists("distance",data) &
     exists("dminus",data) &
     exists("ele",data)){
    if(nrow(data)>0){
      if(exists("delta_time", data)){
        segment <-
          data %>%
          group_by(id_seg) %>%
          summarise(delta_time = sum(delta_time),
                    delta_distance = sum(delta_distance),
                    dplus = sum(dplus),
                    dminus = sum(dminus),
                    time = mean(time),
                    distance = mean(distance),
                    ele = mean(ele),
                    cad = mean(cadence),
                    sd_cad = sd(cadence),
                    HR = mean(heartrate),
                    speed = delta_distance / delta_time)
      }
      if(!exists("delta_time", data)){
        segment <-
          data %>%
          group_by(id_seg) %>%
          summarise(delta_distance = sum(delta_distance),
                    dplus = sum(dplus),
                    dminus = sum(dminus),
                    distance = mean(distance),
                    ele = mean(ele))
      }

    }
  } else {
    message("data must contain delta_distance and dplus and dminus, see ? differential_stream")
  }

    return(segment)
}


#' Rolling window segment
#'
#' @param data dataframe or tibble containing a GNSS stream
#' @param size split size
#' @param value time or distance of the size
#'
#' @return
#' @export
#'
rolling_window <- function(data, size = 100, value = "time"){

  windowed_data <- NULL
  if(value=="distance"){
    data$id_window <- round(data$distance)
  } else if(value == "time"){
    data$id_window <- cumsum(data$delta_time)
  }
  vector <- data$id_window
  vector <- vector[vector<=max(vector)-size]
  for(w in vector){
    data_wind <- data %>% filter(id_window >= w, id_window < (w + size))
    data_wind$id_seg <- paste0("seg",w)
    if(nrow(data_wind)>0 ){
      subwind <- summarise_stream(data_wind)
      windowed_data <- rbind(windowed_data, subwind)
    }

  }
  windowed_data <- unique(windowed_data)

  return(windowed_data)
}

#' Aggregate segments
#'
#' @param data dataframe or tibble containing a GNSS stream
#' @param size split size
#' @param value time or distance of the size
#' @param windowed if it uses rolling window to generate segments.
#'
#' @return
#' @export
#'

agg_stream = function(data, size = 100, value = "time", windowed = TRUE){

  if(windowed){
    segments <- rolling_window(data, size, value)
  } else {
    # if time split segments according to elapsed time, else cumulative distance
    if(value == "time"){
      data$id_seg <- floor(data$time / size)
    } else if(value == "distance"){
      data$id_seg <- floor(data$distance / size) #cumulative distance in meters
    }

    segments <- data %>%
      summarise_stream()
  }

  # return dataframe with segment features
  if(value == "time"){
    return(segments %>% dplyr::filter(delta_time >= size))
  } else if(value == "distance"){
    return(segments %>% dplyr::filter(delta_distance >= size))
  }
}

