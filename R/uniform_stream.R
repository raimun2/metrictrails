#' Uniform streams into name convention
#'
#' @description
#' Rename streams according to convention lon (for longitude), lat (for latitude),
#' ele (for elevation or altitude), timestamp (for time signature of reading) and time (for seconds from start).
#' Other variables such as distance, power, cadence or heartrate are left as they were on original file
#'
#'@param data is a dataframe or tibble containing a GPS stream. See read_stream documentation
#'
#' uniform_stream()
#'
#' @return dataframe stream with coordinates and optionally elevation, time.
#' @export
#'
uniform_stream <- function(data){

  # rename variables as convention lon, lat, ele, timestamp and time
  colnames(data)[grepl("(l|L)on|(l|L)ng", colnames(data))] <- "lon"
  colnames(data)[grepl("(l|L)at", colnames(data))] <- "lat"
  colnames(data)[grepl("(E|e)le|(a|A)lti", colnames(data))] <- "ele"

  timecols <- grepl("(t|T)ime", colnames(data))

  if(sum(timecols)==1){
    df_time <- data %>% dplyr::pull(colnames(data)[timecols])
    if(is.character(df_time)){
      df_time <- lubridate::as_datetime(df_time)
    }
    if(lubridate::is.POSIXct(df_time)){
      colnames(data)[grepl("(t|T)ime", colnames(data))] <- "timestamp"
      data$timestamp <- lubridate::as_datetime(df_time)
      data$time <- as.numeric(data$timestamp - min(data$timestamp))
    } else {
      colnames(data)[grepl("(t|T)ime", colnames(data))] <- "time"
      data$time <- as.numeric(df_time)
    }
    data <- data %>% dplyr::relocate(time)
  }

  data$lon <- as.numeric(data$lon)
  data$lat <- as.numeric(data$lat)
  if(exists("ele",data)){
    data$ele <- as.numeric(data$ele)
    data <- data %>% dplyr::relocate(ele)
  }

  data <- data %>% dplyr::relocate(lon, lat)

  return(data)
}
