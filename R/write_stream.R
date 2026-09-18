#' Write stream into GPX file
#'
#' @param data data stream
#' @param file name of the file
#'
#' @return
#' @export
#'
write_gpx <- function(data, file="file.gpx"){
  # Write stream data to a new gpx file
  lat <- data$lat
  lon <- data$lon
  ele <- data$ele
  if(exists("timestamp", data)){
    time <- data$timestamp

    o   <- c('<gpx version="1.1" creator="GPStream">','<trk>','<trkseg>')
    o   <- c(o, paste('<trkpt lat="',lat,'" lon="',lon,'"><time>',
                      paste("<ele>",ele,"</ele>",sep=""),
                      paste(gsub(' ','T', as.character(time)), 'Z', sep=''),'</time></trkpt>', sep=''))
    o   <- c(o, '</trkseg>', '</trk>', '</gpx>')
  } else {
    o   <- c('<gpx version="1.1" creator="GPStream">','<trk>','<trkseg>')
    o   <- c(o, paste('<trkpt lat="',lat,'" lon="',lon,'">',
                      paste("<ele>",ele,"</ele>",sep=""),
                      '</trkpt>', sep=''))
    o   <- c(o, '</trkseg>', '</trk>', '</gpx>')

  }
  cat(o, file=file, sep='\n')
}



#' Write stream
#'
#' @param data data stream
#' @param file name of the file
#'
#' @return
#' @export
#'
#' @examples
write_stream <- function(data, file){
  if(grepl("*.gpx", file)){
    write_gpx(data, file)
  } else {
    message("only gpx output supported")
  }
}
