#' Read GNSS stream from file
#'
#' @description
#' Reads stream from a single gpx, tcx or fit file and stores it in a Tibble.
#' Allows to process files from Strava's bulk export, which includes compressed fit
#' files (.fit.gz)
#'
#' Keeps the original variable names from the raw files
#'
#' @param filename The file path
#' read_stream()
#'
#' @return tibble with stream readings
#' @export
#'
read_stream <- function(filename){
  # if file ends in "*fit.gz" then unzip it, maintaining the original file
  if(length(grep(".gz$",filename))==1) {
    tempdir <- tempdir()
    tempfile <- file.path(tempdir,  basename(filename))
    file.copy(from = filename, to   = tempfile, overwrite = TRUE)
    if(is.null(list.files(tempdir, basename(filename) , full.names = T))==FALSE){
      filename <- R.utils::gunzip(tempfile, overwrite = TRUE)
    }
    filename <- list.files(tempdir, basename(filename) , full.names = T)
  }

  # if fit file (including uncompressed file) use FITfileR library
  if(length(grep(".fit$",filename))==1){
    file_data <- FITfileR::readFitFile(filename)
    stream_df <- FITfileR::records(file_data)
    if(!is.data.frame(stream_df)){
      stream_df <-  plyr::rbind.fill(lapply(stream_df,function(y){as.data.frame((y))}))
    }
  } else if(length(grep(".gpx$",filename))==1) {
    stream_df <- read_gpx(filename)
  } else if(length(grep(".tcx$",filename))==1) {
    require(plyr)
    lines   <- readLines(filename)
    lines[1] <- gsub("^(.*?)[<]", "<", lines[1])
    doc <- XML::xmlParse(lines)
    nodes <- XML::getNodeSet(doc, "//ns:Trackpoint", "ns")
    rows <-  lapply(nodes, function(x) data.frame(XML::xmlToList(x) ))
    stream_df <- do.call("rbind.fill", rows)
  }

  if(is.null(stream_df)) {
    message("unsupported format, see https://www.alltrails.com/converter")
  }
  return(stream_df %>% tibble::as_tibble()) #return tibble with file stream
}


#' Read gpx file
#'
#' @description
#' reads and parse gpx from file. supports coordinates, elevation and timestamp
#'
#' @param filename The file path to the containing file
#' read_gpx()
#'
#' @return
#' @export
#'
read_gpx<- function(filename){
  pfile <- XML::htmlTreeParse(file = filename,
                              error = function(...) { },
                              useInternalNodes = T)

  eles <-    as.numeric(XML::xpathSApply(pfile, path = "//trkpt/ele|//rtept/ele", XML::xmlValue))
  times <-   XML::xpathSApply(pfile, path = "//trkpt/time|//rtept/time", XML::xmlValue)
  coords <- XML::xpathSApply(pfile, path = "//trkpt|//rtept", XML::xmlAttrs)
  lats <- as.numeric(coords["lat",])
  lons <- as.numeric(coords["lon",])

  stream <- data.frame(lat = lats, lon = lons)

  if(length(eles) == length(lats)){
    stream$ele = eles
  }
  if(length(times) == length(lats)){
    stream$timestamp = lubridate::as_datetime(times)
  }

  return(stream)
}

