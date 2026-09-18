# pacman::p_load(tidyverse, googlePolylines, metrictrails)
#
# ## genera samples
# todo_strava <- read_rds("C:/Users/raimu/Dropbox/Github/30DayMapChallenge/data/streams_rai1058.rds")
#
# mqhue <- todo_strava$act_data[grepl("manquehue",todo_strava$act_data$name),]
#
# strava_metadata <- mqhue %>% filter(total_elevation_gain > 850, total_elevation_gain<1000, distance < 9.55)
#
# bicis <- todo_strava$act_data[todo_strava$act_data$type == "Ride",] %>% drop_na(average_temp) %>%
#   filter(distance > 15 & distance < 30, elapsed_time > 4000)
#
# rides_streams <-  todo_strava$streams %>% filter(id %in% bicis$id)
#
# strava_streams <- todo_strava$streams %>% filter(id %in% strava_metadata$id)
#
# strava_metadata <- rbind(strava_metadata, bicis)
#
# strava_streams <- rbind(strava_streams, rides_streams)
#
# usethis::use_data(strava_metadata, overwrite = TRUE)
# usethis::use_data(strava_streams, overwrite = TRUE)


#ggplot(strava_streams, aes(lng, lat)) + geom_point()

# # prueba ext datas
# dir <- "inst/extdata/"
#
# testfiles <- list.files(dir)
#
# strs <- NULL
# for(fil in testfiles){
#   stream <- read_stream(paste0(dir,fil))  %>% uniform_stream()
#
#   stream$file <- fil
#
#   strs <- plyr::rbind.fill(strs, stream)
#
# }

# library(GPStream)
# library(tidyverse)
#
#
# clean_stream1 <- act_streams %>% uniform_stream() %>% differential_stream()
#
# strems_segmentation <- clean_stream2 %>% differential_stream()
#
# agg_stream(strems_segmeation)
#
# a2 <- agg_stream(clean_stream2 %>% differential_stream())
#
# clean_stream2 <-  read_stream("inst/extdata/gpx_activity.gpx") %>% uniform_stream()
#
# match_stream(clean_stream1, clean_stream2)
#
#
#
#
# strs <- strava_streams %>% uniform_stream() %>% differential_stream()
#
# for(act1 in unique(strs$id)){
#   subi <- strs %>% filter(id == act1)
#   for(act2 in unique(strs$id)){
#     subj <- strs %>% filter(id == act2)
#     if(act1!=act2){
#
#       m <- match_stream(subi, subj)
#       if(is.matrix(m)){
#         (ggplot(mapping=aes(lon,lat)) +
#           geom_point(data=subi, col="red") +
#           geom_point(data=subj, col="blue", size=0.7) )
#       }
#
#     }
#   }
# }
#
# data <- subi
# route <- subj
#
# match_stream(data, route)


