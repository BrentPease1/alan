library(here)
library(data.table)
library(lubridate)
library(suncalc)
library(lutz)
library(sf)
library(activity)
library(stringr)
library(ggplot2)
library(scales)
library(MetBrewer)
setDTthreads(0)


continents <- c("Africa", "Asia", "Europe", "North America", "Oceania", "South America")

holder <- list()
counter = 0
for(c in continents){
  # get filing names correct for each continent
  if(c == "North America" | c == "South America"){
    months <- data.frame(name = c("mar2023", "apr2023", "may2023",
                                  "jun2023", "jul2023", "aug2023",
                                  "sept2023", "oct2023", "nov2023",
                                  "dec2023", "jan2024", "feb2024",
                                  "mar2024"))
  }else{
    months <- data.frame(name = c("mar23", "apr23", "may23",
                                  "jun23", "jul23", "aug23",
                                  "sept23", "oct23", "nov23",
                                  "dec23", "jan24", "feb24",
                                  "mar24"))
  }
  
  # read in bird weather data
  for(m in 1:nrow(months)){
    
    
    # focal month for setting up period variable
    this_month <- months[m,]
    
    
    
    
    this_file <- list.files(path = paste0(here('Data/Birdweather'), "/", c), 
                            pattern = paste0(".*", this_month, "\\.csv$"),
                            full.names = T)
    
    # load in Birdweather
    bw <- fread(this_file)
    
    # deal with different column names in each file
    if(c == "Europe" & m == 1){
      setnames(bw, old = c("confidence",
                           "species.commonName",
                           "species.scientificName",
                           "coords.lat",
                           "coords.lon",
                           "station.name",
                           "timestamp"), 
               new = c("Confidence", 
                       "Common Name", 
                       "Scientific Name",
                       "Latitude", 
                       "Longitude", 
                       "Station",
                       "Timestamp")
      )
    }
    
    # I think station.type should only be birdnetpi or PUC
    # I also think "stream_youtube" is OK but not totally sure?
    # stream_audio seems to be erroneous 
    # I don't think that column always exists, though
    if("station.type" %in% names(bw)){
      bw <- bw[!(station.type %in% c("stream_audio","stream_youtube")),]
    }
    
    # drop unnecessary columns
    bw <- bw[, .(Timestamp, `Common Name`, `Scientific Name`,
                 Latitude, Longitude,Confidence)]
    setnames(bw, old = c("Timestamp", "Common Name", "Scientific Name",
                         "Latitude", "Longitude","Confidence"),
             new = c('date_time', "com_name", "sci_name",
                     "lat", "lon", "conf"))
    
    # store unfiltered species count
    counter = counter + 1
    holder[[counter]] <- data.table(bw[, .N, com_name], continent = c, month = this_month)
    cat('\n\n',c, this_month,'completed','at', as.character(Sys.time()), "\n\n")
    
  }
}

out <-  rbindlist(holder)
fwrite(out, here('Results/Birdweather/row_counter/birdweather_species_detection_counter.csv'))