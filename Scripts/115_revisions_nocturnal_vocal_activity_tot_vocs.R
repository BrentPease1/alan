library(here)
library(data.table)
library(lubridate)
library(suncalc)
library(lutz)
library(sf)
library(activity)
library(stringr)
setDTthreads(0)

# overwrite current files?
overwrite <- T


confidence_cutoff <- c(0.75)
detection_filter <- c(10, 25, 100)
continents <- c("Africa",
                "Asia",
                "Europe",
                "North America",
                "Oceania",
                "South America")

# read in nocturnal species
nocturnal_spp <- fread(here('Data/nocturnal_species.csv'))

continent_holder <- list()
continent_counter = 0
for (c in continents) {
  continent_holder <- list()
  if (length(continent_holder) == 0) {
    continent_counter = continent_counter + 1
  }
  if (c == "North America" | c == "South America") {
    months <- data.frame(
      name = c(
        "mar2023",
        "apr2023",
        "may2023",
        "jun2023",
        "jul2023",
        "aug2023",
        "sept2023",
        "oct2023",
        "nov2023",
        "dec2023",
        "jan2024",
        "feb2024",
        "mar2024"
      )
    )
  } else{
    months <- data.frame(
      name = c(
        "mar23",
        "apr23",
        "may23",
        "jun23",
        "jul23",
        "aug23",
        "sept23",
        "oct23",
        "nov23",
        "dec23",
        "jan24",
        "feb24",
        "mar24"
      )
    )
  }
  
  for (m in 1:nrow(months)) {
    # focal month for setting up period variable
    this_month <- months[m, ]
    
    this_file <- list.files(
      path = paste0(here('Data/Birdweather'), "/", c),
      pattern = paste0(".*", this_month, "\\.csv$"),
      full.names = T
    )
    
    # some data files might not exist
    # because they haven't been downloaded yet
    if (rlang::is_empty(this_file)) {
      cat('\n\n', c, this_month, "doesn't exist\n\n")
      next
    }
    
    # load in Birdweather
    bw <- fread(this_file)
    
    # deal with different column names in each file
    if (c == "Europe" & m == 1) {
      setnames(
        bw,
        old = c(
          "confidence",
          "species.commonName",
          "species.scientificName",
          "coords.lat",
          "coords.lon",
          "station.name",
          "timestamp"
        ),
        new = c(
          "Confidence",
          "Common Name",
          "Scientific Name",
          "Latitude",
          "Longitude",
          "Station",
          "Timestamp"
        )
      )
    }
    
    # I think station.type should only be birdnetpi or PUC
    # I also think "stream_youtube" is OK but not totally sure?
    # stream_audio seems to be erroneous
    # I don't think that column always exists, though
    if ("station.type" %in% names(bw)) {
      bw <- bw[!(station.type %in% c("stream_audio", "stream_youtube")), ]
    }
    
    # drop unnecessary columns
    bw <- bw[, .(Timestamp,
                 `Common Name`,
                 `Scientific Name`,
                 Latitude,
                 Longitude,
                 Confidence)]
    setnames(
      bw,
      old = c(
        "Timestamp",
        "Common Name",
        "Scientific Name",
        "Latitude",
        "Longitude",
        "Confidence"
      ),
      new = c('date_time', "com_name", "sci_name", "lat", "lon", "conf")
    )
    
    
    # filter down to nocturnal spp only
    bw <- bw[sci_name %in% nocturnal_spp$sci_name, ]
    
    # birdnet confidence filter
    bw <- bw[conf >= confidence_cutoff, ]
    
    
    # initial prep of bw timestamps
    bw[, date_time := ymd_hms(date_time, tz = 'UTC')]
    bw <- bw[!is.na(date_time), ] # a few fail to parse
    bw[, date := date(date_time)]
    bw[, week := week(date_time)]
    
    # get grouping variables
    bw[, station_date := .GRP, .(lat, lon, date)]
    setkey(bw, station_date)
    
    # calculate the number of species-specific detections
    # per station_date
    total_spp <- bw[, .(sci_name = first(sci_name), lat = first(lat), 
                        lon = first(lon), date = first(date), N_dets = .N), by = .(com_name, station_date)]
    continent_holder[[continent_counter]] <- total_spp
    
    cat('\n\n',
        c,
        this_month,
        'completed',
        'at',
        as.character(Sys.time()),
        "\n\n")
  } # months
  continent_holder <- rbindlist(continent_holder)
  # make sure directory exists
  ifelse(!dir.exists(file.path(paste0(
    here('Results/Birdweather/nocturnal'), "/", c
  ))), dir.create(file.path(paste0(
    here('Results/Birdweather/nocturnal'), "/", c
  ))), FALSE)
  # write file to directory
  fwrite(continent_holder,
         file = paste0(
           here('Results/Birdweather/nocturnal'),
           "/",
           c,
           "/nocturnal_tot_vocs.csv"
         ))
  
} #continents
