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
    

      focal_spp <- total_spp[, unique(com_name)]
      focal_spp <- sort(focal_spp)
      
      
      
      species_holder <- list()
      species_counter = 0
      for (f in focal_spp) {
        species_counter = species_counter + 1
        single_spp_dets <- bw[com_name == f, ]
        
        # now start working on timezone and clocks
        # Generate the sunlight times
        time_frame <- getSunlightTimes(
          data = single_spp_dets[, .(date = date,
                                     lat = lat,
                                     lon = lon)],
          keep = c(
            "dusk",
            "night",
            "dawn",
            "nightEnd",
            "sunrise",
            "nauticalDawn",
            "sunriseEnd",
            "sunset",
            "solarNoon",
            "nadir"
          ),
          tz = "UTC"
        )
        
        time_frame_pre <- getSunlightTimes(
          data = single_spp_dets[, .(date = date-1,
                                     lat = lat,
                                     lon = lon)],
          keep = c(
            "nadir"
          ),
          tz = "UTC"
        )
        
        time_frame_post <- getSunlightTimes(
          data = single_spp_dets[, .(date = date+1,
                                     lat = lat,
                                     lon = lon)],
          keep = c(
            "nadir"
          ),
          tz = "UTC"
        )
        
        
        # bring everything together and bind
        single_spp_dets <- cbind(single_spp_dets, time_frame[, 4:ncol(time_frame)])
        single_spp_dets <- cbind(single_spp_dets, time_frame_pre[, 4])
        setnames(single_spp_dets, old = 'V2', new = 'pre_nadir')
        single_spp_dets <- cbind(single_spp_dets, time_frame_post[, 4])
        setnames(single_spp_dets, old = 'V2', new = 'post_nadir')
        
        
        single_spp_dets[, `:=`(time_since_nadir = int_length(interval(nadir, date_time)) /
                                 60,
                               time_since_pre_nadir = int_length(interval(pre_nadir, date_time)) /
                                 60,
                               time_since_post_nadir = int_length(interval(post_nadir, date_time)) /
                                 60)]
        
        # minimum time to nadir
        single_spp_dets[, min_time_nadir := apply(.SD, 1, function(x) x[which.min(abs(x))]), 
                        .SDcols = c("time_since_nadir", "time_since_pre_nadir", "time_since_post_nadir")]
        
        out <- single_spp_dets[, .(sci_name, com_name, min_time_nadir, date, week, lat, lon)]
        species_holder[[species_counter]] <- out
        
        
      } #species
      species_holder <- rbindlist(species_holder)
      # make sure directory exists
      ifelse(!dir.exists(file.path(paste0(
        here('Results/Birdweather/nocturnalV3'), "/", c
      ))), dir.create(file.path(paste0(
        here('Results/Birdweather/nocturnalV3'), "/", c
      ))), FALSE)
      # write file to directory
      fwrite(
        species_holder,
        file = paste0(
          here('Results/Birdweather/nocturnalV3'),
          "/",
          c,
          "/nocturnal_measures_",
          this_month,
          "_",
          "conf_",
          confidence_cutoff,
          ".csv"
        )
      )
      
      
      cat(
        '\n\n',
        c,
        this_month,
        "confidence_cutoff",
        confidence_cutoff,
        'completed',
        'at',
        as.character(Sys.time()),
        "\n\n"
      )
      
    } # months
  } # continents
  continent_holder <- rbindlist(continent_holder)
  # make sure directory exists
  ifelse(!dir.exists(file.path(paste0(
    here('Results/Birdweather/nocturnalV3'), "/", c
  ))), dir.create(file.path(paste0(
    here('Results/Birdweather/nocturnalV3'), "/", c
  ))), FALSE)
  # write file to directory
  fwrite(continent_holder,
         file = paste0(
           here('Results/Birdweather/nocturnalV3'),
           "/",
           c,
           "/nocturnal_tot_vocsV3.csv"
         ))
  

