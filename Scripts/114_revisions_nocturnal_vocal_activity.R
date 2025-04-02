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
    
    # sept 17 update: detection filter at three levels
    for (dets in detection_filter) {
      # do I want to overwrite existing output files?
      # TRUE can be used if new analyses or changes needed
      # FALSE to rerun script without overwriting
      if (overwrite == TRUE) {
        cat("")
      } else{
        # check if file exists
        if (file.exists(
          paste0(
            here('Results/Birdweather/nocturnal'),
            "/",
            c,
            "/nocturnal_measures_",
            this_month,
            "_conf_" ,
            confidence_cutoff,
            "_det_",
            dets,
            ".csv"
          )
        )) {
          cat('\n\n', c, this_month, "completed\n\n")
          
          next
        }
      }
      
      
      total_spp <- total_spp[N_dets > dets, ] #min observations per species per station_date
      if (nrow(total_spp) == 0) {
        next
      }
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
                               time_since_post_nadir = int_length(interval(_postnadir, date_time)) /
                                 60,)]
        
        # --- # --- # --- # --- # --- # --- # --- # --- # --- # ---
        #Dawn Cessation: time of last detection - nadir ####
        # --- # --- # --- # --- # --- # --- # --- # --- # --- # ---
        dawn_cessation <- single_spp_dets[date_time < solarNoon, ]
        if (nrow(dawn_cessation) != 0) {
          # store the earliest detection by station_date
          dawn_cessation[, max_time_det := max(date_time), .(station_date)]
          dawn_cessation[, dawn_cessation := int_length(interval(solarNoon, max_time_det)) /
                           60, .(station_date)]
          dawn_cessation <- dawn_cessation[!duplicated(station_date), .(dawn_cessation, station_date)]
          setkey(dawn_cessation, station_date)
          dawn_cessation[, category := "dawn_cessation"]
          setnames(dawn_cessation, old = 'dawn_cessation', new = 'value')
        } else{
          dawn_cessation <- data.table(
            value = NA,
            station_date = NA,
            category = "dawn_cessation"
          )
        }
        
        
        # --- # --- # --- # --- # --- # --- # --- # --- # --- # ---
        #Evening onset: time of earliest detection - nadir ####
        # --- # --- # --- # --- # --- # --- # --- # --- # --- # ---
        
        evening_onset <- single_spp_dets[date_time >= solarNoon, ]
        if (nrow(evening_onset) != 0) {
          evening_onset[, min_time_det := min(date_time), by = station_date]
          evening_onset[, evening_onset := int_length(interval(solarNoon, min_time_det)) /
                          60, by = station_date]
          evening_onset <- evening_onset[!duplicated(station_date), .(evening_onset, station_date)]
          setkey(evening_onset, station_date)
          evening_onset[, category := "evening_onset"]
          setnames(evening_onset, old = 'evening_onset', new = 'value')
        } else{
          evening_onset = data.table(
            value = NA,
            station_date = NA,
            category = "evening_onset"
          )
        }
        
        
        # bring together individual calculations
        out <- rbindlist(l = list(dawn_cessation, evening_onset))
        out <- out[!is.na(station_date), ] # throw out measures we couldn't calculate
        if (nrow(out) == 0) {
          next
        }
        out[, com_name := f, ] #add species name to out
        setkey(out, station_date)
        out <- merge(out, single_spp_dets[!duplicated(station_date), .(sci_name, date_time, date, week, lat, lon, station_date)])
        # drop station_date grouping variable
        out <- out[, !("station_date"), with = FALSE]
        species_holder[[species_counter]] <- out
        
        # I was saving files at the species level but now I am doing a
        # species_holder so no longer need
        # I like keeping because it is nice code :)
        
        # spp_file_name <- gsub("[[:punct:]]", "_", f)
        # spp_file_name <- gsub(" ", "-",spp_file_name)
        # ifelse(!dir.exists(file.path(paste0(here('Results/Birdweather/activity_measures'),
        #                                     "/", c,"/", this_month))),
        #        dir.create(file.path(paste0(here('Results/Birdweather/activity_measures'),
        #                                    "/", c,"/", this_month))), FALSE)
        # fwrite(out, file = paste0(here('Results/Birdweather/activity_measures'),
        #                           "/", c,"/", this_month, "/activity_measures_", spp_file_name, ".csv"))
        
        
      } #species
      species_holder <- rbindlist(species_holder)
      # make sure directory exists
      ifelse(!dir.exists(file.path(paste0(
        here('Results/Birdweather/nocturnal'), "/", c
      ))), dir.create(file.path(paste0(
        here('Results/Birdweather/nocturnal'), "/", c
      ))), FALSE)
      # write file to directory
      fwrite(
        species_holder,
        file = paste0(
          here('Results/Birdweather/nocturnal'),
          "/",
          c,
          "/nocturnal_measures_",
          this_month,
          "_",
          "conf_",
          confidence_cutoff,
          "_det_",
          dets,
          ".csv"
        )
      )
      
      
      cat(
        '\n\n',
        c,
        this_month,
        "confidence_cutoff",
        confidence_cutoff,
        "detection_filter",
        dets,
        'completed',
        'at',
        as.character(Sys.time()),
        "\n\n"
      )
      
    } # detection filter
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
