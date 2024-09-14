library(here)
library(data.table)
library(lubridate)
library(suncalc)
library(lutz)
library(sf)
library(activity)

# Convert dttm to decimal time
convert_dttm_to_decimal <- function(dttm) {
  hours <- hour(dttm)
  minutes <- minute(dttm)
  seconds <- second(dttm)
  return(hours + minutes / 60 + seconds / 3600)
}


# Continue tinkering with data/code
#Calculate response variables for CARW
#Morning onset: time of first detection - local sunrise
#Morning median: time of 50% detection - local sunrise
#Evening cessation: time of last detection - local sunset
#Plot results against ALAN & maybe experiment with an initial model
#Look at resolution of ALAN data / temporal variation in it


continents <- c("Africa", "Asia", "Europe", "North America", "South America")

for(c in continents){
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

  for(m in 1:nrow(months)){


    # focal month for setting up period variable
    this_month <- months[m,]
    if(file.exists(paste0(here('Results/Birdweather/activity_measures'),
                          "/", c,"/activity_measures_",this_month, ".csv"))){
      cat('\n\n',c, this_month, "completed\n\n")
      
      next
    }
    this_file <- list.files(path = paste0(here('Data/Birdweather'), "/", c), 
                            pattern = paste0(".*", this_month, "\\.csv$"),
                            full.names = T)
    
    # some data files might not exist
    if(rlang::is_empty(this_file)){
      cat('\n\n',c, this_month, "doesn't exist\n\n")
      next
    }
    
    # load in Birdweather
    bw <- fread(this_file)
    
    # deal with different column names in each file
    if(c == "Asia" | c == "Africa" | c == "Europe" & m == 1){
      setnames(bw, old = c("confidence",
                           "species.commonName",
                           "coords.lat",
                           "coords.lon",
                           "station.name",
                           "timestamp"), 
               new = c("Confidence", 
                       "Common Name", 
                       "Latitude", 
                       "Longitude", 
                       "Station",
                       "Timestamp")
      )
    }
    
    # a blanket 0.5 confidence score filter (Sept 13 update: 0.625)
    bw <- bw[Confidence >= 0.625,]
    
    
    # going to filter species list down 
    bw[, station_ID := .GRP, .(Station, Latitude, Longitude)]
    bw[, time := ymd_hms(Timestamp,tz = 'UTC')]
    bw <- bw[!is.na(time),] # a few fail to parse
    bw[, date := date(time)]
    bw[, week := week(date)]
    bw[, station_date := .GRP, .(station_ID, date)]
    setkey(bw, station_date)
    not_interested <- c("Engine", "Siren", "Coyote", "Dog", 
                        "Eastern Gray Squirrel", "Red Squirrel",
                        "Power tools")
    total_spp <- bw[!(`Common Name` %in% not_interested),]
    total_spp <- total_spp[!(`Common Name` %like% "Frog"),]
    total_spp <- total_spp[!(`Common Name` %like% "Treefrog"),]
    total_spp <- total_spp[!(`Common Name` %like% "Bullfrog"),]
    total_spp <- total_spp[!(`Common Name` %like% "Cricket"),]
    
    total_spp <- total_spp[, .N, by = .(`Common Name`, station_date)]
    total_spp <- total_spp[N > 100,] #100 observations per species per station_date
    focal_spp <- total_spp[, unique(`Common Name`)]
    focal_spp <- sort(focal_spp)
    
    species_holder <- list()
    species_counter = 0
    for(f in focal_spp){
      species_counter = species_counter + 1
      single_spp_dets <- bw[`Common Name` == f,]
      
      # now start working on timezone and clocks
      single_spp_dets[, tz := tz_lookup_coords(Latitude, Longitude, method = 'fast', warn = F)]
      single_spp_dets[, time := ymd_hms(Timestamp,tz = 'UTC')]
      
      # split by time zone and get local time stamps
      # split the data into a list of data.tables based on time zone
      bw_tz_split <- split(single_spp_dets, by = "tz")
      
      # A holder for the getSunCalcTimes in the loop
      time_frame <- vector("list", length = length(bw_tz_split))
      
      # Loop through each data.table and update the date and generate time_frame
      for(j in seq_along(bw_tz_split)){
        this_tzone <- bw_tz_split[[j]][, unique(tz)]
        bw_tz_split[[j]][, dt_local :=  with_tz(time, tzones = this_tzone, tzone_out = this_tzone)]
        
        bw_tz_split[[j]][, date_local :=  date(dt_local)]
        bw_tz_split[[j]][, time_local :=  hms::as_hms(dt_local)]
        

        # Generate the sunlight times
        time_frame[[j]] <- getSunlightTimes(
          data = bw_tz_split[[j]][, .(date = date_local, lat = Latitude, lon = Longitude)],
          keep = c("dusk", "night", "dawn", "nightEnd","sunrise",
                   "nauticalDawn",  "sunriseEnd", "sunset", "solarNoon"),
          tz = bw_tz_split[[j]][1, tz]
        )
      #  cat(bw_tz_split[[j]][1, tz], "complete\n")
      }
      # bring everything together and bind
      time_frame <- rbindlist(time_frame)
      single_spp_dets <- rbindlist(bw_tz_split)
      single_spp_dets <- cbind(single_spp_dets, time_frame[, 4:ncol(time_frame)])
      
      # the suncalctimes were calculated in local timezone, but they are not 
      # printed that way
      # this lubridate function updates the way they are printed
      # it does not change the time stamp which is already calculated correctly
      # terribly confusing
      # single_spp_dets[,solarNoon := force_tz(ymd_hms(solarNoon), tzone = tz)]
      # single_spp_dets[,sunrise := force_tz(ymd_hms(sunrise), tzone = tz)]
      # single_spp_dets[,dawn := force_tz(ymd_hms(dawn), tzone = tz)]
      
      # the dt_local created in the loop above is still not printing
      # the local TZ, which is again terribly confusing
      #correct_dt_local <- list()
      single_spp_dets[, dt_local_real := with_tz(dt_local, tz), by=tz]
      single_spp_dets[, time_local_real :=  hms::as_hms(dt_local_real)]
      # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- 
      #Morning onset: time of first detection - local sunrise ####
      # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- 
      # okay, let's first onset - dawn  
      # get timestamps between 3am and solar noon
      threeam <- hms("03:00:00")
      midnight <- hms("00:00:01")
      
      first_onset <- single_spp_dets[time_local_real >= threeam & time_local_real < hms::as_hms(solarNoon),]
      if(nrow(first_onset) == 0){
        next # sometimes a species has 100 detections but not at the focal time
      }
      # store the earliest detection by station_date
      first_onset[, min_time_det := min(dt_local_real), .(station_date)]
      first_onset[,first_onset := int_length(interval(sunrise, min_time_det))/60, .(station_date)]
      first_onset <- first_onset[!duplicated(station_date), .(first_onset, station_date)]
      setkey(first_onset, station_date)
      first_onset[, category := "first_onset"]
      setnames(first_onset, old = 'first_onset', new = 'value')
      # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- 
      #Morning median: time of 50% detection - local sunrise ####
      # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- 
      # median activity
      med_voc <- single_spp_dets[time_local_real >= threeam & time_local_real < hms::as_hms(solarNoon),]
      med_voc[, med_voc := quantile(time_local_real, probs = 0.5), .(station_date)]
      med_voc[, med_voc_decimal := convert_dttm_to_decimal(med_voc)]
      med_voc <- med_voc[!duplicated(station_date),.(med_voc_decimal, station_date)]
      setkey(med_voc, station_date)
      med_voc[, category := "median_dawn"]
      setnames(med_voc, old = 'med_voc_decimal', new = 'value')
      
      # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- 
      #Evening cessation: time of last detection - local sunset ####
      # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- 
      
      ev_ces <- single_spp_dets[time_local_real > hms::as_hms(solarNoon),]
      ev_ces[, max_time_det := max(dt_local_real), by = station_date]
      ev_ces[, ev_ces := int_length(interval(sunset, max_time_det))/60, by = station_date]
      ev_ces <- ev_ces[!duplicated(station_date), .(ev_ces, station_date)]
      setkey(ev_ces, station_date)
      ev_ces[, category := "ev_ces"]
      setnames(ev_ces, old = 'ev_ces', new = 'value')
      
      # bring together individual calculations
      out <- rbindlist(l = list(first_onset, med_voc, ev_ces))
      out[, `Common Name` := f,]
      setkey(out, station_date)
      out <- merge(out, bw[!duplicated(station_date), .(station_date, Station, Latitude, Longitude, week)])
      
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
    ifelse(!dir.exists(file.path(paste0(here('Results/Birdweather/activity_measures'),
                                        "/", c))),
           dir.create(file.path(paste0(here('Results/Birdweather/activity_measures'),
                                       "/", c))), FALSE)
    fwrite(species_holder, file = paste0(here('Results/Birdweather/activity_measures'),
                                         "/", c,"/activity_measures_",this_month, ".csv"))
    cat('\n\n',c, this_month, 'completed','at', as.character(Sys.time()), "\n\n")


    
  } #months

} #continents


