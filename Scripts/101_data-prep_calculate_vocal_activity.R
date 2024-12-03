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


confidence_cutoff <- c(0, 0.5, 0.75)
detection_filter <- c(10,25,100)
continents <- c("Africa", "Asia", "Europe", "North America", "Oceania", "South America")
continents <- c("Africa", "Asia")

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
    
    
    
    
    this_file <- list.files(path = paste0(here('Data/Birdweather'), "/", c), 
                            pattern = paste0(".*", this_month, "\\.csv$"),
                            full.names = T)
    
    # some data files might not exist
    # because they haven't been downloaded yet
    if(rlang::is_empty(this_file)){
      cat('\n\n',c, this_month, "doesn't exist\n\n")
      next
    }
    
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
    

    
    # a blanket 0.5 confidence score filter (Sept 13 update: 0.625)
    # sept 17 update: cut offs at 3 levels
    for(cutoff in confidence_cutoff){
      
      bw <- bw[conf >= cutoff,]
      
      
      # initial prep of bw timestamps
      bw[, date_time := ymd_hms(date_time,tz = 'UTC')]
      bw <- bw[!is.na(date_time),] # a few fail to parse
      bw[, date := date(date_time)]
      bw[, week := week(date_time)]
      
      # get grouping variables
      bw[, station_date := .GRP, .(lat, lon, date)]
      setkey(bw, station_date)
      
      # remove noise, mammals, insects, and amphibians
      not_interested <- c("Engine", "Siren", "Coyote", "Dog", 
                          "Eastern Gray Squirrel", "Red Squirrel",
                          "Power tools", "Fireworks", "Gray Wolf", "Gun",
                          "Honey Bee",
                          "Spring Peeper")
      total_spp <- bw[!(com_name %in% not_interested),]
      # this should keep frogmouths but drop anuras
      total_spp <- total_spp[!(str_detect(com_name, "frog(?!mouth)") | str_detect(com_name, "Frog(?!mouth)")),]
      total_spp <- total_spp[!(com_name %like% "Treefrog"),]
      total_spp <- total_spp[!(com_name %like% "Bullfrog"),]
      total_spp <- total_spp[!(com_name %like% "Cricket"),]
      total_spp <- total_spp[!(com_name %like% "Toad"),]
      total_spp <- total_spp[!(com_name %like% "Trig"),]
      total_spp <- total_spp[!(com_name %like% "Katydid"),]
      total_spp <- total_spp[!(com_name %like% "Chipmunk"),]
      total_spp <- total_spp[!(com_name %like% "Conehead"),]
      total_spp <- total_spp[!(com_name %like% "Gryllus assimilis"),]
      total_spp <- total_spp[!(com_name %like% "Human"),]
      total_spp <- total_spp[!(com_name %like% "Monkey"),]
      
      
      
      # OK, these next lines first calculate
      # how many detections a given species has 
      # for each station-date (e.g., raleigh, nc on June 3, 2023)
      # I think we need a minimum number of 
      # detections for a species to ensure that 
      # the samples on that date are representative
      # of the species. 
      # concern that 100 is over restrictive
      # unclear what minimum number should be though
      # 25?
      total_spp <- total_spp[, .N, by = .(com_name, station_date)]
      
      # sept 17 update: detection filter at three levels
      for(dets in detection_filter){
        
        # do I want to overwrite existing output files?
        # TRUE can be used if new analyses or changes needed
        # FALSE to rerun script without overwriting
        if(overwrite == TRUE){
          cat("")
        } else{
          # check if file exists 
          if(file.exists(paste0(here('Results/Birdweather/activity_measures'),
                                "/", c,"/activity_measures_",this_month,
                                "_conf_" ,cutoff, "_det_", dets, ".csv"))){
            cat('\n\n',c, this_month, "completed\n\n")
            
            next
          }
        }
        
        
        total_spp <- total_spp[N > dets,] #min observations per species per station_date
        focal_spp <- total_spp[, unique(com_name)]
        focal_spp <- sort(focal_spp)
        
        species_holder <- list()
        species_counter = 0
        for(f in focal_spp){
          species_counter = species_counter + 1
          single_spp_dets <- bw[com_name == f,]
          
          # now start working on timezone and clocks
          # Generate the sunlight times
          time_frame <- getSunlightTimes(
            data = single_spp_dets[, .(date = date, lat = lat, lon = lon)],
            keep = c("dusk", "night", "dawn", "nightEnd","sunrise",
                     "nauticalDawn",  "sunriseEnd", "sunset", "solarNoon", "nadir"),
            tz = "UTC"
          )
          
          # bring everything together and bind

          single_spp_dets <- cbind(single_spp_dets, time_frame[, 4:ncol(time_frame)])
          
     

          
          
          
          # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- 
          #Morning onset: time of first detection - local sunrise ####
          # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- 
          # okay, let's first onset - dawn  
          # get timestamps between 3am and solar noon
          #threeam <- hms("03:00:00")
          # midnight <- hms("00:00:01")
          
          first_onset <- single_spp_dets[date_time >= nadir & date_time < solarNoon,]
          if(nrow(first_onset) != 0){
            # store the earliest detection by station_date
            first_onset[, min_time_det := min(date_time), .(station_date)]
            first_onset[,first_onset := int_length(interval(sunrise, min_time_det))/60, .(station_date)]
            first_onset <- first_onset[!duplicated(station_date), .(first_onset, station_date)]
            setkey(first_onset, station_date)
            first_onset[, category := "first_onset"]
            setnames(first_onset, old = 'first_onset', new = 'value')
          } else{
              first_onset <- data.table(value = NA, station_date = NA,
                                        category = "first_onset")
            }

          # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- 
          #Morning median: time of 50% detection - local sunrise ####
          # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- 
          # median activity
          med_voc <- single_spp_dets[date_time >= nadir & date_time < solarNoon,]
          if(nrow(med_voc) != 0){
            med_voc[, med_voc := quantile(date_time, probs = 0.5), .(station_date)]
            med_voc <- med_voc[, int_length(interval(sunrise, med_voc))/60, .(station_date)]
            setnames(med_voc,old = "V1", new = 'med_voc' )
            med_voc <- med_voc[!duplicated(station_date),.(med_voc, station_date)]
            setkey(med_voc, station_date)
            med_voc[, category := "median_dawn"]
            setnames(med_voc, old = 'med_voc', new = 'value')
          } else{
            med_voc <- data.table(value = NA, station_date = NA,
                                  category = "median_dawn")
          }

          
          # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- 
          #Evening cessation: time of last detection - local sunset ####
          # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- 
          
          ev_ces <- single_spp_dets[date_time >= solarNoon & date_time < nadir,]
          if(nrow(ev_ces) != 0){
            ev_ces[, max_time_det := max(date_time), by = station_date]
            ev_ces[, ev_ces := int_length(interval(sunset, max_time_det))/60, by = station_date]
            ev_ces <- ev_ces[!duplicated(station_date), .(ev_ces, station_date)]
            setkey(ev_ces, station_date)
            ev_ces[, category := "ev_ces"]
            setnames(ev_ces, old = 'ev_ces', new = 'value')
          } else{
            ev_ces = data.table(value = NA,
                                station_date = NA,
                                category = "ev_ces")
          }

          
          # bring together individual calculations
          out <- rbindlist(l = list(first_onset, med_voc, ev_ces))
          out <- out[!is.na(station_date),] # throw out measures we couldn't calculate
          if(nrow(out) == 0){
            next
          }
          out[, com_name := f,] #add species name to out
          setkey(out, station_date)
          out <- merge(out, single_spp_dets[!duplicated(station_date), 
                                            .(sci_name, date_time, date,
                                              week, lat, lon,
                                              station_date)])
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
        ifelse(!dir.exists(file.path(paste0(here('Results/Birdweather/activity_measures'),
                                            "/", c))),
               dir.create(file.path(paste0(here('Results/Birdweather/activity_measures'),
                                           "/", c))), FALSE)
        # write file to directory
        fwrite(species_holder, 
               file = paste0(here('Results/Birdweather/activity_measures'),
                             "/", c,"/activity_measures_",this_month,
                             "_","conf_", cutoff, "_det_",dets,  ".csv"))
        
        
        cat('\n\n',c, this_month,"confidence_cutoff",cutoff, "detection_filter", dets, 'completed','at', as.character(Sys.time()), "\n\n")
        
      } # detection filter
    } # confidence filter
    
  } #months
  
} #continents


