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
# detection_filter <- c(10,25,100)
continents <- c("Africa",
                "Asia",
                "Europe",
                "North America",
                "Oceania",
                "South America")



continent_holder <- list()
continent_counter = 0
for (c in continents) {
  continent_holder <- list()
  if(length(continent_holder) != 0){
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
    continent_counter = continent_counter + 1
    
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
    
    
    
    # march 14, 2025 - we have been using 0.75 so sticking with that
    
    bw <- bw[conf >= confidence_cutoff, ]
    
    
    # initial prep of bw timestamps
    bw[, date_time := ymd_hms(date_time, tz = 'UTC')]
    bw <- bw[!is.na(date_time), ] # a few fail to parse
    bw[, date := date(date_time)]
    bw[, week := week(date_time)]
    
    # get grouping variables
    # bw[, station_date := .GRP, .(lat, lon, date)]
    # setkey(bw, station_date)
    
    # remove noise, mammals, insects, and amphibians
    not_interested <- c(
      "Engine",
      "Siren",
      "Coyote",
      "Dog",
      "Eastern Gray Squirrel",
      "Red Squirrel",
      "Power tools",
      "Fireworks",
      "Gray Wolf",
      "Gun",
      "Honey Bee",
      "Spring Peeper"
    )
    bw <- bw[!(com_name %in% not_interested), ]
    # this should keep frogmouths but drop anuras
    bw <- bw[!(str_detect(com_name, "frog(?!mouth)") |
                               str_detect(com_name, "Frog(?!mouth)")), ]
    bw <- bw[!(com_name %like% "Treefrog"), ]
    bw <- bw[!(com_name %like% "Bullfrog"), ]
    bw <- bw[!(com_name %like% "Cricket"), ]
    bw <- bw[!(com_name %like% "Toad"), ]
    bw <- bw[!(com_name %like% "Trig"), ]
    bw <- bw[!(com_name %like% "Katydid"), ]
    bw <- bw[!(com_name %like% "Chipmunk"), ]
    bw <- bw[!(com_name %like% "Conehead"), ]
    bw <- bw[!(com_name %like% "Gryllus assimilis"), ]
    bw <- bw[!(com_name %like% "Human"), ]
    bw <- bw[!(com_name %like% "Monkey"), ]
    
    
    
    # total number of species detected with a confidence threshold >= 0.75
    # for each station_date
    # spp_rich <- total_spp[, uniqueN(com_name), station_date]
    # setnames(spp_rich, 'V1', 'spp_rich')
    
    # total number of vocalization events for each station_date
    voc_tots <- bw[, .(sci_name = first(sci_name), N_dets = .N), by = .(com_name, lat, lon, date)]
    
    # don't need to store spp_rich separately because data.table voc_tots[, uniqueN(com_name), station_date]
    # or some crazy tidy stuff neil can do.
    
    continent_holder[[continent_counter]] <- voc_tots
    
    
    
    cat('\n\n',
        c,
        this_month,
        'completed',
        'at',
        as.character(Sys.time()),
        "\n\n")
  } #months
  continent_holder <- rbindlist(continent_holder)
  # make sure directory exists
  ifelse(!dir.exists(file.path(paste0(
    here('Results/Birdweather/spp_rich_tot_vocs'), "/", c
  ))), dir.create(file.path(paste0(
    here('Results/Birdweather/spp_rich_tot_vocs'), "/", c
  ))), FALSE)
  # write file to directory
  fwrite(continent_holder,
         file = paste0(
           here('Results/Birdweather/spp_rich_tot_vocs'),
           "/",
           c,
           "/spp_rich_tot_vocs.csv"
         ))

} # continents
