library(here)
library(data.table)
library(lubridate)
library(suncalc)
library(lutz)
library(sf)
library(activity)
library(stringr)
library(Rfast)
library(geos)
library(terra)
setDTthreads(0)


# read in global roads
# Read a specific feature class
gdb_path <- here("data/roads/groads-v1-global-gdb/groads-v1-global-gdb/gROADS_v1.gdb")
gdb_data <- st_read(gdb_path, layer = "Global_Roads")

# CONUS SOUND MAP
sound <- rast(here("data/sound_map/CONUS_L50dBA_sumDay_exi.tif"))
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
    
    # get grouping variables for anthro noise
    bw[, station_date := .GRP, .(lat, lon, date)]
    setkey(bw, station_date)
    
    # store birdweather detected anthro noise
    noise <- c("Engine", "Siren", "Power tools", "Fireworks", "Gun")
    bw_anthro <- bw[com_name %in% noise,]
    anthro_noise <- bw_anthro[, .(N_anthro_noise = .N), by = .(station_date)]
    
    bw <- merge(bw, anthro_noise, by = 'station_date', all.x = T)
    
    bw[is.na(N_anthro_noise), N_anthro_noise := 0]
    
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

    # get unique lat/lon combos
    bw[, lat_lon_grp := .GRP, .(lat, lon)]
    
    bw_locs <- bw[!duplicated(lat_lon_grp)]

    
    # get locs distance to roads
    bw_locs <- st_as_sf(bw_locs[, .(lat, lon, lat_lon_grp)], coords = c('lon', 'lat'), crs = 4326)
    bw_locs <- st_transform(bw_locs, crs = st_crs(gdb_data))    
    bw_buffs <- st_buffer(bw_locs, dist = units::set_units(16093.44, value = 'meters')) # 10 miles
    
    # Convert sf objects to geos geometries
    buffs_geos <- as_geos_geometry(bw_buffs)
    if(!exists("lines_geos")){
      lines_geos <- as_geos_geometry(gdb_data)
    }
    
    # Create a spatial index for the multiline features
    if(!exists("line_index")){
      line_index <- geos_basic_strtree(lines_geos)
    }

    # Find the nearest line for each point
    tree_query <- geos_basic_strtree_query(line_index, query = buffs_geos)
    
    # Compute distances to nearest lines
    hold <- gdb_data[tree_query$tree,]
    
    nearest_line_indices <- st_nearest_feature(bw_locs, hold)
    
    # Get the nearest lines
    nearest_lines_sf <- hold[nearest_line_indices, ]
    
    # Calculate distances
    distances <- st_distance(bw_locs, nearest_lines_sf, by_element = TRUE)
    
    # add distances to bw_locs
    bw_locs$road_distance <- distances
    
    
    # get conus sound map data
    conus_sound <- extract(sound, vect(bw_locs))
    bw_locs$lcmap_sound <- conus_sound$CONUS_L50dBA_sumDay_exi
    
    # get back to dt for merging
    bw_locs <- as.data.table(bw_locs)
    bw_locs$geometry <- NULL
    
    bw <- merge(bw, bw_locs, by = 'lat_lon_grp', all.x = T)
    
    
    # simplify for storing
    continent_holder[[continent_counter]] <- bw[, .(lat, lon, date, N_anthro_noise, road_distance, lcmap_sound)]
    
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
    here('Results/noise'), "/", c
  ))), dir.create(file.path(paste0(
    here('Results/noise'), "/", c
  ))), FALSE)
  
  # write file to directory
  fwrite(continent_holder,
         file = paste0(
           here('Results/noise'),
           "/",
           c,
           "/", "noise_", which(continents == c), ".csv"
         ))
  
} #continents
    