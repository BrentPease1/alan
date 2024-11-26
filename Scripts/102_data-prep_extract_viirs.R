library(here)
library(data.table)
library(lubridate)
library(sf)
library(terra)
library(rworldmap)
setDTthreads(0)
overwrite = T
# script outline:
#   - read in vocalization activity measures calculated with Scripts/101_data-prep_calculate_vocal_activity_v02.R
#   - read in corresponding month of nighttime_light values
#   - extract point-level values
#   - consider buffers? 500m buffer around point? does effect dissapate or strengthen with distance from point? e.g., 1km buffer, area is saturated with light
#   - Sept 17 update: I considered buffers for 60 cities and point level radiance had highest correlation with city pop.


# Read in calculated vocalization activity ####

if(!file.exists(here('Results/VIIRS/vocal_activity_annotated.csv')) | overwrite == T){
  continents <- c("Africa", "Asia", "Europe", "North America", "Oceania", "South America")
  
  va_holder <- list() #vocal activity holder
  va_counter = 0
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
  
      files <- list.files(path = paste0(here('Results/Birdweather/activity_measures'), "/", c), 
                             # pattern = paste0(".*", this_month, "\\.csv$"),
                             pattern = paste0(".*", this_month, "_conf"),
                              full.names = T)
      
      # some data files might not exist
      if(rlang::is_empty(files)){
        next
      }
  
      
      # load in va files
      for(f in 1:length(files)){
        va <- fread(files[f])
        
        # I forgot to add the confidence and filter level column to
        # the activity measure script so adding in here
        conf_value <- as.numeric(sub(".*_conf_([0-9.]+)_.*", "\\1", files[f]))
        det_value <- as.numeric(sub(".*_det_([0-9]+)\\.csv", "\\1", files[f]))
        va[, conf_filter := conf_value]
        va[, det_filter := det_value]
        va[, lat_lon := .GRP, .(lat, lon)]
        # get unique locations for extracting VIIRS
        va_locs <- va[!duplicated(lat_lon), .(lat_lon, lat, lon)]
        va_locs <- st_as_sf(va_locs, coords = c('lon', 'lat'), crs = 4326)
        
        # Read in corresponding nighttime_lights ####
      #  base_file <- here('Data/VIIRS/nighttime_light/monthly/v10')
        base_file <- "E:/nighttime_data"
        
        # get year from this_month
        if(c == "North America" | c == "South America"){
          this_year <- gsub("\\D", "", this_month)
        } else{
          this_year <- paste0("20", gsub("\\D", "", this_month))
        }
        
        # I wrote month names for Birdweather but VIIRS is numbers
        month_numbers <- data.frame(numbers = c("03","04",'05','06','07','08',
                                                "09","10","11","12","01","02","03"))
        this_number <- month_numbers[m,]
        
        year_num <- paste0(this_year,this_number)
        
        year_num_file <- paste0(base_file, "/", this_year, 
                                "/",year_num, "/","vcmslcfg")
        
        nt_files <- list.files(year_num_file, 
                             #  pattern = "\\.cf_cvg\\.tif$", 
                             # cf_cvg is just the number of cloud free days
                             # used to make the monthly average calculations
                               pattern = "\\.avg_rade9h\\.tif$", 
                               full.names = TRUE, 
                               recursive = TRUE)
        
        # read in viirs files and stack with `terra`
        # Read each raster individually because of different extents
        nt_rast <- lapply(nt_files, rast)
        
        # loop through each file and try to extract values
        # will return NAs for non-overlapping points
        nt_holder <- list()
        for(i in 1:length(nt_rast)){
          this_rast <- nt_rast[[i]]
          nt_holder[[i]] <- extract(this_rast, vect(va_locs))
          names(nt_holder[[i]]) <- c('ID', paste0('avg_rad_',i))
        }
        
        # bring together
        combined_df <- Reduce(function(x, y) merge(x, y, by = "ID", all = TRUE), nt_holder)
        
        # Just get single value across all columns
        combined_df$avg_rad <- apply(combined_df[ , grep("avg_rad", names(combined_df))], 1, function(x) {
          # Return the first non-NA value or NA if all are NA
          x[which(!is.na(x))[1]]
        })
        # keep the two columns 
        nt_estimates <- combined_df[ , c("ID", "avg_rad")]
        setDT(nt_estimates)
        setkey(nt_estimates, "ID")
        
        # bring back to vocal activity
        va <- merge(va, nt_estimates, by.x = "lat_lon", by.y = "ID")
        
  
        va_counter = va_counter + 1
        va_holder[[va_counter]] <- va
      }
  
    } # months
    cat('\n\n',c, 'completed','at', as.character(Sys.time()), "\n\n")
    
  } #continents
    
  out <- rbindlist(va_holder)
  # categorize nighttime light
  out[, rad_cat := fcase(
    avg_rad < quantile(avg_rad, probs = 0.334, na.rm = T), "low",
    avg_rad >= quantile(avg_rad, probs = 0.334, na.rm = T) & avg_rad < quantile(avg_rad, probs = 0.667, na.rm = T), "med",
    avg_rad >= quantile(avg_rad, probs = 0.667, na.rm = T), "high"
  )]
  
  # update lat_lon group
  out[,lat_lon := .GRP, .(lat, lon)]
  
  # clear out memory
  rm(list=ls()[!ls() %in% c("out")])
  gc()
  
  fwrite(out, here('Results/VIIRS/vocal_activity_annotated.csv'))
} else{
  out <- fread(here('Results/VIIRS/vocal_activity_annotated.csv'))
}

# out_simple <- out[conf_filter == 0.5 & det_filter == 25,]
# fwrite(out_simple, here('Results/VIIRS/vocal_activity_annotated_conf05_det25.csv'))
# fwrite(out_simple[!duplicated(lat_lon)], here('Results/VIIRS/vocal_activity_annotated_conf05_det25_nodups.csv'))

# assign grid cell value ####
out[, lat_lon := .GRP, .(lat, lon)]
out[, bad_rad := ifelse(is.na(avg_rad) | avg_rad <0, "bad_rad", "good_rad")]

out_locs <- out[!duplicated(lat_lon),]
out_locs <- out_locs[bad_rad == 'good_rad',]
out_locs <- st_as_sf(out_locs, coords = c("lon", "lat"), crs = 4326)

# Load the continents data
continents <- rworldmap::getMap()
continents <- st_as_sf(continents)
# fix geometries
continents <- st_make_valid(continents)

# some bizaare locations, so restricting obs to terrestrial
# throws out like 20k ish
t <- sapply(st_intersects(out_locs, continents), function(z) if (length(z)==0) NA_integer_ else z[1])
out_locs <- out_locs[!is.na(t),]

# filter down total dataset to just include terrestrial obs & no bad rad
out <- out[lat_lon %in% out_locs$lat_lon,]

cell_size = c(5,10,15) #degrees; 648 cells at 10 degrees, 2500ish at 5 degrees
for(cell in cell_size){
  # get a bounding box across the world
  bounding_box <- st_bbox(c(xmin = -180, ymin = -90, xmax = 180, ymax = 90), crs = st_crs(4326))
  
  # Create a global polygon grid with cell_size spacing
  global_grid <- st_make_grid(st_as_sfc(bounding_box), cellsize = c(cell, cell), crs = st_crs(4326), what = "polygons")
  # make sf object
  global_sf <- st_sf(geometry = global_grid)
  # add grid_id
  global_sf$grid_ID <- 1:nrow(global_sf)
  names(global_sf)[which(names(global_sf) == 'grid_ID')] <- paste0("grid_ID_cell_", cell)
  
  # check it out
  # plot(global_sf)
  
  # next would be to count the number of detectors we have in each box to balance cell size and obs within each cell
  pt_in_poly <- st_join(out_locs, global_sf[, c(paste0("grid_ID_cell_", cell))], join = st_within)
  #also add to out_locs 
  out_locs <- st_join(out_locs, global_sf[, c(paste0("grid_ID_cell_", cell))], join = st_within)
  cat(cell, 'complete\n')
}



out_locs$lon <- st_coordinates(out_locs)[,1]
out_locs$lat <- st_coordinates(out_locs)[,2]
out_locs$geometry <- NULL
out_locs <- as.data.table(out_locs)
setkey(out_locs, "lat_lon")
setkey(out, 'lat_lon')

# add grid_cell ids to out for grouping variable
out <- out[out_locs[, .(lat_lon, grid_ID_cell_5, grid_ID_cell_10, grid_ID_cell_15)], on = c("lat_lon"), nomatch = 0] 
if(overwrite == T){
  fwrite(out, here('Results/VIIRS/vocal_activity_annotated.csv'))
}



# Convert to data.table
# pt_in_poly <- as.data.table(pt_in_poly)
# pt_in_poly$geometry <- NULL
# # Count detectors
# pt_in_poly <- pt_in_poly[, .N, by = grid_ID][, .(grid_ID, detectors = N)]
# 
# global_sf <- merge(global_sf, pt_in_poly, by = 'grid_ID', all.x = T)
# plot(global_sf[2])
# # percent of cells with detectors
# length(which(!is.na(global_sf$detectors))) / nrow(global_sf)
# # how many detectors in a single cell
# max(global_sf$detectors, na.rm = T)
# length(which(global_sf$detectors == 1))
