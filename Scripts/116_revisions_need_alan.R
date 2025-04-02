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

va <- fread(here('Data/noct_locs_id.csv'))
va[, lat_lon := .GRP, .(lat, lon)]
va[, year := year(date)]
va[, month := month(date)]


# Read in corresponding nighttime_lights ####
#  base_file <- here('Data/VIIRS/nighttime_light/monthly/v10')
base_file <- "D:/nighttime_data"

# I wrote month names for Birdweather but VIIRS is numbers
month_numbers <- data.table(month = c("03","04",'05','06','07','08',
                                        "09","10","11","12","01","02","03"),
                            year = c(rep('2023', 10), rep('2024', 3)))

va_holder <- list() #vocal activity holder
va_counter = 0
        
for(i in 1:nrow(month_numbers)){
  this_year <- month_numbers[i, as.numeric(year)]
  this_month <- month_numbers[i, month]
  year_month <- paste0(this_year,this_month)
  
  focal_month <- va[month == month(date(paste0(this_year, "-", this_month, "-", "01"))) & year == this_year,]
  
  
  # read in viirs
  year_num_file <- paste0(base_file, "/", this_year, 
                          "/",year_month, "/","vcmslcfg")
  
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
  
  # get unique locations for extracting VIIRS
  va_locs <- focal_month[!duplicated(id), .(id, lat, lon)]
  va_locs <- st_as_sf(va_locs, coords = c('lon', 'lat'), crs = 4326)
  
  # loop through each file and try to extract values
  # will return NAs for non-overlapping points
  nt_holder <- list()
  for(i in 1:length(nt_rast)){
    this_rast <- nt_rast[[i]]
    nt_holder[[i]] <- extract(this_rast, vect(va_locs), bind = T)
    names(nt_holder[[i]]) <- c('id', paste0('avg_rad_',i))
  }
  
  # bring together
  combined_df <- Reduce(function(x, y) merge(x, y, by = "id", all = TRUE), nt_holder)
  combined_df <- as.data.table(combined_df)
  
  # Identify the avg_rad columns
  rad_cols <- grep("avg_rad", names(combined_df), value = TRUE)
  
  # Get the first non-NA value per row
  combined_df[, avg_rad := apply(.SD, 1, function(x) x[which(!is.na(x))[1]]), .SDcols = rad_cols]
  
  # keep the two columns 
  nt_estimates <- combined_df[ , c("id", "avg_rad")]
  setDT(nt_estimates)
  setkey(nt_estimates, "id")
  
  # bring back to vocal activity
  focal_month <- merge(focal_month, nt_estimates, by = "id")
  
  
  va_counter = va_counter + 1
  va_holder[[va_counter]] <- focal_month
  
  cat('\n\n',year_month, 'completed','at', as.character(Sys.time()), "\n\n")
}

    
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
  
  fwrite(out, here('Results/VIIRS/vocal_activity_annotated_revisions_noct_locs_id.csv'))

# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
