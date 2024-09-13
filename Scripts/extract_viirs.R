library(here)
library(data.table)
library(lubridate)
library(sf)
library(terra)

# script outline:
#   - read in vocalization activity measures calculated with Scripts/calculate_vocal_activity.R
#   - read in corresponding month of nighttime_light values
#   - extract point-level values
#   - consider buffers? 500m buffer around point? does effect dissapate or strengthen with distance from point? e.g., 1km buffer, area is saturated with light

# Read in calculated vocalization activity ####

continents <- c("Africa", "Asia", "Europe", "North America", "South America")

for(c in continents){

    if(c == "North America"){
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

    this_file <- list.files(path = paste0(here('Results/Birdweather/activity_measures'), "/", c), 
                            pattern = paste0(".*", this_month, "\\.csv$"),
                            full.names = T)
    
    # some data files might not exist
    if(rlang::is_empty(this_file)){
      next
    }
    
    # load in va file
    va <- fread(this_file)
    va[, lat_lon := .GRP, .(Latitude, Longitude)]
    # get unique locations for extracting VIIRS
    va_locs <- va[!duplicated(lat_lon), .(lat_lon, Latitude, Longitude)]
    va_locs <- st_as_sf(va_locs, coords = c('Longitude', 'Latitude'), crs = 4326)
    
    # Read in corresponding nighttime_lights ####
    base_file <- here('Data/VIIRS/nighttime_light/monthly/v10')
    
    # get year from this_month
    this_year <- gsub("\\D", "", this_month)
    
    # I wrote month names for Birdweather but VIIRS is numbers
    month_numbers <- data.frame(numbers = c("03","04",'05','06','07','08',
                                            "09","10","11","12","01","02","03"))
    this_number <- month_numbers[m,]
    
    year_num <- paste0(this_year,this_number)
    
    year_num_file <- paste0(base_file, "/", this_year, 
                            "/",year_num, "/","vcmslcfg")
    
    nt_files <- list.files(year_num_file, 
                        pattern = "\\.cf_cvg\\.tif$", 
                        full.names = TRUE, 
                        recursive = TRUE)
    
    # read in and stack with `terra`
    # Read each raster individually because of different extents
    nt_rast <- lapply(nt_files, rast)
    
    # loop through each file and try to extract values
    # will return NAs for non-overlapping points
    nt_holder <- list()
    for(i in 1:length(nt_rast)){
      this_rast <- nt_rast[[i]]
      nt_holder[[i]] <- extract(this_rast, vect(va_locs))
      names(nt_holder[[i]]) <- c('ID', paste0('cf_cvg_',i))
    }
    
    # bring together
    combined_df <- Reduce(function(x, y) merge(x, y, by = "ID", all = TRUE), nt_holder)
    
    # Just get single value across all columns
    combined_df$cf_cvg <- apply(combined_df[ , grep("cf_cvg", names(combined_df))], 1, function(x) {
      # Return the first non-NA value or NA if all are NA
      x[which(!is.na(x))[1]]
    })
    # keep the two columns 
    nt_estimates <- combined_df[ , c("ID", "cf_cvg")]
    setDT(nt_estimates)
    setkey(nt_estimates, "ID")
    
    # bring back to vocal activity
    va <- merge(va, nt_estimates, by.x = "lat_lon", by.y = "ID")
    
    # Assuming ev_ces is your data.table
    va[, nt_cat := fcase(
      cf_cvg < quantile(cf_cvg, probs = 0.1, na.rm = T), "low",
      cf_cvg >= quantile(cf_cvg, probs = 0.1, na.rm = T) & cf_cvg < quantile(cf_cvg, probs = 0.5, na.rm = T), "med",
      cf_cvg >= quantile(cf_cvg, probs = 0.5, na.rm = T), "high"
    )]
    

  } # months
     
} #continents
  

# some exploring