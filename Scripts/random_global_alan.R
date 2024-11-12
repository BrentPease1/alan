library(here)
library(data.table)
library(lubridate)
library(sf)
library(terra)
library(rworldmap)
# Load the continents data
continents <- rworldmap::getMap()
continents <- st_as_sf(continents)
# fix geometries
continents <- st_make_valid(continents)

sample_locs <- st_sample(continents, 8653) #number of unique lat/lons for birdweather
sample_locs <- data.table(lat = st_coordinates(sample_locs)[,2],
                          lon =st_coordinates(sample_locs)[,1])

sample_locs <- st_as_sf(sample_locs, coords = c('lon', 'lat'), crs = 4326)                 
sample_locs$ID <- 1:nrow(sample_locs)
# read in viirs
nt_files <- list.files("E:/nighttime_data/2024/202403", 
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
  nt_holder[[i]] <- extract(this_rast, vect(sample_locs))
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


nt_estimates <- nt_estimates[!is.na(avg_rad)]
nt_estimates[, rad_sc := log1p(avg_rad)]


these_samples <- sample_locs[sample_locs$ID %in% nt_estimates$ID,]
plot(these_samples)



# remove negative or NA radiance alues
nt_estimates[, bad_rad := ifelse(is.na(rad_sc) | rad_sc <0, "bad_rad", "good_rad")]
nt_estimates <- nt_estimates[bad_rad == 'good_rad',]


# Plot the histogram
color_palette <- c(rep("#5e6174",6), rep("#8a7a72",6), rep("#bb996e",6),
                   rep("#e1b36a",6), rep("#f7c267", 6))  # Replace with your hex codes
ggplot() +
  geom_histogram(data = nt_estimates, aes(x = rad_sc), fill = 'black', color = "black", alpha = 0.7) +
  geom_histogram(data = viirs, aes(x = rad_sc), fill = color_palette, color = "black", alpha = 0.7) +
  
  labs(
    x = "Light Pollution: ln(Radiance + 1)",
    y = "Sensors",
    #  title = "Distribution of Average Radiance Across Stations"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.02),
    axis.title = element_text(color = "black", size = 10),
    axis.text = element_text(color = "black", size = 9),
    legend.title = element_text(color = "black", size = 10),
    legend.text = element_text(color = "black", size = 9),
    panel.background = element_rect(color = NA, fill = "white"),
    plot.background = element_rect(color = NA, fill = "white")
  )

ggsave(filename = here('Results/Figures/viirs_talling_global_overlap.png'), 
       plot = last_plot(),
       width = 6, 
       height = 4, 
       units = "in", 
       dpi = 600)
