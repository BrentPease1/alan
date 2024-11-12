library(here)
library(data.table)
library(lubridate)
library(suncalc)
library(lutz)
library(sf)
library(activity)
library(stringr)
library(ggplot2)
library(scales)
library(MetBrewer)
setDTthreads(0)

# overwrite current files?
overwrite <- F

if(!file.exists(here('Results/Birdweather/row_counter/birdweather_row_counter.csv') | overwrite == T)){
confidence_cutoff <- c(0.75)
detection_filter <- c(100)
continents <- c("Africa", "Asia", "Europe", "North America", "Oceania", "South America")

holder <- list()
counter = 0
for(c in continents){
  # get filing names correct for each continent
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
  
  # read in bird weather data
  for(m in 1:nrow(months)){
    
    
    # focal month for setting up period variable
    this_month <- months[m,]
    
    
    
    
    this_file <- list.files(path = paste0(here('Data/Birdweather'), "/", c), 
                            pattern = paste0(".*", this_month, "\\.csv$"),
                            full.names = T)
    
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
    
    # store N rows without any filtering
    unfiltered_rows <- nrow(bw)
    
    # confidence filter of 0.75
    bw <- bw[conf >= confidence_cutoff,]
    confidence_filtered_rows <- nrow(bw)

      # remove noise, mammals, insects, and amphibians
      not_interested <- c("Engine", "Siren", "Coyote", "Dog", 
                          "Eastern Gray Squirrel", "Red Squirrel",
                          "Power tools", "Fireworks", "Gray Wolf", "Gun",
                          "Honey Bee",
                          "Spring Peeper")
      bw <- bw[!(com_name %in% not_interested),]
      # this should keep frogmouths but drop anuras
      bw <- bw[!(str_detect(com_name, "frog(?!mouth)") | str_detect(com_name, "Frog(?!mouth)")),]
      bw <- bw[!(com_name %like% "Treefrog"),]
      bw <- bw[!(com_name %like% "Bullfrog"),]
      bw <- bw[!(com_name %like% "Cricket"),]
      bw <- bw[!(com_name %like% "Toad"),]
      bw <- bw[!(com_name %like% "Trig"),]
      bw <- bw[!(com_name %like% "Katydid"),]
      bw <- bw[!(com_name %like% "Chipmunk"),]
      bw <- bw[!(com_name %like% "Conehead"),]
      bw <- bw[!(com_name %like% "Gryllus assimilis"),]
      bw <- bw[!(com_name %like% "Human"),]
      bw <- bw[!(com_name %like% "Monkey"),]
      
      # initial prep of bw timestamps
      bw[, date_time := ymd_hms(date_time,tz = 'UTC')]
      bw <- bw[!is.na(date_time),] # a few fail to parse
      bw[, date := date(date_time)]
      bw[, week := week(date_time)]
      
      # get grouping variables
      bw[, station_date := .GRP, .(lat, lon, date)]
      
      
      total_spp <- bw[, .N, by = .(com_name, station_date)]
      total_spp <- total_spp[N > 100,]
      detection_filter_rows <- total_spp[, sum(N)]
      
      # bring it all together
      row_holder <- data.table(continent = c,
                               month_year = this_month,
                               unfiltered_rows = unfiltered_rows,
                               confidence_filtered_rows = confidence_filtered_rows,
                               detection_filter_rows = detection_filter_rows)
      counter = counter + 1
      holder[[counter]] <- row_holder
      cat('\n\n',c, this_month,'completed','at', as.character(Sys.time()), "\n\n")
      
  } # months

    } # continents


holder <- rbindlist(holder)

# write file to directory
fwrite(holder, 
       file = here('Results/Birdweather/row_counter/birdweather_row_counter.csv'))


cat('\n\n','ALL DONE','at', as.character(Sys.time()), "\n\n")
} else{
  holder <- here('Results/Birdweather/row_counter/birdweather_row_counter.csv'))
}


# Visualize

# clean column
holder[, month_year := str_replace(month_year, "20", "")]
# get factor
holder[, month_yr_factor := factor(month_year, levels = unique(month_year))]

# Choose colors from the MetBrewer Hiroshige palette
colors <- MetBrewer::MetPalettes$Hiroshige[[1]][c(1, 9)]  # Select two colors from the Hiroshige palette

# Plot with updated aesthetics
ggplot(holder, aes(x = month_yr_factor)) +
  geom_bar(aes(y = unfiltered_rows), stat = "identity", fill = colors[1], alpha = 0.8) +
  geom_bar(aes(y = detection_filter_rows), stat = "identity", fill = colors[2], alpha = 0.8) +
  labs(
    x = "Month of Year",
    y = "Number of Detections",
  ) +
  scale_y_continuous(labels = comma) + 
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.02),
    axis.title = element_text(color = "black", size = 10),
    axis.text = element_text(color = "black", size = 9,angle = 45, hjust = 1),
    legend.title = element_text(color = "black", size = 10),
    legend.text = element_text(color = "black", size = 9),
    panel.background = element_rect(color = NA, fill = "white"),
    plot.background = element_rect(color = NA, fill = "white"),
   # axis.text = element_text(angle = 45, hjust = 1)
  )
ggsave(filename = here('Results/Figures/birdweather_row_counter.png'), 
       plot = last_plot(),
       width = 6, 
       height = 4, 
       units = "in", 
       dpi = 600)
