library(here)
library(tidyverse)
library(sf)
library(rworldmap)
library(cleangeo)

setwd(here::here("data/vocalization_activity"))

load("onset_data_conf_0.75_det_100_grid_10.RData")

sites <- final |> 
  dplyr::select(lat, lon) |> 
  dplyr::distinct() |> 
  sf::st_as_sf(coords = c("lon", "lat"), 
               crs = 4326)

sPDF <- rworldmap::getMap()

sPDF <- cleangeo::clgeo_Clean(sPDF)

continents <- sPDF |> 
  sf::st_as_sf() |> 
  dplyr::filter(!is.na(Stern))

ggplot() +
  geom_sf(data = continents, color = NA, fill = "gray50") +
  geom_sf(data = sites,
          size = 0.1,
          color = MetBrewer::MetPalettes$Hiroshige[[1]][1]) +
  theme_void() +
  theme(
    panel.background = element_rect(color = NA, fill = "black"), 
    plot.background = element_rect(color = NA, fill = "black"))

setwd(here::here("Results/Figures"))
ggsave(
  filename = "figure_s01.png", 
  width = 10, 
  height = 5,
  units = "in", 
  dpi = 600)