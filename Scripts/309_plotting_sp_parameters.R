# quick script showing how to join up species x cell x week level parameters to spatial grid
library(here)
library(sf)
library(tidyverse)
library(MetBrewer)

d <- readr::read_csv( here::here("Results/Tables/species_level_parameters.csv"))

bounding_box <- sf::st_bbox(c(xmin = -180,
                              ymin = -90,
                              xmax = 180,
                              ymax = 90), crs = sf::st_crs(4326))

# Create a global polygon grid with cell_size spacing
global_grid_5 <- sf::st_make_grid(sf::st_as_sfc(bounding_box),
                                  cellsize = c(5, 5), crs = sf::st_crs(4326), what = "polygons")


# make sf object
global_sf_5 <- sf::st_sf(geometry = global_grid_5) |> 
  dplyr::mutate(grid_ID_cell_5 = dplyr::row_number())

params_with_spatial_info <- d |> 
  dplyr::left_join(global_sf_5)

# demo: plot estimated light pollution effects for 6 representative weeks for American Robin
params_with_spatial_info |> 
  dplyr::filter(sci_name == "Turdus migratorius") |> 
  dplyr::filter( week %in% c(1, 10, 20, 30, 40, 50)) |> 
  ggplot(aes( geometry = geometry, fill = slope)) +
  geom_sf() + 
  facet_wrap(~week) +
  scale_fill_gradient2(
    "Light pollution effect",
    low = MetBrewer::MetPalettes$Isfahan1[[1]][1],
    mid = "white",
    high = MetBrewer::MetPalettes$Isfahan1[[1]][6])
