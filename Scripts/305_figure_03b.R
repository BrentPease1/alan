library(here)
library(tidyverse)
library(glmmTMB)
library(MetBrewer)
library(cleangeo)
library(sf)
library(rworldmap)

setwd(here::here("Results"))

load("tmb_onset_models_family.RData")

m1_ranef <- glmmTMB::ranef( m1 )

sPDF <- rworldmap::getMap()

sPDF <- cleangeo::clgeo_Clean(sPDF)

continents <- sPDF |> 
  sf::st_as_sf() |> 
  dplyr::filter(!is.na(Stern))

bounding_box <- sf::st_bbox(c(xmin = -180,
                              ymin = -90,
                              xmax = 180,
                              ymax = 90), crs = sf::st_crs(4326))

crop_box <- sf::st_as_sfc(sf::st_bbox(c(
  xmin = -125,
  ymin = -60, 
  xmax = 160, 
  ymax = 80), crs = sf::st_crs(4326)))

cont_crop <- sf::st_crop(continents, crop_box)

# Create a global polygon grid with cell_size spacing
global_grid_5 <- sf::st_make_grid(sf::st_as_sfc(bounding_box),
                                  cellsize = c(5, 5), crs = sf::st_crs(4326), what = "polygons")


# make sf object
global_sf_5 <- sf::st_sf(geometry = global_grid_5) |> 
  dplyr::mutate(grid_ID_cell_5 = dplyr::row_number())

# 3 example species
# Eurasian Blackbird, Magpie-lark, Northern Cardinal
sp_cells <- d_onset |> 
  dplyr::filter( sci_name %in% c("Turdus merula",
                                 "Grallina cyanoleuca",
                                 "Cardinalis cardinalis")) |> 
  dplyr::select(sci_name, grid_ID_cell_5, week, sp_cell5_week) |> 
  dplyr::distinct() |> 
  dplyr::mutate(sp_cell5_week = as.character(sp_cell5_week))

p <- m1_ranef$cond$`sp_cell5_week:family` |> 
  tibble::as_tibble( rownames = "group") |>
  tidyr::separate(group, into = c("sp_cell5_week", "family"), sep = ":") |> 
  dplyr::right_join(sp_cells) |> 
  dplyr::select(sci_name, week, grid_ID_cell_5, alan_sc) |> 
  dplyr::left_join(global_sf_5) |> 
  sf::st_as_sf() |> 
  sf::st_make_valid()

key <- tribble(
  ~week, ~week_lab, 
  1, "1 January", 
  21, "20 May") |> 
  dplyr::mutate(week_lab = factor(week_lab, 
                           levels = c("1 January", "20 May")))

sp_plot <- sf::st_centroid(p) |> 
  sf::st_coordinates() |> 
  cbind(grid_ID_cell_5 = p$grid_ID_cell_5,
        week = p$week,
        sci_name = p$sci_name) |> 
  tibble::as_tibble() |> 
  dplyr::mutate(across(X:week, function(x) as.numeric(x))) |> 
  dplyr::filter(! (sci_name == "Turdus merula" & Y < 0)) |> 
  dplyr::left_join(p) |> 
  dplyr::filter(week %in% c(1,21)) |> 
  dplyr::left_join(key)
  
ggplot() +
  theme_void() +
  geom_sf(data = cont_crop, aes(geometry = geometry),
          color = NA, fill = "gray90") +
  geom_sf(data = sp_plot, (aes(geometry = geometry,
                            fill = alan_sc)),
          linewidth = 0.1) +
  facet_wrap(~week_lab, nrow = 3) +
  scale_fill_gradient2(
    "Light pollution effect",
    low = MetBrewer::MetPalettes$Isfahan1[[1]][1],
    mid = "white",
    high = MetBrewer::MetPalettes$Isfahan1[[1]][6]) +
  theme(strip.text = element_text(size = 9, 
                                  color = "black"), 
        legend.position = "bottom",
        legend.title.position = "top",
        # legend.key.size = unit(3, units = "mm"),
        legend.key.width = unit(5, units = "mm"),
        panel.spacing = unit(-1, "lines"),
        legend.key.height = unit(2, units = "mm"),
        legend.title = element_text(size = 9,
                                    hjust = 0.5,
                                    color = "black"), 
        legend.box.margin = margin(-25, 0, 5, 0, unit = "pt"),
        legend.text = element_text(size = 8, 
                                   color = "black"), 
        legend.ticks = element_blank(),
        legend.frame = element_rect(color = "black", linewidth = 0.2),
        plot.background = element_rect(color = NA, fill = "white"))

setwd(here::here("Results/Figures"))
ggsave(
  filename = "figure_03b.png", 
  width = 3.25, 
  height = 3.75, 
  units = "in", 
  dpi = 600)
