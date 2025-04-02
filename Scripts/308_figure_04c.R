library(here)
library(tidyverse)
library(mgcv)
library(glmmTMB)
library(MetBrewer)
library(sf)

setwd(here::here("Results"))
load("onset.RData")
load("cessation.RData")

rm( list = setdiff(ls(), c("m1", "e1", "d_onset", "d_e")))

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

cell_lat <- sf::st_centroid(global_sf_5) |> 
  sf::st_coordinates() |> 
  tibble::as_tibble() |> 
  dplyr::pull(Y)

cells <- global_sf_5 |> 
  tibble::add_column(lat = cell_lat) |> 
  sf::st_drop_geometry() |> 
  dplyr::select(grid_ID_cell_5, lat)

sp_cell_key_m <- d_onset |> 
  dplyr::select(sci_name, week, grid_ID_cell_5, sp_cell5_week) |> 
  dplyr::distinct()

sp_cell_key_e <- d_e |> 
  dplyr::select(sci_name, week, grid_ID_cell_5, sp_cell5_week) |> 
  dplyr::distinct()

m1_ranef <- glmmTMB::ranef( m1 )

e1_ranef <- glmmTMB::ranef( e1 )

slopes <- m1_ranef$cond$`sp_cell5_week:family` |> 
  tibble::as_tibble( rownames = "group") |>
  tidyr::separate(group, into = c("sp_cell5_week", "family"), sep = ":") |> 
  dplyr::mutate(sp_cell5_week = as.numeric(sp_cell5_week)) |> 
  dplyr::right_join(sp_cell_key_m) |> 
  dplyr::select(sci_name, week, grid_ID_cell_5, alan_sc) |> 
  dplyr::left_join(cells) |> 
  dplyr::group_by(grid_ID_cell_5, week) |> 
  dplyr::mutate(nsp = n()) |>
  dplyr::filter(nsp > 9) |>
  dplyr::mutate(neg = ifelse(alan_sc < 0, 1, 0)) |> 
  dplyr::summarise( prop = sum(neg) / nsp ) |> 
  dplyr::distinct() |> 
  dplyr::left_join(cells) |> 
  dplyr::mutate(lat_bin = cut(lat, breaks = c(-65, -20, 20, 45, 50, 65))) |> 
  dplyr::filter(!lat_bin == "(-20,20]") |> 
  tibble::add_column(response = "Onset") |> 
  dplyr::select(response, lat_bin, grid_ID_cell_5, week, prop)

slopes_e <- e1_ranef$cond$`sp_cell5_week:family` |> 
  tibble::as_tibble( rownames = "group") |>
  tidyr::separate(group, into = c("sp_cell5_week", "family"), sep = ":") |> 
  dplyr::mutate(sp_cell5_week = as.numeric(sp_cell5_week)) |> 
  dplyr::right_join(sp_cell_key_e) |> 
  dplyr::select(sci_name, week, grid_ID_cell_5, alan_sc) |> 
  dplyr::left_join(cells) |> 
  dplyr::group_by(grid_ID_cell_5, week) |> 
  dplyr::mutate(nsp = n()) |>
  dplyr::filter(nsp > 9) |>
  dplyr::mutate(neg = ifelse(alan_sc > 0, 1, 0)) |> 
  dplyr::summarise( prop = sum(neg) / nsp ) |> 
  dplyr::distinct() |> 
  dplyr::left_join(cells) |> 
  dplyr::mutate(lat_bin = cut(lat, breaks = c(-65, -20, 20, 45, 50, 65))) |> 
  dplyr::filter(!lat_bin == "(-20,20]") |> 
  tibble::add_column(response = "Cessation") |> 
  dplyr::select(response, lat_bin, grid_ID_cell_5, week, prop)

shd.m <- slopes |> 
  dplyr::filter(lat_bin == "(-65,-20]")

shm.m <- mgcv::bam(
  prop ~ s(week, bs = "cc", k = 4), 
  data = shd.m)

shp.m <- predict(shm.m, 
                 newdata = tibble(week = 1:53), 
                 newdata.guaranteed = TRUE, 
                 type = "response")

nhd.m <- slopes |> 
  dplyr::filter(lat_bin == "(50,65]")

nhm.m <- mgcv::bam(
  prop ~ s(week, bs = "cc", k = 4), 
  data = nhd.m)

nhp.m <- predict(nhm.m, 
                 newdata = tibble(week = 1:53), 
                 newdata.guaranteed = TRUE, 
                 type = "response")

nhd.e <- slopes_e |> 
  dplyr::filter(lat_bin == "(50,65]")

nhm.e <- mgcv::bam(
  prop ~ s(week, bs = "cc", k = 4), 
  data = nhd.e)

nhp.e <- predict(nhm.e, 
                 newdata = tibble(week = 1:53), 
                 newdata.guaranteed = TRUE, 
                 type = "response")


gam.fit <- tibble::tibble(week = 1:53) |> 
  tibble::add_column(nh.e = nhp.e,
                     nh.m = nhp.m, 
                     sh.m = shp.m) |> 
  tidyr::pivot_longer(nh.e:sh.m, names_to = "group", values_to = "prop") |> 
  dplyr::mutate(period = ifelse(grepl(".e", group), "Cessation", "Onset"),
                hemi = ifelse(grepl("sh", group), "Southern", "Northern")) |>
  dplyr::mutate(period = factor(period, 
                                levels = c(
                                  "Onset", 
                                  "Cessation")))

both <- dplyr::full_join(slopes, slopes_e) |> 
  dplyr::rename(period = response) |> 
  dplyr::mutate(period = factor(period, 
                                levels = c("Onset", "Cessation"))) 

ggplot() +
  facet_wrap(~period, ncol = 1) +
  geom_point(data = both, aes(x = week, y = prop), alpha = 0.05, size = 0.1) +
  geom_line( data = gam.fit, aes(x = week, y = prop, color = hemi), linewidth = 1.5) +
  scale_y_continuous(limits = c(0.25, 0.75)) +
  scale_color_manual(
    "Hemisphere", 
    values = MetBrewer::MetPalettes$Hiroshige[[1]][c(1,8)]) +
  theme_minimal() +
  labs(x = "Week of the year", 
       y = "Proportion of species with prolonged activity") +
  theme(
    legend.position = "bottom", 
    legend.title.position = "top",
    legend.key.size = unit(3, units = "mm"),
    legend.justification = "left",
    panel.grid = element_line(linewidth = 0.1),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(color = "black", size = 8, hjust = 0.5),
    strip.text = element_text(color = "black", size = 8),
    legend.text = element_text(color = "black", size = 7),
    axis.title = element_text(color = "black", size = 8),
    axis.text = element_text(color = "black", size = 7),
    axis.line = element_line(color = "black", linewidth = 0.15,),
    legend.margin = margin(-5, 0, 0, -15)) +
  guides(color = guide_legend(nrow = 1))

setwd(here::here("Results/Figures"))
ggsave(
  filename = "figure_04c.png", 
  width = 1.5,
  height = 3.75, 
  units = "in", 
  dpi = 600)