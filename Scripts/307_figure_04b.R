library(here)
library(tidyverse)
library(mgcv)
library(glmmTMB)
library(MetBrewer)
library(cleangeo)
library(sf)
library(rworldmap)

setwd(here::here("Results"))

load("onset.RData")
load("cessation.RData")
rm(m1b, m2, m3, m4, m5, m6, m7, m8,
   e2, e3, e4, e5, e6, e7, e8)

m1_ranef <- glmmTMB::ranef( m1 )
e1_ranef <- glmmTMB::ranef( e1 )

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
cell_lat <-
  sf::st_sf(geometry = global_grid_5) |> 
  dplyr::mutate(grid_ID_cell_5 = dplyr::row_number()) |> 
  sf::st_centroid() |> 
  sf::st_coordinates() |> 
  tibble::as_tibble() |> 
  dplyr::pull(Y)

global_sf_5 <- sf::st_sf(geometry = global_grid_5) |> 
  dplyr::mutate(grid_ID_cell_5 = dplyr::row_number()) |> 
  tibble::add_column(lat = cell_lat)

sp_cells <- d_onset |> 
  dplyr::select(sci_name, grid_ID_cell_5, week, sp_cell5_week) |> 
  dplyr::distinct() |> 
  dplyr::mutate(sp_cell5_week = as.character(sp_cell5_week))

m.s <- m1_ranef$cond$`sp_cell5_week:family` |> 
  tibble::as_tibble( rownames = "group") |>
  tidyr::separate(group, into = c("sp_cell5_week", "family"), sep = ":") |> 
  dplyr::right_join(sp_cells) |> 
  dplyr::select(sci_name, week, grid_ID_cell_5, alan_sc) |> 
  dplyr::left_join(global_sf_5) |> 
  sf::st_as_sf() |> 
  sf::st_make_valid() |> 
  sf::st_drop_geometry() |> 
  dplyr::group_by(sci_name) |> 
  dplyr::mutate( nweek = length(unique(week))) |> 
  dplyr::filter(lat > 0) |> 
  dplyr::filter(nweek >50) |>
  dplyr::ungroup() 

e.s <- e1_ranef$cond$`sp_cell5_week:family` |> 
  tibble::as_tibble( rownames = "group") |>
  tidyr::separate(group, into = c("sp_cell5_week", "family"), sep = ":") |> 
  dplyr::right_join(sp_cells) |> 
  dplyr::select(sci_name, week, grid_ID_cell_5, alan_sc) |> 
  dplyr::left_join(global_sf_5) |> 
  sf::st_as_sf() |> 
  sf::st_make_valid() |> 
  sf::st_drop_geometry() |> 
  dplyr::group_by(sci_name) |> 
  dplyr::mutate( nweek = length(unique(week))) |> 
  dplyr::filter(lat > 0) |> 
  dplyr::filter(nweek >50) |> 
  dplyr::filter(!is.na(alan_sc))

sp.list <- sort(unique(m.s$sci_name))
res <- list(list())
for(i in 1:length(sp.list)){
  m <- mgcv::bam(
    alan_sc ~ s(week, bs = "cc", k = 6),
    data = filter(m.s, sci_name == sp.list[i]))
  newdata <- tibble(week = 1:53)
  p <- predict(m, newdata, newdata.guaranteed = TRUE, type = "response")
  min.week <- newdata |> 
    add_column(fit = p) |> 
    filter(fit == min(p)) |> 
    pull(week)
  res[[i]] <- tibble(
    species = sp.list[i], 
    min.week = min.week,
    min.y = min(p), 
    max.y = max(p))
  print(paste("finished", i))
}

sp.list.e <- sort(unique(e.s$sci_name))
res.e <- list(list())
for(i in 1:length(sp.list.e)){
  m <- mgcv::bam(
    alan_sc ~ s(week, bs = "cc", k = 6),
    data = filter(e.s, sci_name == sp.list.e[i]))
  newdata <- tibble(week = 1:53)
  p <- predict(m, newdata, newdata.guaranteed = TRUE, type = "response")
  min.week <- newdata |> 
    add_column(fit = p) |> 
    filter(fit == max(p)) |> 
    pull(week)
  res.e[[i]] <- tibble(
    species = sp.list.e[i], 
    min.week = min.week,
    min.y = min(p), 
    max.y = max(p))
  print(paste("finished", i))
  
}

both <- m.s |> 
  dplyr::select(sci_name, week, alan_sc) |> 
  tibble::add_column(period = "Onset") |> 
  dplyr::full_join(
    e.s |> 
      dplyr::select(sci_name, week, alan_sc) |> 
      tibble::add_column(period = "Cessation")) |> 
  dplyr::mutate(period = factor(period, levels = c("Onset", "Cessation")))

rect_df <-
  dplyr::bind_rows(res) |> 
  dplyr::arrange(min.week) |> 
  dplyr::mutate(flag = 1) |> 
  dplyr::mutate(cum = cumsum(flag) / sum(flag)) |>
  dplyr::mutate(min.y = min(min.y), 
                max.y = max(max.y)) |> 
  dplyr::filter(cum >= 0.25 & cum <= 0.75) |> 
  dplyr::summarise(min.x = min(min.week), 
                   max.x = max(min.week),
                   min.y = unique(min.y), 
                   max.y = unique(max.y)) |> 
  tibble::add_column(period = "Onset") |> 
  dplyr::full_join(
    # 50% of species have strongest ALAN cess responses between week 13 and 35 (42.3% of the year)
    dplyr::bind_rows(res.e) |> 
      dplyr::arrange(min.week) |> 
      dplyr::mutate(flag = 1) |> 
      dplyr::mutate(cum = cumsum(flag) / sum(flag)) |> 
      dplyr::mutate(min.y = min(min.y), 
                    max.y = max(max.y)) |> 
      dplyr::filter(cum >= 0.25 & cum <= 0.75) |> 
      dplyr::summarise(min.x = min(min.week), 
                       max.x = max(min.week),
                       min.y = unique(min.y), 
                       max.y = unique(max.y)) |> 
      tibble::add_column(period = "Cessation")) |> 
  dplyr::mutate(period = factor(period, levels = c("Onset", "Cessation")))

ggplot() +
  facet_wrap(~period, ncol = 1, scales = "free_y") +
  geom_rect(data = rect_df, aes(ymin = min.y, ymax = max.y, xmin = min.x, xmax = max.x),
            color = NA, 
            fill = MetBrewer::MetPalettes$Ingres[[1]][2], 
            alpha = 0.2)  +
  geom_smooth(data = both,
              aes(x = week, y = alan_sc, group = sci_name), formula = y~s(x, bs = "cc", k = 6),
              color = MetBrewer::MetPalettes$Ingres[[1]][6],
              se = FALSE,
              linewidth = 0.25,
              alpha = 0.3) +
  geom_smooth(data = both, 
              aes(x = week, y = alan_sc),
              formula = y~s(x, bs = "cc", k = 6),
              color = MetBrewer::MetPalettes$Ingres[[1]][4],
              se = FALSE,
              linewidth = 1.25) +
  theme_minimal() +
  labs(x = "Week of the year", 
       y = "Light pollution slope") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(color = "black",size = 8),
    legend.title = element_blank(),
    panel.grid = element_line(linewidth = 0.1),
    axis.title = element_text(color = "black", size = 8),
    axis.text = element_text(color = "black", size = 7),
    axis.line = element_line(color = "black", linewidth = 0.15)) 

setwd(here::here("Results/figures"))
ggsave(
  filename = "figure_04b.png", 
  width = 1.5,
  height = 3.75, 
  units = "in", 
  dpi = 600)