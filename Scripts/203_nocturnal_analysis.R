# 2 April 2025
# Analysis: influence of light pollution on nocturnal species
# Evaluating two responses:
## 1) time of detection relative to nadir (same detection and confidence filter as diurnal analysis)
## 2) number of detections per night (only confidence filter)

library(tidyverse)
library(here)
library(glmmTMB)
library(sf)
library(patchwork)
library(MetBrewer)

bounding_box <- sf::st_bbox(c(xmin = -180,
                              ymin = -90,
                              xmax = 180,
                              ymax = 90), crs = sf::st_crs(4326))

global_grid_5 <- sf::st_make_grid(sf::st_as_sfc(bounding_box),
                                  cellsize = c(5, 5), crs = sf::st_crs(4326), what = "polygons") |> 
  sf::st_sf() |> 
  dplyr::mutate(ID = dplyr::row_number())

load(here::here("data/vocalization_activity/nocturnal_nadir_v01.RData"))

coords <- nadir |> 
  dplyr::select(lat, lon) |> 
  dplyr::distinct() |> 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

coords_with_cell <- sf::st_join( coords, global_grid_5, join = st_within) |> 
  dplyr::mutate(lon = sf::st_coordinates(geometry)[,1],
                lat = sf::st_coordinates(geometry)[,2]) |> 
  sf::st_drop_geometry() |> 
  dplyr::group_by(lat, lon) |> 
  dplyr::slice(1)

moon <- readr::read_csv(here::here("data/moon_data.csv")) |> 
  dplyr::group_by(lat, lon, date) |> 
  dplyr::summarise(ill = mean(ill), phase = mean(phase)) |> 
  dplyr::ungroup()

alan <- readr::read_csv(here::here("data/vocal_activity_annotated_revisions_noct_locs_id.csv")) |> 
  dplyr::select(lat, lon, date, avg_rad) |> 
  dplyr::distinct() |> 
  dplyr::mutate(date = lubridate::mdy(date))

nadir <- nadir |> 
  dplyr::mutate(month = lubridate::month(date)) |> 
  dplyr::select(lat, lon, date, week, month, sci_name, com_name, nadir = min_time_nadir) |> 
  dplyr::left_join(alan) |> 
  dplyr::filter(!is.na(avg_rad)) |> 
  dplyr::mutate(nadir_hr = abs(nadir / 60) + 0.01) |> 
  dplyr::left_join(coords_with_cell) |> 
  dplyr::group_by(lat, lon, date, sci_name) |> 
  dplyr::mutate(n = n()) |> 
  dplyr::filter(n >100) |> # detection filter
  dplyr::ungroup() |>
  dplyr::mutate(alan_sc = as.numeric(scale(log1p(avg_rad)))) |> 
  dplyr::group_by(sci_name, ID, week) |> 
  dplyr::mutate(sp.cell.week = dplyr::cur_group_id()) |> 
  dplyr::left_join(moon) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(phase_sc = as.numeric(scale(phase))) 

alan.sc <- scale(log1p(nadir$avg_rad))

m1 <- glmmTMB::glmmTMB(
  nadir_hr ~ 1 + alan_sc + (1 + alan_sc | sp.cell.week), 
  family = Gamma(link = "log"),
  data = nadir)

p.m1 <- predict(m1, tibble::tibble(alan_sc = seq(from = min(nadir$alan_sc), 
                                                 to = max(nadir$alan_sc),
                                                 length.out = 20)),
                re.form = NA,
                type = "response",
                se = TRUE)

m1_me <- tibble::tibble(alan_sc = seq(from = min(nadir$alan_sc), 
                                      to = max(nadir$alan_sc),
                                      length.out = 20)) |> 
  tibble::add_column(fit = p.m1$fit, 
                     se = p.m1$se.fit) |> 
  dplyr::mutate(alan = alan_sc*attr(alan.sc, "scaled:scale") + attr(alan.sc, "scaled:center"))


( nadir.plot <- ggplot() +
    geom_ribbon( data = m1_me, aes(x = alan, ymin = fit - se, ymax = fit + se),
                 fill = MetBrewer::MetPalettes$Hiroshige[[1]][9], 
                 linewidth = 2,
                 color = NA, 
                 alpha = 0.4) +
    geom_line( data = m1_me, aes(x = alan, y = fit),
               color = MetBrewer::MetPalettes$Hiroshige[[1]][9], 
               linewidth = 2) +
    geom_text(aes(x = 1.3, y = 2.6), 
              label = expression(bold("Light:")~"-0.04 ± 0.03, p = 0.20"),
              size = 2.55,
              hjust = 0) +
    theme_minimal() +
    labs( y = "Time from nadir (hr)",
          x = "ln(Radiance + 1)",
          fill = "Count",
          title = "(A)") +
    theme_minimal() +
    theme(
      plot.title = element_text(color = "black", size = 10),
      # panel.grid = element_line(linewidth = 0.1, color ="gray90"),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(color = "black", size = 10), 
      legend.text = element_text(color = "black", size = 9),
      strip.text = element_text(color = "black", size = 11),
      axis.title = element_text(color = "black", size = 10),
      axis.text = element_text(color = "black", size = 9),
      axis.line = element_line(color = "black", linewidth = 0.3)) )

tot <- readr::read_csv(here::here("data/vocalization_activity/nocturnal_species_tot_vocs.csv")) |> 
  dplyr::select(sci_name, com_name, lat, lon, date, n = N_dets) |> 
  dplyr::left_join(alan) |> 
  dplyr::mutate(alan_sc = as.numeric(scale(log1p(avg_rad)))) |> 
  dplyr::left_join(coords_with_cell) |> 
  dplyr::mutate(week = lubridate::week(date)) |> 
  dplyr::group_by(sci_name, ID, week) |> 
  dplyr::mutate(sp.cell.week = dplyr::cur_group_id()) |> 
  dplyr::ungroup() |> 
  dplyr::left_join(moon) |> 
  dplyr::mutate(phase_sc = as.numeric(scale(phase)))

m2 <- glmmTMB(
  n ~ 1 + alan_sc + phase_sc + alan_sc:phase_sc + ( 1 + alan_sc + phase_sc + alan_sc:phase_sc |sp.cell.week), 
  family = "truncated_poisson", 
  data = tot)

new.data <- tidyr::expand_grid(
  alan_sc = seq(from = min(tot$alan_sc), 
                to = max(tot$alan_sc), 
                length.out = 20), 
  phase_sc = c(min(tot$phase_sc), 
               max(tot$phase_sc)))

p <- predict( m2, new.data, re.form = NA, type = "response", se = TRUE)

alan.sc.tot <- scale(log1p(tot$avg_rad))

m2_me <- new.data |> 
  tibble::add_column(fit = p$fit, se = p$se.fit) |> 
  dplyr::mutate(phase_sc = ifelse(phase_sc < 0, "New", "Full")) |> 
  dplyr::mutate(alan = alan_sc*attr(alan.sc.tot, "scaled:scale") + attr(alan.sc.tot, "scaled:center"))

( tot.plot <- ggplot() +
    geom_ribbon(
      data = m2_me, 
      aes(x = alan,ymin = fit - se, ymax = fit + se, fill = factor(phase_sc)), color = NA, alpha = 0.4) +
    geom_line(data = m2_me, aes(x = alan, y = fit, color = factor(phase_sc)),
              linewidth = 2) +
    theme_minimal() +
    labs( y = "Number of vocalizations per night",
          x = "ln(Radiance + 1)",
          color = "Moon\nphase",
          fill = "Moon\nphase",
          title = ("(B)")) +
    theme_minimal() +
    geom_text(aes(x = 1, y = 1.8),
              label = expression(bold("Light:")~"-0.43 ± 0.06, p < 0.0001"),
              size = 2.55, 
              color = "black", 
              hjust = 0) +
    geom_text(aes(x = 1, y = 1.725),
              label = expression(bold("Moon:")~"0.16 ± 0.06, p = 0.006"),
              size = 2.55, 
              color = "black",
              hjust = 0) +
    geom_text(aes(x = 1,
                  y = 1.65),
              label = expression(bold("Light x Moon:")~"0.13 ± 0.06, p = 0.02"),
              size = 2.55, 
              color = "black",
              hjust = 0) +
    scale_color_manual(values = MetBrewer::MetPalettes$Demuth[[1]][c(4, 9)]) +
    scale_fill_manual(values = MetBrewer::MetPalettes$Demuth[[1]][c(4, 9)]) +
    coord_cartesian(clip = "off") +
    theme(
      # legend.position = "bottom",
      legend.justification = "bottom",
      plot.title = element_text(color = "black", size = 10),
      # panel.grid = element_line(linewidth = 0.1, color ="gray90"),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      legend.key.size = unit(c(0.75,0.75), "lines"),
      legend.box.margin = margin(0, -45, 0, 0, unit = "pt"),
      panel.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(color = "black", size = 9), 
      legend.text = element_text(color = "black", size = 8),
      strip.text = element_text(color = "black", size = 11),
      axis.title = element_text(color = "black", size = 10),
      axis.text = element_text(color = "black", size = 9),
      axis.line = element_line(color = "black", linewidth = 0.3)) ) 

nadir.plot | tot.plot

setwd(here::here("Results/Figures"))
ggsave(
  filename = "figure_s03.png", 
  width = 5.5, 
  height = 2.75, 
  units = "in", 
  dpi = 600)
