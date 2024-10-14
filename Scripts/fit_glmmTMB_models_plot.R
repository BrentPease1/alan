library(here)
library(lme4)
library(effects)
library(tidyverse)
library(glmmTMB)
library(sf)

setwd(here::here("data"))

load("onset_data_conf_0.75_det_100_grid_10.RData")

d <- final |> 
  dplyr::ungroup() |> 
  dplyr::mutate(alan_sc = as.numeric(scale(log1p(avg_rad))),
                lat_sc = as.numeric(scale(abs(lat))),
                value_hr = value / 60) |> 
  dplyr::group_by(sci_name, week) |> 
  dplyr::mutate(sp_week = dplyr::cur_group_id()) |> 
  dplyr::group_by(sci_name, grid_ID_cell_5, week) |> 
  dplyr::mutate(sp_cell5_week = dplyr::cur_group_id()) |> 
  dplyr::group_by(sci_name, grid_ID_cell_10, week) |> 
  dplyr::mutate(sp_cell10_week = dplyr::cur_group_id()) |> 
  dplyr::group_by(sci_name, grid_ID_cell_15, week) |> 
  dplyr::mutate(sp_cell15_week = dplyr::cur_group_id()) |> 
  dplyr::ungroup()

m1 <- glmmTMB(
  value_hr ~ 1 + alan_sc + (1 + alan_sc | sp_cell5_week),
  data = d)

setwd(here::here("Results"))
save(
  m1, 
  d, 
  file = "m1_tmb_alan_sp_cell5_week.RData")

m2 <- glmmTMB(
  value_hr ~ 1 + alan_sc + lat_sc + alan_sc:lat_sc + (1 + alan_sc + lat_sc + alan_sc:lat_sc | sp_week),
  data = d)

save(
  m2, 
  d, 
  file = "m2_tmb_alan_lat_sp_week.RData")


bounding_box <- sf::st_bbox(c(xmin = -180, ymin = -90, xmax = 180, ymax = 90), crs = st_crs(4326))

# Create a global polygon grid with cell_size spacing
global_grid_5 <- sf::st_make_grid(st_as_sfc(bounding_box),
                                cellsize = c(5, 5), crs = st_crs(4326), what = "polygons")


# make sf object
global_sf_5 <- sf::st_sf(geometry = global_grid_5) |> 
  dplyr::mutate(grid_ID_cell_5 = dplyr::row_number())

m1_ranef <- glmmTMB::ranef( m1 )

key <- d |> 
  dplyr::select(sci_name, grid_ID_cell_5, week, sp_cell5_week) |> 
  dplyr::distinct() |> 
  dplyr::mutate(sp_cell5_week = as.character(sp_cell5_week))

prop_alan <- m1_ranef$cond$sp_cell5_week |> 
  tibble::as_tibble(rownames = "sp_cell5_week") |> 
  janitor::clean_names() |> 
  dplyr::left_join(key) |> 
  dplyr::select( sci_name, grid_ID_cell_5, week, alan_sc) |> 
  dplyr::distinct( ) |> 
  dplyr::group_by(grid_ID_cell_5, week) |> 
  dplyr::mutate(n = n()) |> 
  dplyr::mutate(n_neg = sum(alan_sc < 0),
                n_neg_0.02 = sum(alan_sc < -0.02)) |>
  dplyr::summarise(prop_neg = n_neg / n, 
                   prop_neg_0.02 = n_neg_0.02 / n,
                   n = unique(n)) |> 
  dplyr::distinct()

map_prop <- global_sf_5 |> 
  dplyr::left_join(prop_alan) |> 
  dplyr::filter(!is.na(week)) |> 
  dplyr::filter(n > 1) # filter out only cells that have >10 species for a given week

map_prop |>
  sf::st_centroid() |> 
  sf::st_coordinates() |> 
  cbind(map_prop) |> 
  dplyr::mutate( lat_bin = cut( Y, breaks = c(-65, -20, 0, 20, 40, 50, 65))) |> 
  dplyr::filter(! lat_bin == "(0,20]") |> 
  dplyr::filter(! lat_bin == "(-20,0]") |> 
  ggplot(aes(x = week, y = prop_neg, color = lat_bin, fill = lat_bin)) +
  geom_jitter(alpha = 0.15) +
  geom_smooth() +
  theme_minimal() +
  scale_color_manual(values = MetBrewer::MetPalettes$Hiroshige[[1]][c(1, 3, 7, 9)]) +
  scale_fill_manual(values = MetBrewer::MetPalettes$Hiroshige[[1]][c(1, 3, 7, 9)]) +
  labs( x = "Week of year",
        y = "Proportion of species with negative light pollution slopes",
        color = "Latitude bin",
        fill = "Latitude bin") +
  theme(axis.line = element_line(color = "black", linewidth = 0.02),
        axis.title = element_text(color = "black", size = 10), 
        axis.text = element_text(color = "black", size = 9), 
        legend.title = element_text(color = "black", size = 10), 
        legend.text = element_text(color = "black", size = 9), 
        panel.background = element_rect(color = NA, fill = "white"), 
        plot.background = element_rect(color = NA, fill = "white"))

setwd(here::here())
ggsave(
  filename = "seasonality_plot.png", 
  width = 6, 
  height = 4, 
  units = "in", 
  dpi = 600
) 

# quick check - for a species, how much of the ALAN gradient do we have data for it?
d |> 
  dplyr::mutate(alan_gradient = max(alan_sc) - min(alan_sc)) |> 
  dplyr::group_by(sci_name) |> 
  dplyr::mutate(alan_gradient_sp = max(alan_sc) - min(alan_sc)) |> 
  dplyr::select(sci_name, alan_gradient_sp, alan_gradient) |> 
  distinct() |> 
  dplyr::mutate( sp_prop = alan_gradient_sp / alan_gradient) |> 
  dplyr::filter(sp_prop > 0.75) |> 
  arrange(-sp_prop) 

# was originall thinking to make marginal effects plots for just robin
# skipping that for now
# make a blank slate of prediction data
# slate <- tidyr::expand_grid(
#   week = unique(d$week), 
#   lat_sc = seq(from = min(d$lat_sc), to = max(d$lat_sc), by = 0.75), 
#   alan_sc = seq(from = min(d$alan_sc), to = max(d$alan_sc), by = 0.75))
# 
# # isolate down to robin data, get relevant values for each week and cell combination
# newdat <- d |> 
#   filter(sci_name == "Turdus migratorius") |> 
#   dplyr::select(lat_sc, alan_sc, week, sp_week) |> 
#   dplyr::distinct() |> 
#   dplyr::group_by(sp_week, week) |> 
#   dplyr::summarise(min_alan = min(alan_sc), 
#                    max_alan = max(alan_sc), 
#                    min_lat = min(lat_sc), 
#                    max_lat = max(lat_sc)) |> 
#   dplyr::ungroup() |> 
#   dplyr::full_join(slate) |> 
#   dplyr::filter(!(alan_sc < min_alan)) |> 
#   dplyr::filter(!(alan_sc > max_alan)) |> 
#   dplyr::filter(!(lat_sc < min_lat)) |> 
#   dplyr::filter(!(lat_sc > max_lat))
# 
# amro_pred <- predict(m2, newdat, se.fit = TRUE)
# 
# pdat <- newdat |> 
#   cbind(fit = amro_pred$fit,
#         se = amro_pred$se.fit) |>
#   dplyr::mutate(lat_sc = factor(round(lat_sc, 1))) |> 
#   as_tibble() |> 
#   filter(week %in% seq(from = 1, to = 53, by = 5))
#   
#   
# ggplot(pdat, aes(x = alan_sc, y = fit, color = lat_sc)) +
#   facet_wrap(~week) +
#   geom_ribbon(aes(ymin = fit - se, ymax = fit + se, fill = lat_sc), color = NA, alpha = 0.2) +
#   geom_line(aes(group = lat_sc))

# cells where we had robins
rob_cells <- d |> 
  dplyr::filter(sci_name == "Turdus migratorius") |> 
  dplyr::select(sci_name, grid_ID_cell_5, week, sp_cell5_week) |> 
  dplyr::distinct() |> 
  dplyr::mutate(sp_cell5_week = as.character(sp_cell5_week))

p <- m1_ranef$cond$sp_cell5_week |> 
  as_tibble(rownames = "sp_cell5_week") |> 
  dplyr::right_join(rob_cells) |> 
  dplyr::select(sci_name, week, grid_ID_cell_5, alan_sc) |> 
  left_join(global_sf_5) |> 
  filter(week %in% seq(from = 1, to = 53, by = 3)) 

north_am <- rnaturalearth::ne_countries( scale = "small",
                                         returnclass = "sf") |> 
  dplyr::filter(!region_un == "Antarctica") |> 
  dplyr::filter(continent == "North America") |> 
  dplyr::cross_join( p |> 
                       dplyr::select(week) |> 
                       dplyr::distinct()) 
ggplot() +
  facet_wrap(~week) +
  geom_sf(data = north_am, aes(geometry = geometry),
          color = NA, fill = "gray90") +
  geom_sf(data = p, aes(geometry = geometry, fill = alan_sc)) +
  scale_fill_gradient2(
    low = MetBrewer::MetPalettes$Isfahan1[[1]][1],
    mid = "white",
    high = MetBrewer::MetPalettes$Isfahan1[[1]][6]
  ) +
  theme_void() +
  labs(fill = "Light pollution effect") +
  theme(legend.ticks = element_blank(),
        legend.direction = "horizontal",
        legend.title.position = "top",
        legend.position = c(0.8, 0.1),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA))

ggsave(
  filename = "amro_alan_effect_map.png", 
  width = 7, 
  height = 5, 
  units = "in", 
  dpi = 600
)