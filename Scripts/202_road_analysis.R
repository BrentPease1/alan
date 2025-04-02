library(tidyverse)
library(here)
library(janitor)
library(glmmTMB)
library(effects)
library(MetBrewer)

# key for joining up birdweather, EltonTraits, etc.
setwd(here::here("data/species_keys"))
key <- readr::read_csv("birdweather_elton_botw_name_key.csv")

# AvoNET trat database
setwd(here::here("data/traits"))
avo <- readr::read_csv("avonet.csv") |> 
  dplyr::mutate(lat_range = Max.Latitude - Min.Latitude)

# table indicating whether each species nests in cavities/burrows (1) or not (0)
cavity <- readr::read_csv("cavity.csv") |> 
  dplyr::rename(sci_name = sci_name_bw)

# EltonTraits database
elton <- read.delim(here::here("data/traits/elton.txt")) |> 
  janitor::clean_names() |> 
  dplyr::select( family = bl_family_latin, 
                 sci_name_elton = scientific,
                 mass = body_mass_value,
                 starts_with("diet"),
                 starts_with("for_strat")) |> 
  dplyr::select(-diet_source, -diet_certainty, -diet_entered_by, 
                -for_strat_source, -for_strat_spec_level, -for_strat_entered_by) |> 
  dplyr::right_join( key ) |> 
  tibble::as_tibble() |> 
  dplyr::select(sci_name = sci_name_bw, 
                family, 
                mass,
                starts_with("diet"), 
                starts_with("for_strat")) |> 
  dplyr::distinct()

setwd(here::here("data/vocalization_activity"))
# filtered and processed birdweather data
load("cessation_data_conf_0.75_det_100_grid_10.RData") # cessation of evening vocalization
load("onset_data_conf_0.75_det_100_grid_10.RData")     # onset of vocalization in the morning

# updated name key to correct for some idiosyncracies between AvoNet and BirdLife names
key2 <- key |> 
  dplyr::mutate(sci_name_avo = sci_name_botw) |> 
  dplyr::mutate(sci_name_avo = ifelse(sci_name_avo == "Curruca communis", 
                                      "Sylvia communis", sci_name_avo)) |> 
  dplyr::mutate(sci_name_avo = ifelse(sci_name_avo == "Curruca curruca", "Sylvia curruca",
                                      sci_name_avo)) |> 
  dplyr::mutate(sci_name_avo = ifelse(sci_name_avo == "Ortygornis sephaena",
                                      "Dendroperdix sephaena", sci_name_avo)) |> 
  dplyr::mutate(sci_name_avo = ifelse(sci_name_avo == "Grus canadensis", "Antigone canadensis",
                                      sci_name_avo)) |> 
  dplyr::mutate(sci_name_avo = ifelse(sci_name_avo  == "Tetrastes bonasia",
                                      "Bonasa bonasia", sci_name_avo))

rd <- readr::read_csv(here::here("data/rd_dist.csv"))

# final formatted dataset: evening
d_e <- final_cess |> 
  dplyr::ungroup() |>
  dplyr::rename(sci_name_bw = sci_name) |> 
  # join with avonet trait database
  dplyr::left_join(
    avo |> 
      janitor::clean_names() |> 
      dplyr::rename(sci_name_avo = species1) |> 
      dplyr::right_join(key2) |>
      dplyr::group_by(sci_name_bw) |> 
      # have to account for some species that have multiple entries (splits/lumps)
      dplyr::summarise(hand_wing_index = mean(hand_wing_index), 
                       habitat = unique(habitat), 
                       hd = unique(habitat_density),
                       migration = unique(migration), 
                       lat_range = mean(lat_range),
                       range_size = mean(range_size)) |> 
      dplyr::distinct() |> 
      dplyr::slice(1) |> 
      dplyr::ungroup()) |> 
  # covariate transformations
  dplyr::mutate(alan_sc = as.numeric(scale(log1p(avg_rad))), # scale ln of radiance + 1
                lat_sc = as.numeric(scale(abs(lat))), # scale absolute latitude
                value_hr = value / 60, # convert response variable from units of minutes to hours
                range_size_sc = as.numeric(scale(log(range_size))),
                lat_range_sc = as.numeric(scale(log(lat_range)))) |>  # scale ln of range size
  dplyr::rename(sci_name = sci_name_bw) |> 
  dplyr::group_by(sci_name, week) |> 
  # create group IDs for random effect groupings
  dplyr::mutate(sp_week = dplyr::cur_group_id()) |>  # species x week
  dplyr::group_by(sci_name, grid_ID_cell_5, week) |> 
  dplyr::mutate(sp_cell5_week = dplyr::cur_group_id()) |>  # species x 5 deg grid cell x week
  dplyr::group_by(sci_name, grid_ID_cell_10, week) |>  
  dplyr::mutate(sp_cell10_week = dplyr::cur_group_id()) |> # species x 10 deg grid cell x week 
  dplyr::group_by(sci_name, grid_ID_cell_15, week) |> 
  dplyr::mutate(sp_cell15_week = dplyr::cur_group_id()) |>  # species x 15 deg grid cell x week
  dplyr::ungroup() |> 
  dplyr::left_join(cavity) |> 
  dplyr::mutate(cavity = factor(cavity)) |> 
  dplyr::left_join( elton ) |> 
  # transformations of various trait columns
  dplyr::mutate(across(starts_with("for_strat"), function(x) x / 100)) |> 
  dplyr::mutate( inv_sc = as.numeric(scale(diet_inv)), 
                 ground_sc = as.numeric(scale(for_strat_ground)), 
                 under_sc = as.numeric(scale(for_strat_understory)), 
                 low_sc = as.numeric(scale (for_strat_ground + for_strat_understory)),
                 mass_sc = as.numeric(scale(log(mass))),
                 migration = factor(migration),
                 hd = factor(hd)) |> 
  dplyr::select(lat, lon, grid_ID_cell_5, date, week, sci_name, family, value_hr, avg_rad, alan_sc, lat_sc,
                range_size, range_size_sc, lat_range, lat_range_sc, migration, hd, for_strat_ground, ground_sc, cavity, sp_cell5_week, sp_week) |> 
  dplyr::left_join(rd) |> 
  dplyr::mutate(rd_raw = rd, 
                rd = as.numeric(scale(log(rd))))

# same data processing for morning onset
d_onset <- final |> 
  dplyr::ungroup() |>
  dplyr::rename(sci_name_bw = sci_name) |> 
  dplyr::left_join(
    avo |> 
      janitor::clean_names() |> 
      dplyr::rename(sci_name_avo = species1) |> 
      dplyr::right_join(key2) |>
      dplyr::group_by(sci_name_bw) |> 
      dplyr::summarise(hand_wing_index = mean(hand_wing_index), 
                       habitat = unique(habitat), 
                       migration = unique(migration),
                       hd = unique(habitat_density),
                       range_size = mean(range_size),
                       lat_range = mean(lat_range, na.rm = TRUE)) |> 
      dplyr::distinct() |> 
      dplyr::slice(1) |> 
      dplyr::ungroup()) |> 
  dplyr::mutate(alan_sc = as.numeric(scale(log1p(avg_rad))),
                lat_sc = as.numeric(scale(abs(lat))),
                value_hr = value / 60,
                range_size_sc = as.numeric(scale(log(range_size))),
                lat_range_sc = as.numeric(scale(log(lat_range))),
                migration = factor(migration),
                hd = factor(hd)) |> 
  dplyr::rename(sci_name = sci_name_bw) |> 
  dplyr::group_by(sci_name, week) |> 
  dplyr::mutate(sp_week = dplyr::cur_group_id()) |> 
  dplyr::group_by(sci_name, grid_ID_cell_5, week) |> 
  dplyr::mutate(sp_cell5_week = dplyr::cur_group_id()) |> 
  dplyr::group_by(sci_name, grid_ID_cell_10, week) |> 
  dplyr::mutate(sp_cell10_week = dplyr::cur_group_id()) |> 
  dplyr::group_by(sci_name, grid_ID_cell_15, week) |> 
  dplyr::mutate(sp_cell15_week = dplyr::cur_group_id()) |> 
  dplyr::ungroup() |> 
  dplyr::left_join(cavity) |> 
  dplyr::mutate(cavity = factor(cavity)) |> 
  dplyr::left_join( elton ) |> 
  dplyr::mutate(across(starts_with("for_strat"), function(x) x / 100)) |> 
  dplyr::mutate( inv_sc = as.numeric(scale(diet_inv)), 
                 ground_sc = as.numeric(scale(for_strat_ground)), 
                 under_sc = as.numeric(scale(for_strat_understory)), 
                 low_sc = as.numeric(scale (for_strat_ground + for_strat_understory)),
                 mass_sc = as.numeric(scale(log(mass)))) |> 
  dplyr::select(lat, lon, grid_ID_cell_5, date, week, sci_name, family, value_hr, avg_rad, alan_sc, lat_sc, 
                range_size, range_size_sc, lat_range, lat_range_sc, migration, hd, for_strat_ground, ground_sc, cavity, sp_cell5_week, sp_week) |> 
  dplyr::left_join(rd) |> 
  dplyr::mutate(rd_raw = rd, 
                rd = as.numeric(scale(log(rd))))

# clean up environment & save memory - remove uneeded tables
rm(avo, cavity, elton, final, 
   final_cess, key, key2, rd)

e_rd <- glmmTMB::glmmTMB(
  value_hr ~ 1 + alan_sc + rd + alan_sc:rd + (1 + alan_sc + rd + alan_sc:rd | family / sp_cell5_week),
  data = d_e)

m_rd <- glmmTMB::glmmTMB(
  value_hr ~ 1 + alan_sc + rd + alan_sc:rd + (1 + alan_sc + rd + alan_sc:rd | family / sp_cell5_week),
  data = d_onset)

setwd(here::here("Results"))
save(
  m_rd,
  e_rd, 
  d_onset, 
  d_e, 
  file = "distance_to_road_models.RData")

load(here::here("Results/distance_to_road_models.RData"))

alan.sc <- scale( log1p(d_e$avg_rad))
rd.sc <- scale( log(d_e$rd_raw) )
alan.sc.m <- scale( log1p(d_onset$avg_rad))
rd.sc.m <- scale( log(d_onset$rd_raw) )

e_rd_me <- effects::effect(
  term = c("alan_sc", "rd"), 
  xlevels = list(alan_sc = seq(from = min(d_e$alan_sc), 
                               to = max(d_e$alan_sc), 
                               length.out = 20), 
                 rd = c(min(d_e$rd), max(d_e$rd))),
  mod = e_rd)

m_rd_me <- effects::effect(
  term = c("alan_sc", "rd"), 
  xlevels = list(
    alan_sc = seq(from = min(d_onset$alan_sc), 
                  to = max(d_onset$alan_sc), 
                  length.out = 20), 
    rd = c(min(d_onset$rd), max(d_onset$rd))),
  mod = m_rd)

both <- m_rd_me |> 
  as.data.frame() |> 
  tibble::as_tibble() |> 
  dplyr::mutate(alan = alan_sc*attr(alan.sc.m, "scaled:scale") + attr(alan.sc.m, "scaled:center"),
                r = round(rd * attr(rd.sc.m, "scaled:scale") + attr(rd.sc.m, "scaled:center"),1)) |> 
  tibble::add_column(response = "Onset") |> 
  dplyr::select(response, alan, r, fit, lower, upper) |> 
  full_join(
    e_rd_me |> 
      as.data.frame() |> 
      tibble::as_tibble() |> 
      dplyr::mutate(alan = alan_sc*attr(alan.sc, "scaled:scale") + attr(alan.sc, "scaled:center"),
                    r = round(rd * attr(rd.sc, "scaled:scale") + attr(rd.sc, "scaled:center"),1)) |> 
      tibble::add_column(response = "Cessation") |> 
      dplyr::select(response, alan, r, fit, lower, upper)) |> 
  dplyr::mutate(response = factor(response, levels = c("Onset", "Cessation"))) 

rect_df <- tidyr::expand_grid(
  response = unique(both$response),
  period = c("Day", "Night")) |> 
  tibble::add_column(xmin = min(both$alan), 
                     xmax = max(both$alan)) |> 
  dplyr::mutate(ymin = ifelse(response == "Onset" & period == "Day", 0, 
                              ifelse(response == "Onset" & period == "Night", min(both$lower),
                                     ifelse(response == "Cessation" & period == "Day", min(both$lower), 0))),
                ymax = ifelse(response == "Onset" & period == "Day", max(both$upper), 
                              ifelse(response == "Onset" & period == "Night", 0, 
                                     ifelse(response == "Cessation" & period == "Day", 0, max(both$upper)))))


ggplot() +
  facet_wrap(~response) +
  geom_rect(data = filter(rect_df, period == "Day"),
            aes( xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = MetBrewer::MetPalettes$Ingres[[1]][5],
            color = NA,
            alpha = 0.15 ) +
  geom_rect(data = filter(rect_df, period == "Night"),
            aes( xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = MetBrewer::MetPalettes$Ingres[[1]][1],
            color = NA,
            alpha = 0.15 ) +
  geom_ribbon(
    data = both, 
    aes(x = alan, ymin = lower, ymax = upper, fill = factor(r)),
    color = NA, alpha = 0.2) + 
  geom_line(
    data = both, 
    aes(x = alan, y = fit, color = factor(r)),
    linewidth = 2) +
  labs( y = "Time relative to sunrise or sunset (hr)",
        x = "Light pollution: ln(Radiance + 1)",
        color = "ln(Distance-to-road [m])",
        fill = "ln(Distance-to-road [m])") +
  theme_minimal() +
  scale_color_manual(values = MetBrewer::MetPalettes$Cassatt2[[1]][c(3, 8)]) +
  scale_fill_manual(values = MetBrewer::MetPalettes$Cassatt2[[1]][c(3, 8)]) +
  theme(
    legend.position = "bottom",
    panel.grid = element_line(linewidth = 0.1, color ="gray90"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(color = "black", size = 10), 
    legend.text = element_text(color = "black", size = 9),
    strip.text = element_text(color = "black", size = 11),
    axis.title = element_text(color = "black", size = 10),
    axis.text = element_text(color = "black", size = 9),
    axis.line = element_line(color = "black", linewidth = 0.3))

setwd(here::here("Results/figures"))
ggsave(
  filename = "figure_s02.png", 
  width = 4, 
  height = 3.5, 
  units = "in", 
  dpi = 600)