library(tidyverse)
library(here)
library(janitor)
library(glmmTMB)

setwd(here::here("data/ELEData/TraitData"))
avo <- readr::read_csv("AVONET1_BirdLife.csv")

setwd(here::here("data"))
key <- readr::read_csv("birdweather_elton_botw_name_key.csv")

cavity <- readr::read_csv("cavity_nesting_reduced.csv") |> 
  dplyr::rename(sci_name = sci_name_bw)

elton <- read.delim("bird_elton_trait.txt") |> 
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

load("onset_data_conf_0.75_det_100_grid_10.RData")

alan_prop_sp <- final |> 
  dplyr::ungroup() |> 
  dplyr::mutate(alan_range = max(avg_rad) - min(avg_rad)) |> 
  dplyr::group_by(sci_name) |> 
  dplyr::mutate(alan_range_sp = max(avg_rad) - min(avg_rad)) |> 
  dplyr::mutate(alan_prop_sp = alan_range_sp / alan_range) |> 
  dplyr::select(sci_name, alan_prop_sp) |> 
  dplyr::distinct() |> 
  dplyr::ungroup()

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

d <- final |> 
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
                       range_size = mean(range_size)) |> 
      dplyr::distinct() |> 
      dplyr::slice(1) |> 
      dplyr::ungroup()) |> 
  dplyr::mutate(alan_sc = as.numeric(scale(log1p(avg_rad))),
                lat_sc = as.numeric(scale(abs(lat))),
                value_hr = value / 60,
                range_size_sc = as.numeric(scale(log(range_size)))) |> 
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
  dplyr::left_join(alan_prop_sp) |> 
  dplyr::mutate(across(starts_with("for_strat"), function(x) x / 100)) |> 
  dplyr::mutate( inv_sc = as.numeric(scale(diet_inv)), 
                 ground_sc = as.numeric(scale(for_strat_ground)), 
                 under_sc = as.numeric(scale(for_strat_understory)), 
                 low_sc = as.numeric(scale (for_strat_ground + for_strat_understory)),
                 mass_sc = as.numeric(scale(log(mass))),
                 alan_prop = as.numeric(scale(alan_prop_sp)))

# Base model: just ALAN, and random intercepts and slopes by species/cell/week combo
m1 <- glmmTMB(
  value_hr ~ 1 + alan_sc + (1 + alan_sc | sp_cell5_week),
  data = d)

# Latitude model
m2 <- glmmTMB(
  value_hr ~ 1 + alan_sc + lat_sc + alan_sc:lat_sc + (1 + alan_sc + lat_sc + alan_sc:lat_sc | sp_week),
  data = d)

# Range size model
m3 <- glmmTMB(
  value_hr ~ 1 + alan_sc + range_size_sc + alan_sc:range_size_sc + (1 + alan_sc | sp_cell5_week),
  data = d)

# Cavity model
m4 <- glmmTMB(
  value_hr ~ 1 + alan_sc + cavity + alan_sc:cavity + (1 + alan_sc | sp_cell5_week),
  data = d)

# Diet (% invertebrate) model
m5 <- glmmTMB(
  value_hr ~ 1 + alan_sc + inv_sc + alan_sc:inv_sc + (1 + alan_sc | sp_cell5_week),
  data = d)

# % Ground foraging model
m6 <- glmmTMB(
  value_hr ~ 1 + alan_sc + ground_sc + alan_sc:ground_sc + (1 + alan_sc | sp_cell5_week),
  data = d)

# % understory foraging model
m7 <- glmmTMB(
  value_hr ~ 1 + alan_sc + under_sc + alan_sc:under_sc + (1 + alan_sc | sp_cell5_week),
  data = d)

# % foraging low (ground + understory)
m8 <- glmmTMB(
  value_hr ~ 1 + alan_sc + low_sc + alan_sc:low_sc + (1 + alan_sc | sp_cell5_week),
  data = d)

# body mass (linear)
m9 <- glmmTMB(
  value_hr ~ 1 + alan_sc + mass_sc + alan_sc:mass_sc + (1 + alan_sc | sp_cell5_week),
  data = d)

# body mass (quadratic)
m10 <- glmmTMB(
  value_hr ~ 1 + alan_sc + mass_sc + I(mass_sc^2) + alan_sc:mass_sc + alan_sc:I(mass_sc^2) + (1 + alan_sc | sp_cell5_week),
  data = d)

# % of ALAN gradient species detected at
m11 <- glmmTMB(
  value_hr ~ 1 + alan_sc + alan_prop + alan_sc:alan_prop + (1 + alan_sc | sp_cell5_week),
  data = d)

setwd(here::here("results"))
save(
  d, 
  m1, m2, m3, 
  m4, m5, m6, 
  m7, m8, m9, 
  m10, m11, 
  file = "tmb_onset_models.RData" 
)