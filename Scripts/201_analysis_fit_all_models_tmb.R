library(tidyverse)
library(here)
library(janitor)
library(glmmTMB)

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

tot.voc <- readr::read_csv("tot_voc.csv")

tot.voc.com <- tot.voc |> 
  dplyr::select(lat, lon, date, tot, sr, shan) |> 
  dplyr::distinct()

eye <- readr::read_csv(here::here("data/traits/ritland_clean.csv"))

eye.sp <- key2 |> 
  dplyr::select(sci_name = sci_name_bw, sci_name_elton) |> 
  dplyr::distinct() |> 
  dplyr::left_join(
    eye |> 
      dplyr::rename(sci_name_elton = species_jetz)) |> 
  dplyr::select(sci_name, cd1) |> 
  dplyr::distinct()

eye.fam <- eye |> 
  group_by(family_jetz) |> 
  dplyr::summarise(cd1.fam = mean(cd1)) |> 
  dplyr::rename(family = family_jetz)

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
  dplyr::left_join( tot.voc.com ) |> 
  dplyr::mutate( 
    shan_raw = shan,
    shan = as.numeric(scale(shan)),
    sr_raw = sr,
    sr = as.numeric(scale(log(sr))),
    tot_raw = tot,
    tot = as.numeric(scale(log(tot)))) |> 
  dplyr::select(lat, lon, grid_ID_cell_5, date, week, sci_name, family, value_hr, avg_rad, alan_sc, lat_sc,
                range_size, range_size_sc, lat_range, lat_range_sc, migration, hd, for_strat_ground, ground_sc, cavity, tot, tot_raw, sr, sr_raw, shan, shan_raw, sp_cell5_week, sp_week) |> 
  left_join(eye.sp) |> 
  left_join(eye.fam) |> 
  dplyr::mutate(
    cd = ifelse(!is.na(cd1), cd1, cd1.fam)) |> 
  dplyr::mutate(
    cd_raw = cd, 
    cd = as.numeric(scale(log(cd))))

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
  dplyr::left_join( tot.voc.com ) |> 
  dplyr::mutate( 
    shan_raw = shan,
    shan = as.numeric(scale(shan)),
    sr_raw = sr,
    sr = as.numeric(scale(log(sr))),
    tot_raw = tot,
    tot = as.numeric(scale(log(tot)))) |> 
  dplyr::select(lat, lon, grid_ID_cell_5, date, week, sci_name, family, value_hr, avg_rad, alan_sc, lat_sc, 
                range_size, range_size_sc, lat_range, lat_range_sc, migration, hd, for_strat_ground, ground_sc, cavity, tot, tot_raw, sr, sr_raw, shan, shan_raw, sp_cell5_week, sp_week) |> 
  left_join(eye.sp) |> 
  left_join(eye.fam) |> 
  dplyr::mutate(
    cd = ifelse(!is.na(cd1), cd1, cd1.fam)) |> 
  dplyr::mutate(
    cd_raw = cd,
    cd = as.numeric(scale(log(cd))))

# clean up environment & save memory - remove uneeded tables
rm(avo, cavity, elton, final, 
   final_cess, key, key2, tot.voc, tot.voc.com, eye, eye.fam, eye.sp)

# base model, evening cessation
# fixed effect of light pollution (alan_sc)
# nested random intercept and slope for alan_sc by family and species x 5 deg cell x week
e1 <- glmmTMB::glmmTMB(
  value_hr ~ 1 + alan_sc + (1 + alan_sc | family / sp_cell5_week),
  data = d_e)

# Latitude model, evening cessation
# fixed effects of light pollution, latitude (absolute), and their interaction
# random intercept and slopes by family and species x week (notice the different grouping - no grid cell)
e2 <- glmmTMB::glmmTMB(
  value_hr ~ 1 + alan_sc + lat_sc + alan_sc:lat_sc + (1 + alan_sc + lat_sc + alan_sc:lat_sc | family / sp_week),
  data = d_e)

# Range size model, evening cessation
# fixed effects of light pollution, range size, and their interaction
# random intercept and slope for light pollution by family and species x 5 deg cell x week
e3 <- glmmTMB::glmmTMB(
  value_hr ~ 1 + alan_sc + range_size_sc + alan_sc:range_size_sc + (1 + alan_sc | family / sp_cell5_week),
  data = d_e)

# Cavity model, evening cessation
# fixed effects of light pollution, cavity nesting (1/0), and their interaction
# random intercept and slope for light pollution by family and species x 5 deg cell x week
e4 <- glmmTMB::glmmTMB(
  value_hr ~ 1 + alan_sc + cavity + alan_sc:cavity + (1 + alan_sc | family / sp_cell5_week),
  data = d_e)

# % Ground foraging model, evening cessation'
# fixed effects of light pollution, % ground foraging, and their interaction
# random intercept and slope for light pollution by family and species x 5 deg cell x week
e5 <- glmmTMB::glmmTMB(
  value_hr ~ 1 + alan_sc + ground_sc + alan_sc:ground_sc + (1 + alan_sc | family / sp_cell5_week),
  data = d_e)

e6 <- glmmTMB::glmmTMB(
  value_hr ~ 1 + alan_sc + sr + alan_sc:sr + ( 1 + alan_sc + sr + alan_sc:sr | family/sp_cell5_week),
  data = d_e)

e7 <- glmmTMB::glmmTMB(
  value_hr ~ 1 + alan_sc + cd + alan_sc:cd + (1 + alan_sc | family/sp_cell5_week), 
  data = d_e)

e8 <- glmmTMB::glmmTMB(
  value_hr ~ 1 + alan_sc + migration + alan_sc:migration + (1 + alan_sc | family/sp_cell5_week), 
  data = d_e)

# save evening cessation results
setwd(here::here("Results"))
save(
  d_e,
  e1, e2, e3, e4, e5, e6, e7, e8,
  file = "cessation.RData" )

d_onset2 <- d_onset |>
  dplyr::filter(sci_name %in% unique(d_e$sci_name) ) |>
  dplyr::mutate(alan_sc = as.numeric(scale(log1p(avg_rad)))) |>
  dplyr::group_by(sci_name, grid_ID_cell_5, week) |>
  dplyr::mutate(sp_cell5_week = dplyr::cur_group_id()) |>
  dplyr::ungroup()

# base model, morning onset vocalization time
# fixed effect of light pollution (alan_sc)
# nested random intercept and slope for alan_sc by family and species x 5 deg cell x week
m1 <- glmmTMB::glmmTMB(
  value_hr ~ 1 + alan_sc + (1 + alan_sc | family / sp_cell5_week),
  data = d_onset)

# base model to the subset of species that occur in the cessation analysis
m1b <- glmmTMB::glmmTMB(
  value_hr ~ 1 + alan_sc + (1 + alan_sc | family / sp_cell5_week),
  data = d_onset2)

# Latitude model, morning onset
m2 <- glmmTMB::glmmTMB(
  value_hr ~ 1 + alan_sc + lat_sc + alan_sc:lat_sc + (1 + alan_sc + lat_sc + alan_sc:lat_sc | family / sp_week),
  data = d_onset)

# Range size model, morning onset
m3 <- glmmTMB::glmmTMB(
  value_hr ~ 1 + alan_sc + range_size_sc + alan_sc:range_size_sc + (1 + alan_sc | family / sp_cell5_week),
  data = d_onset)

# Cavity model, morning onset
m4 <- glmmTMB::glmmTMB(
  value_hr ~ 1 + alan_sc + cavity + alan_sc:cavity + (1 + alan_sc | family / sp_cell5_week),
  data = d_onset)

# habitat density
m5 <- glmmTMB::glmmTMB(
  value_hr ~ 1 + alan_sc + hd + alan_sc:hd + (1 + alan_sc | family / sp_cell5_week),
  data = d_onset)

m6 <- glmmTMB::glmmTMB(
  value_hr ~ 1 + alan_sc + sr + alan_sc:sr + ( 1 + alan_sc + sr + alan_sc:sr | family/sp_cell5_week),
  data = d_onset)

m7 <- glmmTMB::glmmTMB(
  value_hr ~ 1 + alan_sc + cd + alan_sc:cd + (1 + alan_sc | family/sp_cell5_week), 
  data = d_onset)

m8 <- glmmTMB::glmmTMB(
  value_hr ~ 1 + alan_sc + migration + alan_sc:migration + (1 + alan_sc | family/sp_cell5_week), 
  data = d_onset)

setwd(here::here("Results"))
save(
  d_onset,
  m1, m1b, m2, m3, m4, m5, m6, m7, m8,
  file = "onset.RData")