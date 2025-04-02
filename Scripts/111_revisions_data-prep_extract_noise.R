library(here)
library(data.table)
library(sf)
library(terra)

# # key for joining up birdweather, EltonTraits, etc.
# setwd(here::here("data/species_keys"))
# key <- readr::read_csv("birdweather_elton_botw_name_key.csv")
# 
# # AvoNET trat database
# setwd(here::here("data/traits"))
# avo <- readr::read_csv("avonet.csv")
# 
# # table indicating whether each species nests in cavities/burrows (1) or not (0)
# cavity <- readr::read_csv("cavity.csv") |> 
#   dplyr::rename(sci_name = sci_name_bw)
# 
# # EltonTraits database
# elton <- read.delim(here::here("data/traits/elton.txt")) |> 
#   janitor::clean_names() |> 
#   dplyr::select( family = bl_family_latin, 
#                  sci_name_elton = scientific,
#                  mass = body_mass_value,
#                  starts_with("diet"),
#                  starts_with("for_strat")) |> 
#   dplyr::select(-diet_source, -diet_certainty, -diet_entered_by, 
#                 -for_strat_source, -for_strat_spec_level, -for_strat_entered_by) |> 
#   dplyr::right_join( key ) |> 
#   tibble::as_tibble() |> 
#   dplyr::select(sci_name = sci_name_bw, 
#                 family, 
#                 mass,
#                 starts_with("diet"), 
#                 starts_with("for_strat")) |> 
#   dplyr::distinct()
# 
# setwd(here::here("data/vocalization_activity"))
# filtered and processed birdweather data
load(here::here("data/vocalization_activity/cessation_data_conf_0.75_det_100_grid_10.RData")) # cessation of evening vocalization
load(here::here("data/vocalization_activity/onset_data_conf_0.75_det_100_grid_10.RData"))     # onset of vocalization in the morning

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
                       migration = unique(migration), 
                       range_size = mean(range_size)) |> 
      dplyr::distinct() |> 
      dplyr::slice(1) |> 
      dplyr::ungroup()) |> 
  # covariate transformations
  dplyr::mutate(alan_sc = as.numeric(scale(log1p(avg_rad))), # scale ln of radiance + 1
                lat_sc = as.numeric(scale(abs(lat))), # scale absolute latitude
                value_hr = value / 60, # convert response variable from units of minutes to hours
                range_size_sc = as.numeric(scale(log(range_size)))) |>  # scale ln of range size
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
                 mass_sc = as.numeric(scale(log(mass)))) 

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
  dplyr::mutate(across(starts_with("for_strat"), function(x) x / 100)) |> 
  dplyr::mutate( inv_sc = as.numeric(scale(diet_inv)), 
                 ground_sc = as.numeric(scale(for_strat_ground)), 
                 under_sc = as.numeric(scale(for_strat_understory)), 
                 low_sc = as.numeric(scale (for_strat_ground + for_strat_understory)),
                 mass_sc = as.numeric(scale(log(mass))))

# clean up environment & save memory - remove uneeded tables
rm(avo, cavity, elton, final, 
   final_cess, key, key2)

library(data.table) # i just can't anymore
setDT(d_e)
# extract sound layer for CONUS
d_e <- d_e[, lat_lon_grp := .GRP, .(lat,lon)]
d_e_locs <- d_e[!duplicated(lat_lon_grp)]
d_e_locs <- st_as_sf(d_e_locs[, .(lat_lon_grp, lat, lon)], coords = c('lon', 'lat'), crs = 4326)
sound <- rast("C:/Users/siu850591813/Downloads/CONUS_L50dBA_sumDay_exi.tif")


d_e_locs <- extract(sound, vect(d_e_locs))
d_e_locs <- d_e_locs %>%
  filter(!is.na(CONUS_L50dBA_sumDay_exi))

# onset
setDT(d_onset)
d_onset <- d_onset[, lat_lon_grp := .GRP, .(lat,lon)]
d_onset_locs <- d_onset[!duplicated(lat_lon_grp)]
d_onset_locs <- st_as_sf(d_onset_locs[, .(lat_lon_grp, lat, lon)], coords = c('lon', 'lat'), crs = 4326)


d_onset_locs <- extract(sound, vect(d_onset_locs))
d_onset_locs <- d_onset_locs %>%
  filter(!is.na(CONUS_L50dBA_sumDay_exi))

test <- merge(d_onset, d_onset_locs, by.x = 'lat_lon_grp', by.y = 'ID')
test2 <- merge(d_e, d_e_locs, by.x = 'lat_lon_grp', by.y = 'ID')
test2$sound_sc <- scale(test2$CONUS_L50dBA_sumDay_exi)
test$sound_sc <- scale(test$CONUS_L50dBA_sumDay_exi)

m1 <- glmmTMB::glmmTMB(
  value_hr ~ 1 + alan_sc + (1 + alan_sc | family / sp_cell5_week),
  data = test)
m2 <- glmmTMB::glmmTMB(
  value_hr ~ 1 + sound_sc + (1 + sound_sc | family / sp_cell5_week),
  data = test)
m0 <- glmmTMB::glmmTMB(
  value_hr ~ 1,
  data = test)

m3 <- glmmTMB::glmmTMB(
  value_hr ~ 1 + sound_sc + alan_sc + (1 + sound_sc + alan_sc | family / sp_cell5_week),
  data = test)
