# final data formatting
# resolve some naming issues
# and omit "bad" species x site combos
library(here)
library(tidyverse)

setwd(here::here("data"))

key <- readr::read_csv("birdweather_elton_botw_name_key.csv") 

noct <- key |> 
  dplyr::select(sci_name = sci_name_bw, nocturnal) |> 
  dplyr::distinct()

d <- readr::read_csv("vocal_activity_annotated_conf_0.75_det_100.csv") |> 
  dplyr::mutate(
    sci_name = ifelse(sci_name == "Falcipennis canadensis", "Canachites canadensis", sci_name),
    sci_name = ifelse(sci_name == "Glossopsitta porphyrocephala", "Parvipsitta porphyrocephala", sci_name),
    sci_name = ifelse(grepl("Fox Sparrow", com_name), "Passerella iliaca", sci_name),
    com_name = ifelse(grepl("Fox Sparrow", com_name), "Fox Sparrow", com_name))

valid_combos <- readr::read_csv("species_site_combinations_final.csv")

combos <- valid_combos |> 
  dplyr::select(lat, lon, sci_name = sci_name_bw) |> 
  dplyr::distinct()

dat_onset <- d |> 
  dplyr::left_join(noct) |> 
  dplyr::filter(nocturnal == 0) |> 
  dplyr::inner_join( combos ) |> 
  dplyr::filter(category == "first_onset" )

dat_cess <- d |> 
  dplyr::left_join(noct) |> 
  dplyr::filter(nocturnal == 0) |> 
  dplyr::inner_join( combos ) |> 
  dplyr::filter(category == "ev_ces" )

final <- dat_onset |> 
  dplyr::select(lat, lon, starts_with("grid_ID"), week, sci_name, value, avg_rad) |> 
  dplyr::group_by(sci_name, grid_ID_cell_10, week) |> 
  dplyr::mutate(group = dplyr::cur_group_id())

save(
  final, 
  file = "onset_data_conf_0.75_det_100_grid_10.RData")

final_cess <- dat_cess |> 
  dplyr::select(lat, lon, starts_with("grid_ID"), week, sci_name, value, avg_rad) |> 
  dplyr::group_by(sci_name, grid_ID_cell_10, week) |> 
  dplyr::mutate(group = dplyr::cur_group_id())

save(
  final_cess, 
  file = "cessation_data_conf_0.75_det_100_grid_10.RData")