# do some manual filtering out of out-of-range detections

library(tidyverse)
library(here)

setwd(here::here("data/error_species_maps"))
# 8 species to do manual/custom range filtering
error_notes <- readr::read_csv("error_species.csv") 

manual_good <- error_notes |> 
  dplyr::filter(good == "yes") |> 
  dplyr::pull(sci_name_botw)

manual_bad <- error_notes |> 
  dplyr::filter(good == "no") |> 
  dplyr::pull(sci_name_botw)

setwd(here::here("data"))
key <- readr::read_csv("birdweather_elton_botw_name_key.csv")

load("range_map_filtering.RData")

# 9 species that we have to manually filter out bad records
bad <- combos |> 
  dplyr::filter(sci_name_botw %in% manual_bad) |> 
  dplyr::filter(!(sci_name_botw == "Botaurus stellaris" & lon < -30)) |> 
  dplyr::filter(!(sci_name_botw == "Calidris maritima")) |> 
  dplyr::filter(!(sci_name_botw == "Catharus minimus" & lon < -110 & lat < 48)) |> 
  dplyr::filter(!(sci_name_botw == "Coccothraustes coccothraustes" & lon < -30)) |> 
  dplyr::filter(!(sci_name_botw == "Phylloscopus collybita" & lon < -30)) |> 
  dplyr::filter(!(sci_name_botw == "Helopsaltes ochotensis")) |> 
  dplyr::filter(!(sci_name_botw == "Setophaga pinus" & lon < -101.8)) |> 
  dplyr::filter(!(sci_name_botw == "Turdus iliacus" & lon < -30)) |> 
  dplyr::filter(!(sci_name_botw ==  "Zapornia pusilla"))

# manually reviewed species that don't need any custom spatial filtering to remove out-of-range detections
good <- combos |> 
  dplyr::filter(sci_name_botw %in% manual_good)

# assemble the final species x site combinations
# first get lat/lon associated with the updated combos, then stack the manually reviewed tables
final_combos <- combos |> 
  dplyr::select(site, lat, lon) |> 
  dplyr::distinct() |> 
  dplyr::right_join(
    updated_combos
  ) |> 
dplyr::full_join(good) |> 
  dplyr::full_join(bad) |> 
  dplyr::select(-in_range)

setwd(here::here("data"))
write_csv(final_combos, "species_site_combinations_final.csv")