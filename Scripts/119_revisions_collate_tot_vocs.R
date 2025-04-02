library(tidyverse)
library(here)
library(vegan)

setwd(here::here("data/vocalization_activity"))

africa <- readr::read_csv("spp_rich_tot_vocs_africa.csv")
asia <- readr::read_csv("spp_rich_tot_vocs_asia.csv")
europe <- readr::read_csv("spp_rich_tot_vocs_europe.csv")
n.america <- readr::read_csv("spp_rich_tot_vocs_north_america.csv")
oceania <- readr::read_csv("spp_rich_tot_vocs_oceania.csv")
s.america <- readr::read_csv("spp_rich_tot_vocs_south_america.csv")

all <- dplyr::full_join(africa, asia) |> 
  dplyr::full_join(europe) |> 
  dplyr::full_join(n.america) |> 
  dplyr::full_join(oceania) |> 
  dplyr::full_join(s.america) |> 
  dplyr::select(sci_name, com_name,  lat, lon, date, n = N_dets) |> 
  dplyr::group_by(lat, lon, date) |> 
  dplyr::mutate(tot = sum(n)) |> 
  dplyr::mutate(other = tot - n, 
                sr = sum(n>0),
                shan = vegan::diversity(n, index = "shannon")) |> 
  dplyr::mutate(
    sci_name = ifelse(sci_name == "Falcipennis canadensis", "Canachites canadensis", sci_name),
    sci_name = ifelse(sci_name == "Glossopsitta porphyrocephala", "Parvipsitta porphyrocephala", sci_name),
    sci_name = ifelse(grepl("Fox Sparrow", com_name), "Passerella iliaca", sci_name),
    com_name = ifelse(grepl("Fox Sparrow", com_name), "Fox Sparrow", com_name))

readr::write_csv( all, "tot_voc.csv" )