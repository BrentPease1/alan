library(avotrex)
library(tidyverse)
library(here)
library(ape)

data("BirdTree_trees")
data("bird.families")

phy <- BirdTree_trees[[1]]
tax <- BirdTree_tax |> 
  tibble::as_tibble()

setwd(here::here("data"))

key <- readr::read_csv("birdweather_elton_botw_name_key.csv")

load("onset_data_conf_0.75_det_100_grid_10.RData")
load("median_data_conf_0.75_det_100.RData")
load("cessation_data_conf_0.75_det_100_grid_10.RData")

species <- final |> 
  dplyr::ungroup() |> 
  dplyr::select(sci_name) |> 
  dplyr::distinct() |> 
  full_join(
    final_cess |> 
      dplyr::ungroup() |> 
      dplyr::select(sci_name) |> 
      dplyr::distinct()) |> 
  dplyr::full_join(
    final_med |> 
      dplyr::ungroup() |> 
      dplyr::select(sci_name) |> 
      dplyr::distinct()) |> 
  dplyr::rename(sci_name_bw = sci_name) |> 
  dplyr::left_join(key) 

focal_spp <- tax |> 
  mutate(sci_name = paste(Genus, Species)) |>
  dplyr::inner_join( 
    tibble(
      sci_name = unique(species$sci_name_elton)))

not_data <- anti_join(
    tibble(BLFamilyLatin = bird.families$tip.label),
    focal_spp)

my_fam <- ape::drop.tip( bird.families, not_data$BLFamilyLatin )
