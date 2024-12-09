library(tidyverse)
library(here)
library(taxize)

# two species with 2 scientific names in birdweather
d <- readr::read_csv( here::here("data/vocalization_activity/vocal_activity_annotated_conf_0_det_10.csv" )) |> 
  dplyr::mutate( sci_name = ifelse(sci_name == "Falcipennis canadensis", "Canachites canadensis", sci_name),
                 sci_name = ifelse(sci_name == "Glossopsitta porphyrocephala", "Parvipsitta porphyrocephala", sci_name))

setwd(here::here("data/traits"))

# elton traits database
trait <- read.delim("elton.txt") |> 
  dplyr::select(sci_name = Scientific, 
                english = English, 
                nocturnal = Nocturnal)

combos <- d |> 
  dplyr::select( com_name, sci_name, lat, lon ) |> 
  dplyr::distinct()

spp <- combos |> 
  dplyr::select(com_name, sci_name) |> 
  dplyr::distinct() |> 
  dplyr::filter(! (com_name == "Greater Whitethroat" & sci_name == "Curruca communis")) |> 
  dplyr::filter(! (com_name == "Lesser Whitethroat" & sci_name == "Curruca curruca")) |> 
  dplyr::filter(! (com_name == "Leach's Storm-Petrel" & sci_name == "Hydrobates leucorhous")) |> 
  dplyr::filter(! ( grepl("Fox Sparrow", com_name) & sci_name != "Passerella iliaca"))

# basic join of birdweather and elton by birdweather scientific name
spp_trait <- spp |> 
  dplyr::left_join(trait)

# species that are joined on birdweather's scientific name
sp_trait_elton <- spp_trait |> 
  dplyr::filter(!is.na(nocturnal)) |> 
  dplyr::mutate(sci_name_bw = sci_name, 
                sci_name_elton = sci_name) |> 
  dplyr::select(-sci_name)

# search for species synonyms for the species that failed to join on birdweather scientific name
spp_syn <- spp |> 
  dplyr::left_join(trait) |> 
  dplyr::filter(is.na(nocturnal))

# look for synonyms; this takes awhile
syn_list <- taxize::synonyms( spp_syn$sci_name, db = "itis" )

temp <- taxize::synonyms_df( syn_list ) |> 
  tibble::as_tibble() |>
  dplyr::select(sci_name = .id, 
                sci_name_syn = syn_name) |> 
  dplyr::distinct() |> 
  tidyr::separate(sci_name_syn, into = c("genus", "sp", "subsp"), sep = " ") |> 
  dplyr::select(-subsp) |> 
  dplyr::mutate(sci_name_syn = paste(genus, sp)) |> 
  dplyr::select(-genus, -sp) |> 
  dplyr::distinct() 

# stash the synonyms
setwd(here::here("data/species_keys"))
write_csv(temp, "taxize_scientific_synonyms.csv")

# get an extra 170 species with the itis synonyms
trait_dat <- synonyms_df( syn_list ) |> 
  tibble::as_tibble() |>
  dplyr::select(sci_name = .id, 
                sci_name_syn = syn_name) |> 
  dplyr::distinct() |> 
  tidyr::separate(sci_name_syn, into = c("genus", "sp", "subsp"), sep = " ") |> 
  dplyr::select(-subsp) |> 
  dplyr::mutate(sci_name_syn = paste(genus, sp)) |> 
  dplyr::select(-genus, -sp) |> 
  dplyr::distinct() |>
  dplyr::left_join(spp) |> 
  dplyr::select(com_name, sci_name_bw = sci_name, sci_name = sci_name_syn) |> 
  dplyr::left_join(trait) |> 
  dplyr::filter(!is.na(nocturnal)) |> 
  dplyr::select(com_name, sci_name_bw = sci_name_bw, sci_name_elton = sci_name, nocturnal)

# have all but about 60 species
trait_dat_more <- full_join(sp_trait_elton, trait_dat)

# join birdweather and elton traits on common name - gets us another 46 rows
elton_common_name_join <- anti_join(spp, trait_dat_more) |> 
  dplyr::left_join(temp) |> 
  dplyr::select(english = com_name, sci_name_bw = sci_name, sci_name_syn) |> 
  dplyr::left_join(trait) |> 
  dplyr::filter(!is.na(nocturnal)) |> 
  dplyr::select(com_name = english, sci_name_bw, sci_name_elton = sci_name, nocturnal)

trait_dat_more_more <- trait_dat_more |> 
  dplyr::full_join(elton_common_name_join) |> 
  dplyr::select(com_name, sci_name_bw, sci_name_elton, nocturnal) 

# 50 or so species manually reviwed to get names
setwd(here::here("data/species_keys"))
manual_review <- readr::read_csv("review_species.csv")

man_review_traits <- manual_review |> 
  dplyr::select(com_name, sci_name = sci_name_manual) |> 
  dplyr::left_join(trait) |> 
  dplyr::select(com_name, sci_name_elton = sci_name, nocturnal) |> 
  dplyr::left_join(spp) |> 
  dplyr::select(com_name, sci_name_bw = sci_name, sci_name_elton, nocturnal)

final_trait <- trait_dat_more_more |> 
  dplyr::full_join(man_review_traits) |> 
  dplyr::distinct() |> 
  dplyr::filter(!(com_name == "Pine Warbler" & sci_name_elton == "Vermivora pinus")) |> 
  dplyr::filter(!(com_name == "Purple-crowned Lorikeet" & sci_name_bw == "Parvipsitta porphyrocephala")) |> 
  dplyr::filter(!(com_name == "Spruce Grouse" & sci_name_bw == "Falcipennis canadensis")) |> 
  dplyr::filter(!is.na(sci_name_bw))

# scientific names for range maps
bow_species <- readr::read_csv(here::here("data/species_keys/botw_scientific_names.csv")) |> 
  dplyr::select(sci_name = scientific) |> 
  tibble::add_column(flag = 0)

# joined on birdweather scientific name
bow_trait1 <- final_trait |> 
  dplyr::select(com_name, sci_name = sci_name_bw, sci_name_elton) |> 
  dplyr::distinct() |> 
  dplyr::left_join(bow_species) |> 
  dplyr::filter(!is.na(flag)) |> 
  dplyr::rename(sci_name_bw = sci_name) |> 
  dplyr::mutate(sci_name_botw = sci_name_bw)

# joined on eltontraits scientific name
bow_trait2 <- final_trait |>
  dplyr::filter(sci_name_bw != sci_name_elton) |> 
  dplyr::select(com_name, sci_name_bw, sci_name = sci_name_elton) |> 
  dplyr::distinct() |> 
  dplyr::left_join(bow_species) |> 
  dplyr::filter(!is.na(flag)) |> 
  dplyr::select(com_name, sci_name_bw, sci_name_elton = sci_name, flag) |> 
  dplyr::filter(!is.na(com_name)) |> 
  dplyr::mutate(sci_name_botw = sci_name_elton)

# review about 50 species
# full_join(bow_trait1, bow_trait2) |> 
#   filter(!is.na(com_name)) |> 
#   full_join(final_trait) |> 
#   filter(is.na(sci_name_botw)) |> 
#   dplyr::arrange(com_name) |> 
#   write_csv("review_sp_botw.csv")

botw_review <- readr::read_csv(here::here("data/species_keys/review_sp_botw.csv")) |> 
  dplyr::select(-flag, -nocturnal)

name_key <- dplyr::full_join(bow_trait1, bow_trait2) |> 
  dplyr::select(-flag) |> 
  dplyr::full_join(botw_review) |> 
  dplyr::filter(!is.na(com_name)) |> 
  dplyr::left_join(final_trait)

setwd(here::here("data/species_keys"))
readr::write_csv(name_key, "birdweather_elton_botw_name_key.csv")