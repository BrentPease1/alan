library(here)
library(tidyverse)

setwd(here::here("data"))

trait <- read.delim("bird_elton_trait.txt")

d <- readr::read_csv("vocal_activity_annotated_conf_0_det_10.csv")

species <- d |> 
  dplyr::select(`Common Name`) |>
  dplyr::distinct() |> 
  dplyr::arrange(`Common Name`)

test <- trait |> 
  dplyr::select(common = English) |> 
  distinct() 

anti_join(species, test) |> 
  write_csv("review_species.csv")
# Neil manually reviewed the species and resolved split issues, spelling issues, etc


review <- read_csv("review_species.csv")

species %>% 
  left_join(review) %>% 
  dplyr::mutate(omit = replace_na(omit, 0)) %>% 
  dplyr::mutate(English = ifelse(is.na(English), `Common Name`, English)) %>% 
  left_join(trait) %>% 
  dplyr::select(`Common Name`, English, omit, Nocturnal) %>% 
  filter(!(omit == 1)) %>% 
  dplyr::select(English, Nocturnal) %>% 
  distinct() %>% 
  count(Nocturnal)
