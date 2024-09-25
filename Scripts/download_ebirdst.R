# 25 September 2024
# Here's how we would download range maps from ebirdst
# Unfortunately, it seems like ~half of the species in our dataset
# don't have range maps available, so it's probably better to wait 
# to get access to birdlife 
library(ebirdst)
library(here)
library(dplyr)
library(readr)
library(tidyr)
library(janitor)

ebirdst::set_ebirdst_access_key("aqmr25t51ak0", overwrite = TRUE)

setwd(here::here("data"))

d <- readr::read_csv("vocal_activity_annotated_conf_0_det_10.csv")
review <- readr::read_csv("review_species.csv") |> 
  janitor::clean_names()

available <- ebirdst::ebirdst_runs |> 
  dplyr::select(code = species_code, scientific = scientific_name, common_name)

sp_list <-
  d |> 
  janitor::clean_names() |> 
  dplyr::select(common_name) |> 
  dplyr::distinct() |> 
  dplyr::left_join( review ) |> 
  dplyr::mutate(omit = tidyr::replace_na(omit, 0)) |> 
  dplyr::filter(omit == 0) |> 
  dplyr::select(common_name) |> 
  dplyr::mutate( common_name = ifelse(grepl("Fox Sparrow",
                                            common_name),
                                      "Fox Sparrow", common_name)) |> 
  dplyr::distinct() |> 
  dplyr::inner_join(available) |> 
  pull(common_name)

for(i in 1:length(sp_list)){
  ebirdst::ebirdst_download_status(
    species = sp_list[i],
    path = here::here("data"), 
    download_abundance = FALSE,
    download_occurrence = FALSE,
    download_count = FALSE, 
    download_ranges = TRUE, 
    download_regional = FALSE, 
    download_pis = FALSE, 
    download_ppms = FALSE, 
    download_all = FALSE, 
    pattern = "_27km_",
    dry_run = TRUE, 
    show_progress = TRUE
  )
}  
