library(avotrex)
library(tidyverse)
library(here)
library(ape)
library(biscale)
library(janitor)
library(ggtree)
library(cowplot)
library(glmmTMB)

data("BirdTree_trees")
data("bird.families")

phy <- BirdTree_trees[[1]]
tax <- BirdTree_tax |> 
  tibble::as_tibble()

key <- readr::read_csv(here::here("data/species_keys/birdweather_elton_botw_name_key.csv"))

load(here::here("data/vocalization_activity/onset_data_conf_0.75_det_100_grid_10.RData"))
load(here::here("data/vocalization_activity/cessation_data_conf_0.75_det_100_grid_10.RData"))

species <- final |> 
  dplyr::ungroup() |> 
  dplyr::select(sci_name) |> 
  dplyr::distinct() |> 
  dplyr::full_join(
    final_cess |> 
      dplyr::ungroup() |> 
      dplyr::select(sci_name) |> 
      dplyr::distinct()) |>  
  dplyr::rename(sci_name_bw = sci_name) |> 
  dplyr::left_join(key) 

focal_spp <- tax |> 
  dplyr::mutate(sci_name = paste(Genus, Species)) |>
  dplyr::inner_join( 
    tibble::tibble(
      sci_name = unique(species$sci_name_elton))) |> 
  dplyr::group_by(BLFamilyLatin) |> 
  dplyr::slice(1) |> 
  dplyr::select(TipLabel, family = BLFamilyLatin) |> 
  dplyr::ungroup() 

not_data <- dplyr::anti_join( tax, focal_spp)

my_tree <- ape::drop.tip( phy, not_data$TipLabel)

family_map <- focal_spp |> 
  tibble::deframe()

my_tree$tip.label <- family_map[my_tree$tip.label]

setwd(here::here("Results"))
load("tmb_onset_models_family.RData")
load("tmb_evening_models_family.RData")

m1_ranef <- glmmTMB::ranef(m1)
e1_ranef <- glmmTMB::ranef(e1)

fam_re <- m1_ranef$cond$family |> 
  tibble::as_tibble(rownames = "tip.label") |> 
  janitor::clean_names() |> 
  dplyr::rename(tip.label = tip_label)

fam_re_ces <- e1_ranef$cond$family |> 
  tibble::as_tibble(rownames = "tip.label") |> 
  janitor::clean_names() |> 
  dplyr::rename(tip.label = tip_label)

setwd(here::here("data/traits"))
avo <- readr::read_csv("avonet.csv") |> 
  dplyr::select(species_jetz = Species1, mass = Mass) 

setwd(here::here("data"))

eye <- readr::read_csv("Ritland_eyes_raw_FINAL.csv")

eye_fam <- eye |>
  dplyr::select(species_jetz, family_jetz, CD1) |> 
  dplyr::left_join(avo) |> 
  dplyr::filter(!is.na(mass)) |> 
  dplyr::group_by(family_jetz) |>
  dplyr::summarise( CD1 = mean(CD1), 
                    mass = mean(mass)) 

fam_onset_eye <- fam_re |> 
  dplyr::rename(family_jetz = tip.label) |> 
  dplyr::mutate(family_jetz = ifelse(family_jetz == "Diomedeidae", 
                                     "Procellariidae", family_jetz)) |> 
  left_join(eye_fam) |> 
  dplyr::rename(int = intercept, 
                slope = alan_sc) |>
  dplyr::mutate( CD1 = CD1, 
                 mass = as.numeric(scale(log(mass))))

fam.res <- glmmTMB::glmmTMB(
  CD1 ~ 1 + mass, 
  data = fam_onset_eye)

fam_onset_eye2 <- fam_onset_eye |> 
  add_column(res = resid(fam.res)) |> 
  mutate(CD1 = as.numeric(scale(log(CD1))))

fam_ces_eye <- fam_re_ces |> 
  dplyr::rename(family_jetz = tip.label) |> 
  dplyr::mutate(family_jetz = ifelse(family_jetz == "Diomedeidae", 
                                     "Procellariidae", family_jetz)) |> 
  left_join(eye_fam) |> 
  dplyr::rename(int = intercept, 
                slope = alan_sc) |> 
  dplyr::mutate(CD1 = as.numeric(scale(CD1))) |> 
  dplyr::left_join(
    fam_onset_eye2 |> 
      dplyr::select(family_jetz, res)
  )

# raw eye size significant
m1 <- glmmTMB::glmmTMB( 
  int ~ 1 + CD1, 
  data = fam_onset_eye2)

# mass-corrected eye size not significant
m1.r <- glmmTMB::glmmTMB(
  int ~ 1 + res, 
  data = fam_onset_eye2)

# raw eye size not signficaitn for alan slope
m2 <- glmmTMB::glmmTMB( 
  slope ~ 1 + CD1, 
  data = fam_onset_eye2)

# mass-corrected eye size not signficant for alan slope
m2.r <- glmmTMB(
  slope ~ 1 + res, 
  data = fam_onset_eye2)

e1 <- glmmTMB::glmmTMB( 
  int ~ 1 + CD1, 
  data = fam_ces_eye)

e1.r <- glmmTMB::glmmTMB( 
  int ~ 1 + res, 
  data = fam_ces_eye)

e2 <- glmmTMB::glmmTMB( 
  slope ~ 1 + CD1, 
  data = fam_ces_eye)

e2.r <- glmmTMB::glmmTMB( 
  slope ~ 1 + res, 
  data = fam_ces_eye)
