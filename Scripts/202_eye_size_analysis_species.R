library(here)
library(tidyverse)
library(glmmTMB)
library(MetBrewer)
library(sf)

setwd(here::here("results"))

load("tmb_onset_models_family.RData")
load("tmb_evening_models_family.RData")

rm( list = setdiff(ls(), c("m1", "e1", "d_onset", "d_e")))

bounding_box <- sf::st_bbox(c(xmin = -180,
                              ymin = -90,
                              xmax = 180,
                              ymax = 90), crs = sf::st_crs(4326))

# Create a global polygon grid with cell_size spacing
global_grid_5 <- sf::st_make_grid(sf::st_as_sfc(bounding_box),
                                  cellsize = c(5, 5), crs = sf::st_crs(4326), what = "polygons")


# make sf object
global_sf_5 <- sf::st_sf(geometry = global_grid_5) |> 
  dplyr::mutate(grid_ID_cell_5 = dplyr::row_number())

cell_lat <- sf::st_centroid(global_sf_5) |> 
  sf::st_coordinates() |> 
  tibble::as_tibble() |> 
  dplyr::pull(Y)

cells <- global_sf_5 |> 
  tibble::add_column(lat = cell_lat) |> 
  sf::st_drop_geometry() |> 
  dplyr::select(grid_ID_cell_5, lat)

sp_cell_key_m <- d_onset |> 
  dplyr::select(sci_name, week, grid_ID_cell_5, sp_cell5_week) |> 
  dplyr::distinct()

sp_cell_key_e <- d_e |> 
  dplyr::select(sci_name, week, grid_ID_cell_5, sp_cell5_week) |> 
  dplyr::distinct()

m1_ranef <- glmmTMB::ranef( m1 )

e1_ranef <- glmmTMB::ranef( e1 )

onset_re <- m1_ranef$cond$`sp_cell5_week:family` |> 
  tibble::as_tibble( rownames = "group") |>
  tidyr::separate(group, into = c("sp_cell5_week", "family"), sep = ":") |> 
  dplyr::mutate(sp_cell5_week = as.numeric(sp_cell5_week)) |> 
  dplyr::right_join(sp_cell_key_m) |> 
  dplyr::select(sci_name, week, grid_ID_cell_5, intercept = `(Intercept)`, slope = alan_sc) |>
  dplyr::left_join(cells) 

ces_re <- e1_ranef$cond$`sp_cell5_week:family` |> 
  tibble::as_tibble( rownames = "group") |>
  tidyr::separate(group, into = c("sp_cell5_week", "family"), sep = ":") |> 
  dplyr::mutate(sp_cell5_week = as.numeric(sp_cell5_week)) |> 
  dplyr::right_join(sp_cell_key_m) |> 
  dplyr::select(sci_name, week, grid_ID_cell_5, intercept = `(Intercept)`, slope = alan_sc) |>
  dplyr::left_join(cells)

setwd(here::here("data"))
key <- readr::read_csv("species_keys/birdweather_elton_botw_name_key.csv")
eye <- readr::read_csv("Ritland_eyes_raw_FINAL.csv") 

setwd(here::here("data/traits"))

trait <- read.delim("elton.txt") |> 
  dplyr::select(family = BLFamilyLatin, sci_name_elton = Scientific, mass = BodyMass.Value) |> 
  dplyr::left_join(key) |> 
  filter(!is.na(sci_name_bw)) |> 
  dplyr::select(family, sci_name_bw, sci_name_elton, sci_name_botw, mass)

eye.sum <- eye |>
  dplyr::select(species_jetz, family_jetz, CD1, CD2, AD, TD1, TD2, CT, EYED) |>
  dplyr::group_by(species_jetz) |>
  dplyr::summarise(across(CD1:EYED, mean)) |>
  dplyr::left_join(
    trait |>
      dplyr::rename(species_jetz = sci_name_elton) |>
      dplyr::select(species_jetz, mass))

sp_eye <- onset_re |> 
  dplyr::select(sci_name) |> 
  dplyr::distinct() |> 
  dplyr::left_join(
    key |> 
      rename(sci_name = sci_name_bw)) |> 
  dplyr::select(sci_name, com_name, species_jetz = sci_name_elton) |> 
  dplyr::distinct() |> 
  dplyr::left_join( eye.sum ) |> 
  filter(!is.na(CD1)) |> 
  dplyr::select(sci_name, CD1, mass) |> 
  dplyr::distinct() |> 
  dplyr::mutate( CD1 = CD1, 
                 lCD1 = log(CD1),
                 mass = as.numeric(scale(log(mass))))

allo_mod <- glmmTMB(
  lCD1 ~ 1 + mass,
  data = sp_eye)

sp_eye2 <- sp_eye |>
  add_column( res = resid( allo_mod ))
  
sp_eye_ces <- ces_re |> 
  dplyr::select(sci_name) |> 
  dplyr::distinct() |> 
  dplyr::left_join(
    key |> 
      rename(sci_name = sci_name_bw)) |> 
  dplyr::select(sci_name, com_name, species_jetz = sci_name_elton) |> 
  dplyr::distinct() |> 
  dplyr::right_join(
    sp_eye2)

onset_eye <- onset_re |> 
  dplyr::group_by(sci_name) |> 
  summarise(across(c(intercept, slope), min)) |>
  dplyr::left_join(sp_eye2) |> 
  filter(!is.na(CD1)) |> 
  dplyr::rename(int = intercept) |> 
  dplyr::left_join(
    d_onset |> 
      dplyr::select(sci_name, cavity) |> 
      dplyr::distinct()
  ) |> 
  dplyr::mutate(CD1 = as.numeric(scale(CD1)))

ces_eye <- ces_re |> 
  dplyr::group_by(sci_name) |> 
  summarise(across(c(intercept, slope), max)) |>
  dplyr::left_join(sp_eye_ces) |> 
  filter(!is.na(CD1)) |> 
  filter(!is.na(intercept)) |> 
  # dplyr::mutate(across(CD1:EYED, function(x) as.numeric(scale(x)))) |> 
  dplyr::rename(int = intercept) |> 
  dplyr::mutate(CD1 = as.numeric(scale(CD1)))


m1 <- glmmTMB::glmmTMB( int ~ 1 + CD1 , 
                        data = onset_eye)

m1.r <- glmmTMB::glmmTMB( int ~ 1 + res, 
                          data = onset_eye)

m2 <- glmmTMB::glmmTMB( slope ~ 1 + CD1,
                        data = onset_eye)

m2.r <- glmmTMB::glmmTMB( slope ~ 1 + res,
                        data = onset_eye)

e1 <- glmmTMB::glmmTMB( int ~ 1 + CD1, 
                        data = ces_eye)

e1.r <- glmmTMB::glmmTMB( int ~ 1 + res, 
                          data = ces_eye)

e2 <- glmmTMB::glmmTMB( slope ~ 1 + CD1, 
                        data = ces_eye)

e2.r <- glmmTMB::glmmTMB( slope ~ 1 + res, 
                        data = ces_eye)
