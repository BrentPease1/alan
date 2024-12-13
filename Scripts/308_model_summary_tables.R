library(here)
library(tidyverse)
library(glmmTMB)
library(modelsummary)
library(janitor)

setwd(here::here("results"))

load("tmb_onset_models_family.RData")
load("tmb_evening_models_family.RData")

m1_ranef <- glmmTMB::ranef( m1 )
e1_ranef <- glmmTMB::ranef( e1 )

fam_param <- m1_ranef$cond$family |> 
  tibble::as_tibble(rownames = "family") |> 
  janitor::clean_names() |> 
  tibble::add_column(response = "onset") |> 
  dplyr::select(family, response, intercept, slope = alan_sc) |> 
  dplyr::full_join(
    e1_ranef$cond$family |> 
      tibble::as_tibble(rownames = "family") |> 
      janitor::clean_names() |> 
      tibble::add_column(response = "cessation") |> 
      dplyr::select(family, response, intercept, slope = alan_sc)) |> 
  tidyr::pivot_wider(names_from = response, values_from = intercept:slope) |> 
  dplyr::select(family, onset_intercept = intercept_onset, onset_slope = slope_onset, 
                cessation_intercept = intercept_cessation, cessation_slope = slope_cessation)

sp_param <- m1_ranef$cond$`sp_cell5_week:family` |> 
  as_tibble(rownames = "group") |> 
  tidyr::separate(group, into = c("sp_cell5_week", "family", sep = ":")) |> 
  janitor::clean_names() |> 
  dplyr::mutate(sp_cell5_week = as.numeric(sp_cell5_week)) |> 
  dplyr::left_join(
    d_onset |> 
      dplyr::select(sp_cell5_week, sci_name, grid_ID_cell_5, week) |> 
      dplyr::distinct()) |> 
  dplyr::select(family, sci_name, grid_ID_cell_5, week, sp_cell5_week, intercept, slope = alan_sc)

setwd(here::here("Results/Tables"))
readr::write_csv( fam_param, "family_level_parameters.csv")
readr::write_csv( sp_param, "species_level_parameters.csv")

modelsummary::modelsummary( 
  models = list(
    "Onset, base" = m1,
    "Onset, range size" = m3, 
    "Onset, cavity" = m4,
    "Onset, foraging" = m5,
    "Onset, latitude" = m2,
    "Cessation, base" = e1,
    "Cessation, range size" = e3, 
    "Cessation, cavity" = e4,
    "Cessation, foraging" = e5,
    "Cessation, latitude" = e2),
  output = "model_summaries.csv")

d <- readr::read_csv("model_summaries.csv")

formatted <-  d |>
  janitor::clean_names() |>
  tidyr::pivot_longer( onset_base:cessation_latitude,
                       names_to = "model",
                       values_to = "value") |> 
  dplyr::filter(!is.na(value)) |> 
  tidyr::separate( model, into = c("response", "predictor"), sep = "_") |> 
  dplyr::mutate( term = stringr::str_replace_all(term, "\\(Intercept\\)", "intercept")) |>
  dplyr::mutate( term = stringr::str_replace_all(term, "Intercept", "intercept")) |>
  dplyr::mutate( term = stringr::str_replace_all(term, "alan_sc", "light")) |> 
  dplyr::mutate( term = stringr::str_replace_all(term, "range_size_sc", "range_size")) |> 
  dplyr::mutate( term = stringr::str_replace_all(term, "cavity1", "cavity")) |> 
  dplyr::mutate( term = stringr::str_replace_all(term, "ground_sc", "ground")) |> 
  dplyr::mutate( term = stringr::str_replace_all(term, "lat_sc", "latitude")) |> 
  dplyr::mutate( term = stringr::str_replace_all(term, "sp_weekfamily", "sp_week : family")) |> 
  dplyr::mutate( term = stringr::str_replace_all(term, "sp_cell5_weekfamily", "sp_cell_week : family")) |>
  dplyr::arrange(response, predictor) |> 
  dplyr::filter(!term %in% c("Num.Obs.", "R2 Marg.", "BIC", "ICC")) |> 
  dplyr::mutate(value = readr::parse_number( value )) |> 
  tidyr::pivot_wider(names_from = statistic, values_from = value) |> 
  tidyr::unnest() |> 
  dplyr::rename( gof = `NA`) |> 
  dplyr::select(-part) |> 
  dplyr::mutate(predictors = paste0("light + ", predictor, " + light x ", predictor)) |> 
  dplyr::mutate( predictors = ifelse(predictor == "base", "light", predictors)) |> 
  dplyr::select(response, predictors, term, estimate, std_error = std.error, gof) |> 
  dplyr::mutate( term = stringr::str_replace_all(term, "~", ",")) |> 
  dplyr::mutate(grouping = ifelse( grepl("sp_week : family", term), "sp.week:family", NA)) |> 
  dplyr::mutate(term = str_replace_all(term, " sp_week : family", "")) |> 
  dplyr::mutate(grouping = ifelse( grepl("sp_cell_week : family", term), "sp.cell.week:family", grouping)) |> 
  dplyr::mutate(term = stringr::str_replace_all(term, " sp_cell_week : family", "")) |> 
  dplyr::mutate( grouping = ifelse(grepl("family", term), "family", grouping)) |> 
  dplyr::mutate(term = stringr::str_replace_all(term, " family", "")) |> 
  dplyr::mutate(term = stringr::str_replace_all(term, ",", ", ")) |>
  dplyr::select(response, predictors, term, grouping, estimate, std_error, gof) |> 
  dplyr::mutate(term = stringr::str_replace_all(term, "×", "x"))

readr::write_csv(formatted, "formatted_model_summaries.csv")
