library(tidyverse)
library(MetBrewer)
library(here)

holder <- readr::read_csv(here::here("Results/Birdweather/row_counter/birdweather_row_counter.csv"))

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

response <- dat_onset |> 
  dplyr::left_join(
    tibble::tibble(
      month = 1:12, 
      month_name = month.name)) |> 
  dplyr::group_by(year, month_name) |> 
  dplyr::summarise( response = n()) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(year = as.character(year)) |> 
  dplyr::rename(month = month_name)

key <- tibble(
  month = month.name) |> 
  dplyr::mutate(lab = ifelse(
    month == "September", str_sub(month, 1, 4), str_sub(month, 1, 3))) |> 
  dplyr::mutate(lab = tolower(lab))

final <- holder |> 
  dplyr::mutate(yr_nu = readr::parse_number(month_year)) |> 
  dplyr::mutate(yr_nu = stringr::str_remove_all(yr_nu, "20")) |>
  dplyr::mutate(year = paste0(20, yr_nu)) |> 
  dplyr::mutate(lab = gsub('[[:digit:]]+', '', month_year)) |> 
  dplyr::left_join(key) |> 
  dplyr::group_by(month, year) |> 
  dplyr::summarise(unfilt = sum(unfiltered_rows), 
                   conf = sum(confidence_filtered_rows), 
                   det = sum(detection_filter_rows)) |>
  dplyr::ungroup() |> 
  dplyr::left_join(response) |> 
  dplyr::mutate(month = stringr::str_sub(month, 1, 3)) |> 
  dplyr::mutate(month = factor(month, levels = stringr::str_sub(month.name, 1, 3))) |> 
  dplyr::mutate(year = as.numeric(year)) |> 
  dplyr::arrange(year, month) |> 
  dplyr::mutate( month_year = paste(month, year)) |> 
  dplyr::mutate(month_year = factor(month_year, levels = unique(month_year))) |> 
  dplyr::select(month_year, unfilt:response) |> 
  dplyr::mutate(across(unfilt:response, function(x) x / 1e6)) |>
  tidyr::pivot_longer(unfilt:response, names_to = "type", values_to = "num") |> 
  dplyr::filter(!type == "conf") |> 
  dplyr::mutate(type = factor(type, levels = c("unfilt", "det", "response")))
  
  ggplot(final, aes(x = month_year)) +
    geom_bar(aes(y = num, fill = type), stat = "identity", alpha = 1) +
    labs(
      x = "Month of Year",
      y = "Millions of observations") +
    scale_fill_manual(
      "Type", 
      labels = c("Unfiltered detections", "Filtered detections", "Response variable"), 
      values = MetPalettes$Hiroshige[[1]][c(1, 3, 8)]) +
    theme_classic() +
    theme(
      legend.position = c(0.4, 0.8),
      axis.title.x = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.1),
      axis.title.y = element_text(color = "black", size = 8),
      axis.text.x = element_text(color = "black", size = 7, angle = 45, hjust = 1),
      axis.text.y = element_text(color = "black", size = 7),
      legend.title = element_blank(),
      axis.ticks = element_blank(),
      legend.background = element_rect(color = NA, fill = "white"),
      legend.text = element_text(color = "black", size = 7),
      legend.key.size = unit(3, units = "mm"),
      panel.background = element_rect(color = NA, fill = "white"),
      plot.background = element_rect(color = NA, fill = "white"))
    
setwd(here::here("figures"))
ggsave(
  filename = "sample_size_barplot.png", 
  width = 2.2, 
  height = 1.75, 
  units = "in", 
  dpi = 600)  