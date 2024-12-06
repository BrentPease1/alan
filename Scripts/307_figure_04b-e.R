library(here)
library(tidyverse)
library(MetBrewer)
library(effects)
library(patchwork)

setwd(here::here("Results"))

load("tmb_onset_models_family.RData")
load("tmb_evening_models_family.RData")

lat_m_z <- scale( abs( d_onset$lat ) )
lat_e_z <- scale(abs(d_e$lat))
alan_m_z <- scale( log1p( d_onset$avg_rad) )
alan_e_z <- scale(log1p( d_e$avg_rad))
range_m_z <- scale( log( d_onset$range_size ) )
range_e_z <- scale(log(d_e$range_size))
gr_m_z <- scale( d_onset$for_strat_ground)
gr_e_z <- scale(d_e$for_strat_ground)

m2_me <- effects::effect(
  term = c("alan_sc", "lat_sc"), 
  xlevels = list( alan_sc = seq(from = min(d_onset$alan_sc), 
                                to = max(d_onset$alan_sc), 
                                length.out = 10),
                  lat_sc = (c(20, 40, 60) - attr(lat_m_z, "scaled:center"))/attr(lat_m_z, "scaled:scale")),
  mod = m2)

e2_me <- effect(
  term = c("alan_sc", "lat_sc"), 
  xlevels = list( alan_sc = seq(from = min(d_e$alan_sc), 
                                to = max(d_e$alan_sc), 
                                length.out = 10),
                  lat_sc = (c(20, 40, 60) - attr(lat_e_z, "scaled:center"))/attr(lat_e_z, "scaled:scale")),
  mod = e2)

lat_all <- as.data.frame(m2_me) |> 
  dplyr::mutate( lat = round(lat_sc * attr(lat_m_z, "scaled:scale") +
                               attr(lat_m_z, "scaled:center"), 0)) |> 
  dplyr::mutate(alan = alan_sc*attr(alan_m_z, "scaled:scale") +
                  attr(alan_m_z, "scaled:center")) |> 
  tibble::add_column(response = "Morning onset") |> 
  dplyr::full_join(
    as.data.frame(e2_me) |> 
      dplyr::mutate( lat = round(lat_sc * attr(lat_e_z, "scaled:scale") +
                                   attr(lat_e_z, "scaled:center"), 0)) |> 
      dplyr::mutate(alan = alan_sc*attr(alan_e_z, "scaled:scale") +
                      attr(alan_e_z, "scaled:center")) |> 
      tibble::add_column(response = "Evening cessation")) |> 
  dplyr::mutate( response = factor( response, 
                                    levels = c("Morning onset", 
                                               "Evening cessation")))

m3_me <- effect(
  term = c("alan_sc", "range_size_sc"), 
  xlevels = list( alan_sc = seq(from = min(d_onset$alan_sc), 
                                to = max(d_onset$alan_sc), 
                                length.out = 10),
                  range_size_sc = (c(12, 16, 18) - 
                                     attr(range_m_z, "scaled:center"))/
                    attr(range_m_z, "scaled:scale")),
  mod = m3)

e3_me <- effect(
  term = c("alan_sc", "range_size_sc"), 
  xlevels = list( alan_sc = seq(from = min(d_e$alan_sc), 
                                to = max(d_e$alan_sc), 
                                length.out = 10),
                  range_size_sc = (c(12, 16, 18) - 
                                     attr(range_e_z, "scaled:center"))/
                    attr(range_e_z, "scaled:scale")),
  mod = e3)

range_all <- as.data.frame(m3_me) |> 
  dplyr::mutate(alan = alan_sc*attr(alan_m_z, "scaled:scale") + attr(alan_m_z, "scaled:center")) |>
  dplyr::mutate(range = round(range_size_sc*attr(range_m_z, "scaled:scale") + 
                                attr(range_m_z, "scaled:center"), 0)) |> 
  tibble::add_column(response = "Morning onset") |> 
  dplyr::full_join(
    as.data.frame(e3_me) |> 
      dplyr::mutate(alan = alan_sc*attr(alan_e_z, "scaled:scale") + attr(alan_e_z, "scaled:center")) |>
      dplyr::mutate(range = round(range_size_sc*attr(range_e_z, "scaled:scale") + 
                                    attr(range_e_z, "scaled:center"), 0)) |> 
      tibble::add_column(response = "Evening cessation")) |> 
  dplyr::mutate( response = factor( response, 
                                    levels = c("Morning onset", 
                                               "Evening cessation")))

gr_m_z <- scale(d_onset$for_strat_ground )  

m4_me <- effect(
  term = c("alan_sc", "cavity"), 
  xlevels = list( alan_sc = seq(from = min(d_onset$alan_sc), 
                                to = max(d_onset$alan_sc), 
                                length.out = 10),
                  cavity = c(0, 1)),
  mod = m4)

e4_me <- effect(
  term = c("alan_sc", "cavity"), 
  xlevels = list( alan_sc = seq(from = min(d_e$alan_sc), 
                                to = max(d_e$alan_sc), 
                                length.out = 10),
                  cavity = c(0, 1)),
  mod = e4)

cavity_me_all <- m4_me |> 
  as.data.frame() |> 
  dplyr::mutate(alan = alan_sc*attr(alan_m_z, "scaled:scale") +
                  attr(alan_m_z, "scaled:center")) |> 
  tibble::add_column(response = "Morning onset") |> 
  dplyr::full_join(
    e4_me |> 
      as.data.frame() |> 
      dplyr::mutate(alan = alan_sc*attr(alan_e_z, "scaled:scale") + 
                      attr(alan_e_z, "scaled:center")) |> 
      tibble::add_column(response = "Evening cessation")) |> 
  dplyr::mutate( response = factor( response, 
                                    levels = c("Morning onset", 
                                               "Evening cessation")))

m5_me <- effect(
  term = c("alan_sc", "ground_sc"), 
  xlevels = list( alan_sc = seq(from = min(d_onset$alan_sc), 
                                to = max(d_onset$alan_sc), 
                                length.out = 10),
                  ground_sc = (c(0, 0.5, 1) - attr(gr_m_z, "scaled:center"))/
                    attr(gr_m_z, "scaled:scale")),
  mod = m5)

e5_me <- effect(
  term = c("alan_sc", "ground_sc"), 
  xlevels = list( alan_sc = seq(from = min(d_e$alan_sc), 
                                to = max(d_e$alan_sc), 
                                length.out = 10),
                  ground_sc = (c(0, 0.5, 1) - attr(gr_e_z, "scaled:center"))/
                    attr(gr_e_z, "scaled:scale")),
  mod = e5)

gr_all <- as.data.frame(m5_me) |> 
  dplyr::mutate(alan = alan_sc*attr(alan_m_z, "scaled:scale") + attr(alan_m_z, "scaled:center")) |>
  dplyr::mutate(ground = round(ground_sc*attr(gr_m_z, "scaled:scale") + 
                                 attr(gr_m_z, "scaled:center"), 1)) |> 
  tibble::add_column(response = "Morning onset") |> 
  dplyr::full_join(
    as.data.frame(e5_me) |> 
      dplyr::mutate(alan = alan_sc*attr(alan_e_z, "scaled:scale") + attr(alan_e_z, "scaled:center")) |>
      dplyr::mutate(ground = round(ground_sc*attr(gr_e_z, "scaled:scale") + 
                                     attr(gr_e_z, "scaled:center"), 1)) |> 
      tibble::add_column(response = "Evening cessation")) |> 
  dplyr::mutate( response = factor( response, 
                                    levels = c("Morning onset", 
                                               "Evening cessation")))

( lat_plot <- ggplot( data = filter(lat_all, response == "Morning onset" | response == "Evening cessation"),
                      aes(x = alan, y = fit, color = factor(lat))) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = factor(lat)), color = NA, 
                alpha = 0.2) +
    facet_wrap(~response, scales = "free_y") +
    geom_line(size = 2) +
    labs( x = "Light pollution: ln(Radiance + 1)",
          y = "Time relative to sunrise (hr)",
          fill = "Latitude", 
          color = "Latitude",
          title = "Latitude") +
    theme_minimal() +
    scale_color_manual(
      values = MetBrewer::MetPalettes$Hiroshige[[1]][c(1,3,8)]) +
    scale_fill_manual(
      values = MetBrewer::MetPalettes$Hiroshige[[1]][c(1,3,8)]) +
    theme(
      plot.title = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      strip.text = element_blank(),
      legend.key.size = unit(3, "mm"),
      legend.title = element_text(color = "black", size = 9, hjust = 0.5),
      legend.title.position = "top",
      legend.margin = margin(-5, 0, 0, 0),
      legend.text = element_text(color = "black", size = 8),
      axis.title = element_blank(), 
      axis.text.x = element_blank(),
      axis.text = element_text(color = "black", size = 8),
      axis.line = element_line(color = "black", linewidth = 0.3)) )

( range_plot <- ggplot( data = filter(range_all, response == "Morning onset" | response == "Evening cessation"),
                      aes(x = alan, y = fit, color = factor(range))) +
    facet_wrap(~response, scales = "free_y") +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = factor(range)), color = NA, 
                alpha = 0.2) +
    geom_line(size = 2) +
    labs( x = "Light pollution: ln(Radiance + 1)",
          y = "Time relative to sunrise (hr)",
          fill = bquote("ln(Range size,"~km^2~")"), 
          color = bquote("ln(Range size,"~km^2~")"),
          title = bquote("ln(Range size,"~km^2~")")) +
    theme_minimal() +
    scale_color_manual(
      values = MetBrewer::MetPalettes$Tam[[1]][c(3,5,7)]) +
    scale_fill_manual(
      values = MetBrewer::MetPalettes$Tam[[1]][c(3,5,7)]) +
    theme(
      plot.title = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(color = "black", size = 9, hjust = 0.5),
      legend.title.position = "top",
      legend.margin = margin(-5, 0, 0, 0),
      legend.key.size = unit(3, "mm"),
      legend.text = element_text(color = "black", size = 8),
      axis.title = element_blank(), 
      axis.text.x = element_blank(),
      axis.text = element_text(color = "black", size = 8),
      axis.line = element_line(color = "black", linewidth = 0.3)) )

(cavity_plot <- ggplot(data = filter(cavity_me_all, response == "Morning onset" | response == "Evening cessation"),
       aes(x = alan, y = fit, color = factor(cavity), 
                          fill = factor(cavity))) +
  facet_wrap(~response, scales = "free_y") +
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.2) +
  geom_line(size = 2)  +
  scale_color_manual(
    values = MetBrewer::MetPalettes$Isfahan1[[1]][c(6,2)],
    labels = c("Open", "Cavity")) +
  scale_fill_manual(
    values = MetBrewer::MetPalettes$Isfahan1[[1]][c(6,2)],
    labels = c("Open", "Cavity")) +
  labs( x = "Light pollution: ln(Radiance + 1)",
        y = "Time relative to sunrise (hr)",
        fill = "Nest type",
        color = "Nest type",
        title = "Nest type") +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    strip.text = element_blank(),
    legend.title = element_text(color = "black", size = 9, hjust = 0.5),
    legend.title.position = "top",
    legend.margin = margin(-5, 0, 0, 0),
    legend.key.size = unit(3, "mm"),
    legend.text = element_text(color = "black", size = 8),
    axis.title = element_blank(),
    axis.text.y = element_text(color = "black", size = 8),
    axis.text.x = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.3)) )

( ground_plot <- ggplot( data = filter(gr_all, response == "Morning onset" | response == "Evening cessation"),
                      aes(x = alan, y = fit, color = factor(ground))) +
    facet_wrap(~response, scales = "free_y") +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = factor(ground)), color = NA, 
                alpha = 0.2) +
    geom_line(size = 2) +
    labs( x = "Light pollution: ln(Radiance + 1)",
          y = "Time relative to sunrise (hr)",
          fill = "Proportion ground foraging", 
          color = "Proportion ground foraging",
          title = "Proportion ground foraging") +
    theme_minimal() +
    scale_color_manual(
      values = MetBrewer::MetPalettes$Kandinsky[[1]][c(1,2,4)]) +
    scale_fill_manual(
      values = MetBrewer::MetPalettes$Kandinsky[[1]][c(1,2,4)]) +
    theme(
      legend.position = "bottom",
      plot.title = element_blank(),
      legend.direction = "horizontal",
      legend.key.size = unit(3, "mm"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      strip.text = element_blank(),
      legend.title = element_text(color = "black", size = 9, hjust = 0.5),
      legend.title.position = "top",
      legend.margin = margin(-5, 0, 0, 0),
      legend.text = element_text(color = "black", size = 8),
      axis.title = element_blank(),
      axis.text.y = element_text(color = "black", size = 8),
      axis.text.x = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.3)) )

range_plot / cavity_plot / ground_plot / lat_plot

setwd(here::here("Results/Figures"))
ggsave(
  filename = "figure_04b-e.png", 
  width = 3,
  height = 7.2, 
  units = "in", 
  dpi = 600)