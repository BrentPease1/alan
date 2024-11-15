library(here)
library(tidyverse)
library(MetBrewer)
library(effects)

setwd(here::here("results"))

load("tmb_onset_models_family.RData")
load("tmb_median_models_family.RData")
load("tmb_evening_models_family.RData")

lat_m_z <- scale( abs( d_onset$lat ) )
lat_med_z <- scale(abs(d_med$lat))
lat_e_z <- scale(abs(d_e$lat))
alan_m_z <- scale( log1p( d_onset$avg_rad) )
alan_med_z <- scale( log1p( d_med$avg_rad))
alan_e_z <- scale(log1p( d_e$avg_rad))
range_m_z <- scale( log( d_onset$range_size ) )
range_med_z <- scale(log(d_med$range_size))
range_e_z <- scale(log(d_e$range_size))
gr_m_z <- scale( d_onset$for_strat_ground)
gr_med_z <- scale(d_med$for_strat_ground)
gr_e_z <- scale(d_e$for_strat_ground)
prop_m_z <- scale(d_onset$alan_prop_sp)
prop_med_z <- scale(d_med$alan_prop_sp)
prop_e_z <- scale(d_e$alan_prop_sp)

m2_me <- effect(
  term = c("alan_sc", "lat_sc"), 
  xlevels = list( alan_sc = seq(from = min(d_onset$alan_sc), 
                                to = max(d_onset$alan_sc), 
                                length.out = 10),
                  lat_sc = (c(20, 40, 60) - attr(lat_m_z, "scaled:center"))/attr(lat_m_z, "scaled:scale")),
  mod = m2)

med2_me <- effect(
  term = c("alan_sc", "lat_sc"), 
  xlevels = list( alan_sc = seq(from = min(d_med$alan_sc), 
                                to = max(d_med$alan_sc), 
                                length.out = 10),
                  lat_sc = (c(20, 40, 60) - attr(lat_med_z, "scaled:center"))/attr(lat_med_z, "scaled:scale")),
  mod = med2)

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
    as.data.frame(med2_me) |> 
      dplyr::mutate( lat = round(lat_sc * attr(lat_med_z, "scaled:scale") +
                                   attr(lat_med_z, "scaled:center"), 0)) |> 
      dplyr::mutate(alan = alan_sc*attr(alan_med_z, "scaled:scale") +
                      attr(alan_med_z, "scaled:center")) |> 
      tibble::add_column(response = "Morning median")) |> 
  dplyr::full_join(
    as.data.frame(e2_me) |> 
      dplyr::mutate( lat = round(lat_sc * attr(lat_e_z, "scaled:scale") +
                                   attr(lat_e_z, "scaled:center"), 0)) |> 
      dplyr::mutate(alan = alan_sc*attr(alan_e_z, "scaled:scale") +
                      attr(alan_e_z, "scaled:center")) |> 
      tibble::add_column(response = "Evening cessation")) |> 
  dplyr::mutate( response = factor( response, 
                                    levels = c("Morning onset", 
                                               "Morning median", 
                                               "Evening cessation")))

ggplot(lat_all, aes(x = alan, y = fit, color = factor(lat))) +
  facet_wrap(~response, scales = "free_y") +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = factor(lat)), color = NA, 
              alpha = 0.2) +
  geom_line(size = 2) +
  labs( x = "Light pollution: ln(Radiance + 1)",
        y = "Time relative to sunrise (hr)",
        fill = "Latitude", 
        color = "Latitude") +
  theme_minimal() +
  scale_color_manual(
    values = MetBrewer::MetPalettes$Hiroshige[[1]][c(1,3,8)]) +
  scale_fill_manual(
    values = MetBrewer::MetPalettes$Hiroshige[[1]][c(1,3,8)]) +
  theme(
    # legend.position = "none", 
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    # strip.text = element_text(color = "black", size = 10), 
    legend.title = element_text(color = "black", size = 11),
    legend.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 9),
    axis.text = element_text(color = "black", size = 8),
    axis.line = element_line(color = "black", linewidth = 0.3))

m3_me <- effect(
  term = c("alan_sc", "range_size_sc"), 
  xlevels = list( alan_sc = seq(from = min(d_onset$alan_sc), 
                                to = max(d_onset$alan_sc), 
                                length.out = 10),
                  range_size_sc = (c(12, 16, 18) - 
                                     attr(range_m_z, "scaled:center"))/
                    attr(range_m_z, "scaled:scale")),
  # c(-4, 0, 2)),
  mod = m3)

med3_me <- effect(
  term = c("alan_sc", "range_size_sc"), 
  xlevels = list( alan_sc = seq(from = min(d_med$alan_sc), 
                                to = max(d_med$alan_sc), 
                                length.out = 10),
                  range_size_sc = (c(12, 16, 18) - 
                                     attr(range_med_z, "scaled:center"))/
                    attr(range_med_z, "scaled:scale")),
  mod = med3)

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
    
    as.data.frame(med3_me) |> 
      dplyr::mutate(alan = alan_sc*attr(alan_med_z, "scaled:scale") + attr(alan_med_z, "scaled:center")) |>
      dplyr::mutate(range = round(range_size_sc*attr(range_med_z, "scaled:scale") + 
                                    attr(range_med_z, "scaled:center"), 0)) |> 
      tibble::add_column(response = "Morning median")) |> 
  dplyr::full_join(
    
    as.data.frame(e3_me) |> 
      dplyr::mutate(alan = alan_sc*attr(alan_e_z, "scaled:scale") + attr(alan_e_z, "scaled:center")) |>
      dplyr::mutate(range = round(range_size_sc*attr(range_e_z, "scaled:scale") + 
                                    attr(range_e_z, "scaled:center"), 0)) |> 
      tibble::add_column(response = "Evening cessation")) |> 
  
  dplyr::mutate( response = factor( response, 
                                    levels = c("Morning onset", 
                                               "Morning median", 
                                               "Evening cessation")))

ggplot(range_all, aes(x = alan, y = fit, color = factor(range),
                      fill = factor(range))) +
  facet_wrap(~response, scales = "free_y") +
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.2) +
  geom_line(size = 2) +
  theme_minimal() +
  scale_color_manual(
    values = MetBrewer::MetPalettes$Hiroshige[[1]][c(1,3,8)]) +
  scale_fill_manual(
    values = MetBrewer::MetPalettes$Hiroshige[[1]][c(1,3,8)]) +
  labs( x = "Light pollution: ln(Radiance + 1)",
        y = "Time relative to sunrise (hr)",
        fill = bquote("ln(Range size,"~km^2~")"), 
        color = bquote("ln(Range size,"~km^2~")")) +
  theme(
    # legend.position = "none", 
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    # strip.text = element_text(color = "black", size = 10), 
    legend.title = element_text(color = "black", size = 11),
    legend.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 9),
    axis.text = element_text(color = "black", size = 8),
    axis.line = element_line(color = "black", linewidth = 0.3))

gr_m_z <- scale(d_onset$for_strat_ground )  

m4_me <- effect(
  term = c("alan_sc", "cavity"), 
  xlevels = list( alan_sc = seq(from = min(d_onset$alan_sc), 
                                to = max(d_onset$alan_sc), 
                                length.out = 10),
                  cavity = c(0, 1)),
  mod = m4)

med4_me <- effect(
  term = c("alan_sc", "cavity"), 
  xlevels = list( alan_sc = seq(from = min(d_med$alan_sc), 
                                to = max(d_med$alan_sc), 
                                length.out = 10),
                  cavity = c(0, 1)),
  mod = med4)

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
    med4_me |> 
      as.data.frame() |> 
      dplyr::mutate(alan = alan_sc*attr(alan_med_z, "scaled:scale") + 
                      attr(alan_med_z, "scaled:center")) |> 
      tibble::add_column(response = "Morning median")) |> 
  dplyr::full_join(
    e4_me |> 
      as.data.frame() |> 
      dplyr::mutate(alan = alan_sc*attr(alan_e_z, "scaled:scale") + 
                      attr(alan_e_z, "scaled:center")) |> 
      tibble::add_column(response = "Evening cessation")) |> 
  dplyr::mutate( response = factor( response, 
                                    levels = c("Morning onset", 
                                               "Morning median", 
                                               "Evening cessation")))

ggplot(cavity_me_all, aes(x = alan, y = fit, color = factor(cavity), 
                          fill = factor(cavity))) +
  facet_wrap(~response, scales = "free_y") +
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.2) +
  geom_line(size = 2)  +
  scale_color_manual(
    values = MetBrewer::MetPalettes$Hokusai1[[1]][c(4,7)],
    labels = c("Open", "Cavity")) +
  scale_fill_manual(
    values = MetBrewer::MetPalettes$Hokusai1[[1]][c(4,7)],
    labels = c("Open", "Cavity")) +
  labs( x = "Light pollution: ln(Radiance + 1)",
        y = "Time relative to sunrise (hr)",
        fill = "Nest type",
        color = "Nest type") +
  theme_minimal() +
  theme(
    # legend.position = "none", 
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    # strip.text = element_text(color = "black", size = 10), 
    legend.title = element_text(color = "black", size = 11),
    legend.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 9),
    axis.text = element_text(color = "black", size = 8),
    axis.line = element_line(color = "black", linewidth = 0.3))

m5_me <- effect(
  term = c("alan_sc", "ground_sc"), 
  xlevels = list( alan_sc = seq(from = min(d_onset$alan_sc), 
                                to = max(d_onset$alan_sc), 
                                length.out = 10),
                  ground_sc = (c(0, 0.5, 1) - attr(gr_m_z, "scaled:center"))/
                    attr(gr_m_z, "scaled:scale")),
  mod = m5)

med5_me <- effect(
  term = c("alan_sc", "ground_sc"), 
  xlevels = list( alan_sc = seq(from = min(d_med$alan_sc), 
                                to = max(d_med$alan_sc), 
                                length.out = 10),
                  ground_sc = (c(0, 0.5, 1) - attr(gr_med_z, "scaled:center"))/
                    attr(gr_med_z, "scaled:scale")),
  mod = med5)

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
    as.data.frame(med5_me) |> 
      dplyr::mutate(alan = alan_sc*attr(alan_med_z, "scaled:scale") + attr(alan_med_z, "scaled:center")) |>
      dplyr::mutate(ground = round(ground_sc*attr(gr_med_z, "scaled:scale") + 
                                     attr(gr_med_z, "scaled:center"), 1)) |> 
      tibble::add_column(response = "Morning median")) |> 
  dplyr::full_join(
    as.data.frame(e5_me) |> 
      dplyr::mutate(alan = alan_sc*attr(alan_e_z, "scaled:scale") + attr(alan_e_z, "scaled:center")) |>
      dplyr::mutate(ground = round(ground_sc*attr(gr_e_z, "scaled:scale") + 
                                     attr(gr_e_z, "scaled:center"), 1)) |> 
      tibble::add_column(response = "Evening cessation")) |> 
  
  dplyr::mutate( response = factor( response, 
                                    levels = c("Morning onset", 
                                               "Morning median", 
                                               "Evening cessation")))

ggplot(gr_all, aes(x = alan, y = fit, color = factor(ground),
                   fill = factor(ground))) +
  facet_wrap(~response, scales = "free_y") +
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.2) +
  geom_line(size = 2) +
  theme_minimal() +
  scale_color_manual(
    values = MetBrewer::MetPalettes$Hiroshige[[1]][c(1,3,8)]) +
  scale_fill_manual(
    values = MetBrewer::MetPalettes$Hiroshige[[1]][c(1,3,8)]) +
  labs( x = "Light pollution: ln(Radiance + 1)",
        y = "Time relative to sunrise (hr)",
        fill = "Proportion ground foraging", 
        color = "Proportion ground foraging") +
  theme(
    # legend.position = "none", 
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    # strip.text = element_text(color = "black", size = 10), 
    legend.title = element_text(color = "black", size = 11),
    legend.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 9),
    axis.text = element_text(color = "black", size = 8),
    axis.line = element_line(color = "black", linewidth = 0.3))

m6_me <- effect(
  term = c("alan_sc", "alan_prop"), 
  xlevels = list( alan_sc = seq(from = min(d_onset$alan_sc), 
                                to = max(d_onset$alan_sc), 
                                length.out = 10),
                  alan_prop = (c(0, 0.5, 1) - attr(prop_m_z, "scaled:center"))/
                    attr(prop_m_z, "scaled:scale")),
  mod = m6)

med6_me <- effect(
  term = c("alan_sc", "alan_prop"), 
  xlevels = list( alan_sc = seq(from = min(d_med$alan_sc), 
                                to = max(d_med$alan_sc), 
                                length.out = 10),
                  alan_prop = (c(0, 0.5, 1) - attr(prop_med_z, "scaled:center"))/
                    attr(prop_med_z, "scaled:scale")),
  mod = med6)

e6_me <- effect(
  term = c("alan_sc", "alan_prop"), 
  xlevels = list( alan_sc = seq(from = min(d_e$alan_sc), 
                                to = max(d_e$alan_sc), 
                                length.out = 10),
                  alan_prop = (c(0, 0.5, 1) - attr(prop_e_z, "scaled:center"))/
                    attr(prop_e_z, "scaled:scale")),
  mod = e6)

prop_all <- as.data.frame(m6_me) |> 
  dplyr::mutate(alan = alan_sc*attr(alan_m_z, "scaled:scale") + attr(alan_m_z, "scaled:center")) |>
  dplyr::mutate(prop = round(alan_prop*attr(prop_m_z, "scaled:scale") + 
                               attr(prop_m_z, "scaled:center"), 1)) |> 
  tibble::add_column(response = "Morning onset") |> 
  dplyr::full_join(
    as.data.frame(med6_me) |> 
      dplyr::mutate(alan = alan_sc*attr(alan_med_z, "scaled:scale") + attr(alan_med_z, "scaled:center")) |>
      dplyr::mutate(prop = round(alan_prop*attr(prop_med_z, "scaled:scale") + 
                                   attr(prop_med_z, "scaled:center"), 1)) |> 
      tibble::add_column(response = "Morning median")) |> 
  dplyr::full_join(
    as.data.frame(e6_me) |> 
      dplyr::mutate(alan = alan_sc*attr(alan_e_z, "scaled:scale") + attr(alan_e_z, "scaled:center")) |>
      dplyr::mutate(prop = round(alan_prop*attr(prop_e_z, "scaled:scale") + 
                                   attr(prop_e_z, "scaled:center"), 1)) |> 
      tibble::add_column(response = "Evening cessation")) |> 
  
  dplyr::mutate( response = factor( response, 
                                    levels = c("Morning onset", 
                                               "Morning median", 
                                               "Evening cessation")))

ggplot(prop_all, aes(x = alan, y = fit, color = factor(prop),
                     fill = factor(prop))) +
  facet_wrap(~response, scales = "free_y") +
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.2) +
  geom_line(size = 2) +
  theme_minimal() +
  scale_color_manual(
    values = MetBrewer::MetPalettes$Hiroshige[[1]][c(1,3,8)]) +
  scale_fill_manual(
    values = MetBrewer::MetPalettes$Hiroshige[[1]][c(1,3,8)]) +
  labs( x = "Light pollution: ln(Radiance + 1)",
        y = "Time relative to sunrise (hr)",
        fill = "Proportion of ALAN gradient species detected at", 
        color = "Proportion of ALAN gradient species detected at") +
  theme(
    # legend.position = "none", 
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    # strip.text = element_text(color = "black", size = 10), 
    legend.title = element_text(color = "black", size = 11),
    legend.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 9),
    axis.text = element_text(color = "black", size = 8),
    axis.line = element_line(color = "black", linewidth = 0.3))

( lat_plot <- ggplot( data = filter(lat_all, response == "Morning onset"),
                      aes(x = alan, y = fit, color = factor(lat))) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = factor(lat)), color = NA, 
                alpha = 0.2) +
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
      # plot.title = element_text(hjust = 0.5, 
      #                           color = "black", 
      #                           size = 9),
      plot.title = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      # legend.title.position = "top",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      # strip.text = element_text(color = "black", size = 10), 
      # legend.title = element_text(color = "black", size = 9),
      # legend.title = element_blank(),
      legend.key.size = unit(3, "mm"),
      legend.title = element_text(color = "black", size = 9, hjust = 0.5),
      legend.title.position = "top",
      legend.margin = margin(-5, 0, 0, 0),
      legend.text = element_text(color = "black", size = 8),
      axis.title = element_blank(), 
      axis.text.x = element_blank(),
      # axis.title.y = element_text(color = "black", size = 9),
      axis.text = element_text(color = "black", size = 8),
      axis.line = element_line(color = "black", linewidth = 0.3)) )

( range_plot <- ggplot( data = filter(range_all, response == "Morning onset"),
                      aes(x = alan, y = fit, color = factor(range))) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = factor(range)), color = NA, 
                alpha = 0.2) +
    geom_line(size = 2) +
    labs( x = "Light pollution: ln(Radiance + 1)",
          y = "Time relative to sunrise (hr)",
          fill = bquote("ln(Range size,"~km^2~")"), 
          color = bquote("ln(Range size,"~km^2~")"),
          title = bquote("ln(Range size,"~km^2~")")
            ) +
    theme_minimal() +
    scale_color_manual(
      values = MetBrewer::MetPalettes$Tam[[1]][c(3,5,7)]) +
    scale_fill_manual(
      values = MetBrewer::MetPalettes$Tam[[1]][c(3,5,7)]) +
    theme(
      # plot.title = element_text(hjust = 0.5, 
      #                           color = "black", 
      #                           size = 9),
      plot.title = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      # legend.title.position = "top",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      # strip.text = element_text(color = "black", size = 10), 
      # legend.title = element_text(color = "black", size = 9),
      # legend.title = element_blank(),
      legend.title = element_text(color = "black", size = 9, hjust = 0.5),
      legend.title.position = "top",
      legend.margin = margin(-5, 0, 0, 0),
      legend.key.size = unit(3, "mm"),
      legend.text = element_text(color = "black", size = 8),
      # axis.title = element_text(color = "black", size = 9),
      axis.title = element_blank(), 
      axis.text.x = element_blank(),
      axis.text = element_text(color = "black", size = 8),
      axis.line = element_line(color = "black", linewidth = 0.3)) )

(cavity_plot <- ggplot(data = filter(cavity_me_all, response == "Morning onset"),
       aes(x = alan, y = fit, color = factor(cavity), 
                          fill = factor(cavity))) +
  # facet_wrap(~response, scales = "free_y") +
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
    # plot.title = element_text(hjust = 0.5, 
    #                           color = "black", 
    #                           size = 9),
    plot.title = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    # legend.title.position = "top",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    # strip.text = element_text(color = "black", size = 10), 
    # legend.title = element_text(color = "black", size = 9),
    legend.title = element_text(color = "black", size = 9, hjust = 0.5),
    legend.title.position = "top",
    legend.margin = margin(-5, 0, 0, 0),
    legend.key.size = unit(3, "mm"),
    legend.text = element_text(color = "black", size = 8),
    # axis.title = element_text(color = "black", size = 9),
    axis.title = element_blank(),
    axis.text.y = element_text(color = "black", size = 8),
    axis.text.x = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.3)) )

( ground_plot <- ggplot( data = filter(gr_all, response == "Morning onset"),
                      aes(x = alan, y = fit, color = factor(ground))) +
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
      # plot.title = element_text(hjust = 0.5, 
      #                           color = "black", 
      #                           size = 9),
      plot.title = element_blank(),
      legend.direction = "horizontal",
      legend.key.size = unit(3, "mm"),
      # legend.title = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      # strip.text = element_text(color = "black", size = 10), 
      legend.title = element_text(color = "black", size = 9, hjust = 0.5),
      legend.title.position = "top",
      legend.margin = margin(-5, 0, 0, 0),
      legend.text = element_text(color = "black", size = 8),
      axis.title = element_text(color = "black", size = 9),
      axis.title.y = element_blank(),
      axis.text = element_text(color = "black", size = 8),
      axis.line = element_line(color = "black", linewidth = 0.3)) )

library(patchwork)

lat_plot / range_plot / cavity_plot / ground_plot

setwd(here::here("figures"))
ggsave(
  filename = "trait_plots.png", 
  width = 2.5,
  height = 7.2, 
  units = "in", 
  dpi = 600
)
