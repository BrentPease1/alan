library(tidyverse)
library(here)
library(effects)
library(MetBrewer)

setwd(here::here("Results"))

load("onset.RData")
load("cessation.RData")

rm(m1b, m2, m3, m4, m5, m6, m7, m8,
   e2, e3, e4, e5, e6, e7, e8)

m1_me <- effects::effect(term = "alan_sc", mod = m1)

alan_z <- scale(log1p(d_onset$avg_rad))

m1_df <- as.data.frame(m1_me) |> 
  dplyr::mutate(alan = alan_sc * attr(alan_z, "scaled:scale") +
                  attr(alan_z, "scaled:center")) |> 
  tibble::add_column(response = "Onset")

e1_me <- effects::effect(term = "alan_sc", mod = e1)

alan_z_e <- scale(log1p(d_e$avg_rad))

e1_df <- as.data.frame(e1_me) |> 
  dplyr::mutate(alan = alan_sc * attr(alan_z_e, "scaled:scale") +
                  attr(alan_z_e, "scaled:center")) |>
  tibble::add_column(response = "Cessation")

ggplot( data = m1_df, 
        aes(x = alan, y = fit)) +
  geom_ribbon( aes(ymin = lower, ymax = upper),
               fill = MetBrewer::MetPalettes$Tam[[1]][8],
               color = NA, alpha = 0.3) +
  geom_line(linewidth = 2,
            color = MetBrewer::MetPalettes$Tam[[1]][8]) + 
  theme_minimal() +
  facet_wrap(~response) +
  labs( y = "Time relative to sunrise (hr)",
        x = "Light pollution: ln(Radiance + 1)") +
  theme(
    legend.position = "none", 
    panel.grid = element_line(linewidth = 0.1, color ="gray90"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(color = "black", size = 10), 
    axis.title = element_text(color = "black", size = 9),
    axis.text = element_text(color = "black", size = 8),
    axis.line = element_line(color = "black", linewidth = 0.3))

setwd(here::here("Results/Figures"))
ggsave(
  filename = "figure_02b.png", 
  width = 2.5, 
  height = 2, 
  units = "in", 
  dpi = 600)

e1_df |> 
  ggplot(aes(x = alan, y = fit, color = response, fill = response)) +
  facet_wrap(~response) +
  geom_ribbon( aes(ymin = lower, ymax = upper),
               color = NA, alpha = 0.3) +
  geom_line(linewidth = 2) + 
  theme_minimal() +
  scale_color_manual(values = MetBrewer::MetPalettes$Tam[[1]][c(5)]) +
  scale_fill_manual(values = MetBrewer::MetPalettes$Tam[[1]][c(5)]) +
  labs( y = "Time relative to sunset (hr)",
        x = "Light pollution: ln(Radiance + 1)") +
  theme(
    legend.position = "none", 
    panel.grid = element_line(linewidth = 0.1, color ="gray90"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(color = "black", size = 10), 
    axis.title = element_text(color = "black", size = 9),
    axis.text = element_text(color = "black", size = 8),
    axis.line = element_line(color = "black", linewidth = 0.3))

setwd(here::here("Results/Figures"))
ggsave(
  filename = "figure_02c.png", 
  width = 2.5, 
  height = 2, 
  units = "in", 
  dpi = 600)

e1_df |> 
  dplyr::filter(alan == min(alan) | alan == max(alan)) |> 
  dplyr::select(alan, fit) |> 
  dplyr::mutate( alan = ifelse(alan < 0, "Dark", "Light")) |> 
  pivot_wider(names_from = alan, values_from = fit) |> 
  mutate(diff = ( Light - Dark ) * 60)

m1_df |>
  dplyr::select( alan, fit) |> 
  dplyr::filter(alan == min(alan) | alan == max(alan)) |> 
  dplyr::mutate(alan = ifelse(alan < 0, "Dark", "Bright")) |> 
  pivot_wider(names_from = alan, values_from = fit) |> 
  dplyr::mutate(diff = (Dark - Bright) * 60)