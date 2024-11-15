library(tidyverse)
library(here)
library(effects)

setwd(here::here("results"))

load("tmb_onset_models_family.RData")

m1_me <- effect(term = "alan_sc", mod = m1)

alan_z <- scale(log1p(d_onset$avg_rad))

m1_df <- as.data.frame(m1_me) |> 
  mutate(alan = alan_sc * attr(alan_z, "scaled:scale") +
           attr(alan_z, "scaled:center")) |> 
  add_column(response = "Morning onset")

load("tmb_median_models_family.RData")

alan_z_med <- scale(log1p(d$avg_rad))

med1_me <- effect(term = "alan_sc", mod = med1)

med1_df <- as.data.frame(med1_me) |> 
  mutate(alan = alan_sc * attr(alan_z_med, "scaled:scale") +
           attr(alan_z_med, "scaled:center")) |>
  add_column(response = "Morning median")

load("e1_family.RData")

e1_me <- effect(term = "alan_sc", mod = e1)

alan_z_e <- scale(log1p(d$avg_rad))

e1_df <- as.data.frame(e1_me) |> 
  mutate(alan = alan_sc * attr(alan_z_e, "scaled:scale") +
           attr(alan_z_e, "scaled:center")) |>
  add_column(response = "Evening cessation")

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
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(color = "black", size = 10), 
    # legend.title = element_text(color = "black", size = 11),
    # legend.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 10),
    axis.text = element_text(color = "black", size = 9),
    axis.line = element_line(color = "black", linewidth = 0.3))

ggplot( data = med1_df, 
        aes(x = alan, y = fit)) +
  geom_ribbon( aes(ymin = lower, ymax = upper),
               fill = MetBrewer::MetPalettes$Tam[[1]][6],
               color = NA, alpha = 0.3) +
  geom_line(linewidth = 2,
            color = MetBrewer::MetPalettes$Tam[[1]][6]) + 
  theme_minimal() +
  facet_wrap(~response) +
  labs( y = "Time relative to sunrise (hr)",
        x = "Light pollution: ln(Radiance + 1)") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(color = "black", size = 10), 
    # legend.title = element_text(color = "black", size = 11),
    # legend.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 10),
    axis.text = element_text(color = "black", size = 9),
    axis.line = element_line(color = "black", linewidth = 0.3))

full_join(m1_df, med1_df) |> 
  ggplot(aes(x = alan, y = fit, color = response, fill = response)) +
  geom_ribbon( aes(ymin = lower, ymax = upper),
               color = NA, alpha = 0.3) +
  geom_line(linewidth = 2) + 
  theme_minimal() +
    scale_color_manual(values = MetBrewer::MetPalettes$Tam[[1]][c(6,8)]) +
    scale_fill_manual(values = MetBrewer::MetPalettes$Tam[[1]][c(6,8)]) +
  labs( y = "Time relative to sunrise (hr)",
        x = "Light pollution: ln(Radiance + 1)") +
  annotate("text", x = 4, y = 1.9, label = "Median", size = 2.75,
           color = MetBrewer::MetPalettes$Tam[[1]][c(6)]) +
  annotate("text", x = 1, y = 0.75, label = "Onset", size = 2.75,
           color = MetBrewer::MetPalettes$Tam[[1]][c(8)]) +
  theme(
    legend.position = "none", 
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(color = "black", size = 10), 
    # legend.title = element_text(color = "black", size = 11),
    # legend.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 9),
    axis.text = element_text(color = "black", size = 8),
    axis.line = element_line(color = "black", linewidth = 0.3))

setwd(here::here("figures"))
ggsave(
  filename = "figure_2_morning.png", 
  width = 2.5, 
  height = 2, 
  units = "in", 
  dpi = 600
)

e1_df |> 
ggplot(aes(x = alan, y = fit, color = response, fill = response)) +
  geom_ribbon( aes(ymin = lower, ymax = upper),
               color = NA, alpha = 0.3) +
  geom_line(linewidth = 2) + 
  theme_minimal() +
  scale_color_manual(values = MetBrewer::MetPalettes$Tam[[1]][c(5)]) +
  scale_fill_manual(values = MetBrewer::MetPalettes$Tam[[1]][c(5)]) +
  labs( y = "Time relative to sunset (hr)",
        x = "Light pollution: ln(Radiance + 1)") +
  # annotate("text", x = 4, y = 1.9, label = "Median", size = 2.75,
           # color = MetBrewer::MetPalettes$Tam[[1]][c(6)]) +
  # annotate("text", x = 1, y = 0.75, label = "Onset", size = 2.75,
           # color = MetBrewer::MetPalettes$Tam[[1]][c(8)]) +
  theme(
    legend.position = "none", 
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(color = "black", size = 10), 
    # legend.title = element_text(color = "black", size = 11),
    # legend.text = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 9),
    axis.text = element_text(color = "black", size = 8),
    axis.line = element_line(color = "black", linewidth = 0.3))

setwd(here::here("figures"))
ggsave(
  filename = "figure_2_evening.png", 
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

