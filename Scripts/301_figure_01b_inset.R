library(here)
library(tidyverse)

setwd(here::here("data"))

load("onset_data_conf_0.75_det_100_grid_10.RData")

d <- final |> 
  dplyr::ungroup() |> 
  dplyr::select( lat, lon, avg_rad ) |> 
  dplyr::distinct() |> 
  dplyr::mutate(alan = log1p( avg_rad ))

ggplot(d, aes(x = alan)) + 
  geom_histogram(fill = "#F7C267", color = "black", linewidth = 0.2) +
  theme_classic() +
  labs(x = "ln(Radiance + 1)",
       y = "Count" ) +
  theme( plot.background = element_rect( fill = "#04050F", color = NA),
         panel.background = element_rect( fill = "#04050F", color = NA),
         axis.line = element_line(color = "white", linewidth = 0.2),
         axis.ticks = element_blank(),
         axis.text = element_text(color = "white", size = 7),
         axis.title = element_text(color = "white", size = 8))

setwd(here::here("figures"))
ggsave(
  filename = "alan_histogram.png",
  width = 1.75, 
  height = 1, 
  units = "in", 
  dpi = 600)  