library(here)
library(data.table)
library(lubridate)
library(suncalc)
library(lutz)
library(sf)
library(activity)
library(stringr)
library(ggplot2)
library(scales)
library(MetBrewer)
setDTthreads(0)

viirs <- fread(here('Results/VIIRS/vocal_activity_annotated_conf_0.75_det_100.csv'))

viirs[, station_locs := .GRP, .(lat, lon)]

viirs <- viirs[!duplicated(station_locs),.(station_locs, avg_rad, lat, lon)]
viirs[, rad_sc := log1p(avg_rad)]
# Choose a color from the MetBrewer Hiroshige palette for the fill
fill_color <- MetBrewer::MetPalettes$Hiroshige[[1]][1]
color_palette <- c(rep("#5e6174",6), rep("#8a7a72",6), rep("#bb996e",6),
                   rep("#e1b36a",6), rep("#f7c267", 6))  # Replace with your hex codes
# Plot the histogram
ggplot(viirs, aes(x = rad_sc)) +
  geom_histogram(fill = color_palette, color = "black", alpha = 0.7) +
  labs(
    x = "Light Pollution: ln(Radiance + 1)",
    y = "Sensors",
  #  title = "Distribution of Average Radiance Across Stations"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.02),
    axis.title = element_text(color = "black", size = 10),
    axis.text = element_text(color = "black", size = 9),
    legend.title = element_text(color = "black", size = 10),
    legend.text = element_text(color = "black", size = 9),
    panel.background = element_rect(color = NA, fill = "white"),
    plot.background = element_rect(color = NA, fill = "white")
  )

ggsave(filename = here('Results/Figures/viirs_talling_plotv02.png'), 
       plot = last_plot(),
       width = 6, 
       height = 4, 
       units = "in", 
       dpi = 600)

largest <- viirs[avg_rad == max(avg_rad),]
smallest <- viirs[avg_rad == min(avg_rad),]

fwrite(viirs, file = here('Results/VIIRS/station_locs_viirs.csv'))

# categorize nighttime light
viirs[, rad_cat := fcase(
  avg_rad < quantile(avg_rad, probs = 0.334, na.rm = T), "low",
  avg_rad >= quantile(avg_rad, probs = 0.334, na.rm = T) & avg_rad < quantile(avg_rad, probs = 0.667, na.rm = T), "med",
  avg_rad >= quantile(avg_rad, probs = 0.667, na.rm = T), "high"
)]

viirs[, rad_cat := factor(rad_cat, levels = c("low", "med", "high"))]

# Plot histogram with custom gradient fill based on avg_rad values
ggplot(viirs, aes(x = avg_rad)) +
  geom_histogram(aes(fill = rad_cat), binwidth = 5, color = "black", alpha = 0.8) +
  labs(
    x = "Average Radiance",
    y = "Frequency",
    #  title = "Distribution of Average Radiance Across Stations"
  ) +
  # Apply a custom gradient color scale
  scale_fill_manual(
    values = c("low" = "#04050e", "med" = "#998265", "high" = "#f0ead4"),
    name = "Radiance Quantile"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.02),
    axis.title = element_text(color = "black", size = 10),
    axis.text = element_text(color = "black", size = 9),
    legend.title = element_text(color = "black", size = 10),
    legend.text = element_text(color = "black", size = 9),
    panel.background = element_rect(color = NA, fill = "white"),
    plot.background = element_rect(color = NA, fill = "white")
  )

ggsave(filename = here('Results/Figures/viirs_talling_plot_categories.png'), 
       plot = last_plot(),
       width = 6, 
       height = 4, 
       units = "in", 
       dpi = 600)

