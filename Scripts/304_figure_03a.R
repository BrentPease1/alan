library(glmmTMB)
library(here)
library(tidyverse)
library(MetBrewer)
library(effects)
library(patchwork)

setwd(here::here("Results"))

load("onset.RData")
load("cessation.RData")

lat_m_z <- scale( abs( d_onset$lat ) )
lat_e_z <- scale(abs(d_e$lat))
alan_m_z <- scale( log1p( d_onset$avg_rad) )
alan_e_z <- scale(log1p( d_e$avg_rad))
range_m_z <- scale( log( d_onset$range_size ) )
range_e_z <- scale(log(d_e$range_size))
gr_m_z <- scale( d_onset$for_strat_ground)
gr_e_z <- scale(d_e$for_strat_ground)
cd_m_z <- scale(log(d_onset$cd_raw))
cd_e_z <- scale(log(d_e$cd_raw))
sr_m_z <- scale(log(d_onset$sr_raw))
sr_e_z <- scale(log(d_e$sr_raw))

m2_me <- effects::effect(
  term = c("alan_sc", "lat_sc"), 
  xlevels = list( alan_sc = seq(from = min(d_onset$alan_sc), 
                                to = max(d_onset$alan_sc), 
                                length.out = 10),
                  lat_sc = (c(20, 60) - attr(lat_m_z, "scaled:center"))/attr(lat_m_z, "scaled:scale")),
  mod = m2)

e2_me <- effect(
  term = c("alan_sc", "lat_sc"), 
  xlevels = list( alan_sc = seq(from = min(d_e$alan_sc), 
                                to = max(d_e$alan_sc), 
                                length.out = 10),
                  lat_sc = (c(20, 60) - attr(lat_e_z, "scaled:center"))/attr(lat_e_z, "scaled:scale")),
  mod = e2)

lat_all <- as.data.frame(m2_me) |> 
  dplyr::mutate( lat = round(lat_sc * attr(lat_m_z, "scaled:scale") +
                               attr(lat_m_z, "scaled:center"), 0)) |> 
  dplyr::mutate(alan = alan_sc*attr(alan_m_z, "scaled:scale") +
                  attr(alan_m_z, "scaled:center")) |> 
  tibble::add_column(response = "Onset") |> 
  dplyr::full_join(
    as.data.frame(e2_me) |> 
      dplyr::mutate( lat = round(lat_sc * attr(lat_e_z, "scaled:scale") +
                                   attr(lat_e_z, "scaled:center"), 0)) |> 
      dplyr::mutate(alan = alan_sc*attr(alan_e_z, "scaled:scale") +
                      attr(alan_e_z, "scaled:center")) |> 
      tibble::add_column(response = "Cessation")) |> 
  dplyr::mutate( response = factor( response, 
                                    levels = c("Onset", 
                                               "Cessation"))) |> 
  tibble::add_column(cov.lab = "Latitude") |> 
  dplyr::mutate(cov.raw = lat, 
                cov.code = ifelse(cov.raw == min(cov.raw), "Low", "High")) |> 
  dplyr::select(response, cov.lab, alan, cov.raw, cov.code, fit, lower, upper)

m3_me <- effect(
  term = c("alan_sc", "range_size_sc"), 
  xlevels = list( alan_sc = seq(from = min(d_onset$alan_sc), 
                                to = max(d_onset$alan_sc), 
                                length.out = 10),
                  range_size_sc = (c(12, 18) - 
                                     attr(range_m_z, "scaled:center"))/
                    attr(range_m_z, "scaled:scale")),
  mod = m3)

e3_me <- effect(
  term = c("alan_sc", "range_size_sc"), 
  xlevels = list( alan_sc = seq(from = min(d_e$alan_sc), 
                                to = max(d_e$alan_sc), 
                                length.out = 10),
                  range_size_sc = (c(12, 18) - 
                                     attr(range_e_z, "scaled:center"))/
                    attr(range_e_z, "scaled:scale")),
  mod = e3)

range_all <- as.data.frame(m3_me) |> 
  dplyr::mutate(alan = alan_sc*attr(alan_m_z, "scaled:scale") + attr(alan_m_z, "scaled:center")) |>
  dplyr::mutate(range = round(range_size_sc*attr(range_m_z, "scaled:scale") + 
                                attr(range_m_z, "scaled:center"), 0)) |> 
  tibble::add_column(response = "Onset") |> 
  dplyr::full_join(
    as.data.frame(e3_me) |> 
      dplyr::mutate(alan = alan_sc*attr(alan_e_z, "scaled:scale") + attr(alan_e_z, "scaled:center")) |>
      dplyr::mutate(range = round(range_size_sc*attr(range_e_z, "scaled:scale") + 
                                    attr(range_e_z, "scaled:center"), 0)) |> 
      tibble::add_column(response = "Cessation")) |> 
  dplyr::mutate( response = factor( response, 
                                    levels = c("Onset", 
                                               "Cessation"))) |> 
  tibble::add_column(cov.lab = "Range size") |> 
  dplyr::mutate(cov.raw = range, 
                cov.code = ifelse(cov.raw == min(cov.raw), "Low", "High")) |> 
  dplyr::select(response, cov.lab, alan, cov.raw, cov.code, fit, lower, upper)

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
  tibble::add_column(response = "Onset") |> 
  dplyr::full_join(
    e4_me |> 
      as.data.frame() |> 
      dplyr::mutate(alan = alan_sc*attr(alan_e_z, "scaled:scale") + 
                      attr(alan_e_z, "scaled:center")) |> 
      tibble::add_column(response = "Cessation")) |> 
  dplyr::mutate( response = factor( response, 
                                    levels = c("Onset", 
                                               "Cessation"))) |> 
  tibble::add_column(cov.lab = "Nest") |> 
  dplyr::mutate(cov.raw = as.numeric(cavity), 
                cov.code = ifelse(cov.raw == min(cov.raw), "Low", "High")) |> 
  dplyr::select(response, cov.lab, alan, cov.raw, cov.code, fit, lower, upper)

m5_me <- effect(
  term = c("alan_sc", "hd"), 
  xlevels = list( alan_sc = seq(from = min(d_onset$alan_sc), 
                                to = max(d_onset$alan_sc), 
                                length.out = 10),
                  hd = unique(d_onset$hd)),
  mod = m5)

e5_me <- effect(
  term = c("alan_sc", "hd"), 
  xlevels = list( alan_sc = seq(from = min(d_e$alan_sc), 
                                to = max(d_e$alan_sc), 
                                length.out = 10),
                  hd = unique(d_e$hd)),
  
  mod = e5)

hd_all <- as.data.frame(m5_me) |> 
  dplyr::mutate(alan = alan_sc*attr(alan_m_z, "scaled:scale") + attr(alan_m_z, "scaled:center")) |>
  tibble::add_column(response = "Onset") |> 
  dplyr::filter(!hd==2) |> 
  dplyr::full_join(
    as.data.frame(e5_me) |> 
      dplyr::mutate(alan = alan_sc*attr(alan_e_z, "scaled:scale") + attr(alan_e_z, "scaled:center")) |>
      tibble::add_column(response = "Cessation") |> 
      dplyr::filter(!hd==2)) |> 
  dplyr::mutate( response = factor( response, 
                                    levels = c("Onset", 
                                               "Cessation"))) |> 
  tibble::add_column(cov.lab = "Habitat") |> 
  dplyr::mutate(cov.raw = hd, 
                cov.code = ifelse(cov.raw == 1, "Low", "High")) |> 
  dplyr::select(response, cov.lab, alan, cov.raw, cov.code, fit, lower, upper) |> 
  dplyr::mutate(cov.raw = as.numeric(cov.raw))

m6_me <- effect(
  term = c("alan_sc", "sr"), 
  xlevels = list( alan_sc = seq(from = min(d_onset$alan_sc), 
                                to = max(d_onset$alan_sc), 
                                length.out = 10),
                  sr = c(min(d_onset$sr), max(d_onset$sr))),
  mod = m6)

e6_me <- effect(
  term = c("alan_sc", "sr"), 
  xlevels = list( alan_sc = seq(from = min(d_e$alan_sc), 
                                to = max(d_e$alan_sc), 
                                length.out = 10),
                  sr = c(min(d_e$sr), max(d_e$sr))),
  mod = e6)

sr_all <- as.data.frame(m6_me) |> 
  dplyr::mutate(alan = alan_sc*attr(alan_m_z, "scaled:scale") + attr(alan_m_z, "scaled:center")) |>
  dplyr::mutate( sr = sr*attr(sr_m_z, "scaled:scale") + attr(sr_m_z, "scaled:center")) |> 
  tibble::add_column(response = "Onset") |> 
  dplyr::full_join(
    as.data.frame(e6_me) |> 
      dplyr::mutate(alan = alan_sc*attr(alan_e_z, "scaled:scale") + attr(alan_e_z, "scaled:center")) |>
      dplyr::mutate( sr = sr*attr(sr_e_z, "scaled:scale") + attr(sr_e_z, "scaled:center")) |> 
      tibble::add_column(response = "Cessation")) |> 
  dplyr::mutate( response = factor( response, 
                                    levels = c("Onset", 
                                               "Cessation"))) |> 
  tibble::add_column(cov.lab = "Species richness") |> 
  group_by(response) |> 
  dplyr::mutate(cov.raw = sr, 
                cov.code = ifelse(cov.raw == min(cov.raw), "Low", "High")) |>
  dplyr::ungroup() |> 
  dplyr::select(response, cov.lab, alan, cov.raw, cov.code, fit, lower, upper)

m7_me <- effect(
  term = c("alan_sc", "cd"), 
  xlevels = list( alan_sc = seq(from = min(d_onset$alan_sc), 
                                to = max(d_onset$alan_sc), 
                                length.out = 10),
                  cd = c(min(d_onset$cd), 
                         max(d_onset$cd))),
  mod = m7)

e7_me <- effect(
  term = c("alan_sc", "cd"), 
  xlevels = list( alan_sc = seq(from = min(d_onset$alan_sc), 
                                to = max(d_onset$alan_sc), 
                                length.out = 10),
                  cd = c(min(d_e$cd), 
                         max(d_e$cd))),
  mod = e7)

cd_all <- as.data.frame(m7_me) |> 
  dplyr::mutate(alan = alan_sc*attr(alan_m_z, "scaled:scale") + attr(alan_m_z, "scaled:center")) |>
  dplyr::mutate(cd = round(cd*attr(cd_m_z, "scaled:scale") + attr(cd_m_z, "scaled:center"), 1)) |> 
  tibble::add_column(response = "Onset") |> 
  dplyr::full_join(
    as.data.frame(e7_me) |> 
      dplyr::mutate(alan = alan_sc*attr(alan_e_z, "scaled:scale") + attr(alan_e_z, "scaled:center")) |>
      dplyr::mutate(cd = round(cd*attr(cd_e_z, "scaled:scale") + attr(cd_e_z, "scaled:center"), 1)) |> 
      tibble::add_column(response = "Cessation")) |> 
  dplyr::mutate( response = factor( response, 
                                    levels = c("Onset", 
                                               "Cessation"))) |> 
  tibble::add_column(cov.lab = "Eye size") |> 
  dplyr::mutate(cov.raw = cd, 
                cov.code = ifelse(cov.raw == min(cov.raw), "Low", "High")) |> 
  dplyr::select(response, cov.lab, alan, cov.raw, cov.code, fit, lower, upper)

m8_me <- effects::effect(
  term = c("alan_sc", "migration"), 
  xlevels = list( alan_sc = seq(from = min(d_onset$alan_sc), 
                                to = max(d_onset$alan_sc), 
                                length.out = 10),
                  migration = unique(d_onset$migration)),
  
  mod = m8)

e8_me <- effects::effect(
  term = c("alan_sc", "migration"), 
  xlevels = list( alan_sc = seq(from = min(d_e$alan_sc), 
                                to = max(d_e$alan_sc), 
                                length.out = 10),
                  migration = unique(d_e$migration)),
  mod = e8)

migr_all <- as.data.frame(m8_me) |> 
  dplyr::mutate(alan = alan_sc*attr(alan_m_z, "scaled:scale") + attr(alan_m_z, "scaled:center")) |>
  dplyr::filter(migration == 1 | migration == 3) |> 
  tibble::add_column(response = "Onset") |> 
  dplyr::full_join(
    as.data.frame(e8_me) |> 
      dplyr::mutate(alan = alan_sc*attr(alan_e_z, "scaled:scale") + attr(alan_e_z, "scaled:center")) |>
      dplyr::filter(migration == 1 | migration == 3) |> 
      tibble::add_column(response = "Cessation")) |> 
  dplyr::mutate( response = factor( response, 
                                    levels = c("Onset", 
                                               "Cessation"))) |> 
  tibble::add_column(cov.lab = "Migration") |> 
  dplyr::mutate(cov.raw = migration, 
                cov.code = ifelse(cov.raw == 1, "Low", "High")) |> 
  dplyr::select(response, cov.lab, alan, cov.raw, cov.code, fit, lower, upper) |> 
  dplyr::mutate(cov.raw = as.numeric(cov.raw))

all <- full_join(range_all, cavity_me_all) |> 
  full_join(hd_all) |> 
  full_join(lat_all) |> 
  full_join(cd_all) |> 
  full_join(sr_all) |>
  full_join(migr_all) |> 
  dplyr::mutate(cov.code = factor(cov.code, levels = c("Low", "High"))) |> 
  dplyr::mutate(cov.lab = ifelse(cov.lab == "Species richness", "Sp. richness", cov.lab)) |> 
  mutate(
    cov.lab = factor(cov.lab,
                     levels = c("Eye size",
                                "Nest",
                                "Migration",
                                "Range size",
                                "Habitat",
                                "Latitude",
                                "Sp. richness"))) 

rect_df <- tidyr::expand_grid(
  response = unique(all$response),
  cov.lab = unique(all$cov.lab),
  period = c("Day", "Night")) |> 
  tibble::add_column(xmin = min(all$alan), 
                     xmax = max(all$alan)) |> 
  dplyr::mutate(ymin = ifelse(response == "Onset" & period == "Day", 0, 
                              ifelse(response == "Onset" & period == "Night", min(all$lower),
                                     ifelse(response == "Cessation" & period == "Day", min(all$lower), 0))),
                ymax = ifelse(response == "Onset" & period == "Day", max(all$upper), 
                              ifelse(response == "Onset" & period == "Night", 0, 
                                     ifelse(response == "Cessation" & period == "Day", 0, max(all$upper))))) |> 
  # dplyr::mutate(cov.lab = ifelse(cov.lab == "Species richness", "Sp. richness", cov.lab)) |> 
  mutate(
    cov.lab = factor(cov.lab,
                     levels = c("Eye size",
                                "Nest",
                                "Migration",
                                "Range size",
                                "Habitat",
                                "Latitude",
                                "Sp. richness")))

coefficients <- summary(m2)$coefficients$cond |> 
  tibble::as_tibble(rownames = "param") |> 
  janitor::clean_names() |> 
  tibble::add_column(period = "Onset", 
                     cov.lab = "Latitude") |> 
  dplyr::select(period, cov.lab, param, estimate, std_error, z_value, pr_z) |> 
  dplyr:full_join(
    summary(e2)$coefficients$cond |> 
      tibble::as_tibble(rownames = "param") |> 
      janitor::clean_names() |> 
      tibble::add_column(period = "Cessation", 
                         cov.lab = "Latitude") |> 
      dplyr::select(period, cov.lab, param, estimate, std_error, z_value, pr_z)) |> 
  dplyr::full_join(
    summary(m3)$coefficients$cond |> 
      tibble::as_tibble(rownames = "param") |> 
      janitor::clean_names() |> 
      tibble::add_column(period = "Onset", 
                         cov.lab = "Range size") |> 
      dplyr::select(period, cov.lab, param, estimate, std_error, z_value, pr_z)|> 
      full_join(
        summary(e3)$coefficients$cond |> 
          tibble::as_tibble(rownames = "param") |> 
          janitor::clean_names() |> 
          tibble::add_column(period = "Cessation", 
                             cov.lab = "Range size") |> 
          dplyr::select(period, cov.lab, param, estimate, std_error, z_value, pr_z)))|> 
  dplyr::full_join(
    summary(m4)$coefficients$cond |> 
      tibble::as_tibble(rownames = "param") |> 
      janitor::clean_names() |> 
      tibble::add_column(period = "Onset", 
                         cov.lab = "Nest") |> 
      dplyr::select(period, cov.lab, param, estimate, std_error, z_value, pr_z)|> 
      full_join(
        summary(e4)$coefficients$cond |> 
          tibble::as_tibble(rownames = "param") |> 
          janitor::clean_names() |> 
          tibble::add_column(period = "Cessation", 
                             cov.lab = "Nest") |> 
          dplyr::select(period, cov.lab, param, estimate, std_error, z_value, pr_z))) |> 
  dplyr::full_join(
    summary(m5)$coefficients$cond |> 
      tibble::as_tibble(rownames = "param") |> 
      janitor::clean_names() |> 
      tibble::add_column(period = "Onset", 
                         cov.lab = "Habitat") |> 
      dplyr::select(period, cov.lab, param, estimate, std_error, z_value, pr_z)|> 
      full_join(
        summary(e5)$coefficients$cond |> 
          tibble::as_tibble(rownames = "param") |> 
          janitor::clean_names() |> 
          tibble::add_column(period = "Cessation", 
                             cov.lab = "Habitat") |> 
          dplyr::select(period, cov.lab, param, estimate, std_error, z_value, pr_z))) |> 
  dplyr::full_join(
    summary(m6)$coefficients$cond |> 
      tibble::as_tibble(rownames = "param") |> 
      janitor::clean_names() |> 
      tibble::add_column(period = "Onset", 
                         cov.lab = "Sp. richness") |> 
      dplyr::select(period, cov.lab, param, estimate, std_error, z_value, pr_z)|> 
      full_join(
        summary(e6)$coefficients$cond |> 
          as_tibble(rownames = "param") |> 
          janitor::clean_names() |> 
          tibble::add_column(period = "Cessation", 
                             cov.lab = "Sp. richness") |> 
          dplyr::select(period, cov.lab, param, estimate, std_error, z_value, pr_z))) |> 
  dplyr::full_join(
    summary(m7)$coefficients$cond |> 
      as_tibble(rownames = "param") |> 
      janitor::clean_names() |> 
      tibble::add_column(period = "Onset", 
                         cov.lab = "Eye size") |> 
      dplyr::select(period, cov.lab, param, estimate, std_error, z_value, pr_z)|> 
      full_join(
        summary(e7)$coefficients$cond |> 
          tibble::as_tibble(rownames = "param") |> 
          janitor::clean_names() |> 
          tibble::add_column(period = "Cessation", 
                             cov.lab = "Eye size") |> 
          dplyr::select(period, cov.lab, param, estimate, std_error, z_value, pr_z))) |> 
  dplyr::full_join(
    summary(m8)$coefficients$cond |> 
      tibble::as_tibble(rownames = "param") |> 
      janitor::clean_names() |> 
      tibble::add_column(period = "Onset", 
                         cov.lab = "Migration") |> 
      dplyr::select(period, cov.lab, param, estimate, std_error, z_value, pr_z)|> 
      full_join(
        summary(e8)$coefficients$cond |> 
          tibble::as_tibble(rownames = "param") |> 
          janitor::clean_names() |> 
          tibble::add_column(period = "Cessation", 
                             cov.lab = "Migration") |> 
          dplyr::select(period, cov.lab, param, estimate, std_error, z_value, pr_z))) |> 
  dplyr:mutate(
    cov.lab = factor(cov.lab,
                     levels = c("Eye size",
                                "Nest",
                                "Migration",
                                "Range size",
                                "Habitat",
                                "Latitude",
                                "Sp. richness")))


star <- coefficients |> 
  dplyr::filter(grepl(":", param)) |>
  dplyr::filter(!grepl("hd2", param)) |> 
  dplyr::filter(!grepl("migration2", param)) |> 
  dplyr::mutate(star.lab = ifelse(pr_z < 0.001, "***", 
                                  ifelse(pr_z >= 0.001 & pr_z < 0.01, "**", 
                                         ifelse(pr_z >= 0.01 & pr_z < 0.05, "*", 
                                                ifelse(pr_z >= 0.05 & pr_z < 0.1, ".", ""))))) |> 
  dplyr::select(response = period, 
                cov.lab, star.lab) |> 
  dplyr::mutate(cov.lab = as.character(cov.lab)) |> 
  dplyr::mutate(cov.lab = ifelse(is.na(cov.lab), "Sp. richness", cov.lab)) |> 
  dplyr:mutate(
    cov.lab = factor(cov.lab,
                     levels = c("Eye size",
                                "Nest",
                                "Migration",
                                "Range size",
                                "Habitat",
                                "Latitude",
                                "Sp. richness"))) |>
  dplyr::mutate( response = factor( response, 
                                    levels = c("Onset", 
                                               "Cessation"))) |> 
  dplyr::mutate(dir.lab = ifelse(cov.lab == "Habitat" & response == "Cessation", "Oppose", "Support")) |> 
  tibble::add_column(alan = 0.65, 
                     fit = 2.08)

ggplot() +
  facet_grid(cov.lab~response) +
  geom_rect(data = filter(rect_df, period == "Day"),
            aes( xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = MetBrewer::MetPalettes$Ingres[[1]][5],
            color = NA,
            alpha = 0.15 ) +
  geom_rect(data = filter(rect_df, period == "Night"),
            aes( xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = MetBrewer::MetPalettes$Ingres[[1]][1],
            color = NA,
            alpha = 0.15 ) +
  geom_ribbon(data = all, aes(x = alan, ymin = lower, ymax = upper, fill = cov.code), color = NA, alpha = 0.4) +
  geom_line(data = all, aes(x = alan, y = fit, color = cov.code), linewidth = 1.5) +
  scale_color_manual(values = MetBrewer::MetPalettes$Cassatt2[[1]][c(3, 8)]) +
  scale_fill_manual(values = MetBrewer::MetPalettes$Cassatt2[[1]][c(3, 8)]) +
  ggnewscale::new_scale_color() +
  geom_text( data = star,
             aes(x = alan, y = fit, label = star.lab,
                 color = dir.lab)) +
  scale_color_manual(values = c(MetBrewer::MetPalettes$Hiroshige[[1]][1], "black")) +
  theme_minimal() +
  labs( x = "Light pollution: ln(Radiance + 1)",
        y = "Time relative to sunrise or sunset (hr)",
        fill = "Covariate value", 
        color = "Covariate value") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title = element_blank(),
    legend.direction = "horizontal",
    legend.key.size = unit(3, "mm"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(color = "black", size = 9.5),
    axis.title = element_text(color = "black", size = 9.5),
    axis.text = element_text(color = "black", size = 8.5),
    axis.line = element_line(color = "black", linewidth = 0.3))

setwd(here::here("Results/Figures"))
ggsave(
  filename = "figure_03a.png", 
  width = 2.5,
  height = 7.3, 
  units = "in", 
  dpi = 600)

migr_all |> 
  dplyr::group_by(response) |> 
  dplyr::filter(alan == min(alan) | alan == max(alan)) |> 
  dplyr::select(response, migr = cov.raw, alan,  fit) |> 
  dplyr::mutate(migr = ifelse(migr == 1, "Sedentary", "Migrant")) |> 
  dplyr::mutate(alan = ifelse(alan < 0, "Dark", "bright")) |> 
  tidyr::pivot_wider(names_from = alan, values_from = fit) |> 
  dplyr::mutate(diff = (Dark - bright) * 60)

hd_all |> 
  dplyr::group_by(response, cov.raw) |> 
  dplyr::filter(alan == min(alan) | alan == max(alan)) |> 
  dplyr::select(response, hd = cov.raw, alan,  fit) |> 
  dplyr::mutate(hd = ifelse(hd == 1, "Dense", "Open")) |> 
  dplyr::mutate(alan = ifelse(alan < 1, "Dark", "bright")) |> 
  tidyr::pivot_wider(names_from = alan, values_from = fit) |> 
  dplyr::mutate(diff = (Dark - bright) * 60)