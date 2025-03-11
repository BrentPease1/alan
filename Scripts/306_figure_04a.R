library(avotrex)
library(tidyverse)
library(here)
library(ape)
library(biscale)
library(janitor)
library(ggtree)
library(cowplot)
library(glmmTMB)

data("BirdTree_trees")
data("bird.families")

phy <- BirdTree_trees[[1]]
tax <- BirdTree_tax |> 
  tibble::as_tibble()

key <- readr::read_csv(here::here("data/species_keys/birdweather_elton_botw_name_key.csv"))

load(here::here("data/vocalization_activity/onset_data_conf_0.75_det_100_grid_10.RData"))
load(here::here("data/vocalization_activity/cessation_data_conf_0.75_det_100_grid_10.RData"))

species <- final |> 
  dplyr::ungroup() |> 
  dplyr::select(sci_name) |> 
  dplyr::distinct() |> 
  dplyr::full_join(
    final_cess |> 
      dplyr::ungroup() |> 
      dplyr::select(sci_name) |> 
      dplyr::distinct()) |>  
  dplyr::rename(sci_name_bw = sci_name) |> 
  dplyr::left_join(key) 

focal_spp <- tax |> 
  dplyr::mutate(sci_name = paste(Genus, Species)) |>
  dplyr::inner_join( 
    tibble::tibble(
      sci_name = unique(species$sci_name_elton))) |> 
  dplyr::group_by(BLFamilyLatin) |> 
  dplyr::slice(1) |> 
  dplyr::select(TipLabel, family = BLFamilyLatin) |> 
  dplyr::ungroup() 

not_data <- dplyr::anti_join( tax, focal_spp)

my_tree <- ape::drop.tip( phy, not_data$TipLabel)

family_map <- focal_spp |> 
  tibble::deframe()

my_tree$tip.label <- family_map[my_tree$tip.label]

setwd(here::here("Results"))
load("tmb_onset_models_family.RData")
load("tmb_evening_models_family.RData")

m1_ranef <- glmmTMB::ranef(m1)
e1_ranef <- glmmTMB::ranef(e1)

fam_re <- m1_ranef$cond$family |> 
  tibble::as_tibble(rownames = "tip.label") |> 
  janitor::clean_names() |> 
  dplyr::rename(tip.label = tip_label,
                int.m = intercept, 
                slope.m = alan_sc)

fam_re_e <- e1_ranef$cond$family |> 
  tibble::as_tibble(rownames = "tip.label") |> 
  janitor::clean_names() |> 
  dplyr::rename(tip.label = tip_label,
                int.e = intercept, 
                slope.e = alan_sc)

fam_bi <- fam_re |> 
  dplyr::left_join(
    fam_re_e) |> 
  dplyr::mutate( int.m.cat = ifelse(int.m < 0, 1, 2), 
                 slope.m.cat = ifelse( slope.m < 0, 1, 2),
                 
                 int.e.cat = ifelse(!is.na(int.e) & int.e > 0, 1,
                                    ifelse(!is.na(int.e) & int.e < 0, 2, NA)),
                 slope.e.cat = ifelse(!is.na(slope.e) & slope.e > 0, 1, 
                                      ifelse(!is.na(slope.e) & slope.e < 0, 2, NA))) |> 
  dplyr::mutate(bi.m = paste(int.m.cat, slope.m.cat, sep = "-"),
                bi.e = ifelse(is.na(int.e.cat), NA, paste(int.e.cat, slope.e.cat, sep = "-"))) |> 
  dplyr::select(tip.label, bi.m, bi.e) 

p <- ggtree::ggtree( my_tree, layout = "rectangular", linewidth = 0.2)

p2 <-
  p %<+% fam_bi + 
  geom_tippoint( aes(color = bi.m), size = 1.5) +
  geom_tippoint( aes(color = bi.e), size = 1.5, position = position_nudge(x = 3)) + 
  biscale::bi_scale_color(pal = "BlueOr", na.value = "white", dim = 2, rotate_pal = TRUE) +
  theme(legend.position = "none") +
  geom_tiplab(offset = 4.1, size = 2.5) +
  theme(plot.margin = unit(c(2, 20, 2, 2), "mm")) +
  coord_cartesian(clip = "off")
    
leg <- bi_legend(
  pal = "BlueOr", dim = 2, rotate_pal = TRUE,
            xlab = "Baseline onset",
            ylab = "Light pollution effect",
            size = 9,
            arrows = FALSE) +
  theme(plot.background = element_rect(fill = NA, color = NA), 
        panel.background = element_rect(fill = NA, color = NA))

cowplot::ggdraw() +
  draw_plot( p2 ) +
  draw_plot( leg, 0.03, 0.55, 0.47, 0.47)

setwd(here::here("Results/Figures"))

ggsave(
  filename = "figure_04a.png", 
  width = 4.25, 
  height = 7.25, 
  units = "in", 
  dpi = 600)
