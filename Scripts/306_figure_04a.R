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

setwd(here::here("data"))

key <- readr::read_csv("birdweather_elton_botw_name_key.csv")

load("onset_data_conf_0.75_det_100_grid_10.RData")
load("median_data_conf_0.75_det_100.RData")
load("cessation_data_conf_0.75_det_100_grid_10.RData")

species <- final |> 
  dplyr::ungroup() |> 
  dplyr::select(sci_name) |> 
  dplyr::distinct() |> 
  dplyr::full_join(
    final_cess |> 
      dplyr::ungroup() |> 
      dplyr::select(sci_name) |> 
      dplyr::distinct()) |> 
  dplyr::full_join(
    final_med |> 
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

setwd(here::here("results"))
load("tmb_onset_models_family.RData")

m1_ranef <- glmmTMB::ranef(m1)

fam_re <- m1_ranef$cond$family |> 
  tibble::as_tibble(rownames = "tip.label") |> 
  janitor::clean_names() |> 
  dplyr::rename(tip.label = tip_label)

fam_bi <- fam_re |> 
  dplyr::mutate(alan_cat = ifelse(alan_sc < 0, 1, 2), 
                int_cat = ifelse(intercept < 0, 1, 2)) |> 
  dplyr::mutate(bi_class = paste(int_cat, alan_cat, sep = "-"))

p <- ggtree::ggtree( my_tree, layout = "rectangular", linewidth = 0.2)

p2 <- p %<+% fam_bi + 
  geom_tippoint( aes(color = bi_class), size = 1.5) +
  biscale::bi_scale_color(pal = "BlueOr", dim = 2, rotate_pal = TRUE) +
  theme(legend.position = "none") +
  geom_tiplab(aes(color = bi_class), offset = 1.5, size = 2.5) +
  theme(plot.margin = unit(c(2, 20, 2, 2), "mm")) +
  coord_cartesian(clip = "off")
    
leg <- bi_legend(
  pal = "BlueOr", dim = 2, rotate_pal = TRUE,
            xlab = "Baseline onset",
            ylab = "Light pollution effect",
            size = 9,
            arrows = FALSE) 

cowplot::ggdraw() +
  draw_plot( p2 ) +
  draw_plot( leg, 0.08, 0.65, 0.4, 0.4)

ggsave(
  filename = "figure_04a.png", 
  width = 4.25, 
  height = 7.2, 
  units = "in", 
  dpi = 600)