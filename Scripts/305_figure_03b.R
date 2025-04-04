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
load("onset.RData")
load("cessation.RData")
rm(m1b, m2, m3, m4, m5, m6, m7, m8,
   e2, e3, e4, e5, e6, e7, e8)

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

both <- fam_re |> 
  dplyr::left_join(fam_re_e) 

# figuring out custom palette colors
# x <- tibble(
#   period = c("Night", "Day"),
#   colorx = MetBrewer::MetPalettes$Ingres[[1]][c(3, 5)])
# 
# y <- tibble(
#   effect = c("prolong", "shorten"),
#   colory = c("gray97", MetBrewer::MetPalettes$Ingres[[1]][c(7)]))
# 
# rgb.x <- col2rgb(x$colorx) |> 
#   as_tibble(rownames = "col") |> 
#   pivot_longer(V1:V2, names_to = "cat", values_to = "val") |> 
#   pivot_wider(names_from = col, values_from = val) |> 
#   add_column(period = x$period) |> 
#   dplyr::select(period, red.x =  red, green.x = green, blue.x = blue)
# 
# rgb.y <- col2rgb(y$colory) |> 
#   as_tibble(rownames = "col") |> 
#   pivot_longer(V1:V2, names_to = "cat", values_to = "val") |> 
#   pivot_wider(names_from = col, values_from = val) |> 
#   add_column(effect = y$effect) |> 
#   dplyr::select(effect, red.y = red, green.y = green, blue.y = blue)  
# 
# test <- tidyr::expand_grid(
#   effect = y$effect, 
#   period = x$period) |> 
#   left_join(rgb.x) |> 
#   left_join(rgb.y) |> 
#   rowwise() |> 
#   mutate( red = mixcolor(alpha = 0.5, sRGB(red.x, green.x, blue.x), sRGB(red.y, green.y, blue.y))@coords[1],
#           green = mixcolor(alpha = 0.5, sRGB(red.x, green.x, blue.x), sRGB(red.y, green.y, blue.y))@coords[2],
#           blue = mixcolor(alpha = 0.5, sRGB(red.x, green.x, blue.x), sRGB(red.y, green.y, blue.y))@coords[3]) |> 
#   mutate(hex = rgb(red = red, green = green, blue = blue, maxColorValue = 255))

custom_pal <- c(
  "1-1" = "#413927",
  "2-1" = "#A7833A",
  "1-2" = "#7D8A91",
  "2-2" = "#E4D4A4")

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
  biscale::bi_scale_color(pal = custom_pal, na.value = "white", dim = 2) +
  theme(legend.position = "none") +
  geom_tiplab(offset = 4.1, size = 2.5) +
  theme(plot.margin = unit(c(2, 20, 2, 2), "mm")) +
  coord_cartesian(clip = "off")
    
leg <- bi_legend(
  pal = custom_pal, dim = 2, rotate_pal = FALSE,
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
  filename = "figure_03b.png", 
  width = 4.25, 
  height = 7.3, 
  units = "in", 
  dpi = 600)
