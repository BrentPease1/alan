library(here)
library(tidyverse)
library(sf)

setwd(here::here("data"))

key <- readr::read_csv("birdweather_elton_botw_name_key.csv")

botw_species <- key |> 
  dplyr::filter(nocturnal == 0) |> 
  dplyr::select(sci_name_botw) |> 
  dplyr::distinct() |> 
  dplyr::pull(sci_name_botw)

d <- readr::read_csv("vocal_activity_annotated_conf_0_det_10.csv")

# clunky huge data file...
# maps <- sf::st_read("BOTW.gdb", layer = "All_Species")
# 
# map_focal <- maps |> 
#   dplyr::filter(sci_name %in% botw_species) |> 
#   dplyr::select(sisid, sci_name, geometry = Shape)

# read in reduced map file
map_focal <- sf::st_read("focal_spp_maps.shp")

noct <- key |> 
  dplyr::select(com_name, sci_name = sci_name_bw, nocturnal) |> 
  dplyr::distinct()

combos <- d |> 
  dplyr::mutate( sci_name = ifelse(sci_name == "Falcipennis canadensis",
                                   "Canachites canadensis", sci_name),
                 sci_name = ifelse(sci_name == "Glossopsitta porphyrocephala",
                                   "Parvipsitta porphyrocephala", sci_name)) |> 
  dplyr::left_join(noct) |> 
  dplyr::filter(nocturnal == 0) |> 
  dplyr::select( lat, lon, com_name, sci_name_bw = sci_name) |> 
  dplyr::distinct() |> 
  dplyr::left_join(
    key |> 
      dplyr::select(com_name, sci_name_bw, sci_name_botw) |> 
      dplyr::distinct()) |> 
  dplyr::group_by(lat, lon) |> 
  dplyr::mutate(site = cur_group_id()) |> 
  dplyr::ungroup()

con <- rnaturalearth::ne_countries( scale = "small",
                                    returnclass = "sf") |> 
  dplyr::filter(!region_un == "Antarctica")

# distance with which to buffer range edges - picking 100 km
threshold <- units::set_units(100, km)

map_focal_sp <- sort(unique(map_focal$sci_name))

# loop through species
res <- list(list())
error_groups <- c()
for(i in 1:length(map_focal_sp)){
  
  tryCatch({
    # step 1: filter the species x location data to just the species and convert to sf
    sp_data <- combos |> 
      dplyr::filter(sci_name_botw == map_focal_sp[i]) |> 
      sf::st_as_sf(coords = c("lon", "lat"), 
                   crs = 4326)  
    
    # step 2: filter the species' range map and buffer range edges by 100 km
    sp_map <- map_focal |> 
      dplyr::filter(sci_name == map_focal_sp[i] ) |> 
      sf::st_make_valid() |> 
      sf::st_buffer(dist = threshold)
    
    # step 3: retain only records that are within buffered range
    sp_filtered <- sf::st_filter( sp_data, 
                                  sp_map )
    
    res[[i]] <- sp_filtered
    print(paste("Finished", i, "of", length(map_focal_sp)))
  }, error = function(e){
    message(paste("Error in species:", map_focal_sp[i], e$message))
    # stash species that had map issues (i.e., sf::st_make_valid throws error)
    error_groups <<- c(error_groups, map_focal_sp[i])
  })
}

# filtered species x location data - ONLY RETAINING GOOD SITES
updated_combos <- bind_rows(res) |> 
  sf::st_drop_geometry() |> 
  tibble::add_column( in_range = "yes" )

# save for convenience
save(
  combos, 
  updated_combos,
  error_groups,
  file = "range_map_filtering.RData"
)

map_focal_error <- map_focal |> 
  dplyr::filter(sci_name %in% error_groups)

combos_error <- combos |> 
  dplyr::filter(sci_name_botw %in% error_groups)

# I saved this info and then restarted the session to free up memory
save(
  map_focal_error,
  combos_error,
  error_groups,
  file = "inspect_error_species.RData"
)

load("inspect_error_species.RData")

# save error species to CSV for taking notes on maps
# tibble(sci_name_botw = error_groups, notes = NA) |>
#   write_csv("error_species.csv")

# loop through the error species, save maps as PDF for manual inspection
setwd(here::here("data/error_species_maps"))
for( i in 1:length(error_groups)){
  
  map <- map_focal_error |> 
    dplyr::filter(sci_name == error_groups[i])
  
  dat <- combos_error |> 
    dplyr::filter(sci_name_botw == error_groups[i]) |> 
    sf::st_as_sf(coords = c("lon", "lat"), 
                 crs = 4326) 
  
  p <-  ggplot() +
    geom_sf(data = con) +
    geom_sf(data = map, 
            fill = MetBrewer::MetPalettes$Hiroshige[[1]][8]) +
    geom_sf(data = dat, 
            color = MetBrewer::MetPalettes$Hiroshige[[1]][2]) +
    ggtitle(error_groups[i])
  
  ggsave(
    filename = paste0(error_groups[i], "_map.pdf"),
    p, 
    width = 8, 
    height = 8, 
    units = "in", 
    dpi = 600
  )
  
  rm(p)
  
  print(paste("finished", i, "of", length(error_groups)))
  
}

setwd(here::here("data/error_species_maps"))
# 8 species to do manual/custom range filtering
error_notes <- readr::read_csv("error_species.csv") 

manual_good <- error_notes |> 
  dplyr::filter(good == "yes") |> 
  dplyr::pull(sci_name_botw)

manual_bad <- error_notes |> 
  dplyr::filter(good == "no") |> 
  dplyr::pull(sci_name_botw)

# If restarting to save memory...
load("range_map_filtering.RData")

# 9 species that we have to manually filter out bad records
bad <- combos |> 
  dplyr::filter(sci_name_botw %in% manual_bad) |> 
  dplyr::filter(!(sci_name_botw == "Botaurus stellaris" & lon < -30)) |> 
  dplyr::filter(!(sci_name_botw == "Calidris maritima")) |> 
  dplyr::filter(!(sci_name_botw == "Catharus minimus" & lon < -110 & lat < 48)) |> 
  dplyr::filter(!(sci_name_botw == "Coccothraustes coccothraustes" & lon < -30)) |> 
  dplyr::filter(!(sci_name_botw == "Phylloscopus collybita" & lon < -30)) |> 
  dplyr::filter(!(sci_name_botw == "Helopsaltes ochotensis")) |> 
  dplyr::filter(!(sci_name_botw == "Setophaga pinus" & lon < -101.8)) |> 
  dplyr::filter(!(sci_name_botw == "Turdus iliacus" & lon < -30)) |> 
  dplyr::filter(!(sci_name_botw ==  "Zapornia pusilla"))

# manually reviewed species that don't need any custom spatial filtering to remove out-of-range detections
good <- combos |> 
  dplyr::filter(sci_name_botw %in% manual_good)

# assemble the final species x site combinations
# first get lat/lon associated with the updated combos, then stack the manually reviewed tables
final_combos <- combos |> 
  dplyr::select(site, lat, lon) |> 
  dplyr::distinct() |> 
  dplyr::right_join(
    updated_combos) |> 
  dplyr::full_join(good) |> 
  dplyr::full_join(bad) |> 
  dplyr::select(-in_range)

# final product: valid species x site combinations
setwd(here::here("data"))
write_csv(final_combos, "species_site_combinations_final.csv")