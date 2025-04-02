library(here)
library(tidyverse)

setwd(here::here("data/noise/Africa"))
n1 <- readr::read_csv("noise_1.csv")
setwd(here::here("data/noise/Asia"))
n2 <- readr::read_csv("noise_2.csv")
setwd(here::here("data/noise/Europe"))
n3 <- readr::read_csv("noise_3.csv")
setwd(here::here("data/noise/North America"))
n4 <- readr::read_csv("noise_4.csv")
setwd(here::here("data/noise/Oceania"))
n5 <- readr::read_csv("noise_5.csv")
setwd(here::here("data/noise/South America"))
n6 <- readr::read_csv("noise_6.csv")

rd1 <- n1 |> 
  dplyr::group_by(lat, lon) |> 
  dplyr::summarise(rd = unique(road_distance))

rd2 <- n2 |> 
  dplyr::group_by(lat, lon) |> 
  dplyr::summarise(rd = unique(road_distance))

rd3 <- n3 |> 
  dplyr::group_by(lat, lon) |> 
  dplyr::summarise(rd = unique(road_distance))

rd4 <- n4 |> 
  dplyr::group_by(lat, lon) |> 
  dplyr::summarise(rd = unique(road_distance))

rd5 <- n5 |> 
  dplyr::group_by(lat, lon) |> 
  dplyr::summarise(rd = unique(road_distance))

rd6 <- n6 |> 
  dplyr::group_by(lat, lon) |> 
  dplyr::summarise(rd = unique(road_distance))

rd <- full_join(rd1, rd2) |> 
  dplyr::full_join(rd3) |> 
  dplyr::full_join(rd4) |> 
  dplyr::full_join(rd5) |> 
  dplyr::full_join(rd6) |> 
  dplyr::filter(! (lat == 0 & lon == 0)) |> 
  dplyr::group_by(lat, lon) |> 
  dplyr::summarise( rd = mean(rd))

setwd(here::here("data"))
readr::write_csv(rd, "rd_dist.csv")