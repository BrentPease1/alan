library(here)
library(tidyverse)
library(R.utils)

setwd(here::here("data/nocturnalV2/Africa"))

# This removes CSV files with 0 rows
lapply(Filter(function(x) R.utils::countLines(x)==0, list.files(pattern='.csv')), unlink)

africa <- readr::read_csv(list.files(here::here("data/nocturnalV2/Africa"), pattern = "vocsV2.csv", full.names = TRUE)) 

setwd(here::here("data/nocturnalV2/Asia"))
# This removes CSV files with 0 rows
lapply(Filter(function(x) R.utils::countLines(x)==0, list.files(pattern='.csv')), unlink)

asia <- readr::read_csv(list.files(here::here("data/nocturnalV2/Asia"), pattern = "vocsV2.csv", full.names = TRUE)) 

setwd(here::here("data/nocturnalV2/europe"))
# This removes CSV files with 0 rows
lapply(Filter(function(x) R.utils::countLines(x)==0, list.files(pattern='.csv')), unlink)

europe <- readr::read_csv(list.files(here::here("data/nocturnalV2/europe"), pattern = "vocsV2.csv", full.names = TRUE)) 

setwd(here::here("data/nocturnalV2/North America"))
# This removes CSV files with 0 rows
lapply(Filter(function(x) R.utils::countLines(x)==0, list.files(pattern='.csv')), unlink)

nam <- readr::read_csv(list.files(here::here("data/nocturnalV2/North America"), pattern = "vocsV2.csv", full.names = TRUE)) 
  
setwd(here::here("data/nocturnalV2/Oceania"))
# This removes CSV files with 0 rows
lapply(Filter(function(x) R.utils::countLines(x)==0, list.files(pattern='.csv')), unlink)

oceania <- readr::read_csv(list.files(here::here("data/nocturnalV2/Oceania"), pattern = "vocsV2.csv", full.names = TRUE)) 

setwd(here::here("data/nocturnalV2/South America"))
# This removes CSV files with 0 rows
lapply(Filter(function(x) R.utils::countLines(x)==0, list.files(pattern='.csv')), unlink)

sam <- readr::read_csv(list.files(here::here("data/nocturnalV2/South America"), pattern = "vocsV2.csv", full.names = TRUE))

all <- dplyr::full_join(africa, asia) |> 
  dplyr::full_join(europe) |> 
  dplyr::full_join(nam) |> 
  dplyr::full_join(oceania) |> 
  dplyr::full_join(sam)

setwd(here::here("data/vocalization_activity"))
write_csv(all, "nocturnal_species_tot_vocs.csv")

# info_for_alan <- all |> 
#   dplyr::select(lat, lon, date) |> 
#   dplyr::distinct()
# 
# write_csv(info_for_alan, "need_alan.csv")

setwd(here::here("data/nocturnalV3/Africa"))

# This removes CSV files with 0 rows
lapply(Filter(function(x) R.utils::countLines(x)==0, list.files(pattern='.csv')), unlink)

africa <- readr::read_csv(list.files(here::here("data/nocturnalV3/Africa"), pattern = ".csv", full.names = TRUE)) 

setwd(here::here("data/nocturnalV3/Asia"))
# This removes CSV files with 0 rows
lapply(Filter(function(x) R.utils::countLines(x)==0, list.files(pattern='.csv')), unlink)

asia <- readr::read_csv(list.files(here::here("data/nocturnalV3/Asia"), pattern = ".csv", full.names = TRUE)) 

setwd(here::here("data/nocturnalV3/europe"))
# This removes CSV files with 0 rows
lapply(Filter(function(x) R.utils::countLines(x)==0, list.files(pattern='.csv')), unlink)

europe <- readr::read_csv(list.files(here::here("data/nocturnalV3/europe"), pattern = ".csv", full.names = TRUE)) 

setwd(here::here("data/nocturnalV3/North America"))
# This removes CSV files with 0 rows
lapply(Filter(function(x) R.utils::countLines(x)==0, list.files(pattern='.csv')), unlink)

nam <- readr::read_csv(list.files(here::here("data/nocturnalV3/North America"), pattern = ".csv", full.names = TRUE)) 

setwd(here::here("data/nocturnalV3/Oceania"))
# This removes CSV files with 0 rows
lapply(Filter(function(x) R.utils::countLines(x)==0, list.files(pattern='.csv')), unlink)

oceania <- readr::read_csv(list.files(here::here("data/nocturnalV3/Oceania"), pattern = ".csv", full.names = TRUE)) 

setwd(here::here("data/nocturnalV3/South America"))
# This removes CSV files with 0 rows
lapply(Filter(function(x) R.utils::countLines(x)==0, list.files(pattern='.csv')), unlink)

sam <- readr::read_csv(list.files(here::here("data/nocturnalV3/South America"), pattern = ".csv", full.names = TRUE))

nadir <- dplyr::full_join(africa, asia) |> 
  dplyr::full_join(europe) |> 
  dplyr::full_join(nam) |> 
  dplyr::full_join(oceania) |> 
  dplyr::full_join(sam)

setwd(here::here("data/vocalization_activity"))
# saving as RData instead of CSV to stay within file size limits for GitHub
save(nadir, file = "nocturnal_nadir_v01.RData")