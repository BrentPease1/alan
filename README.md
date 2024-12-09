# Light pollution prolongs avian activity globally

### [Brent S. Pease](https://peaselab.com/)`*`, [Neil A. Gilbert](https://www.gilbertecology.com/)`*`

`*` _Shared first authorship_
### Data/code DOI:

#### Please contact the authors for questions about the code or data: Brent Pease (bpease1@siu.edu), Neil Gilbert (neil.gilbert@okstate.edu)

__________________________________________________________________________________________________________________________________________

## Abstract

## Repository Directory

### Scripts
 * [101_data-prep_calculate_vocal_activity.R](./Scripts/101_data-prep_calculate_vocal_activity.R). Script to calculate morning onset and evening cessation from raw BirdWeather data downloads. NOTE: raw BirdWeather data files were too large to include in this directory, but are available for download at: [BirdWeather Data Explorer](https://app.birdweather.com/data). 
 * [102_data-prep_extract_viirs.R](./Scripts/102_data-prep_extract_viirs.R). After running [101_data-prep_calculate_vocal_activity_v02.R](./Scripts/101_data-prep_calculate_vocal_activity_v02.R), this script will annotate BirdWeather stations with VIIRs data. We also assigned grid cells to BirdWeather stations in this script for use in [201_analysis_fit_all_models_tmb.R](./Scripts/201_analysis_fit_all_models_tmb.R). Monthly cloud-free VIIRS Day Night Band data publicly available for download from the [Earth Obsevation Group](https://eogdata.mines.edu/products/vnl/). VIIRS data is not included in repository due to size. We downloaded every tile available from March 2023 - March 2024 from the Earth Observation Group. Example path to files: [Home > nighttime_light > monthly > v10 > 2023 > 202301 > vcmslcfg](https://eogdata.mines.edu/nighttime_light/monthly/v10/2023/202301/vcmslcfg/)
 * [103_data-prep_resolve_names.R](./Scripts/103_data-prep_resolve_names.R). Script reads in Elton traits database and joins with VIIRS annotated BirdWeather vocal acitivity. Deals with differences in taxonomy. Also preps for [range map filtering](./Scripts/104_data-prep_range_map_filter.R). Output from this script is stored at [birdweather_elton_botw_name_key.csv](./data/ELEData/birdweather_elton_botw_name_key.csv).
* [104_data-prep_range_map_filter.R](./Scripts/104_data-prep_range_map_filter.R). Script reads in range maps for all focal species. Range maps come from [BirdLife International](https://datazone.birdlife.org/species/requestdis). Spatial data files are too large to upload in this repository but can be requested for download for the BirdLife link. The script buffers range maps by 100km and removes detections of a species if outside of the buffered range. Range maps for a small number of species had invalid geometries, which prevented the automated spatial range map filtering, and so these species we plotted the range maps and species detections, saved as PDF in [error_species_maps](./data/error_species_maps), and manually removed the maps to identify out-of-range records to remove.
* [105_data-prep_final_formatting.R](./Scripts/105_data-prep_final_formatting.R). Final script for data prep prior to analysis. Resolves outstanding species name issues and omits potentially errorenous BirdNET detections from the BirdWeather output based on the previous range filtering step. Output from this script is stored in [vocalization_activity](./data/vocalization_activity) and is read into the [analysis script](./Scripts/201_analysis_fit_all_models_tmb.R). 
* [201_analysis_fit_all_models_tmb.R](./Scripts/201_analysis_fit_all_models_tmb.R). Script for running all analyses presented in the manuscript. Both vocalization onset and evening cessation analyzed within.
* [301_figure_01b_inset.R](./Scripts/301_figure_01b_inset.R). Script for creating Fig. 1b
* [302_figure_01d.R](./Scripts/302_figure_01d.R). Script for creating Fig. 1d
* [303_figure_02b-c.R](./Scripts/303_figure_02b-c.R). Script for creating Fig. 2b-c
* [304_figure_03a.R](./Scripts/304_figure_03a.R). Script for creating Fig 3a
* [305_figure_03b.R](./Scripts/305_figure_03b.R). Script for creating Fig. 3b
* [306_figure_04a.R](./Scripts/306_figure_04a.R). Script for creating Fig. 4a
* [307_figure_04b-e.R](./Scripts/307_figure_04b-e.R). Script for creating Fig. 4b-e

### Data
* [error_species_maps](./data/error_species_maps) Folder containing output (PDFs with species range maps with BirdWeather detections overlaid) from [104_data-prep_range_map_filter.R](./Scripts/104_data-prep_range_map_filter.R) for manual review.
* [species_keys](./data/species_keys) Folder containing several tables with name keys to join datasets
   * [birdweather_elton_botw_name_key.csv](./data/species_keys/birdweather_elton_botw_name_key.csv) Table containing key to connect birdweather, elton traits, and BirdLife international names
     | Variable name | Meaning |
     |---------------|---------|
     | com_name | Common name |
     | sci_name_bw | Scientific name according to BirdWeather |
     | sci_name_elton | Scientific name according to EltonTraits database |
     | sci_name_botw | Scientific name according to BirdLife International |
     | nocturnal | Indicator variable for whether species is nocturnal (1) or not (0) |
     | cavity | Indicator variable for whether species nests in cavities (1) or not (0) |
  * [botw_scientific_names.csv](./data/species_keys/botw_scientific_names.csv)
    | Variable name | Meaning |
    |---------------|---------|
    | scientific | Scientific name according to BirdLife International |
   * [error_species.csv](./data/species_keys/error_species.csv) For manual review of species with range maps with invalid geometries
     | Variable | Meaning |
     |----------|---------|
     | sci_name_botw | Scientific name according to BirdLife |
     | good | Are the detection locations all good ("good") or are there out-of-range detections that need to be omitted ("no") |
     | notes | Instructions for manually reviewing/filtering bad records |
    * [review_sp_botw.csv](./data/species_keys/review_sp_botw.csv) Worksheet used to manually resolve some scientific names for BirdLife
      | Variable | Meaning |
      |----------|--------|
      | com_name | Common name |
      | sci_name_bw | BirdWeather scientific name |
      | sci_name_elton | EltonTraits scientific name |
      | flag | variable used to identify taxa to manually resolve |
      | sci_name_botw | BirdLife scientific name (manually resolved |
      | nocturnal | Binary variable indicating whether species is nocturnal (1) or not (0) |
    * [review_species.csv](.data/species_keys/review_species.csv)
      | Variable | Meaning |
      |----------|---------|
      | com_name | Common name |
      | sci_name | BirdWeather scientific name |
      | sci_name_manual | Scientific name (for matching to EltonTraits), had to resolve these manually |
    * [taxize_scientific_synonyms.csv](./data/taxize_scientific_synonyms.csv)
      | Variable | Meaning |
      | ---------| --------|
      | sci_name | BirdWeather scientific name |
      | sci_name_syn | Synonym found using taxize R package |
      
* [traits](./data/traits) Folder containing the trait tables
  * [avonet.csv](./data/traits/avonet.csv) This table is from the AVONET databse; see [Tobias et al. 2022](https://onlinelibrary.wiley.com/doi/10.1111/ele.13898) for details. The relevant column that we use in the analysis is the `Range.Size` column, which is the area (in sq. km) of the species' geoegraphic range.
  * [cavity.csv](./data/traits/cavity.csv)
    | Variable | Meaning |
    |----------|---------|
    | com_name | Common name (BirdWeather) |
    | sci_name_bw | Scientific name (BirdWeather) |
    | cavity | Binary variable indicating whether species nests in cavities (1) or not (0) |
  * [elton.txt](./data/traits/elton.txt) This table comes from [EltonTraits 1.0](https://figshare.com/collections/EltonTraits_1_0_Species-level_foraging_attributes_of_the_world_s_birds_and_mammals/3306933/1). It contains many columns; the relevant ones used in our analyses are taxonomic family `BLFamilyLatin`, proportion ground foraging `ForStrat.ground`, and whether or not species is nocturnal `Nocturnal`.  
* [vocalization_activity](./data/vocalization_activity) Vocalization data. Contains output from [101_data-prep_calculate_vocal_activity.R](./Scripts/101_data-prep_calculate_vocal_activity.R), [104_data-prep_range_map_filter.R](./Scripts/104_data-prep_range_map_filter.R), and [105_data-prep_final_formatting.R](./Scripts/105_data-prep_final_formatting.R).
  * [cessation_data_conf_0.75_det_100_grid_10.RData](./data/vocalization_activity/cessation_data_conf_0.75_det_100_grid_10.RData). Final data formatted for modeling (evening cessation). The RData object contains one table, "final_cess", with the following columns.
    | Variable | Meaning |
    |----------|---------|
    | lat | Latitude (EPSG code: 4326) of sensor location |
    | lon | Longitude (EPSG code: 4326) of sensor location |
    | grid_ID_cell_5 | Spatial grid cell ID (5°) for the sensor location |
    | grid_ID_cell_10 | Spatial grid cell ID (10°) for the sensor location (we did not use this in analyses) |
    | grid_ID_cell_15 | Spatial grid cell ID (15°) for the sensor location (we did not use this in analyses) |
    | week | Week of the year |
    | sci_name | BirdWeather scientific name |
    | value | Observed cessation of vocal activity in evening (minutes relative to local sunset); negative values represent final detection for the date occuring before sunset, while positive values represent final detection for the date occuring after sunset |
    | avg_rad | Measure of light pollution; radiance from VIIRS data product from sensor coordinates |
    | group | Grouping by species, spatial grid cell, and week (we re-created groupings for analysis the modeling script |      
  * [onset_data_conf_0.75_det_100_grid_10.RData](.data/vocalization_activity/onset_data_conf_0.75_det_100_grid_10.RData)
  * <ins>vocal_activity_annotated_conf_0_det10.csv</ins>. Observed vocalization timing response variables with least conservative filtering (confidence >=0 with >=10 detections for a species per station-date). **NOTE:** this file is too large to share on GitHub (~2GB, 12.9 million rows). It can be downloaded with [this GoogleDrive link](https://drive.google.com/file/d/1jFnviuHFIiTEZnk-knrscRLgfCQt7mam/view?usp=sharing). Download this file and put it in the [data/vocalization_activity](./data/vocalization_activity) folder to run data preparation scripts [103_data-prep_resolve_names.R](./Scripts/103_data-prep_resolve_names.R) and [104_data-prep_range_map_filter.R](./Scripts/104_data-prep_range_map_filter.R).
    | Variable | Meaning |
    |----------|---------|
    | lat_lon | Latitude-longitude grouping (unique location ID) |
    | value   | Observed time of vocalization (minutes) relative to local sunrise (morning response variables) or sunset (cessation response variable) |
    | category | Which response variable: morning onset ("first_onset"), evening cessation ("ev_ces"), or morning median vocalization time ("median_dawn"); this latter variable was not used or included in analyses |
    | com_name | Species common name according to BirdWeather |
    | sci_name | Species scientific name according to BirdWeather |
    | date_time | date-time stamp of detection |
    | date | date of detection |
    | week | Week of the year |
    | lat | Latitude of station (EPSG code: 4326) |
    | lon | Longitude of station (EPSG code: 4326) |
    | conf_filter | Detection confidence filter used to calculate response variables. In this table, all values will be equal to 0. |
    | det_filter | How many species detections have to occur per station-date to be included. In this table, all values will be equal to 10. |
    | avg_rad | Average radiance for coordinates for the month of the detection |
    | rad_cat | Radiance category for coordinates (low, medium, high) |
    | grid_ID_cell_5 | Spatial grid cell ID (5°) for the sensor location |
    | grid_ID_cell_10 | Spatial grid cell ID (10°) for the sensor location (we did not use this in analyses) |
    | grid_ID_cell_15 | Spatial grid cell ID (15°) for the sensor location (we did not use this in analyses) |
    | month | Month of the year |
    | day | Day of the year |
    | year| Year | 
    


### Results
* [BirdWeather](./Results/Birdweather) Contains raw output from [101_data-prep_calculate_vocal_activity.R](./Scripts/101_data-prep_calculate_vocal_activity.R)
* [Figures](./Results/Figures) Stores output from all figure scripts.
* [VIIRS](./Results/VIIRS) Stores output from [102_data-prep_extract_viirs.R](./Scripts/102_data-prep_extract_viirs.R)
* <ins>tmb_evening_models_family.RData</ins>. Model outputs for the cessation models. **NOTE:** this file is too large to share on GitHub. It can be downloaded with [this GoogleDrive link](https://drive.google.com/file/d/1wBKt16Fy2mLdSVHeb0S2pl376LBOiGBE/view?usp=sharing). Download the file and put it in the [Results](./Results) directory to reproduce figures.
* <ins>tmb_onset_models_family.RData</ins>. Mode outputs for onset models. **NOTE:** this file is too large to share on GitHub. It can be downloaded with [this GoogleDrive link](https://drive.google.com/file/d/1jx0pyFHKJa1PFT70AGrWoiP7h4j-lVop/view?usp=sharing). Download the file and put it in the [Results](./Results) directory to reproduce figures.
