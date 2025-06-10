# Light pollution prolongs avian activity globally

### [Brent S. Pease](https://peaselab.com/)†, [Neil A. Gilbert](https://www.gilbertecology.com/)†

† _Shared first authorship_
### Data/code DOI:

#### Please contact the authors for questions about the code or data: Brent Pease (bpease1@siu.edu), Neil Gilbert (neil.gilbert@okstate.edu)

__________________________________________________________________________________________________________________________________________

## Abstract
Light pollution disrupts natural light–dark cues that organisms use as timetables for life. While studies—typically focusing on individual species—have documented earlier morning onset of bird vocalization in light-polluted landscapes, a synthesis of light pollution effects across species, space, and seasons is lacking. We used a global acoustic dataset of more than 60 million vocal detections, representing 583 diurnal bird species, to synthesize effects of light pollution on avian vocalization timing. Averaged across species, space, and seasons, light pollution prolonged avian vocal activity by approximately 50 minutes. Light pollution responses were strongest for species with large eyes, open nests, migratory habits, and large ranges, and moreover were strongest during the breeding season. Prolonged activity may confer negative, neutral, or positive fitness effects; documenting these fitness effects and curbing light pollution are formidable challenges for 21st-century conservation.

 $~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~$ <img src="https://github.com/BrentPease1/alan/blob/main/Results/Figures/figure_02.png" width="600" />
 
## Repository Directory

### Scripts
 * [101_data-prep_calculate_vocal_activity.R](./Scripts/101_data-prep_calculate_vocal_activity.R). Script to calculate morning onset and evening cessation from raw BirdWeather data downloads. NOTE: raw BirdWeather data files were too large to include in this directory, but are available for download at: [BirdWeather Data Explorer](https://app.birdweather.com/data). 
 * [102_data-prep_extract_viirs.R](./Scripts/102_data-prep_extract_viirs.R). After running [101_data-prep_calculate_vocal_activity.R](./Scripts/101_data-prep_calculate_vocal_activity.R), this script will annotate BirdWeather stations with VIIRs data. We also assigned grid cells to BirdWeather stations in this script for use in [201_analysis_fit_all_models_tmb.R](./Scripts/201_analysis_fit_all_models_tmb.R). Monthly cloud-free VIIRS Day Night Band data publicly available for download from the [Earth Observation Group](https://eogdata.mines.edu/products/vnl/). VIIRS data is not included in repository due to size. We downloaded every tile available from March 2023 - March 2024 from the Earth Observation Group. Example path to files: [Home > nighttime_light > monthly > v10 > 2023 > 202301 > vcmslcfg](https://eogdata.mines.edu/nighttime_light/monthly/v10/2023/202301/vcmslcfg/)
 * [103_data-prep_resolve_names.R](./Scripts/103_data-prep_resolve_names.R). Script reads in Elton traits database and joins with VIIRS annotated BirdWeather vocal acitivity. Deals with differences in taxonomy. Also preps for [range map filtering](./Scripts/104_data-prep_range_map_filter.R). Output from this script is stored at [birdweather_elton_botw_name_key.csv](./data/species_keys/birdweather_elton_botw_name_key.csv).
* [104_data-prep_range_map_filter.R](./Scripts/104_data-prep_range_map_filter.R). Script reads in range maps for all focal species. Range maps come from [BirdLife International](https://datazone.birdlife.org/species/requestdis). Spatial data files are too large to upload in this repository but can be requested for download from [BirdLife International](https://datazone.birdlife.org/species/requestdis). The script buffers range maps by 100km and removes detections of a species if outside of the buffered range. Range maps for a small number of species had invalid geometries, which prevented the automated spatial range map filtering. For these species, we plotted the range maps and species detections, saved as PDF in [error_species_maps](./data/error_species_maps), and manually reviwed the maps to identify out-of-range records to remove.
* [105_data-prep_final_formatting.R](./Scripts/105_data-prep_final_formatting.R). Final script for data prep prior to analysis. Resolves outstanding species name issues and omits potentially errorenous BirdNET detections from the BirdWeather output based on the previous range filtering step. Output from this script is stored in [vocalization_activity](./data/vocalization_activity) and is read into the [analysis script](./Scripts/201_analysis_fit_all_models_tmb.R).
* [111_revisions_data-prep_extract_noisev2.R](./Scripts/111_revisions_data-prep_extract_noisev2.R). Script to calculate distance to nearest road as a proxy for noise pollution for supplemental analysis. 
* [112_revisions_calculate_spp_rich_tot_vocs.R](./Scripts/112_revisions_calculate_spp_rich_tot_vocs.R). Calculate species richness and total number of vocalizations for each BirdWeather station-date.
* [113_revisions_calculate_moon_phase.R](./Scripts/113_revisions_calculate_moon_phase.R). Retrieve moon phase data for the supplemental analysis on nocturnal species.
* [114_revisions_nocturnal_vocal_activityV3.R](./Scripts/114_revisions_nocturnal_vocal_activityV3.R). Calculate time of nocturnal detections relative to nadir for supplemental nocturnal analysis.  
* [115_revisions_nocturnal_vocal_activity_tot_vocs.R](./Scripts/115_revisions_nocturnal_vocal_activity_tot_vocs.R). Calculate number of vocalizations per species per BirdWeather station-date for supplemental nocturnal analysis. 
* [116_revisions_need_alan.R](./Scripts/116_revisions_need_alan.R). Script to calculate light pollution data for additional sites for the supplemental nocturnal analysis (the "number of detections" analysis did not include as stringent of filters as the main text analysis, so there were some sites that did not appear in the diurnal analysis). 
* [117_revisions_collate_road_distance_data.R](./Scripts/117_revisions_collate_road_distance_data.R). Road distance data were calculated by continent; this quick script joins the data up into one file.
* [118_revisions_collate_nocturnal_files.R](./Scripts/118_revisions_collate_nocturnal_files.R). Similarly, nocturnal files are by continent; this script joins them into one. 
* [119_revisions_collate_tot_vocs.R](./Scripts/119_revisions_collate_tot_vocs.R). Again, script to join up continent files.
* [201_analysis_fit_all_models_tmb.R](./Scripts/201_analysis_fit_all_models_tmb.R). Script for running all analyses presented in the manuscript. Both vocalization onset and evening cessation analyzed within.
* [202_road_analysis.R](./Scripts/202_road_analysis.R) Script to fit models with distance-to-road as a proxy for noise pollution (see supplement)
* [203_nocturnal_analysis.R](./Scripts/203_nocturnal_analysis.R) Script to fit models for nocturnal species (see supplement)
* [301_figure_01b_inset.R](./Scripts/301_figure_01b_inset.R). Script for creating Fig. 1b
* [302_figure_01d.R](./Scripts/302_figure_01d.R). Script for creating Fig. 1d
* [303_figure_02b-c.R](./Scripts/303_figure_02b-c.R). Script for creating Fig. 2b-c
* [304_figure_03a.R](./Scripts/304_figure_03a.R). Script for creating Fig 3a
* [305_figure_03b.R](./Scripts/305_figure_03b.R). Script for creating Fig. 3b
* [306_figure_04a.R](./Scripts/306_figure_04a.R). Script for creating Fig. 4a
* [307_figure_04b.R](./Scripts/307_figure_04b.R). Script for creating Fig. 4b
* [308_figure_04c.R](./Scripts/308_figure_04c.R). Script for creating Fig. 4c
* [309_figure_s01.R](./Scripts/309_figure_s01.R). Script for creating Fig. S1
* [310_model_summary_tables.R](./Scripts/310_model_summary_tables.R) Script for creating Excel tables with parameters estimated by models
* [311_plotting_sp_parameters.R](./Scripts/311_plotting_sp_parameters.R) Script demonstrating how to join species-level parameters with spatial grid

### Data
* [error_species_maps](./data/error_species_maps) Folder containing output (PDFs with species range maps with BirdWeather detections overlaid) from [104_data-prep_range_map_filter.R](./Scripts/104_data-prep_range_map_filter.R) for manual review.
* [range_maps](./data/range_maps) Folder used to hold range map files for focal species. We do not have share permission, so folder will not appear on repository. Range maps are available through BirdLife International, which can be accessed at the [BirdLife Data Zone](https://datazone.birdlife.org/species/requestdis).
  * focal_spp_maps.dbf
  * focal_spp_maps.prj
  * focal_spp_maps.shp
  * focal_spp_maps.shx
* [species_keys](./data/species_keys) Folder containing several tables with name keys to join datasets
   * [birdweather_elton_botw_name_key.csv](./data/species_keys/birdweather_elton_botw_name_key.csv) Table containing key to connect BirdWeather, EltonTraits, and BirdLife International names
     | Variable name | Meaning |
     |---------------|---------|
     | com_name | Common name |
     | sci_name_bw | Scientific name according to BirdWeather |
     | sci_name_elton | Scientific name according to EltonTraits database |
     | sci_name_botw | Scientific name according to BirdLife International |
     | nocturnal | Indicator variable for whether species is nocturnal (1) or not (0) |
     | cavity | Indicator variable for whether species nests in cavities (1) or not (0) |
  * [botw_scientific_names.csv](./data/species_keys/botw_scientific_names.csv) List of scientific names used in BirdLife International range maps.
    | Variable name | Meaning |
    |---------------|---------|
    | scientific | Scientific name according to BirdLife International |
   * [error_species.csv](./data/species_keys/error_species.csv) For manual review of species with range maps with invalid geometries
     | Variable | Meaning |
     |----------|---------|
     | sci_name_botw | Scientific name according to BirdLife |
     | good | Are the detection locations all good ("good") or are there out-of-range detections that need to be omitted ("no") |
     | notes | Instructions for manually reviewing/filtering bad records |
    * [review_sp_botw.csv](./data/species_keys/review_sp_botw.csv) Worksheet used to manually resolve some scientific names for BirdLife International range maps.
      | Variable | Meaning |
      |----------|--------|
      | com_name | Common name |
      | sci_name_bw | BirdWeather scientific name |
      | sci_name_elton | EltonTraits scientific name |
      | flag | variable used to identify taxa to manually resolve |
      | sci_name_botw | BirdLife scientific name (manually resolved |
      | nocturnal | Binary variable indicating whether species is nocturnal (1) or not (0) |
    * [review_species.csv](./data/species_keys/review_species.csv) Worksheet used to manually resolve some scientific names for EltonTraits
      | Variable | Meaning |
      |----------|---------|
      | com_name | Common name |
      | sci_name | BirdWeather scientific name |
      | sci_name_manual | Scientific name (for matching to EltonTraits), had to resolve these manually |
    * [taxize_scientific_synonyms.csv](./data/species_keys/taxize_scientific_synonyms.csv) Table with BirdWeather scientific names and synonyms found using the `taxize` R package.
      | Variable | Meaning |
      | ---------| --------|
      | sci_name | BirdWeather scientific name |
      | sci_name_syn | Synonym found using taxize R package |
      
* [traits](./data/traits) Folder containing the trait tables
  * [avonet.csv](./data/traits/avonet.csv) This table is from the [AVONET database](https://figshare.com/s/b990722d72a26b5bfead); see [Tobias et al. 2022](https://onlinelibrary.wiley.com/doi/10.1111/ele.13898) for details. The relevant columns that we use in the analysis are `Range.Size` (area [sq. km] of the species' geographic range), `Habitat.Density` (1 = dense habitats, 2 = semi-open habitats, 3 = open habitats), and `Migration` (1 = sedentary, 2 = partial migrant, 3 = migrant).  
  * [cavity.csv](./data/traits/cavity.csv)
    | Variable | Meaning |
    |----------|---------|
    | com_name | Common name (BirdWeather) |
    | sci_name_bw | Scientific name (BirdWeather) |
    | cavity | Binary variable indicating whether species nests in cavities (1) or not (0) |
  * [elton.txt](./data/traits/elton.txt) This table comes from [EltonTraits 1.0](https://figshare.com/collections/EltonTraits_1_0_Species-level_foraging_attributes_of_the_world_s_birds_and_mammals/3306933/1). It contains many columns; the relevant ones used in our analyses are taxonomic family `BLFamilyLatin` and whether or not species is nocturnal `Nocturnal`.
  * [ritland_clean.csv](./data/traits/ritland_clean.csv) Eye size data from Stanley Ritland's dissertation; see [this Dryad repository](https://datadryad.org/dataset/doi:10.5061/dryad.3xsj3txq7#citations) for full details. The relevant column for eye size is `cd1`, or minimum corneal diameter.   
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
  * [nocturnal_nadir_v01.RData](./data/vocalization_activity/nocturnal_nadir_v01.RData). Data for supplemental nocturnal species analysis for time relative to nadir. This RData object contains one table, "nadir", which has the following column meanings:
    | Variable | Meaning |
    |----------|---------|
    | sci_name | Scientific name according to BirdWeather |
    | com_name | Common name according to BirdWeather |
    | min_time_nadir | The minimum time (min) between a detection and nadir time for the day of, previous day, or next day of detection. This gives a measure of how close a detection is to the middle of the night.
    | date | Date |
    | week | Week of the year |
    | lat | Latitude of BirdWeather sensor |
    | lon | Longitude of BirdWeather sensor |
    | det | Detection filter (not relevant) |
  * [nocturnal_species_tot_vocs.csv](./data/vocalization_activity/nocturnal_species_tot_vocs.csv). Table with total number of detections per night for nocturnal species
    | Variable | Meaning |
    |----------|---------|
    | com_name | Common name according to BirdWeather |
    | station_date | ID for station-date combo |
    | sci_name | Scientific name according to BirdWeather |
    | lat | Latitude of BirdWeather sensor |
    | lon | Longitude of BirdWeather sensor |
    | N_dets | Number of detections for a species-site-date combination |
     
  * [onset_data_conf_0.75_det_100_grid_10.RData](./data/vocalization_activity/onset_data_conf_0.75_det_100_grid_10.RData). Final data formatted for modeling (morning onset). The RData object contains one table, "final", which has the same column meanings as the cessation data (see table above). The one difference is that the `value` variable is the time of the first vocalization relative to local sunrise, with negative values representing onset prior to sunrise and positive values representing onset after sunrise.
  * [species_site_combinations_final.csv](./data/vocalization_activity/species_site_combinations_final.csv). Table with valid (based on species range maps) species-site combinations; output of [104_data-prep_range_map_filter.R](./Scripts/104_data-prep_range_map_filter.R). The table has the following column meanings:
    | Variable | Meaning |
    |----------|---------|
    | site | Sensor ID |
    | lat | Latitude (EPSG code: 4326) of sensor location |
    | lon | Longitude (EPSG code: 4326) of sensor location |
    | com_name | BirdWeather common name |
    | sci_name_bw | BirdWeather scientific name |
    | sci_name_botw | BirdLife scientific name |
  * [tot_voc.RData](./data/vocalization_activity/tot_voc.RData) Data on bird diversity at BirdWeather sensor-date combinations. RData object containing one table, "tot.voc", with the following columns:
    | Variable | Meaning |
    |----------|---------|
    | lat | Latitude of BirdWeather sensor |
    | lon | Longitude of BirdWeather sensor |
    | tot | Total number of vocalizations across all species |
    | sr | Number of species detected at station-date combination |
    | shan | Shannon diversity of species detections at station-date |
    
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
   * <ins>vocal_activity_annotated_conf_0.75_det100.csv</ins>. Observed vocalization timing response variables with conservative filtering (confidence >=0.75 with >=100 detections for a species per station-date). **NOTE:** this file is too large to share on GitHub (7.5 million rows). It can be downloaded with [this GoogleDrive link](https://drive.google.com/file/d/1tiRojwiy-04pXxNqryKWm72y_LS0jKRq/view?usp=sharing). Download this file and put it in the [data/vocalization_activity](./data/vocalization_activity) folder to run data preparation script [105_data-prep_final_formatting.R](./Scripts/105_data-prep_final_formatting.R). This file has the same column meanings as the table above.
 
* [moon_data.csv](./data/moon_data.csv) Moonlight data for nocturnal analysis
  | Variable | Meaning |
  |----------|---------|
  | lat | Latitude of BirdWeather sensor |
  | lon | Longitufde of BirdWeather sensor |
  | date | Date |
  | ill | Illumination (brightness) |
  | phase | Percent of moon face illuminated |

* [noct_locs_id.csv](./data/noct_locs_id.csv) Coordinate/date information for extracting light values for updated nocturnal analysis
  | Variable | Meaning |
  |----------|---------|
  | id | ID for latitude, longitude, date combination |
  | lat | Latitude of BirdWeather sensor |
  | lon | Longitude of BirdWeather sensor |
  | date | Date |
  
* [nocturnal_species.csv](./data/nocturnal_species.csv) List of nocturnal species
  | Variable | Meaning |
  |----------|---------|
  | sci_name | Scientific name according to BirdWeather |
  | nocturnal | indicator column (1) flagging species as nocturnal (from EltonTraits) |
  
* [rd_dist.csv](./data/rd_dist.csv) Distance-to-road data (proxy for noise pollution)
  | Variable | Meaning |
  |----------|---------|
  | lat | Latitude of BirdWeather sensor |
  | lon | Longitufde of BirdWeather sensor |
  | rd  | Distance (m) between BirdWeather sensor and nearest road |
* [vocal_activity_annotated_revisions_noct_locs_id.csv](./data/vocal_activity_annotated_revisions_noct_locs_id.csv) Updated light data for nocturnal species analysis. We needed light data for more locations, since the total number of vocalizations analysis had more sites (b/c of the omission of the detection filter)
  | Variable | Meaning |
  |----------|---------|
  | id | ID for latitutde, longitude, date combination |
  | lat | Latitude of BirdWeather sensor |
  | lon | Longitude of BirdWeather sensor |
  | date | Date |
  | year | Year |
  | month | Month of the year |
  | avg_rad | Radiance (light pollution) extracted from VIIRS data product for coordinates and month |

### Results
* [BirdWeather](./Results/Birdweather) Contains raw output from [101_data-prep_calculate_vocal_activity.R](./Scripts/101_data-prep_calculate_vocal_activity.R)
  * [birdweather_row_counter.csv](./Results/Birdweather/birdweather_row_counter.csv) Information on number of detections used to create barplot in Fig. 1.    
* [Figures](./Results/Figures) Manuscript figures.
  * [figure_01.png](./Results/Figures/figure_01.png). Figure 1.
  * [figure_01.pptx](./Results/Figures/figure_01.pptx). Figure 1, PowerPoint file for assembling panels.
  * [figure_01b.png](./Results/Figures/figure_01b.png). Figure 1B; map created in QGIS.
  * [figure_01b_inset.png](./Results/Figures/figure_01b_inset.png). Histogram of light pollution values from sensor locations; inset for Fig. 1B.
  * [figure_01d.png](./Results/Figures/figure_01d.png). Figure 1D
  * [figure_02.png](./Results/Figures/figure_02.png) Figure 2
  * [figure_02.pptx](./Results/Figures/figure_02.pptx) Figure 2, PowerPoint file for assembling panels
  * [figure_02b.png](./Results/Figures/figure_02b.png) Figure 2B
  * [figure_02c.png](./Results/Figures/figure_02c.png) Figure 2C
  * [figure_03.png](./Results/Figures/figure_03.png) Figure 3
  * [figure_03.pptx](./Results/Figures/figure_03.pptx) Figure 3, PowerPoint file for assembling panels
  * [figure_03a.png](./Results/Figures/figure_03a.png) Figure 3A
  * [figure_03b.png](./Results/Figures/figure_03b.png) Figure 3B
  * [figure_04.png](./Results/Figures/figure_04.png) Figure 4
  * [figure_04.pptx](./Results/Figures/figure_04.pptx) Figure 4, PowerPoint file for assembling panels
  * [figure_04a.png](./Results/Figures/figure_04a.png) Figure 4A
  * [figure_04b.png](./Results/Figures/figure_04b.png) Figure 4B
  * [figure_04c.png](./Results/Figures/figure_04c.png) Figure 4C
  * [figure_s01.png](./Results/Figures/figure_s01.png) Figure S1
  * [figure_s02.png](./Results/Figures/figure_s02.png) Figure S2
  * [figure_s02.pptx](./Results/Figures/figure_s02.pptx) Figure S2, PowerPoint file for annotation
  * [figure_s03.png](./Results/Figures/figure_s03.png) Figure S3
* [Tables](./Results/Tables) Contains several supplemental tables summarizing model output
  * [family_level_parameters.csv](./Results/Tables/family_level_parameters.csv) Family-level parameters (intercepts and slopes) from base onset and cessation models
    | Variable | Meaning |
    |----------|---------|
    | family | Taxonomic family |
    | onset_intercept | Estimated average onset; 0 = sunrise, negative = before sunrise, positive = after sunrise |
    | onset_slope | Estimated effect of light pollution; negative = earlier onset with light pollution, positive = later onset with light pollution |
    | cessation_intercept | Estimated average cessation; 0 = sunset, negative = before sunset, positive = after sunset |
    | cessation_slope | Estimated effect of light pollution; negative = earlier cessation with light pollution, positve = later cessation with light pollution |
  * [formatted_model_summaries.csv](./Results/Tables/formatted_model_summaries.csv) Summary of higher-level model parameters
    | Variable | Meaning |
    |----------|---------|
    | response | Response variable (onset or cessation) |
    | predictors | Predictor variables in model |
    | term | Term within the model. These include fixed effects, standard deviation for random terms (SD), correlations between random terms (Cor), and several model fit statitics( R2 Cond, AIC, RMSE). In this column, *light* is light pollution, *cavity* is nest type (cavity: 1; open: 0), *eye size* is corneal diameter, *semi-open habitat* is the 2nd-level of the habitat density variable, *open habitat* is the 3rd-level of the habitat density variable (intercept for this model representes closed habitats), *latitude* is latitude, *partial migrant* is the 2nd-level of the migration variable, *migrant* is the 3rd-level of the migration variable (intercept for this model represents sedentary species), *species richness* is the number of species present at a BirdWeather station, and *range_size* is geographic range size |
    | estimate | Value of the point estimate |
    | std_error | Standard error of the estimate |
    | gof | Value of the goodness of fit stats |
  * [model_summaries.csv](./Results/Tables/model_summaries.csv) Same information as above, but in a less intuitive and friendly format.
  * [species_level_parameters.csv](./Results/Tables/species_level_parameters.csv) Species-grid cell-week level parameters. Refer to [311_plotting_sp_parameters.R](./Scripts/311_plotting_sp_parameters.R) for information on joining these species-level parameters to the spatial grid cells to which estimates correspond for mapping, etc.
    | Variable | Meaning |
    |----------|---------|
    | family | Taxonomic family |
    | sci_name | Scientific name according to BirdWeather |
    | grid_ID_cell_5 | Identity of 5-degree grid cell |
    | week | Week of the year |
    | onset_intercept | Estimated average onset; 0 = sunrise, negative = before sunrise, positive = after sunrise |
    | onset_slope | Estimated effect of light pollution; negative = earlier onset with light pollution, positive = later onset with light pollution |
    | cessation_intercept | Estimated average cessation; 0 = sunset, negative = before sunset, positive = after sunset |
    | cessation_slope | Estimated effect of light pollution; negative = earlier cessation with light pollution, positve = later cessation with light pollution |
* <ins>onset.RData</ins>. Model outputs for the onset models. **NOTE:** this file is too large to share on GitHub. It can be downloaded with [this GoogleDrive link](https://drive.google.com/file/d/1QeURSNT8vzthCnwmjD7CmsmncTqkRV9I/view?usp=sharing). Download the file and put it in the [Results](./Results) directory to reproduce figures.
* <ins>cessation.RData</ins>. Model outputs for cessation models. **NOTE:** this file is too large to share on GitHub. It can be downloaded with [this GoogleDrive link](https://drive.google.com/file/d/1F0d6uzJh22tMReN1dvmaL9yPACL_b7X-/view?usp=sharing). Download the file and put it in the [Results](./Results) directory to reproduce figures.
* <ins>distance_to_road_models.RData</ins>. Model outputs for distance-to-road models. **NOTE:** this file is too large to share on GitHub. It can be downloaded with [this GoogleDrive link](https://drive.google.com/file/d/1fF2plci3XmopgN0FFo_LTrMRbz88NR5h/view?usp=sharing). Download the file and put it in the [Results](./Results) directory to reproduce figures.
