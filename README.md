# Light pollution prolongs avian vocalization

### [Brent S. Pease](https://peaselab.com/)`*`, [Neil A. Gilbert](https://www.gilbertecology.com/)`*`

`*` _Shared first authorship_
### Data/code DOI:

#### Please contact the authors for questions about the code or data: Brent Pease (bpease1@siu.edu), Neil Gilbert (neil.gilbert@okstate.edu)

__________________________________________________________________________________________________________________________________________

## Abstract

## Repository Directory

### Scripts
 * [101_data-prep_calculate_vocal_activity_v02.R](./Scripts/101_data-prep_calculate_vocal_activity_v02.R). Script to calculate morning onset and evening cessation from raw BirdWeather data downloads. NOTE: raw BirdWeather data files were too large to include in this directory, but are available for download at: [BirdWeather Data Explorer](https://app.birdweather.com/data). 
  * [102_data-prep_extract_viirs.R](./Scripts/102_data-prep_extract_viirs.R). After running [101_data-prep_calculate_vocal_activity_v02.R](./Scripts/101_data-prep_calculate_vocal_activity_v02.R), this script will annotate BirdWeather stations with VIIRs data. We also assigned grid cells to BirdWeather stations in this script for use in [201_analysis_fit_all_models_tmb.R](./Scripts/201_analysis_fit_all_models_tmb.R)  Monthly cloud-free VIIRS Day Night Band data publicly available for download from the [Earth Obsevation Group](https://eogdata.mines.edu/products/vnl/). VIIRS data is not included in repository due to size. We downloaded every tile available from March 2023 - March 2024 from the Earth Observation Group. Example path to files: [Home > nighttime_light > monthly > v10 > 2023 > 202301 > vcmslcfg](https://eogdata.mines.edu/nighttime_light/monthly/v10/2023/202301/vcmslcfg/)
  * [103_data-prep_resolve_names.R](./Scripts/103_data-prep_resolve_names.R). Script reads in Elton traits database and joins with VIIRS annotated BirdWeather vocal acitivity. Deals with differences in taxonomy. Also preps for [range map filtering](./Scripts/104_data-prep_range_map_filter.R). Output from this script is stored at [birdweather_elton_botw_name_key.csv](./data/ELEData/birdweather_elton_botw_name_key.csv).
* [104_data-prep_range_map_filter.R](./Scripts/104_data-prep_range_map_filter.R). Script reads in range maps for all focal species. Buffers range maps by 100km and removes detections of a species if outside of the buffered range. Saves PDFs for manual inspection in [error_species_maps](./data/error_species_maps)
