# fishingvizr
This R package can be used to request spatiotemporal fishing effort information from the Global Fishing Watch API (https://globalfishingwatch.org/our-apis/) and generate map plots for data visualization.

## Introduction
#### Global Fishing Watch (https://globalfishingwatch.org/about-us/) is an online platform that uses satellite technology to monitor and track fishing activities across the world’s oceans in real-time. It was founded in 2015 as a partnership between Google, Oceana, and SkyTruth, with the aim of increasing transparency and accountability in the fishing industry. One of the key features of Global Fishing Watch is its Application Programming Interface (API), providing a wealth of information, including vessel positions and movements and fishing activity. This R package was developed to ease the process of requesting fishing effort data from the Global Fishing Watch API (https://globalfishingwatch.org/our-apis/) and generating map plots for data visualization. The requested data are formatted as a data frame, and map plots can be printed to jpg files for users’ applications. 

#### For example:
#### Example 1: A fishing effort data frame with the effort is measured in fishing hours. 
### ![text](https://github.com/LeoUtas/fishingvizr/blob/main/example/dataframe.jpg?raw=true)
 
#### Example 2: 
### ![text](https://github.com/LeoUtas/fishingvizr/blob/main/example/viz/effort_map_1.jpg?raw=true)
#### Visualization of the fishing effort (fishing hours) requested for all gear types operating in 2022 within the 3LN divisions under the Northwest Atlantic Fisheries Organization (NAFO) management areas.

#### Example 3: 
### ![text](https://github.com/LeoUtas/fishingvizr/blob/main/example/viz/effort_map_2.jpg?raw=true)
#### Visualization of the fishing effort (fishing hours) requested for trawlers operating in 2022 within the 3LN divisions under the Northwest Atlantic Fisheries Organization (NAFO) management areas.

## User guide

#### The author assumes that users have some experience with R programming language and general knowledge of fisheries management related to FAO fishing area codes (see https://www.fao.org/fishery/en/area/search).  

#### Package installation
#### devtools::install_github("LeoUtas/fishingvizr")

#### The first step is determining the fishing area/s. If the determined area/s is within the Northwest Atlantic (FAO Major Fishing Area 21), please see option 1; otherwise, please refer to option 2 below.

#### Option 1:
#### Due to the availability of geographic data for fishing areas within the Northwest Atlantic, it only requires two functions (i.e., get_rasters() and make_bubmap()) for generating fishing effort data and map visualization.

#### get_rasters() takes the following arguments:

| Argument | Required | Format | Choices | Recommended |
| :---: | :---: | :---: | :---: | --- |
| spatial_resolution | True | String | "low" for 0.1 degree / "high" for 0.01 degree | "high" |
| temporal_resolution | True | String | "daily" / "monthly" / "yearly" | "daily" |
| group_by | False | String | "vessel_id" | "vessel_id" |
| begin | True | integer | 2012:2023 | 2012:2022 |
| end | True | integer | 2012:2023 | 2012:2022 |
| selected_region | True | String | any of the available region codes | None |
| gear_type | True | String | any of the available gear type codes / default is to select all available gear types | None |
| region_source | False | String | "user_json" | "user_json" |
| key | True | String | visit (https://globalfishingwatch.org/our-apis) | None |

#### Available region codes include 0A", "0B", "1A", "1B", "1C", "1D", "1E", "1F", "2G", "2H", "2J", "3K", "3L", "3M", "3N", "3O", "3Pn", "3Ps", "4R", "4S", "4T", "4Vn", "4Vs", "4W", "4X", "5Y", "5Ze", "5Zw", "6A", "6B", "6C", "6D", "6E", "6F", "6G", "6H."

#### Available gear type codes include "trawlers", "drifting_longlines", "fixed_gear", "set_longlines", "pots_and_traps" "set_gillnets", "purse_seines", "other_purse_seines", "tuna_purse_seines","pole_and_line."

#### make_bubmap() takes the following arguments:

| Argument | Required | Format | Choices | Recommended |
| :---: | :---: | :---: | :---: | --- |
| spatial_resolution | True | String | "low" for 0.1 degree / "high" for 0.01 degree | "high" |
| temporal_resolution | True | String | "daily" / "monthly" / "yearly" | "daily" |
| group_by | False | String | "vessel_id" | "vessel_id" |
| begin | True | integer | 2012:2023 | 2012:2022 |
| end | True | integer | 2012:2023 | 2012:2022 |
| selected_region | True | String | any of the available region codes | None |
| gear_type | True | String | any of the available gear type codes / default is to select all available gear types | None |
| region_source | False | String | "user_json" | "user_json" |
| key | True | String | visit (https://globalfishingwatch.org/our-apis) | None |


## Bug report & collaboration 


### disclaimer



### 
### to install the package
### devtools::install_github("LeoUtas/fishingvizr")
