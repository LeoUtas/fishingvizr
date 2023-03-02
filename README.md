# fishingvizr
This R package can be used to request spatiotemporal fishing effort information from the Global Fishing Watch API (https://globalfishingwatch.org/our-apis/) and generate map plots for data visualization.

## Introduction
#### Global Fishing Watch (https://globalfishingwatch.org/about-us/) is an online platform that uses satellite technology to monitor and track fishing activities across the world’s oceans in real-time. It was founded in 2015 as a partnership between Google, Oceana, and SkyTruth, with the aim of increasing transparency and accountability in the fishing industry. One of the key features of Global Fishing Watch is its Application Programming Interface (API), providing a wealth of information, including vessel positions and movements and fishing activity. This R package was developed to ease the process of requesting fishing effort data from the Global Fishing Watch API (https://globalfishingwatch.org/our-apis/) and generating map plots for data visualization. The requested data are formatted as a data frame, and map plots can be printed to jpg files for users’ applications. 

#### Spatial fishing effort information is conventionally acquired through vessel monitoring systems (VMSs), making it typically a scarce resource in fisheries science due to confidentiality. The potential of utilizing the spatial fishing effort data and, more generally, fishing vessel tracking information is huge. For example, such information can be used to develop spatial stock assessment models for fish resources, analyze the impacts of fishing nearby marine protected areas (MPAs), assess fishing patterns, events, and so many more applications. 

#### Example 1: A fishing effort data frame with the effort is measured in fishing hours. 
### ![text](https://github.com/LeoUtas/fishingvizr/blob/main/example/dataframe.jpg?raw=true)
#### *The actual vessel names are modified

#### Example 2: 
### ![text](https://github.com/LeoUtas/fishingvizr/blob/main/example/viz/effort_map_1.jpg?raw=true)
#### Visualization of the fishing effort (fishing hours) requested for all gear types operating in 2022 within the 3LNO divisions under the Northwest Atlantic Fisheries Organization (NAFO) management areas.

#### Example 3: 
### ![text](https://github.com/LeoUtas/fishingvizr/blob/main/example/viz/effort_map_2.jpg?raw=true)
#### Visualization of the fishing effort (fishing hours) requested for trawlers operating in 2022 within the 3LNO divisions under the Northwest Atlantic Fisheries Organization (NAFO) management areas.

## User guide

#### It is more comfortable to use the package, if users have some experience with R programming language (https://www.r-project.org/) and general knowledge of fisheries management related to FAO fishing area codes (see https://www.fao.org/fishery/en/area/search).  

#### Package installation: devtools::install_github("LeoUtas/fishingvizr")

#### The first step is visiting (https://globalfishingwatch.org/our-apis/) to register and request an API access token. It is required to have access to GFW API. The next step is determining the fishing area/s. If the determined area/s is within the Northwest Atlantic (FAO Major Fishing Area 21), please see option 1; otherwise, please refer to option 2.

### Option 1:
#### Due to the availability of geographic data for fishing areas within the Northwest Atlantic, it only requires two functions (i.e., get_rasters() and make_bubmap()) for generating fishing effort data and map visualization.

#### get_rasters() takes the following arguments:

| Argument | Required | Format | Choices | Recommended |
| :---: | :---: | :---: | :---: | --- |
| spatial_resolution | True | String | "low" for 0.1 degree / "high" for 0.01 degree | "high" |
| temporal_resolution | True | String | "daily" / "monthly" / "yearly" | "daily" |
| group_by | False | String | "vessel_id" | "vessel_id" |
| begin | True | integer | 2012:2023 | 2012:2022 |
| end | True | integer | 2012:2023 | 2012:2022 |
| selected_region | True | String | Any of the available region codes | None |
| gear_type | True | String | Any of the available gear type codes / default is to select all available gear types | None |
| region_source | False | String | "user_json" | "user_json" |
| key | True | String | visit (https://globalfishingwatch.org/our-apis) | None |

#### Available region codes include "0A", "0B", "1A", "1B", "1C", "1D", "1E", "1F", "2G", "2H", "2J", "3K", "3L", "3M", "3N", "3O", "3Pn", "3Ps", "4R", "4S", "4T", "4Vn", "4Vs", "4W", "4X", "5Y", "5Ze", "5Zw", "6A", "6B", "6C", "6D", "6E", "6F", "6G", "6H."

#### Available gear type codes include "trawlers", "drifting_longlines", "fixed_gear", "set_longlines", "pots_and_traps" "set_gillnets", "purse_seines", "other_purse_seines", "tuna_purse_seines","pole_and_line."

#### * It is recommended that the user runs the function make_annual_ls(), taking the output of the function get_rasters() before running the function make_bubmap().

#### make_bubmap() takes the following arguments:

| Argument | Required | Format | Choices | Recommended |
| :---: | :---: | :---: | :---: | --- |
| data | True | an R object generated by running the function make_annual_ls() |  | None |
| selected_map_region | True | String | any of the available region codes | None |
| begin | True | integer | 2012:2023 | 2012:2022 |
| end | True | integer | 2012:2023 | 2012:2022 |
| alpha | False | integer |  | 0.3 by default |
| over_zero_color | False | String |  | "#002E94" by default |
| over_zero_fill | False | String |  | "#002E94" by default |
| zero_fill | False | String |  | "#1A1A40" by default |
| legend_title | True | String |  | None |
| plot_tilte | False | String |  | "Fishing effort map" by default |

#### There is an option to print map plots to jpg files using the function print_effort_map(), taking the output of the function make_bubmap() as its input to create a series of jpg files in a viz folder under the working directory.

### Example of recommended practice:
### ![text](https://github.com/LeoUtas/fishingvizr/blob/main/example/practice.jpg?raw=true)

### Option 2:
### will be written up on Sunday



## Bug report & collaboration 
#### For reporting bugs, please email hnguyenthe@mun.ca.
#### For collaborating, the author is seeking collaborations to improve the data visualization of the package, please email hnguyenthe@mun.ca.

### Disclaimer
#### This package was created to make retrieving and visualizing fishing effort data from the GFW API easier. The complex technologies applied to produce the data belong to Global Fishing Watch.
