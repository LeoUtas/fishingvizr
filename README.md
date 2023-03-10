# fishingvizr
This R package can be used to request spatiotemporal fishing effort information from the Global Fishing Watch API (https://globalfishingwatch.org/our-apis/) and generate map plots for data visualization.

## Introduction
#### Global Fishing Watch (https://globalfishingwatch.org/about-us/) is an online platform that uses satellite technology to monitor and track fishing activities across the world’s oceans in real-time. It was founded in 2015 as a partnership between Google, Oceana, and SkyTruth, with the aim of increasing transparency and accountability in the fishing industry. One of the key features of Global Fishing Watch is its Application Programming Interface (API), providing a wealth of information, including vessel positions and movements and fishing activity. This R package was developed to ease the process of requesting fishing effort data from the Global Fishing Watch API (https://globalfishingwatch.org/our-apis/) and generating map plots for data visualization. The requested data are formatted as a data frame, and map plots can be printed to jpg files for users’ applications. 

#### Spatial fishing effort information is conventionally acquired through vessel monitoring systems (VMSs), making it typically a scarce resource in fisheries science due to confidentiality. The potential of utilizing the spatial fishing effort data and, more generally, fishing vessel tracking information is huge. For example, such information can be used to develop spatial stock assessment models for fish resources, analyze the impacts of fishing nearby marine protected areas (MPAs), assess fishing patterns, events, and so many more applications. 

#### Example 1: A fishing effort data frame with the effort is measured in fishing hours. 
### ![text](https://github.com/LeoUtas/fishingvizr/blob/main/example/dataframe.jpg?raw=true)
#### *The actual vessel names were modified

#### Example 2: 
### ![text](https://github.com/LeoUtas/fishingvizr/blob/main/example/viz/effort_map_1.jpg?raw=true)
#### Visualization of the fishing effort (fishing hours) by all gear types operating in 2022 within the NAFO 3LNO divisions (i.e., the FAO 21.3.L, FAO 21.3.N, and FAO 21.3.O).

#### Example 3: 
### ![text](https://github.com/LeoUtas/fishingvizr/blob/main/example/viz/effort_map_2.jpg?raw=true)
#### Visualization of the fishing effort (fishing hours) by trawlers operating in 2022 within the NAFO 3LNO divisions (i.e., the FAO 21.3.L, FAO 21.3.N, and FAO 21.3.O).

#### Example 4: 
### ![text](https://github.com/LeoUtas/fishingvizr/blob/main/example/viz/effort_map_3.jpg?raw=true)
#### Visualization of the fishing effort (fishing hours) by trawlers operating in 2022 within the Southern Australia (i.e., the FAO 57.6).

## User guide

#### It is more comfortable to use the package, if users have some experience with R programming language (https://www.r-project.org/) and general knowledge of fisheries management related to FAO fishing area codes (see https://www.fao.org/fishery/en/area/search).  

#### Package installation: devtools::install_github("LeoUtas/fishingvizr")
#### Required package installation: devtools::install_github("GlobalFishingWatch/gfwr") (possibly required)

#### The first step is visiting (https://globalfishingwatch.org/our-apis/) to register and request an API access token. It is required to have access to GFW API. Secondly, it is determining the fishing area/s. The next step is running the following functions.

#### get_rasters() requests fishing effort data from the GFW API and turns the data into a data frame. It takes the following arguments:

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

#### Available region codes include (see https://www.fao.org/fishery/en/area/search, and https://fish-commercial-names.ec.europa.eu/fish-names/fishing-areas_en)
### ![text](https://github.com/LeoUtas/fishingvizr/blob/main/example/available_regions.jpg?raw=true)

#### Available gear type codes include "trawlers", "drifting_longlines", "fixed_gear", "set_longlines", "pots_and_traps" "set_gillnets", "purse_seines", "other_purse_seines", "tuna_purse_seines","pole_and_line."

#### * It is recommended that the user runs the function make_annual_ls(), taking the output of the function get_rasters() before running the function make_bubmap().

#### make_bubmap() takes the output of the function make_annual_ls() to generate a list of map plots, has the following arguments:

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
| zeronote | False | String |  | "" to avoid redundant legend |


#### There is an option to print map plots to jpg files using the function print_effort_map(), taking the output of the function make_bubmap() as its input to create a series of jpg files in a viz folder under the working directory.

### Example of recommended practice:
### ![text](https://github.com/LeoUtas/fishingvizr/blob/main/example/practice.jpg?raw=true)

## Bug report & collaboration 
#### For reporting bugs, please email hnguyenthe@mun.ca.
#### For collaborating, the author is seeking collaborations to improve the data visualization of the package, please email hnguyenthe@mun.ca.

### Disclaimer
#### This package was created to make retrieving and visualizing fishing effort data from the GFW API easier. The complex technologies applied to produce the data belong to Global Fishing Watch. The accuracy and reliability of the data are highly dependent on GFW's technologies.




#### “Powered by Global Fishing Watch.”(https://globalfishingwatch.org)
