

# ---------- LOAD REQUIRED LIBRARIES ----------- #
#' @importFrom sf read_sf st_coordinates
#' @importFrom dplyr group_by reframe
#' @importFrom gfwr get_raster
#' @importFrom jsonlite toJSON
#' @importFrom magrittr %>%


# ---------- AVAILABLE REGIONS ----------- #
shp_source = read_sf("inst/extdata/shp/NAFO_divisions.shp")
available_regions = sort(unique(shp_source$ZONE))


# ---------- DATA PREPARATION FUNCTION 1 ----------- #
#' Creates selected fishing regions
#'
#' @param region
#'
#' @return Returns the geometry information of the selected fishing regions
#' @export
#'
#' @examples
#' make_selected_region(c("3L","3N"))
#' # Returns:
#' Simple feature collection with 2 features and 5 fields
#' Geometry type: POLYGON
#' Dimension:     XY
#' Bounding box:  xmin: -54.5 ymin: 39 xmax: -46.5 ymax: 49.25
#' Geodetic CRS:  NAD83
#' A tibble: 2 × 6
#' AREA PERIMETER NAFO_ NAFO_ID ZONE                                              geometry
#' <dbl>     <dbl> <int>   <int> <chr>                                        <POLYGON [°]>
#' 1  23.0      48.9   529       0 3L    ((-53.73892 48.99732, -53.74253 49.00458, -53.74747…
#' 2  31.0      22.4   594       0 3N    ((-50.76667 46, -50.75 46, -50.73333 46, -50.71667 …
#'
make_selected_region = function(region) {

  region = region

  selected_region = subset(shp_source, ZONE %in% region)
  return(selected_region)

}


# ---------- DATA PREPARATION FUNCTION 2 ----------- #
#' Extracts the coordinates information and turn them into json data
#' to use in the function get_raster()
#'
#' @param selected_region
#'
#' @return
#' Returns a json file containing coordinates information of the
#' selected fishing regions that is required in the function get_rasters()
#' @export
#'
#' @examples
#' make_region_json(c("3L","3N"))
#' # Returns:
#' "{\"geojson\":{\"type\":\"Polygon\",\"coordinates\":[ [[-53.7389,48.9973],
#' [-53.7425,49.0046],[-53.7475,49.0045],[-53.7484,49.0037],[-53.7559,49.0037],
#' [-53.7567,49.0029],[-53.7717,49.0029],[-53.7717,49.0037],[-53.7667,49.0046],
#' [-53.765,49.0071],[-53.7634,49.0062],[-53.76,49.0062],...]]}}"
#'
make_region_json = function(selected_region) {

  region = make_selected_region(selected_region)

  coords <- st_coordinates(region)
  coords = coords[,-c(3,4)]

  list_coords = list()
  for (i in 1:nrow(coords)) {
    list_coords[[i]] = c(coords[i,1], coords[i,2])
  }
  json <- toJSON(list_coords)
  region_json = sprintf('{"geojson":{"type":"Polygon","coordinates":[ %s ]}}',json)

  return(region_json)
}


# ---------- DATA PREPARATION FUNCTION 3 ----------- #
#' Creates begin and end year objects from the provided date information
#' that is required in the function get_rasters()
#'
#' @param begin the beginning year
#' @param end the end year
#'
#' @return
#' Returns list of strings containing begin and end years
#' @export
#'
#' @examples
#'make_year_range(2012,2018)
#'
#'
make_year_range = function(begin,end) {

  begin_i = begin %% 100
  end_i = end %% 100

  date_str = list()
  for (i in begin_i:end_i) {
    date_str[i] = paste("20", i, "-01-01,20", i, "-12-31", sep = "")
  }

  output = list(date_str, begin_i, end_i)
  return(output)
}


# ---------- DATA PREPARATION FUNCTION 4 ----------- #
#' This is a main function to request fishing effort data from the Global Fishing Watch API
#' and turn the data into a dataframe containing the information of
#' vessel name, year, month, day, fishing effort, latitude, longitude, gear type and vessel flag
#'
#' @param spatial_resolution is required to select either low, equivalent to 0.1 degree,
#'  or high, equivalent to 0.01 degree
#' @param temporal_resolution is required to select either daily or monthly or yearly
#' it is recommended to use daily
#' @param group_by is designed to group the data by vessel ID in this version
#' @param begin is required to provide the beginning year
#' @param end is required to provide the end year
#' @param selected_region is required to provide selected fishing regions
#' @param gear_type by default, the function selects all gear types but it can be selected
#' by assigning the gear_type to any of the following strings,
#' "trawlers", "drifting_longlines", "fixed_gear", "set_longlines", "pots_and_traps"
#' "set_gillnets", "purse_seines", "other_purse_seines", "tuna_purse_seines","pole_and_line"
#' @param region_source is assigned to be "user_json" in this version
#' @param key is required to provide the API access token,
#' visit globalfishingwatch.org/our-apis for more details regarding registration
#'
#' @return
#' Returns a dataframe containing the information of
#' vessel name, year, month, day, fishing effort, latitude, longitude, gear type and vessel flag
#'
#' @export
#'
#' @examples
#' effort_df = get_rasters(spatial_resolution = "high",
#'                         temporal_resolution = "daily",
#'                         begin = 2012, end = 2016,
#'                         selected_region = c("3L","3N"),
#'                         key = "Your secret API access token")
#'# Example returns
#'head(effort_df)
#'  vessel_name year month day   effort   lat   long gear_type flag positionStr
#'     VESSEL1 2012    08  13 0.975833 48.61 -49.86  trawlers  CAN 48.61-49.86
#'     VESSEL1 2012    08  13 0.608056 48.63 -49.87  trawlers  CAN 48.63-49.87
#'     VESSEL1 2012    08  16 4.123889 48.61 -49.82  trawlers  CAN 48.61-49.82
#'     VESSEL1 2012    08  19 1.110833 48.60 -49.78  trawlers  CAN  48.6-49.78
#'     VESSEL1 2012    08  14 1.472222 48.60 -49.81  trawlers  CAN  48.6-49.81
#'     VESSEL1 2012    08  17 1.234167 48.59 -49.79  trawlers  CAN 48.59-49.79
#'
get_rasters = function(spatial_resolution, # low (0.1 degree) or high (0.01 degree)
                       temporal_resolution, # daily, monthly, or yearly
                       group_by = "vessel_id", # vessel_id, flag, gearType, or flagAndGearType
                       begin, end, # begin and end dates
                       selected_region, # def by user
                       gear_type = "all",
                       region_source = "user_json", # user_json for this version
                       key) {


  spatial_resolution = spatial_resolution
  if (! (spatial_resolution %in% c("low", "high"))) {
    stop("An inapplicable spatial_resolution provided, please refer to the documentation")
  }

  temporal_resolution = temporal_resolution
  if (! (temporal_resolution %in% c("daily", "monthly", "yearly"))) {
    stop("An inapplicable temporal_resolution provided, please refer to the documentation")
  }

  begin = begin
  end = end
  if (!(begin %in% 2012:2023) & !(end %in% 2012:2023) & begin > end) {
    stop("Inapplicable begin/end provided, please refer to the documentation")
  }

  date_range = make_year_range(begin, end)[[1]]
  begin_i = make_year_range(begin, end)[[2]]
  end_i = make_year_range(begin, end)[[3]]


  selected_region = selected_region

  if (any(!selected_regions %in% available_regions)){
    stop("selected_regions are out of available regions, please refer to the documentation")
  } else {
    region_json = make_region_json(selected_regions)
  }

  response = list()
  for (i in begin_i:end_i) {
    response[i] = list(data.frame(get_raster(spatial_resolution = spatial_resolution,
                                                   temporal_resolution = temporal_resolution,
                                                   group_by = group_by,
                                                   date_range = date_range[[i]],
                                                   region = region_json,
                                                   region_source = region_source,
                                                   key = key)
    )
    )
  }

  response_df = as.data.frame(do.call(rbind,response[-c(1:11)]))
  response_df$vessel_name = response_df$Vessel.Name
  response_df$year = format(as.Date(response_df$Time.Range), "%Y")
  response_df$month = format(as.Date(response_df$Time.Range), "%m")
  response_df$day = format(as.Date(response_df$Time.Range), "%d")
  response_df$effort = response_df$Apparent.Fishing.Hours
  response_df$lat = response_df$Lat
  response_df$long = response_df$Lon
  response_df$gear_type = response_df$Gear.Type
  response_df$flag = response_df$Flag

  response_df$positionStr = paste(as.character(response_df$Lat), as.character(response_df$Lon), sep = "")

  response_df = response_df[response_df$effort > 0,]

  retained_gear_type = tolower(gear_type)
  if (retained_gear_type != "all"){
    response_df = subset(response_df, gear_type %in% retained_gear_type)
  } else {
    response_df = response_df
  }

  response_df = response_df %>% select(15:ncol(response_df))


  return(response_df)
}


# ---------- DATA PREPARATION FUNCTION 5 ----------- #
#' This function takes the output of the function get_rasters() to create
#' a list of fishing effort data for different years that is helpful for
#' data visualization when using the function make_bubmap() in this package
#'
#' @param data Takes the output of the function get_rasters() as input
#'
#' @return Returns a list of fishing effort data for different years
#' @export
#'
#' @examples
#' effort_df = get_rasters(spatial_resolution = "high",
#'                         temporal_resolution = "daily",
#'                         begin = 2012, end = 2016,
#'                         selected_region = c("3L","3N"),
#'                         key = "Your secret API access token")
#'
#' effort_ls = make_annual_ls(effort_df)
#' # Returns:
#' effort_ls
#' [[1]]
#'     positionStr N    effort   lat   long year
#' 1   39.01-46.55 1  1.705278 39.01 -46.55 2012
#' 2   39.01-47.43 1  1.771389 39.01 -47.43 2012
#' 3   39.01-47.72 1  3.224722 39.01 -47.72 2012
#' 4   39.01-47.77 1  1.270556 39.01 -47.77 2012
#'
#' [[2]]
#'     positionStr N    effort   lat   long year
#' 1   39-47.29    1 10.806389 39.00 -47.29 2013
#' 2   39-47.39    1  1.774444 39.00 -47.39 2013
#' 3   39-47.46    1  1.609444 39.00 -47.46 2013
#' 4   39.01-46.55 1  1.656944 39.01 -46.55 2013
#' ...
#'#'
make_annual_ls = function(data) {

  ls_org = split(data, data$year)

  ls = list()
  for (i in 1:length(ls_org)) {

    ls[[i]] = ls_org[[i]] %>%
      group_by(positionStr) %>%
      reframe(N=n(),
              effort = sum(effort),
              lat = lat, long = long,
              year = year) %>%

      as.data.frame(ls[[i]])
  }

  return(ls)
}
