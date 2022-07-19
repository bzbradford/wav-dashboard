# global.R

library(tidyverse)
library(sf)
library(janitor)


# Map layers
counties <- read_sf("shp/wi-counties.shp")
nkes <- read_sf("shp/nke-plans-2022.shp")
huc8 <- read_sf("shp/wi-huc-8.shp")
huc10 <- read_sf("shp/wi-huc-10.shp")
huc12 <- read_sf("shp/wi-huc-12.shp")


# Load station list
station_list <- read_csv("data/station-list.csv")
station_pts <- st_as_sf(station_list, coords = c("longitude", "latitude"), crs = 4326, remove = F)

get_coverage <- function(df) {
  df %>%
    distinct(station_id, year) %>%
    group_by(station_id) %>%
    summarise(data_years = paste(year, collapse = ", "))
}

# Baseline data
baseline_data <- read_csv("data/baseline-data.csv")
baseline_pts <- station_pts %>%
  filter(station_id %in% baseline_data$station_id) %>%
  left_join(get_coverage(baseline_data), by = "station_id")
baseline_stns <- baseline_pts %>%
  st_set_geometry(NULL) %>%
  clean_names(case = "title")


# Thermistor data
therm_data <- read_csv("data/therm-data.csv.gz")
therm_info <- read_csv("data/therm-info.csv")
therm_pts <- station_pts %>%
  filter(station_id %in% therm_data$station_id) %>%
  left_join(get_coverage(therm_data), by = "station_id")
therm_stns <- therm_pts %>%
  st_set_geometry(NULL) %>%
  clean_names(case = "title")


# Nutrient data
nutrient_data <- read_csv("data/tp-data.csv")
nutrient_pts <- station_pts %>%
  filter(station_id %in% nutrient_data$station_id) %>%
  left_join(get_coverage(nutrient_data), by = "station_id")
nutrient_stns <- nutrient_pts %>%
  st_set_geometry(NULL) %>%
  clean_names(case = "title")


all_pts <- station_pts %>%
  mutate(
    baseline_stn = station_id %in% baseline_pts$station_id,
    therm_stn = station_id %in% therm_pts$station_id,
    nutrient_stn = station_id %in% nutrient_data$station_id
  ) %>%
  filter(baseline_stn | therm_stn | nutrient_stn)

all_stns <- all_pts %>%
  st_set_geometry(NULL) %>%
  clean_names(case = "title")

