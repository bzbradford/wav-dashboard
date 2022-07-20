# global.R

library(tidyverse)
library(sf)
library(janitor)
library(lubridate)


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
  years <- df %>%
    distinct(station_id, year) %>%
    group_by(station_id) %>%
    summarise(
      data_years = paste(year, collapse = ", "),
      max_fw_year = max(year, na.rm = T))
  dates <- df %>%
    group_by(station_id) %>%
    summarise(max_fw_date = max(date, na.rm = T))
  left_join(years, dates, by = "station_id")
}

# Baseline data
baseline_data <- read_csv("data/baseline-data.csv")
baseline_coverage <- get_coverage(baseline_data)
baseline_pts <- station_pts %>%
  filter(station_id %in% baseline_data$station_id) %>%
  left_join(baseline_coverage, by = "station_id")
baseline_stns <- baseline_pts %>%
  st_set_geometry(NULL) %>%
  clean_names(case = "title")
baseline_years <- unique(baseline_data$year)


# Thermistor data
therm_data <- read_csv("data/therm-data.csv.gz")
therm_info <- read_csv("data/therm-info.csv")
therm_coverage <- get_coverage(therm_data)
therm_coverage %>%
  filter(station_id %in% station_list$station_id)
therm_pts <- station_pts %>%
  filter(station_id %in% therm_data$station_id) %>%
  left_join(therm_coverage, by = "station_id")
therm_stns <- therm_pts %>%
  st_set_geometry(NULL) %>%
  clean_names(case = "title")
therm_years <- unique(therm_data$year)


# Nutrient data
nutrient_data <- read_csv("data/tp-data.csv")
nutrient_coverage <- get_coverage(nutrient_data)
nutrient_pts <- station_pts %>%
  filter(station_id %in% nutrient_data$station_id) %>%
  left_join(nutrient_coverage, by = "station_id")
nutrient_stns <- nutrient_pts %>%
  st_set_geometry(NULL) %>%
  clean_names(case = "title")
nutrient_years <- unique(nutrient_data$year)


# Data coverage
all_coverage <- bind_rows(
  mutate(baseline_coverage, source = "Baseline"),
  mutate(therm_coverage, source = "Thermistor"),
  mutate(nutrient_coverage, source = "Nutrient")
) %>%
  group_by(station_id) %>%
  summarise(
    data_sources = paste(source, collapse = ", "),
    data_years = paste(data_years, collapse = ", "),
    max_fw_year = max(max_fw_year),
    max_fw_date = as.character(max(max_fw_date))
  ) %>%
  mutate(data_years = paste(unique(as.numeric(strsplit(data_years, ", ")[[1]])), collapse = ", "))

data_years <- unique(c(baseline_years, therm_years, nutrient_years))



# Finalize list of sites
all_pts <- station_pts %>%
  mutate(
    baseline_stn = station_id %in% baseline_pts$station_id,
    therm_stn = station_id %in% therm_pts$station_id,
    nutrient_stn = station_id %in% nutrient_pts$station_id
  ) %>%
  filter(baseline_stn | therm_stn | nutrient_stn) %>%
  select(-c("max_fw_year", "max_fw_date")) %>%
  left_join(all_coverage, by = "station_id")

all_stns <- all_pts %>%
  st_set_geometry(NULL) %>%
  clean_names(case = "title")


# these stations appear in the data but don't have a location
# all_coverage %>%
#   filter(!(station_id %in% all_pts$station_id)) %>%
#   write_csv("stations missing locations.csv")




