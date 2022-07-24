# global.R

library(tidyverse)
library(sf)
library(janitor)
library(lubridate)
library(shiny)

create_popup <- function(data, title) {
  data %>% {
    cols <- names(.)
    lapply(1:nrow(.), function(r) {
      row <- .[r,]
      details <-
        lapply(1:length(cols), function(c) {
          paste0("<br><b>", cols[c], ":</b> ", row[c])
        }) %>%
        paste0(collapse = "")
      paste0(title, details)
    }) %>% paste0()
  }
}

# Map layers
counties <- read_sf("shp/wi-counties.shp")
nkes <- read_sf("shp/nke-plans-2022.shp")
huc8 <- read_sf("shp/wi-huc-8.shp")
huc10 <- read_sf("shp/wi-huc-10.shp")
huc12 <- read_sf("shp/wi-huc-12.shp")


# Load station list
station_list <- read_csv("data/station-list.csv")
station_pts <- st_as_sf(station_list, coords = c("longitude", "latitude"), crs = 4326, remove = F)
station_types <- list(
  "Baseline (stream monitoring)" = "baseline",
  "Thermistor (temperature loggers)" = "therm",
  "Nutrient (total phosphorus)" = "nutrient"
)

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

make_stn_list <- function(sf) {

}

# Baseline data
baseline_data <- read_csv("data/baseline-data.csv")
baseline_coverage <- get_coverage(baseline_data)
baseline_stn_years <- baseline_data %>% distinct(station_id, year)
baseline_years <- unique(baseline_stn_years$year)

baseline_pts <- station_pts %>%
  filter(station_id %in% baseline_data$station_id) %>%
  left_join(baseline_coverage, by = "station_id")
baseline_stns <- baseline_pts %>%
  st_set_geometry(NULL) %>%
  clean_names(case = "title")



# Thermistor data
therm_data <- read_csv("data/therm-data.csv.gz")
therm_info <- read_csv("data/therm-info.csv")
therm_coverage <- get_coverage(therm_data)
therm_stn_years <- therm_data %>% distinct(station_id, year)
therm_years <- unique(therm_stn_years$year)

therm_pts <- station_pts %>%
  filter(station_id %in% therm_data$station_id) %>%
  left_join(therm_coverage, by = "station_id")
therm_stns <- therm_pts %>%
  st_set_geometry(NULL) %>%
  clean_names(case = "title")



# Nutrient data
nutrient_data <- read_csv("data/tp-data.csv")
nutrient_coverage <- get_coverage(nutrient_data)
nutrient_stn_years <- nutrient_data %>% distinct(station_id, year)
nutrient_years <- unique(nutrient_stn_years$year)

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

all_stn_years <- bind_rows(
  baseline_stn_years,
  therm_stn_years,
  nutrient_stn_years
) %>%
  distinct(station_id, year) %>%
  arrange(station_id, year) %>%
  left_join(station_list, by = "station_id") %>%
  mutate(label = paste(station_id, station_name, sep = ": ")) %>%
  mutate(
    baseline_stn = station_id %in% baseline_pts$station_id,
    therm_stn = station_id %in% therm_pts$station_id,
    nutrient_stn = station_id %in% nutrient_pts$station_id
  )
data_years <- sort(unique(all_stn_years$year))


# Finalize list of sites
all_pts <- station_pts %>%
  mutate(label = paste(station_id, station_name, sep = ": ")) %>%
  mutate(
    baseline_stn = station_id %in% baseline_pts$station_id,
    therm_stn = station_id %in% therm_pts$station_id,
    nutrient_stn = station_id %in% nutrient_pts$station_id
  ) %>%
  filter(baseline_stn | therm_stn | nutrient_stn) %>%
  select(-c("max_fw_year", "max_fw_date")) %>%
  left_join(all_coverage, by = "station_id")

all_stns <- all_pts %>%
  st_set_geometry(NULL)

all_labels <- lapply(paste0("<b>WAV Monitoring Site</b><br>Station ID: ", all_pts$station_id, "<br>Name: ", all_pts$station_name), HTML) %>%
  setNames(all_pts$station_id)

all_popups <- all_stns %>%
  clean_names(case = "title") %>%
  create_popup("<b>WAV Monitoring Site</b><br>") %>%
  setNames(all_stns$station_id)


all_stn_list <- all_stns %>%
  select(label, station_id) %>%
  deframe() %>%
  as.list()


# these stations appear in the data but don't have a location
# all_coverage %>%
#   filter(!(station_id %in% all_pts$station_id)) %>%
#   write_csv("stations missing locations.csv")




