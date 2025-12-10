## SETUP ##

# resets Rdata and generates variables for the app that take a little time to compute


# Load packages ----

library(tidyverse)
library(janitor)
library(shiny)
library(sf)



# Clear environment ----

rm(list = ls(all.names = TRUE)) # remove all objects
gc() # garbage collect


# Functions ----

## Used here only ----

create_popups <- function(df) {
  title <- "<div class=popup-title>Monitoring Station Details</div>"
  cols <- names(df)
  lapply(seq_len(nrow(df)), function(r) {
    row <- slice(df, r)
    details <-
      lapply(seq_along(cols), function(c) {
        paste0("<b>", cols[c], ":</b> ", row[c])
      }) %>%
      paste0(collapse = "<br>")
    paste0(title, details)
  }) %>% paste0()
}


get_coverage <- function(df) {
  years <- df %>%
    distinct(station_id, year) %>%
    group_by(station_id) %>%
    summarise(
      data_years = paste(year, collapse = ", "),
      max_fw_year = max(year, na.rm = T)
    ) %>%
    rowwise() %>%
    mutate(data_year_list = list(unique(sort(strsplit(data_years, ", ")[[1]]))))
  dates <- df %>%
    group_by(station_id) %>%
    summarise(max_fw_date = max(date, na.rm = T))
  left_join(years, dates, by = "station_id")
}

check_missing_stns <- function(data, pts, type) {
  missing <- data %>%
    distinct(station_id, station_name) %>%
    filter(!(station_id %in% pts$station_id)) %>%
    arrange(station_id)

  if (nrow(missing) > 0) {
    warning(nrow(missing), "/", nrow(pts) + nrow(missing), " ", type, " stations are missing from the station list!", call. = F)
  }
}


# Load data ---------------------------------------------------------------

data_dir <- function(f) {
  file.path("../data", f)
}


## Shapefiles ----

# also used in watershed tab
fmt_area <- function(area) {
  sq_km <- area / 1e6
  sq_mi <- sq_km * 0.3861
  sprintf(
    "%s sq km (%s sq mi)",
    formatC(sq_km, format = "f", big.mark = ",", digits = 1),
    formatC(sq_mi, format = "f", big.mark = ",", digits = 1)
  )
}

wi_counties <- data_dir("shp/counties.rds") %>%
  read_rds() %>%
  st_make_valid()
wi_state <- st_union(wi_counties)
waterbodies <- data_dir("shp/waterbodies.rds") %>%
  read_rds()
flowlines <- data_dir("shp/flowlines.rds") %>%
  read_rds()
nkes <- data_dir("shp/nkes.rds") %>%
read_rds() %>%
  mutate(Label = paste0(
    "<b>", PlanName, "</b>",
    "<br>Ends: ", EndDate,
    "<br>Objective: ", Objective
  ))
huc8 <- data_dir("shp/huc8.rds") %>%
  read_rds() %>%
  mutate(Label = paste0(
    "<b>", Huc8Name, " Subbasin</b>",
    "<br>Area: ", fmt_area(Area),
    "<br>HUC8 Code: ", Huc8Code,
    "<br>HUC6 basin: ", MajorBasin
  ))
huc10 <- data_dir("shp/huc10.rds") %>%
  read_rds() %>%
  mutate(Label = paste0(
    "<b>", Huc10Name, " Watershed</b>",
    "<br>Area: ", fmt_area(Area),
    "<br>HUC10 Code: ", Huc10Code,
    "<br>HUC8 subbasin: ", Huc8Name,
    "<br>HUC6 basin: ", MajorBasin
  ))
suppressWarnings({
  huc10_centroids <- st_centroid(huc10)
})
huc12 <- data_dir("shp/huc12.rds") %>%
  read_rds() %>%
  mutate(Label = paste0(
    "<b>", Huc12Name, " Subwatershed</b>",
    "<br>Area: ", fmt_area(Area),
    "<br>HUC12 Code: ", Huc12Code,
    "<br>HUC10 watershed: ", Huc10Name,
    "<br>HUC8 subbasin: ", Huc8Name,
    "<br>HUC6 basin: ", MajorBasin
  ))


## Station lists ----

station_list <- read_csv(data_dir("stn_list.csv"))
station_pts <- station_list %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F)

## Baseline data ----

# adds a column right after `col` with the units in it
add_units <- function(.data, col, units) {
  mutate(.data, "{col}_units" := case_when(is.na(.data[[col]]) ~ NA, T ~ units), .after = {{ col }})
}

baseline_data <- read_csv(data_dir("baseline_data.csv")) %>%
  arrange(station_id, date) %>%
  rename(fieldwork_seq_no = fsn) %>%
  add_units("water_temp", "C") %>%
  add_units("air_temp", "C") %>%
  add_units("d_o", "mg/L") %>%
  add_units("d_o_saturation", "%") %>%
  add_units("transparency", "cm") %>%
  add_units("transparency_tube_length", "cm") %>%
  add_units("specific_cond", "μS/cm") %>%
  add_units("stream_width", "ft") %>%
  add_units("average_stream_depth", "ft") %>%
  add_units("cross_sectional_area", "sq ft") %>%
  add_units("length_assessed", "ft") %>%
  add_units("average_surface_velocity", "ft/s") %>%
  add_units("corrected_surface_velocity", "ft/s") %>%
  add_units("calculated_streamflow", "cfs") %>%
  add_units("corrected_streamflow", "cfs") %>%
  add_units("entered_streamflow", "cfs") %>%
  add_units("streamflow", "cfs")

baseline_coverage <- get_coverage(baseline_data)
baseline_stn_years <- baseline_data %>% distinct(station_id, year)
baseline_years <- unique(baseline_stn_years$year)

baseline_pts <- station_pts %>%
  filter(station_id %in% baseline_data$station_id) %>%
  left_join(baseline_coverage, by = "station_id")

# this will produce a warning if there are missing stations for the data
check_missing_stns(baseline_data, baseline_pts, "baseline")


## Macroinvertebrates ----

macro_params <- read_csv(data_dir("macro_parameters.csv")) %>%
  drop_na(group) %>%
  rename(species_name = dnr_parameter_description)

macro_species <- macro_params$species_name

# 1=Sensitive (Blue), 4=Tolerant (Red), Invasive (Purple)
macro_groups <- c("Group 1", "Group 2", "Group 3", "Group 4", "Invasive")

macro_species_counts <- read_csv(data_dir("macro_species_counts.csv")) %>%
  mutate(
    species_name = factor(species_name, levels = macro_species),
    group = factor(group, levels = macro_groups)
  ) %>%
  arrange(datetime, species_name)


## Nutrient data ----

phoslimit <- 0.075 # mg/L or ppm

nutrient_data <- read_csv(data_dir("tp_data.csv")) %>%
  rename(fieldwork_seq_no = fsn) %>%
  arrange(station_id, date) %>%
  mutate(exceeds_limit = tp > phoslimit, .after = tp)
nutrient_coverage <- get_coverage(nutrient_data)
nutrient_stn_years <- nutrient_data %>% distinct(station_id, year)
nutrient_years <- unique(nutrient_stn_years$year)

nutrient_pts <- station_pts %>%
  filter(station_id %in% nutrient_data$station_id) %>%
  left_join(nutrient_coverage, by = "station_id")

check_missing_stns(nutrient_data, nutrient_pts, "nutrient")


## Thermistor data ----

therm_data <- read_csv(data_dir("therm_data.csv.gz"))
therm_info <- read_csv(data_dir("therm_inventory.csv"))
therm_coverage <- get_coverage(therm_data)
therm_stn_years <- therm_data %>% distinct(station_id, year)
therm_years <- unique(therm_stn_years$year)

therm_pts <- station_pts %>%
  filter(station_id %in% therm_data$station_id) %>%
  left_join(therm_coverage, by = "station_id")

check_missing_stns(therm_data, therm_pts, "thermistor")


# Data coverage ----

all_coverage <- bind_rows(
  mutate(baseline_coverage, source = "Baseline"),
  mutate(nutrient_coverage, source = "Nutrient"),
  mutate(therm_coverage, source = "Thermistor")
) %>%
  group_by(station_id) %>%
  summarise(
    data_sources = paste(source, collapse = "/"),
    data_years = paste(data_years, collapse = ", "),
    max_fw_year = max(max_fw_year),
    max_fw_date = as.character(max(max_fw_date))
  ) %>%
  rowwise() %>%
  mutate(
    data_year_list = list(unique(sort(strsplit(data_years, ", ")[[1]]))),
    data_years = paste(data_year_list, collapse = ", ")
  ) %>%
  ungroup() %>%
  left_join(count(baseline_data, station_id, name = "baseline_data_obs"), by = "station_id") %>%
  left_join(count(nutrient_data, station_id, name = "nutrient_data_obs"), by = "station_id") %>%
  left_join(
    {
      therm_data %>%
        count(station_id, date) %>%
        count(station_id, name = "thermistor_days_recorded")
    },
    by = "station_id"
  ) %>%
  replace_na(list(
    baseline_data_obs = 0,
    nutrient_data_obs = 0,
    thermistor_days_recorded = 0
  ))

# all_coverage %>%
#   select(-"data_year_list") %>%
#   write_csv("station data coverage.csv")

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

baseline_tallies <- baseline_data %>%
  count(station_id, year, name = "baseline") %>%
  mutate(baseline = paste("\u2705", baseline, "obs"))

nutrient_tallies <- nutrient_data %>%
  count(station_id, year, name = "nutrient") %>%
  mutate(nutrient = paste("\u2705", nutrient, "obs"))

therm_tallies <- therm_data %>%
  count(station_id, year, date) %>%
  count(station_id, year, name = "thermistor") %>%
  mutate(thermistor = paste("\u2705", thermistor, "days"))

all_stn_data <- all_stn_years %>%
  select(station_id, year) %>%
  left_join(baseline_tallies, by = c("station_id", "year")) %>%
  left_join(nutrient_tallies, by = c("station_id", "year")) %>%
  left_join(therm_tallies, by = c("station_id", "year")) %>%
  mutate(across(where(is.numeric), as.character)) %>%
  replace_na(list(baseline = "\u274c", nutrient = "\u274c", thermistor = "\u274c"))

# all_stn_data %>%
#   mutate(across(everything(), ~gsub("\u2705 ", "", .x))) %>%
#   mutate(across(everything(), ~gsub("\u274c", "", .x))) %>%
#   write_csv("station data coverage.csv")



# Finalize sites lists ----

all_pts <- station_pts %>%
  mutate(label = paste(station_id, station_name, sep = ": ")) %>%
  mutate(
    baseline_stn = station_id %in% baseline_pts$station_id,
    therm_stn = station_id %in% therm_pts$station_id,
    nutrient_stn = station_id %in% nutrient_pts$station_id
  ) %>%
  filter(baseline_stn | therm_stn | nutrient_stn) %>%
  left_join(all_coverage, by = "station_id") %>%
  mutate(
    station_id = as.numeric(station_id),
    label = paste(station_id, station_name, sep = ": "),
    map_label = lapply(str_glue("
      <b>{data_sources} Monitoring Station</b><br>
      Station ID: {station_id}<br>
      Name: {str_trunc(station_name, 50)}<br>
      Most recent data: {format(as.Date(max_fw_date), '%b %d, %Y')}
    "), shiny::HTML)
  )

all_stns <- all_pts %>%
  select(-c(data_year_list)) %>%
  st_set_geometry(NULL)

all_labels <- setNames(all_pts$label, as.character(all_pts$station_id))

all_popups <- all_pts %>%
  st_set_geometry(NULL) %>%
  select(-c(baseline_stn, therm_stn, nutrient_stn, data_year_list, label, map_label)) %>%
  clean_names(case = "title", abbreviations = c("ID", "DNR", "WBIC", "HUC")) %>%
  create_popups() %>%
  setNames(all_pts$station_id)

all_stn_list <- all_pts %>%
  st_set_geometry(NULL) %>%
  select(label, station_id) %>%
  deframe() %>%
  as.list()


# Generate station totals for map coloring ----

#' color map by
#' n years
#' n fieldwork
#' baseline and nutrient params

stn_fieldwork_counts <- bind_rows(
  baseline_data %>%
    summarize(n_fieldwork = n_distinct(fieldwork_seq_no), .by = c(station_id, year)),
  nutrient_data %>%
    summarize(n_fieldwork = n_distinct(fieldwork_seq_no), .by = c(station_id, year)),
  therm_data %>%
    summarize(n_fieldwork = 2, .by = c(station_id, year))
) %>%
  summarize(
    n_years = n_distinct(year),
    n_fieldwork = sum(n_fieldwork),
    .by = station_id
  )

# names, plot, and map settings for baseline and nutrient data
data_opts <- read_csv("column_options.csv") %>%
  mutate(label = if_else(is.na(units), name, str_glue("{name} ({units})")), .after = name) %>%
  replace_na(list(units = ""))

stn_measure_stats <- bind_rows(
    baseline_data %>%
      pivot_longer(
        cols = any_of(data_opts$col),
        names_to = "measure"
      ) %>%
      select(station_id, date, measure, value),
    nutrient_data %>%
      pivot_longer(tp, names_to = "measure") %>%
      select(station_id, date, measure, value)
  ) %>%
  drop_na(value) %>%
  summarize(
    n = n(),
    across(value, c(mean = mean, median = median, min = min, max = max), .names = "{.fn}"),
    .by = c(station_id, measure)
  )

map_color_data <- stn_fieldwork_counts %>%
  left_join({
    stn_measure_stats %>%
      select(station_id, measure, mean) %>%
      pivot_wider(names_from = measure, values_from = mean)
  })


# Landscape data ----

landcover_classes <- read_csv(data_dir("nlcd_classes.csv"))
landscape_data <- read_csv(data_dir("nlcd_landcover.csv")) %>%
  left_join(landcover_classes) %>%
  group_by(across(-c(class, area, pct_area))) %>%
  summarize(across(c(area, pct_area), sum), .groups = "drop")

mean_landscape <- landscape_data %>%
  group_by(huc_level, class_name, hex) %>%
  summarize(pct_area = mean(pct_area), .groups = "drop")

watershed_sizes <- landscape_data %>%
  group_by(huc_level, huc) %>%
  summarize(area = mean(total_area), .groups = "drop_last") %>%
  summarize(area = mean(area)) %>%
  deframe()


# Save environment ----

save.image()
