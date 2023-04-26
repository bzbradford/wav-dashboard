## GLOBAL ##

# Dependencies ----

suppressMessages({
  # core
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(sf)
  library(rlang)

  # shiny
  library(shiny)
  library(shinyBS)
  library(shinyjs)
  library(shinythemes)
  library(shinyWidgets)
  library(htmltools)

  # display
  library(DT)
  library(leaflet)
  library(leaflet.extras)
  library(plotly)
  library(RColorBrewer)
})


# Functions ----

c_to_f <- function(c, d = 1) {
  round(c * 9.0 / 5.0 + 32, d)
}

f_to_c <- function(f, d = 1) {
  round((f - 32) * 5.0 / 9.0, d)
}

colorize <- function(text, color = tolower(text)) {
  shiny::HTML(paste0("<span style='color: ", color, "'>", text, "</span>"))
}

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

hline <- function(y = 0, color = "black") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color, dash = "dash")
  )
}

rect <- function(ymin, ymax, color = "red") {
  list(
    type = "rect",
    fillcolor = color,
    line = list(color = color),
    opacity = 0.1,
    y0 = ymin,
    y1 = ymax,
    xref = "paper",
    x0 = 0,
    x1 = 1,
    layer = "below"
  )
}

get_coverage <- function(df) {
  years <- df %>%
    distinct(station_id, year) %>%
    group_by(station_id) %>%
    summarise(
      data_years = paste(year, collapse = ", "),
      max_fw_year = max(year, na.rm = T)) %>%
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

year_choices <- function(years) {
  if (length(years) > 1) {
    c(years, "All")
  } else {
    years
  }
}

min_max <- function(v) {
  possibly(
    return(c(floor(min(v, na.rm = T)), ceiling(max(v, na.rm = T)))),
    return(c(NA, NA))
  )
}

random_baseline_stn <- function() {
  all_pts %>%
    filter(baseline_stn) %>%
    filter(max_fw_year == max(data_years)) %>%
    pull(station_id) %>%
    sample(1)
}

fmt_area <- function(area) {
  sq_km <- area / 1e6
  sq_mi <- sq_km * 0.3861
  paste0(
    formatC(sq_km, format = "f", big.mark = ",", digits = 1),
    " sq km (",
    formatC(sq_mi, format = "f", big.mark = ",", digits = 1),
    " sq mi)"
  )
}


# Defs ----

stn_colors <- list(
  "baseline" = "green",
  "thermistor" = "purple",
  "nutrient" = "orange",
  "current" = "deepskyblue"
)


# Load shapefiles ----

counties <- readRDS("data/shp/counties")
nkes <- readRDS("data/shp/nkes") %>%
  mutate(Label = paste0(
    "<b>", PlanName, "</b>",
    "<br>Ends: ", EndDate,
    "<br>Objective: ", Objective
  ))
huc8 <- readRDS("data/shp/huc8") %>%
  mutate(Label = paste0(
    "<b>", Huc8Name, " Subbasin</b>",
    "<br>Area: ", fmt_area(Area),
    "<br>HUC8 Code: ", Huc8Code,
    "<br>HUC6 basin: ", MajorBasin
  ))
huc10 <- readRDS("data/shp/huc10") %>%
  mutate(Label = paste0(
    "<b>", Huc10Name, " Watershed</b>",
    "<br>Area: ", fmt_area(Area),
    "<br>HUC10 Code: ", Huc10Code,
    "<br>HUC8 subbasin: ", Huc8Name,
    "<br>HUC6 basin: ", MajorBasin
  ))
huc12 <- readRDS("data/shp/huc12") %>%
  mutate(Label = paste0(
    "<b>", Huc12Name, " Subwatershed</b>",
    "<br>Area: ", fmt_area(Area),
    "<br>HUC12 Code: ", Huc12Code,
    "<br>HUC10 watershed: ", Huc10Name,
    "<br>HUC8 subbasin: ", Huc8Name,
    "<br>HUC6 basin: ", MajorBasin
  ))



# Station lists ----

station_list <- read_csv(
  "data/station-list.csv.gz",
  col_types = list(station_id = "c"),
  progress = F)
station_pts <- station_list %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F)
station_types <- list(
  "Baseline (stream monitoring)" = "baseline",
  "Nutrient (total phosphorus)" = "nutrient",
  "Thermistor (temperature loggers)" = "thermistor"
)


# Baseline data ----

baseline_data <- read_csv(
  "data/baseline-data.csv.gz",
  col_types = list(station_id = "c"),
  progress = F) %>%
  arrange(station_id, date)
baseline_coverage <- get_coverage(baseline_data)
baseline_stn_years <- baseline_data %>% distinct(station_id, year)
baseline_years <- unique(baseline_stn_years$year)

baseline_pts <- station_pts %>%
  filter(station_id %in% baseline_data$station_id) %>%
  left_join(baseline_coverage, by = "station_id")

check_missing_stns(baseline_data, baseline_pts, "baseline")


# Nutrient data ----

nutrient_data <- read_csv(
  "data/tp-data.csv.gz",
  col_types = list(station_id = "c"),
  progress = F) %>%
  arrange(station_id, date)
nutrient_coverage <- get_coverage(nutrient_data)
nutrient_stn_years <- nutrient_data %>% distinct(station_id, year)
nutrient_years <- unique(nutrient_stn_years$year)

nutrient_pts <- station_pts %>%
  filter(station_id %in% nutrient_data$station_id) %>%
  left_join(nutrient_coverage, by = "station_id")

check_missing_stns(nutrient_data, nutrient_pts, "nutrient")


# Thermistor data ----

therm_data <- read_csv(
  "data/therm-data.csv.gz",
  col_types = list(station_id = "c"),
  progress = F)
therm_info <- read_csv(
  "data/therm-info.csv",
  col_types = list(station_id = "c"),
  progress = F)
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
  mutate(therm_coverage, source = "Thermistor")) %>%
  group_by(station_id) %>%
  summarise(
    data_sources = paste(source, collapse = ", "),
    data_years = paste(data_years, collapse = ", "),
    max_fw_year = max(max_fw_year),
    max_fw_date = as.character(max(max_fw_date))) %>%
  rowwise() %>%
  mutate(
    data_year_list = list(unique(sort(strsplit(data_years, ", ")[[1]]))),
    data_years = paste(data_year_list, collapse = ", ")) %>%
  ungroup() %>%
  left_join(count(baseline_data, station_id, name = "baseline_data_obs"), by = "station_id") %>%
  left_join(count(nutrient_data, station_id, name = "nutrient_data_obs"), by = "station_id") %>%
  left_join({
    therm_data %>%
      count(station_id, date) %>%
      count(station_id, name = "thermistor_days_recorded")
  }, by = "station_id") %>%
  replace_na(list(
    baseline_data_obs = 0,
    nutrient_data_obs = 0,
    thermistor_days_recorded = 0))

# all_coverage %>%
#   select(-"data_year_list") %>%
#   write_csv("station data coverage.csv")

all_stn_years <- bind_rows(
  baseline_stn_years,
  therm_stn_years,
  nutrient_stn_years) %>%
  distinct(station_id, year) %>%
  arrange(station_id, year) %>%
  left_join(station_list, by = "station_id") %>%
  mutate(label = paste(station_id, station_name, sep = ": ")) %>%
  mutate(
    baseline_stn = station_id %in% baseline_pts$station_id,
    therm_stn = station_id %in% therm_pts$station_id,
    nutrient_stn = station_id %in% nutrient_pts$station_id)

data_years <- rev(as.character(sort(unique(all_stn_years$year))))

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
    nutrient_stn = station_id %in% nutrient_pts$station_id) %>%
  filter(baseline_stn | therm_stn | nutrient_stn) %>%
  left_join(all_coverage, by = "station_id") %>%
  mutate(stn_color = case_when(
    baseline_stn ~ stn_colors$baseline,
    therm_stn ~ stn_colors$thermistor,
    nutrient_stn ~ stn_colors$nutrient)) %>%
  mutate(station_id = as.numeric(station_id))

all_stns <- all_pts %>%
  select(-c(data_year_list, stn_color)) %>%
  st_set_geometry(NULL)

all_labels <- all_pts %>%
  st_set_geometry(NULL) %>%
  mutate(title = paste(data_sources, "Monitoring Site")) %>%
  mutate(label = paste0("<b>", title, "</b><br>Station ID: ", station_id, "<br>Name: ", station_name)) %>%
  pull(label) %>%
  lapply(HTML) %>%
  setNames(as.character(all_pts$station_id))

all_popups <- all_pts %>%
  st_set_geometry(NULL) %>%
  select(-c(baseline_stn, therm_stn, nutrient_stn, label, data_year_list)) %>%
  clean_names(case = "title") %>%
  create_popup("<b>WAV Monitoring Site</b><br>") %>%
  setNames(all_pts$station_id)

all_stn_list <- all_pts %>%
  st_set_geometry(NULL) %>%
  select(label, station_id) %>%
  deframe() %>%
  as.list()


# Landscape data ----


landcover_classes <- read_csv("data/nlcd_classes.csv", col_types = cols(), progress = F) %>%
  mutate(across(where(is.character), fct_inorder))
landscape_data <- read_csv("data/landcover.csv.gz", col_types = cols(), progress = F) %>%
  left_join(landcover_classes, by = "class")

