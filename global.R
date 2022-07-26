# global.R

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

# Functions ---------------------------------------------------------------

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


# Defs --------------------------------------------------------------------

stn_colors <- list(
  "baseline" = "green",
  "thermistor" = "purple",
  "nutrient" = "orange",
  "current" = "deepskyblue"
)


# Map layers --------------------------------------------------------------

counties <- read_sf("shp/wi-counties.shp")
nkes <- read_sf("shp/nke-plans-2022.shp")
huc8 <- read_sf("shp/wi-huc-8.shp")
huc10 <- read_sf("shp/wi-huc-10.shp")
huc12 <- read_sf("shp/wi-huc-12.shp")


# Station lists -----------------------------------------------------------

station_list <- read_csv("data/station-list.csv", col_types = list(station_id = "c"))
station_pts <- st_as_sf(station_list, coords = c("longitude", "latitude"), crs = 4326, remove = F)
station_types <- list(
  "Baseline (stream monitoring)" = "baseline",
  "Nutrient (total phosphorus)" = "nutrient",
  "Thermistor (temperature loggers)" = "thermistor"
)


# Baseline data -----------------------------------------------------------

baseline_data <- read_csv("data/baseline-data.csv.gz", col_types = list(station_id = "c")) %>%
  arrange(station_id, date)
baseline_coverage <- get_coverage(baseline_data)
baseline_stn_years <- baseline_data %>% distinct(station_id, year)
baseline_years <- unique(baseline_stn_years$year)

baseline_pts <- station_pts %>%
  filter(station_id %in% baseline_data$station_id) %>%
  left_join(baseline_coverage, by = "station_id")

check_missing_stns(baseline_data, baseline_pts, "baseline")


# Thermistor data ---------------------------------------------------------

therm_data <- read_csv("data/therm-data.csv.gz", col_types = list(station_id = "c"))
therm_info <- read_csv("data/therm-info.csv", col_types = list(station_id = "c"))
therm_coverage <- get_coverage(therm_data)
therm_stn_years <- therm_data %>% distinct(station_id, year)
therm_years <- unique(therm_stn_years$year)

therm_pts <- station_pts %>%
  filter(station_id %in% therm_data$station_id) %>%
  left_join(therm_coverage, by = "station_id")

check_missing_stns(therm_data, therm_pts, "thermistor")



# Nutrient data -----------------------------------------------------------

nutrient_data <- read_csv("data/tp-data.csv", col_types = list(station_id = "c")) %>%
  arrange(station_id, date)
nutrient_coverage <- get_coverage(nutrient_data)
nutrient_stn_years <- nutrient_data %>% distinct(station_id, year)
nutrient_years <- unique(nutrient_stn_years$year)

nutrient_pts <- station_pts %>%
  filter(station_id %in% nutrient_data$station_id) %>%
  left_join(nutrient_coverage, by = "station_id")

check_missing_stns(nutrient_data, nutrient_pts, "nutrient")




# Data coverage -----------------------------------------------------------

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
  rowwise() %>%
  mutate(
    data_year_list = list(unique(sort(strsplit(data_years, ", ")[[1]]))),
    data_years = paste(data_year_list, collapse = ", ")) %>%
  ungroup()

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
data_years <- rev(as.character(sort(unique(all_stn_years$year))))

all_stn_data <- all_stn_years %>%
  select(
    station_id,
    year,
    baseline = baseline_stn,
    nutrient = nutrient_stn,
    thermistor = therm_stn
  ) %>%
  complete(station_id, nesting(year), fill = list(
    "baseline" = F,
    "nutrient" = F,
    "thermistor" = F
  )) %>%
  mutate(across(where(is_logical), ~ ifelse(.x, "\u2705", "\u274c"))) %>%
  mutate(year = as.character(year))


# Finalize sites ----------------------------------------------------------

all_pts <- station_pts %>%
  mutate(label = paste(station_id, station_name, sep = ": ")) %>%
  mutate(
    baseline_stn = station_id %in% baseline_pts$station_id,
    therm_stn = station_id %in% therm_pts$station_id,
    nutrient_stn = station_id %in% nutrient_pts$station_id
  ) %>%
  filter(baseline_stn | therm_stn | nutrient_stn) %>%
  left_join(all_coverage, by = "station_id") %>%
  mutate(stn_color = case_when(
    baseline_stn ~ stn_colors$baseline,
    therm_stn ~ stn_colors$thermistor,
    nutrient_stn ~ stn_colors$nutrient
  )) %>%
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
  setNames(all_pts$station_id)

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



# TEST ZONE ---------------------------------------------------------------

# these stations appear in the data but don't have a location
# all_coverage %>%
#   filter(!(station_id %in% all_pts$station_id)) %>%
#   write_csv("stations missing locations.csv")


# test = c("2019", "2021")
# all_coverage %>%
#   rowwise() %>%
#   filter(setequal(intersect(test, data_year_list), test)) %>%
#   pull(station_id)

#
# baseline_data %>%
#   filter(station_id == station_id[1]) %>%
#   clean_names(case = "title") %>%
#   rownames_to_column() %>%
#   mutate(rowname = paste("Obs", rowname)) %>%
#   mutate(across(everything(), as.character)) %>%
#   pivot_longer(cols = -rowname, names_to = "Parameter") %>%
#   pivot_wider(names_from = rowname)
#
#
# names(df) <- paste("Obs", ncol(df))
