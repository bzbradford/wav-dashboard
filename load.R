##  load.R  ##

# source to perform all data loading and processing and save to .RData


# Load packages ----

library(tidyverse)
library(lubridate)
library(janitor)
library(sf)
library(shiny)
library(shinycssloaders)


# Clear environment ----

rm(list = ls(all.names = TRUE)) # remove all objects
gc() # garbage collect


# Functions ----

## Used here only ----

create_popups <- function(data) {
  title <- "<div class=popup-title>Monitoring Station Details</div>"
  data %>% {
    cols <- names(.)
    lapply(1:nrow(.), function(r) {
      row <- .[r,]
      details <-
        lapply(1:length(cols), function(c) {
          paste0("<b>", cols[c], ":</b> ", row[c])
        }) %>%
        paste0(collapse = "<br>")
      paste0(title, details)
    }) %>% paste0()
  }
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


## Utility ----

c_to_f <- function(c, d = 1) {
  round(c * 1.8 + 32, d)
}

f_to_c <- function(f, d = 1) {
  round((f - 32) * 5.0 / 9.0, d)
}


## HTML / JS ----

colorize <- function(text, color = tolower(text)) {
  shiny::HTML(paste0("<span style='font-weight:bold; color:", color, "'>", text, "</span>"))
}

setURL <- function(id) {
  if (!is.null(id)) {
    shinyjs::runjs(sprintf("window.history.replaceState(null, null, window.location.origin + window.location.pathname + '?stn=%s')", id))
  } else {
    shinyjs::runjs("window.history.replaceState(null, null, window.location.origin + window.location.pathname)")
  }
}

setTitle <- function(label) {
  if (!is.null(label)) {
    title <- sprintf("Station %s - WAV Dashboard", str_trunc(label, 40))
    shinyjs::runjs(sprintf("document.title = '%s'", title))
  } else {
    shinyjs::runjs("document.title = 'WAV Data Dashboard'")
  }
}

withSpinnerProxy <- function(ui, ...) {
  ui %>% shinycssloaders::withSpinner(type = 8, color = "#30a67d", ...)
}


## Plotly helpers ----

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


## Used in app ----

# pick random baseline station to show initially
random_baseline_stn <- function() {
  all_pts %>%
    filter(baseline_stn) %>%
    filter(max_fw_year == max(data_years)) %>%
    pull(station_id) %>%
    sample(1)
}

# adds "All" to end of years list
year_choices <- function(years) {
  if (length(years) > 1) {
    c(years, "All")
  } else {
    years
  }
}

# get the min and max of a vector for plotly axis ranges
min_max <- function(v) {
  possibly(
    return(c(floor(min(v, na.rm = T)), ceiling(max(v, na.rm = T)))),
    return(c(NA, NA))
  )
}

### Baseline tab ----

do_color <- function(do) {
  i <- min(max(round(do), 1), 11)
  brewer.pal(11, "RdBu")[i]
}

find_max <- function(vals, min_val) {
  vals <- na.omit(vals)
  if (length(vals) == 0) return(min_val)
  ceiling(max(min_val, max(vals)) * 1.1)
}

make_min_max <- function(df, var) {
  v <- df[[var]]
  if (length(v) == 0) return(tibble())
  tibble(
    observations = length(na.omit(v)),
    min = df[which.min(v), ][[var]],
    max = df[which.max(v), ][[var]],
    date_of_min = df[which.min(v), ]$date,
    date_of_max = df[which.max(v), ]$date
  )
}


### Nutrient tab ----

phoslimit <- 0.075 # mg/L or ppm

get_phos_estimate <- function(vals) {
  vals <- na.omit(vals)
  log_vals <- log(vals)
  n <- length(vals)
  meanp <- mean(log_vals)
  se <- sd(log_vals) / sqrt(n)
  suppressWarnings({
    tval <- qt(p = 0.90, df = n - 1)
  })

  params <- list(
    mean = meanp,
    median = median(log_vals),
    lower = meanp - tval * se,
    upper = meanp + tval * se
  )

  params <- lapply(params, exp)
  params <- lapply(params, round, 3)
  params["n"] <- n
  params
}

get_phos_exceedance <- function(median, lower, upper, limit = phoslimit) {
  fail_msg <- "Unable to determine phosphorus exceedance type based on the data shown above."

  if (anyNA(c(median, lower, upper))) return(fail_msg)

  if (lower >= limit) {
    "Total phosphorus clearly exceeds the DNR's criteria (lower confidence interval > phosphorus limit.)"
  } else if (lower <= limit & median >= limit) {
    "Total phosphorus may exceed the DNR's criteria (median greater than phosphorus limit, but lower confidence interval below limit)."
  } else if (upper >= limit & median <= limit) {
    "Total phosphorus may meet the DNR's criteria (median below phosphorus limit, but upper confidence interval above limit)."
  } else if (upper <= limit) {
    "Total phosphorus clearly meets the DNR's criteria (upper confidence interval below limit)."
  } else {
    fail_msg
  }
}


### Watershed tab ----
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
  baseline = "green",
  nutrient = "orange",
  thermistor = "purple",
  current = "deepskyblue"
)

tab_names <- list(
  baseline = "Baseline data",
  nutrient = "Nutrient data",
  thermistor = "Thermistor data",
  watershed = "Watershed/landscape context",
  reports = "Downloadable reports",
  more = "Learn more"
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
  "data/therm-info.csv.gz",
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
stn_year_choices <- append(
  setNames(data_years[1:4], data_years[1:4]),
  setNames(data_years[5], paste0(last(data_years), "-", data_years[5]))
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
    nutrient_stn = station_id %in% nutrient_pts$station_id) %>%
  filter(baseline_stn | therm_stn | nutrient_stn) %>%
  left_join(all_coverage, by = "station_id") %>%
  mutate(
    station_id = as.numeric(station_id),
    stn_color = case_when(
      baseline_stn ~ stn_colors$baseline,
      therm_stn ~ stn_colors$thermistor,
      nutrient_stn ~ stn_colors$nutrient),
    label = paste(station_id, station_name, sep = ": "),
    map_label = lapply(glue::glue("
      <b>{data_sources} Monitoring Station</b><br>
      Station ID: {station_id}<br>
      Name: {str_trunc(station_name, 50)}
    "), shiny::HTML),
  )

all_stns <- all_pts %>%
  select(-c(data_year_list, stn_color)) %>%
  st_set_geometry(NULL)

all_labels <- setNames(all_pts$label, as.character(all_pts$station_id))

all_popups <- all_pts %>%
  st_set_geometry(NULL) %>%
  select(-c(baseline_stn, therm_stn, nutrient_stn, data_year_list, stn_color, label, map_label)) %>%
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
#' max water_temp
#' mean d_o
#' mean transparency_average
#' mean streamflow_cfs

stn_fieldwork_counts <- bind_rows(
  baseline_data %>%
    summarize(n_fieldwork = n_distinct(fieldwork_seq_no), .by = c(station_id, year)),
  nutrient_data %>%
    summarize(n_fieldwork = n(), .by = c(station_id, year)),
  therm_data %>%
    summarize(n_fieldwork = 1, .by = c(station_id, year))
) %>%
  summarize(
    n_years = n_distinct(year),
    n_fieldwork = sum(n_fieldwork),
    .by = station_id
  )

# baseline means from 10 most recent fieldwork events
baseline_means <- baseline_data %>%
  slice_max(date, n = 10, by = station_id) %>%
  summarize(
    water_temp = mean(water_temp, na.rm = T),
    d_o = mean(d_o, na.rm = T),
    transparency = mean(transparency_average, na.rm = T),
    streamflow = mean(streamflow_cfs, na.rm = T),
    .by = station_id
  ) %>% {
    df <- .
    df[sapply(df, is.infinite)] <- NA
    df[sapply(df, is.nan)] <- NA
    df
  } %>%
  mutate(across(water_temp:streamflow, ~signif(.x, 3)))

# from 12 most recent months
nutrient_means <- nutrient_data %>%
  drop_na(tp) %>%
  slice_max(date, n = 12, by = station_id) %>%
  summarize(tp = signif(mean(tp), 3), .by = station_id)

stn_attr_totals <- stn_fieldwork_counts %>%
  left_join(baseline_means, join_by(station_id)) %>%
  left_join(nutrient_means, join_by(station_id)) %>%
  mutate(station_id = as.numeric(station_id))

# summary(stn_attr_totals)

stn_color_opts <- tribble(
  ~label,            ~value,          ~domain,   ~rev, ~pal,
  "Years of data",    "n_years",      c(0, 10),  F,    "viridis",
  # "Fieldwork events", "n_fieldwork",  c(0, 100), F,    "viridis",
  "Mean water temp (°C)",       "water_temp",   c(10, 30), T,    "RdYlBu",
  "Mean dissolved oxygen (mg/L)", "d_o",          c(3, 12),  F,    "RdYlBu",
  "Mean transparency (cm)",     "transparency", c(0, 120), F,    "BrBG",
  "Mean streamflow (cfs)",       "streamflow",   c(0, 50),  T,    "RdBu",
  "Mean phosphorus (mg/L)", "tp",           c(0, .25), T,    "Spectral",
)

stn_color_choices <- append(
  list("Station type" = "stn_type"),
  deframe(stn_color_opts[,1:2])
)


# Landscape data ----

landcover_classes <- read_csv("data/nlcd_classes.csv", col_types = cols(), progress = F) %>%
  mutate(across(where(is.character), fct_inorder))
landscape_data <- read_csv("data/landcover.csv.gz", col_types = cols(), progress = F) %>%
  left_join(landcover_classes, by = "class") %>%
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
