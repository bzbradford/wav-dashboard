## WAV DASHBOARD ##
# Ben Bradford, UW-Madison
# Requires data prep in separate RProj
# Source this file to prepare .RData for deployment


#- Renv for pkg management -#

# renv::init()         # initiate renv if not already
# renv::dependencies() # show project dependencies
# renv::clean()        # remove unused packages
# renv::update()       # update project libraries
# renv::snapshot()     # save updated lock file to project
# renv::restore()      # restore versions from lockfile


#- Testing -#

# shiny::devmode(TRUE)
# shiny::devmode(FALSE)


# Load packages ----

library(tidyverse)
library(lubridate)
library(janitor)
library(sf)
library(shiny)


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



## Nutrient tab ----

phoslimit <- 0.075 # mg/L or ppm

#' @param vals vector of phosphorus readings
getPhosEstimate <- function(vals) {
  suppressWarnings({
    vals <- na.omit(vals)
    n <- length(vals)
    log_vals <- log(vals + .001)
    log_mean <- mean(log_vals)
    se <- sd(log_vals) / sqrt(n)
    tval <- qt(p = 0.80, df = n - 1)
  })
  params <- list(
    mean = log_mean,
    median = median(log_vals),
    lower = log_mean - tval * se,
    upper = log_mean + tval * se
  ) %>% lapply(exp) %>% lapply(signif, 3)
  params$n <- n
  params$limit <- phoslimit
  params
}

#' @param vals
#'   `n` number of observations
#'   `median` median value
#'   `lower` lower confidence limit
#'   `upper` upper confidence limit
#' @param limit state phosphorus exceedance limit
getPhosExceedanceText <- function(vals, limit = phoslimit) {
  median <- vals$median
  lower <- vals$lower
  upper <- vals$upper

  msg <- "Insufficient data to determine phosphorus exceedance language based on the data shown above."
  if (anyNA(c(median, lower, upper))) return(msg)

  msg <- case_when(
    lower >= limit ~ "Total phosphorus levels clearly exceed the DNR's criteria (median and entire confidence interval above phosphorus standard) and the stream is likely impaired.",
    (lower <= limit) & (median >= limit) ~ "Total phosphorus levels may exceed the DNR's criteria (median greater than the standard, but lower confidence interval below the standard).",
    (upper >= limit) & (median <= limit) ~ "Total phosphorus levels may meet the DNR's criteria (median below phosphorus standard, but upper confidence interval above standard).",
    upper <= limit ~ "Total phosphorus levels clearly meet the DNR's criteria (median and entire confidence interval below phosphorus standard).",
    .default = msg
  )
  msg <- paste(msg, ifelse(vals$n < 6, "However, less than the required 6 monthly measurements were taken at this station.", ""))
}


## Reports ----

# baseline temperature data normally stored in C, must be converted to F
report_baseline_cols <- c(
  `Air temp (°C)` = "air_temp",
  `Water temp (°C)` = "water_temp",
  `DO (mg/L)` = "d_o",
  `DO (% sat.)` = "d_o_percent_saturation",
  `pH` = "ph",
  `Specific conductance (μS/cm)` = "specific_cond",
  `Transparency (cm)` = "transparency",
  `Streamflow (cfs)` = "streamflow"
)

# will be excluded if all NA
report_baseline_optional_cols <- c("ph", "specific_cond")

# will be included if any streamflow cfs data
report_streamflow_cols <- c(
  `Stream width (ft)` = "stream_width",
  `Average depth (ft)` = "average_stream_depth",
  `Surface velocity (ft/s)` = "average_surface_velocity"
)

# creates a paragraph of text describing the data
buildReportSummary <- function(params) {
  yr <- params$year
  data <- params$data

  counts <- list(
    baseline = nrow(data$baseline),
    nutrient = sum(!is.na(data$nutrient$tp)),
    thermistor = n_distinct(data$thermistor$date)
  )

  baseline_count_cols <- c(
    air_temp = "air temperature",
    water_temp = "water temperature",
    d_o = "dissolved oxygen",
    ph = "ph",
    specific_cond = "specific conductivity",
    transparency = "water transparency",
    streamflow = "streamflow"
  )

  for (var in names(baseline_count_cols)) {
    counts[[var]] = sum(!is.na(data$baseline[[var]]))
  }

  has <- sapply(counts, function(n) { n > 0 }, simplify = F)

  # generate summary paragraph
  base_counts <- tribble(
    ~count, ~text,
    counts$baseline, "baseline water quality measurements",
    counts$nutrient, "total phosphorus samples",
    counts$thermistor, "days of continuous water temperature logging"
  ) %>%
    filter(count > 0) %>%
    mutate(text = paste(count, text)) %>%
    pull(text) %>%
    combine_words()

  msg <- str_glue("This report covers monitoring data collected between Jan 1 and Dec 31, {yr}, and includes {base_counts}.")

  if (has$baseline) {
    baseline_counts <- data$baseline %>%
      select(all_of(names(baseline_count_cols))) %>%
      pivot_longer(everything()) %>%
      summarize(count = sum(!is.na(value)), .by = name) %>%
      filter(count != 0) %>%
      left_join(enframe(baseline_count_cols), join_by(name)) %>%
      summarize(text = combine_words(value), .by = count) %>%
      arrange(desc(count)) %>%
      mutate(text = paste(count, text, if_else(count == 1, "measurement", "measurements"))) %>%
      pull(text) %>%
      combine_words()
    msg <- paste0(msg, " Baseline water quality monitoring included ", baseline_counts, ".")
    msg <- paste0(msg, " Report downloaded on ", format(Sys.Date(), "%b %d, %Y"), ".")
  }

  list(counts = counts, has = has, message = msg)
}

# min/max etc for data cols
summarizeReportCols <- function(df, cols) {
  df %>%
    rename(all_of(cols)) %>%
    pivot_longer(all_of(names(cols)), names_to = "Parameter") %>%
    mutate(Parameter = factor(Parameter, levels = names(cols))) %>%
    drop_na(value) %>%
    summarize(
      across(value, list(
          N = ~n(),
          Min = min,
          Max = max,
          Median = median,
          Mean = mean,
          SD = sd
        ), .names = "{.fn}"),
      .by = Parameter) %>%
    mutate(CV = scales::percent(SD / Mean, accuracy = 1)) %>%
    mutate(across(Min:SD, ~if_else(is.na(.x), NA, as.character(signif(.x, 3)))))
}

# summary table
makeReportBaselineTable <- function(baseline) {
  df <- baseline
  for (col in report_baseline_optional_cols) {
    if (all(is.na(df[[col]]))) df[[col]] <- NULL
  }
  df <- df %>% select(`Date` = formatted_date, any_of(report_baseline_cols))
  names(df) <- gsub(" (", "\\\n(", names(df), fixed = T) # add line breaks
  df
}

# summary table
makeReportStreamflowTable <- function(baseline) {
  baseline %>%
    mutate(across(flow_method_used, ~gsub(" Method", "", .x))) %>%
    select(
      `Date` = formatted_date,
      all_of(report_streamflow_cols),
      `Streamflow (cfs)` = streamflow,
      `Flow method` = flow_method_used
    )
}

# creates some paragraphs with fieldwork details for the report
buildReportFieldworkComments <- function(baseline) {
  baseline %>%
    select(
      date,
      fsn = fieldwork_seq_no,
      names = group_desc,
      wx = weather_conditions,
      rec_wx = weather_last_2_days,
      com1 = fieldwork_comments,
      com2 = additional_comments) %>%
    mutate(across(where(is.character), xtable::sanitize)) %>%
    rowwise() %>%
    mutate(comments = paste(na.omit(com1, com2), collapse = ". ")) %>%
    mutate(fieldwork_desc = str_glue(
      "* **{format(date, '%b %d, %Y')}** - ",
      "SWIMS fieldwork number: {fsn}. ",
      if_else(is.na(wx), "", " Weather: {wx}."),
      if_else(is.na(rec_wx), "", " Weather past 2 days: {rec_wx}."),
      if_else(nchar(comments) == 0, "", " Fieldwork comments: {comments}."),
      if_else(is.na(names), "", " Submitted by: {names}.")
    )) %>%
    pull(fieldwork_desc) %>%
    gsub("..", ".", ., fixed = T)
}



# Load shapefiles ----

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

counties <- readRDS("data/shp/counties.rds")
waterbodies <- readRDS("data/shp/waterbodies.rds")
nkes <- readRDS("data/shp/nkes.rds") %>%
  mutate(Label = paste0(
    "<b>", PlanName, "</b>",
    "<br>Ends: ", EndDate,
    "<br>Objective: ", Objective
  ))
huc8 <- readRDS("data/shp/huc8.rds") %>%
  mutate(Label = paste0(
    "<b>", Huc8Name, " Subbasin</b>",
    "<br>Area: ", fmt_area(Area),
    "<br>HUC8 Code: ", Huc8Code,
    "<br>HUC6 basin: ", MajorBasin
  ))
huc10 <- readRDS("data/shp/huc10.rds") %>%
  mutate(Label = paste0(
    "<b>", Huc10Name, " Watershed</b>",
    "<br>Area: ", fmt_area(Area),
    "<br>HUC10 Code: ", Huc10Code,
    "<br>HUC8 subbasin: ", Huc8Name,
    "<br>HUC6 basin: ", MajorBasin
  ))
suppressWarnings({ huc10_centroids <- st_centroid(huc10) })
huc12 <- readRDS("data/shp/huc12.rds") %>%
  mutate(Label = paste0(
    "<b>", Huc12Name, " Subwatershed</b>",
    "<br>Area: ", fmt_area(Area),
    "<br>HUC12 Code: ", Huc12Code,
    "<br>HUC10 watershed: ", Huc10Name,
    "<br>HUC8 subbasin: ", Huc8Name,
    "<br>HUC6 basin: ", MajorBasin
  ))


# Station lists ----

station_list <- readRDS("data/station-list.rds")
station_pts <- station_list %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F)
station_types <- list(
  "Baseline (stream monitoring)" = "baseline",
  "Nutrient (total phosphorus)" = "nutrient",
  "Thermistor (temperature loggers)" = "thermistor"
)


# Baseline data ----

baseline_data <- readRDS("data/baseline-data.rds") %>%
  arrange(station_id, date) %>%
  rename(fieldwork_seq_no = fsn)
baseline_coverage <- get_coverage(baseline_data)
baseline_stn_years <- baseline_data %>% distinct(station_id, year)
baseline_years <- unique(baseline_stn_years$year)

baseline_pts <- station_pts %>%
  filter(station_id %in% baseline_data$station_id) %>%
  left_join(baseline_coverage, by = "station_id")

# this will produce a warning if there are missing stations for the data
check_missing_stns(baseline_data, baseline_pts, "baseline")


# Nutrient data ----

nutrient_data <- readRDS("data/tp-data.rds") %>%
  rename(fieldwork_seq_no = fsn) %>%
  arrange(station_id, date)
nutrient_coverage <- get_coverage(nutrient_data)
nutrient_stn_years <- nutrient_data %>% distinct(station_id, year)
nutrient_years <- unique(nutrient_stn_years$year)

nutrient_pts <- station_pts %>%
  filter(station_id %in% nutrient_data$station_id) %>%
  left_join(nutrient_coverage, by = "station_id")

check_missing_stns(nutrient_data, nutrient_pts, "nutrient")


# Thermistor data ----

therm_data <- readRDS("data/therm-data.rds")
therm_info <- readRDS("data/therm-inventory.rds")
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
    data_sources = paste(source, collapse = "/"),
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
#' max water_temp
#' mean d_o
#' mean transparency
#' mean streamflow

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
    transparency = mean(transparency, na.rm = T),
    streamflow = mean(streamflow, na.rm = T),
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
  ~label, ~value, ~domain, ~rev, ~pal,
  "Years of data", "n_years", c(0, 10), F, "viridis",
  "Fieldwork events", "n_fieldwork", c(0, 100), F, "viridis",
  "Mean water temp (°C)", "water_temp", c(5, 25), T, "RdYlBu",
  "Mean dissolved oxygen (mg/L)", "d_o", c(3, 12), F, "RdYlBu",
  "Mean transparency (cm)", "transparency", c(0, 120), F, "BrBG",
  "Mean streamflow (cfs)", "streamflow", c(0, 50), T, "RdBu",
  "Mean phosphorus (mg/L)", "tp", c(0, .25), T, "Spectral",
)

stn_color_choices <- append(
  list("Station type" = "stn_type"),
  deframe(stn_color_opts[,1:2])
)


# Landscape data ----

landcover_classes <- readRDS("data/nlcd-classes.rds")
landscape_data <- readRDS("data/nlcd-landcover.rds") %>%
  left_join(landcover_classes, join_by(class)) %>%
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
