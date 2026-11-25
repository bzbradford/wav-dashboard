#' Data prep for WAV dashboard
#'
#' 1. Move new data from SWIMS queries into /swims
#' 2. Source this script
#'

library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(sf)
library(terra)
library(leaflet)

# load(".RData")
# save.image()

# in case query returns newer stuff we don't want yet
MAX_DATE <- as_date("2026-12-31") # SET THIS

# load functions
source("functions.R")


# SWIMS link builders ----

# (the app has its own copy of the *_link functions in app/global.R)

swims_fw_url <- function(seq_no) {
  str_glue(
    "https://apps.dnr.wi.gov/swims/Fieldwork/FieldworkDetails?id={seq_no}"
  )
}

swims_stn_url <- function(seq_no) {
  str_glue("https://apps.dnr.wi.gov/swims/Stations/StationDetails?id={seq_no}")
}

swims_fw_link <- function(seq_no, link_text = seq_no) {
  str_glue(
    "<a title='View on SWIMS' href='{swims_fw_url(seq_no)}' target='_blank'>{link_text}</a>"
  )
}

swims_stn_link <- function(seq_no, link_text = seq_no) {
  str_glue(
    "<a title='View on SWIMS' href='{swims_stn_url(seq_no)}' target='_blank'>{link_text}</a>"
  )
}

# SHAPEFILES ===================================================================

# update and export shapefiles if needed. Will take a few min
if (FALSE) {
  source("update_shapefiles.R")
} else {
  load("shapefiles.RData")
}


# STATIONS =====================================================================

## Load ----
stn_xl <- load_xl("wav_all_stns.xlsx")
names(stn_xl)

# non-numeric station ids
# stn_xl |> filter(!is.finite(as.numeric(station_id)))

stns_in <- stn_xl |>
  select(
    station_id,
    station_name = primary_station_name,
    latitude = calc_ll_lat_dd_amt,
    longitude = calc_ll_long_dd_amt,
    station_seq_no = monit_station_seq_no,
    wbic,
    waterbody = official_waterbody_name,
    station_type = station_type_code,
    county_name
  ) |>
  mutate(station_id = as.integer(station_id)) |>
  drop_na(station_id) |>
  distinct(station_id, .keep_all = T) |>
  arrange(station_id)

stn_corrections <- tribble(
  ~station_id , ~latitude , ~longitude ,
     10055326 , 44.048073 , -88.688243 ,
     10060233 , 45.998043 , -89.404756 ,
     10060454 , 45.823530 , -92.012559 ,
     10060158 , 42.70712  , -89.9069
)


## Validation ----
stns_valid <- stns_in |>
  bind_rows(stn_corrections) |>
  merge_by("station_id") |>
  validate_stns()

stns_valid.sf <- stns_valid |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

stns_invalid <- validate_stns(stns_in, F)

# data check
if (FALSE) {
  stns_in |>
    filter(station_id == 10060158)
  # optionally export invalid station lists
  for (nm in names(stns_invalid)) {
    local({
      df <- stns_invalid[[nm]]$data
      print(nm)
      print(df)
      if (nrow(df) > 0) {
        write_excel_csv(df, sprintf("QC/invalid_stns_%s.csv", nm), na = "")
      }
    })
  }

  # map the stations
  stns_valid |>
    leaflet() |>
    addTiles() |>
    addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      radius = 1,
      fillColor = "orange",
      weight = 0,
      fillOpacity = 1
    )
}

## Get station elevations ----
# optional extra data, can skip if raster not available
elev <- rast("D:/GIS/raster/wi_elevation_10m/NED_10m_elevation.tif")
stn_elev <- terra::extract(
  elev,
  stns_valid.sf |> st_transform(crs = st_crs(elev))
)$BinValues


## Join shapefile data ----
stn_list.sf <- stns_valid.sf |>
  select(-county_name) |>
  st_join(
    counties |> select(CountyName, DnrRegion),
    join = st_nearest_feature # some sites are outside the strict state boundaries so we just join by nearest county
  ) |>
  st_join(huc12 |> select(-Area)) |>
  st_join(dnr_watersheds |> select(DnrWatershedCode, DnrWatershedName)) |>
  select(
    station_id,
    station_name,
    latitude,
    longitude,
    station_seq_no,
    wbic,
    waterbody,
    station_type,
    county_name = CountyName,
    dnr_region = DnrRegion,
    huc12 = Huc12Code,
    sub_watershed = Huc12Name,
    huc10 = Huc10Code,
    watershed = Huc10Name,
    dnr_watershed_name = DnrWatershedName,
    dnr_watershed_code = DnrWatershedCode,
    huc8 = Huc8Code,
    sub_basin = Huc8Name,
    major_basin = MajorBasin,
    geometry
  ) |>
  mutate(elevation_ft = stn_elev, .before = geometry) |>
  distinct(station_id, .keep_all = TRUE)

# convert to tibble
stn_list <- stn_list.sf |> st_set_geometry(NULL)

message("Total stations: ", nrow(stn_list))

# inspect
if (FALSE) {
  stn_list |> filter(is.na(station_name))
  stn_list |> filter(is.na(county_name))
}

## Export full station list ----
stn_list |> write_csv(data_dir("stn_list_full.csv"), na = "")


# BASELINE / FLOW / MACRO ======================================================

## Baseline ----

baseline_xl <- load_xl("wav_baseline.xlsx")
# names(baseline_xl)

baseline_raw <- baseline_xl |>
  select(
    fsn = fieldwork_seq_no,
    datetime = start_date_time,
    station_id,
    air_temp = ambient_air_temp,
    water_temp,
    d_o = do_mg,
    d_o_saturation = do_pct,
    ph,
    specific_cond,
    transparency = transparency_avg,
    transparency_tube_length,
    weather_conditions,
    weather_last_2_days,
    current_stream_condition,
    group_seq_no,
    group_desc,
    fieldwork_comment,
    additional_comments
  ) |>
  mutate(datetime = with_tz(datetime, "America/Chicago")) |>
  mutate(date = as_date(datetime), .after = datetime) |>
  mutate(suppressWarnings(across(c(fsn, station_id), as.integer))) |>
  distinct(fsn, station_id, date, .keep_all = T) |>
  filter(between(date, as_date("2015-1-1"), MAX_DATE))

# collect and record non-numeric entries
baseline_nan <- baseline_raw |>
  try_convert_numeric(c(
    "air_temp",
    "water_temp",
    "d_o",
    "d_o_saturation",
    "ph",
    "specific_cond",
    "transparency",
    "transparency_tube_length"
  ))
baseline_nan |> write_csv("QC/baseline non-numeric values.csv")

# convert to numeric, anything invalid => NaN
baseline_obs <- baseline_raw |>
  mutate(across(
    c(
      air_temp,
      water_temp,
      d_o,
      d_o_saturation,
      ph,
      specific_cond,
      transparency,
      transparency_tube_length
    ),
    to_numeric
  ))

message(
  "Total baseline: ",
  nrow(baseline_obs),
  ", most recent: ",
  max(baseline_obs$date)
)

# data check
if (FALSE) {
  baseline_obs |> count(year(date))

  baseline_obs |> filter(is.nan(air_temp))

  baseline_obs |>
    # filter(between(specific_cond, 5, 100)) |>
    # filter(between(specific_cond, 0, 5)) |>
    ggplot(aes(specific_cond)) +
    geom_histogram()

  baseline_obs |>
    filter(date >= "2026-1-1")
}


## Streamflow ----

flow_xl <- load_xl("wav_flow.xlsx")
# names(flow_xl)

flow_raw <- flow_xl |>
  select(
    fsn = fieldwork_seq_no,
    datetime = start_date_time,
    station_id,
    flow_method_used,
    current_stream_conditions,
    stream_width,
    average_stream_depth,
    cross_sectional_area,
    length_assessed,
    average_surface_velocity,
    velocity_correction_factor,
    corrected_surface_velocity,
    calculated_streamflow = calculated_streamflow_cfs,
    corrected_streamflow = calculated_corrected_streamflow_cfs,
    entered_streamflow = stream_flow_cfs,
    weather_conditions,
    weather_last_2_days = weather_past_two_days,
    group_seq_no,
    group_desc,
    fieldwork_comment
  ) |>
  mutate(datetime = with_tz(datetime, "America/Chicago")) |>
  mutate(date = as_date(datetime), .after = datetime) |>
  mutate(across(c(fsn, station_id), as.integer)) |>
  distinct(fsn, station_id, date, .keep_all = T) |>
  filter(between(date, as_date("2015-1-1"), MAX_DATE))

# identify bad values
flow_nan <- flow_raw |>
  try_convert_numeric(c(
    "stream_width",
    "average_stream_depth",
    "cross_sectional_area",
    "length_assessed",
    "average_surface_velocity",
    "velocity_correction_factor",
    "corrected_surface_velocity",
    "calculated_streamflow",
    "corrected_streamflow",
    "entered_streamflow"
  ))
flow_nan |> write_csv("QC/flow non-numeric values.csv")

# save good values and finalize flow
flow_obs <- flow_raw |>
  mutate(across(
    stream_width:entered_streamflow,
    to_numeric
  )) |>
  mutate(
    # need to turn NaNs into Inf so they don't get treated as NA
    streamflow = nan_coalesce(entered_streamflow, corrected_streamflow),
    .after = entered_streamflow
  )

message(
  "Total streamflow: ",
  nrow(flow_obs),
  ", most recent: ",
  max(flow_obs$date)
)

# data check
if (FALSE) {
  flow_obs |> count(year(datetime))

  max(flow_obs$date)
  compare_fieldwork(baseline_obs, flow_obs)

  # check for infinity
  flow_obs |> filter(is.nan(streamflow))
  flow_obs |> filter(if_any(where(is.numeric), is.nan)) |> view()

  flow_obs |> filter(station_id == 10059011) |> view()

  # shows need to set timezone correctly
  tz(flow_obs$datetime)
  hist(hour(flow_obs$datetime))

  flow_obs$datetime |>
    with_tz("America/Chicago") |>
    hour() |>
    hist()
}


## Macroinvertebrates ----------------------------------------------------------

macro_index <- read_csv("macro_species_index.csv")
macro_species <- macro_index |>
  drop_na(group) |>
  pull(dnr_parameter_description)

macro_xl <- load_xl("wav_macro.xlsx", clean = FALSE)
# names(macro_xl)

### Fieldwork and scores ----

macro_raw <- macro_xl |>
  select(-all_of(macro_species)) |>
  janitor::clean_names() |>
  select(
    fsn = fieldwork_seq_no,
    station_id,
    # plan_name,
    datetime = start_date_time,
    biotic_index_group_1_animals = no_of_group_1_animals,
    biotic_index_group_2_animals = no_of_group_2_animals,
    biotic_index_group_3_animals = no_of_group_3_animals,
    biotic_index_group_4_animals = no_of_group_4_animals_present,
    biotic_index_total_animals = total_animals,
    # ends_with("total_value"),
    biotic_index_score = index_score,
    group_seq_no,
    group_desc,
    fieldwork_comment
  ) |>
  mutate(datetime = with_tz(datetime, "America/Chicago")) |>
  mutate(date = as_date(datetime), .after = datetime) |>
  mutate(across(c(fsn, station_id), as.integer)) |>
  arrange(datetime)

macro_obs <- macro_raw |>
  filter(date <= MAX_DATE) |>
  drop_na(biotic_index_score) |>
  mutate(across(ends_with("_animals"), as.integer)) |>
  mutate(across(biotic_index_score, to_numeric)) |>
  # the form doesn't explicitly ask if you did a biotic index and if people click
  # and then unclick an animal it fills in zeros. We discard this as invalid data
  filter(biotic_index_total_animals > 0)

message(
  "Total macros: ",
  nrow(macro_obs),
  ", most recent: ",
  max(macro_obs$date),
  ". ",
  nrow(macro_raw) - nrow(macro_obs),
  " obs LOST due to filters."
)

# data check
if (FALSE) {
  macro_obs |> count(year(datetime))

  str(macro_obs)
  max(macro_obs$date)
  compare_fieldwork(baseline_obs, flow_obs, macro_obs)

  macro_obs |>
    filter(!(fsn %in% baseline_data$fsn)) |>
    view()

  macro_obs |>
    filter(biotic_index_total_animals == 0) |>
    view()

  macro_raw |> filter(station_id == 10057411) |> view()
  macro_raw |> filter(station_id == 10057413)
}


### Individual species counts ----

macro_species_counts <- macro_xl |>
  mutate(across(all_of(macro_species), ~ .x %in% c("Present", "YES"))) |>
  select(fsn = FIELDWORK_SEQ_NO, all_of(macro_species)) |>
  pivot_longer(
    all_of(macro_species),
    names_to = "species_name",
    values_to = "present"
  ) |>
  left_join(macro_index, join_by(species_name == dnr_parameter_description)) |>
  right_join(
    macro_obs |>
      select(fsn, station_id, datetime, date)
  ) |>
  relocate(station_id, datetime, date, .after = fsn) |>
  mutate(year = year(date), .after = date) |>
  mutate(species_name = factor(species_name, levels = macro_species)) |>
  arrange(datetime, species_name)


# data check
if (FALSE) {
  macro_species_counts |>
    select(-c(dnr_parameter_code, group)) |>
    pivot_wider(names_from = species_name, values_from = present)

  macro_species_counts |>
    filter(fsn %in% sample(fsn, 100)) |>
    write_csv("macro_data_subset.csv")
}


## Combine datasets ----

baseline_merged <- bind_rows(baseline_obs, flow_obs, macro_obs) |>
  merge_by("fsn")

# finalize baseline data
baseline_data <- baseline_merged |>
  arrange(datetime) |>
  mutate(
    year = year(date),
    month = month(date),
    day = day(date),
    yday = yday(date),
    .after = date
  ) |>
  left_join_at(stn_list, "station_id") |>
  left_join_at(
    macro_species_counts |>
      select(fsn, species_name, present) |>
      pivot_wider(names_from = species_name, values_from = present),
    "biotic_index_score"
  ) |>
  mutate(across(
    c(weather_last_2_days, fieldwork_comment, additional_comments),
    ~ str_to_sentence(str_squish(.x))
  )) |>
  mutate(
    weather_conditions = str_to_sentence(gsub("_", " ", weather_conditions))
  ) |>
  relocate(
    weather_conditions,
    weather_last_2_days,
    fieldwork_comment,
    additional_comments,
    group_seq_no,
    group_desc,
    .after = everything()
  ) |>
  arrange(datetime)


## Data validation ----

# look at histograms of each parameter to determine appropriate ranges
if (FALSE) {
  names(baseline_data)
  bd <- baseline_data

  # baseline params
  names(select_if(baseline_obs, is.numeric))
  validate(bd$air_temp) # convert some from F to C
  validate(bd$air_temp, -20, 40)
  validate(bd$water_temp) # convert some from F to C
  validate(bd$d_o)
  validate(bd$d_o, 0, 25)
  validate(bd$d_o_saturation)
  validate(bd$d_o_saturation, 0, 150)
  validate(bd$ph)
  validate(bd$ph, 4, 10)
  validate(bd$specific_cond)
  validate(bd$specific_cond, 0, 5)
  validate(bd$specific_cond, 5, 5000)
  validate(bd$transparency, 0, 120)

  # streamflow params
  names(select_if(flow_obs, is.numeric))
  validate(bd$stream_width)
  validate(log(bd$stream_width))
  validate(bd$stream_width, 1, 150)
  validate(bd$average_stream_depth)
  validate(bd$average_stream_depth, 0, 20)
  validate(bd$average_surface_velocity)
  validate(log(bd$average_surface_velocity))
  validate(bd$average_surface_velocity, 0, 6)
  validate(bd$streamflow)
  validate(log(bd$streamflow))
  validate(bd$streamflow, 0, 300)
  validate(bd$entered_streamflow)
  validate(log(bd$entered_streamflow))
  validate(bd$entered_streamflow, 0, 2000)
  validate(bd$calculated_streamflow)
  validate(log(bd$calculated_streamflow))
  validate(bd$calculated_streamflow, 0, 300)
  validate(bd$corrected_streamflow)
  validate(log(bd$corrected_streamflow))
  validate(bd$corrected_streamflow, 0, 250)

  # macroinvertebrates
  validate(bd$biotic_index_total_animals)
  validate(bd$biotic_index_score)
}

# Validation rules, applied by validate_measures() in functions.R
#   min/max:   expected range. Values outside are KEPT but flagged as suspect
#   zero_ok:   whether exactly 0 is acceptable (if not, 0 is flagged)
#   hard_min/hard_max: physically impossible outside this range -> value REMOVED
#   fix:       automatic correction tried first, result is then re-checked
#     f_to_c   values above hard_max are assumed °F and converted to °C
#     ms_to_us values between 0 and min are assumed mS/cm -> µS/cm (0 removed)
#     cap_tube transparency capped at the tube length (replaces max per row)
validation_rules <- tribble(
  ~measure                     , ~min , ~max , ~zero_ok , ~hard_min , ~hard_max , ~fix       ,
  "air_temp"                   ,  -40 ,   40 , TRUE     ,       -50 ,        40 , "f_to_c"   ,
  "water_temp"                 ,   -1 ,   30 , TRUE     ,        -2 ,        40 , "f_to_c"   ,
  "d_o"                        ,    0 ,   25 , FALSE    ,         0 , Inf       , NA         ,
  "d_o_saturation"             ,    0 ,  180 , FALSE    ,         0 , Inf       , NA         ,
  "ph"                         ,    4 ,   11 , TRUE     ,         1 ,        14 , NA         ,
  "specific_cond"              ,    5 , 5000 , FALSE    ,         0 , Inf       , "ms_to_us" ,
  "transparency"               ,    0 ,  120 , TRUE     ,         0 , Inf       , "cap_tube" ,
  "stream_width"               ,    0 ,  150 , TRUE     ,         0 , Inf       , NA         ,
  "average_stream_depth"       ,    0 ,   20 , TRUE     ,         0 , Inf       , NA         ,
  "length_assessed"            ,    0 , Inf  , TRUE     ,         0 , Inf       , NA         ,
  "cross_sectional_area"       ,    0 , Inf  , TRUE     ,         0 , Inf       , NA         ,
  "average_surface_velocity"   ,    0 ,    6 , TRUE     ,         0 , Inf       , NA         ,
  "corrected_surface_velocity" ,    0 , Inf  , TRUE     ,         0 , Inf       , NA         ,
  "entered_streamflow"         ,    0 , 2000 , TRUE     ,         0 , Inf       , NA         ,
  "calculated_streamflow"      ,    0 , Inf  , TRUE     ,         0 , Inf       , NA         ,
  "corrected_streamflow"       ,    0 ,  250 , TRUE     ,         0 , Inf       , NA         ,
  "streamflow"                 ,    0 ,  300 , TRUE     ,         0 , Inf       , NA         ,
  "biotic_index_total_animals" ,    1 ,   22 , FALSE    ,         0 , Inf       , NA         ,
  "biotic_index_score"         ,    1 ,    4 , FALSE    ,         0 , Inf       , NA         ,
  # derived DO checks, computed after the values above are cleaned (2nd pass)
  "calc_do_sat_conc"           ,    0 ,   25 , FALSE    , -Inf      , Inf       , NA         ,
  "calc_do_sat"                ,    0 ,  180 , FALSE    , -Inf      , Inf       , NA         ,
  "calc_do_sat_diff"           ,  -20 ,   20 , TRUE     , -Inf      , Inf       , NA
)

# a fieldwork event "has baseline data" if any of these have a value
key_measures <- c(
  "air_temp",
  "water_temp",
  "d_o",
  "d_o_saturation",
  "calc_do_sat_conc",
  "calc_do_sat",
  "calc_do_sat_diff",
  "ph",
  "specific_cond",
  "transparency",
  "stream_width",
  "average_stream_depth",
  "average_surface_velocity",
  "entered_streamflow",
  "corrected_streamflow",
  "streamflow",
  "biotic_index_total_animals",
  "biotic_index_score"
)

# calculate saturated DO mg/L at a given water temp (C) using APHA equation
# mean station elevation in WI = 850
calc_do_full_sat <- function(t, elevation_ft = 0) {
  # Return NA for non-valid water temperatures
  t[t < 0 | t > 30] <- NA

  # Base sea-level DO saturation (APHA polynomial)
  sat_sea_level <- 14.652 - 0.41022 * t + 7.991e-3 * t^2 - 7.7774e-5 * t^3

  # Atmospheric pressure correction ratio relative to sea level
  p_ratio <- (1 - (elevation_ft / 145442))^5.25588

  # Corrected saturation
  sat_corrected <- sat_sea_level * p_ratio

  round(sat_corrected, 2)
}

if (FALSE) {
  calc_do_full_sat(20)
  calc_do_full_sat(20, 1000)
  expand_grid(temp = seq(0, 30, by = 0.1), elevation = c(0, 600, 1000, 1500)) |>
    mutate(do = calc_do_full_sat(temp, elevation)) |>
    ggplot(aes(x = temp, y = do, color = elevation)) +
    geom_point()
  baseline_data |>
    mutate(
      do_sat_conc = calc_do_full_sat(water_temp),
      do_sat_calc = 100 * d_o / do_sat_conc,
      do_sat_diff = d_o_saturation - do_sat_calc,
      .after = d_o
    ) |>
    ggplot(aes(do_sat_calc)) +
    geom_histogram()
}

calc_rules <- validation_rules |> filter(str_starts(measure, "calc_"))
measured_rules <- validation_rules |> filter(!str_starts(measure, "calc_"))

# pass 1: validate the measured values (converts °F, removes impossible values)
validation_measured <- validate_measures(baseline_data, measured_rules)

baseline_validated <- baseline_data |>
  select(-all_of(measured_rules$measure)) |>
  left_join(
    validation_measured |>
      select(fsn, measure, new_value) |>
      pivot_wider(names_from = measure, values_from = new_value),
    by = "fsn"
  ) |>
  select(all_of(names(baseline_data))) |>
  # pass 2: DO consistency, calculated from the *cleaned* temps and DO so that
  # temps entered in °F still get checked
  mutate(
    calc_do_sat_conc = calc_do_full_sat(
      water_temp,
      coalesce(elevation_ft, 700) # sub in for missing elevations probably just outside the DEM
    ),
    calc_do_sat = round(100 * d_o / calc_do_sat_conc),
    calc_do_sat_diff = round(d_o_saturation - calc_do_sat, 2)
  )

validation_calc <- validate_measures(baseline_validated, calc_rules)

# one row per fsn x measure, with status and message for anything not OK
baseline_validation <- bind_rows(validation_measured, validation_calc)

# calc_do_* values are only flagged, never changed, so just add the messages
baseline_clean <- baseline_validated |>
  left_join(
    baseline_validation |>
      summarize(
        data_validation = paste(na.omit(msg), collapse = "; "),
        .by = fsn
      ) |>
      mutate(across(data_validation, ~ if_else(.x == "", "OK", .x))),
    by = "fsn"
  )

# data check
if (FALSE) {
  baseline_validation |> count(measure, status)
  baseline_validation |> filter(status == "removed")
  baseline_validation |> filter(status == "flagged")

  baseline_clean |>
    filter(str_detect(data_validation, "specific_cond")) |>
    pull(data_validation)

  baseline_clean |>
    filter(specific_cond == 0)

  # check for problems with baseline stations
  baseline_clean |>
    distinct(station_id, latitude, longitude, wbic) |>
    validate_stns()

  # how often are people using 100cm transparency tubes
  baseline_clean |>
    filter(year >= 2023) |>
    count(transparency_tube_length)
}


## Data checks ----

# any missing lat/lng?
baseline_stns_invalid <- baseline_clean |>
  distinct(station_id, .keep_all = TRUE) |>
  filter(is.na(latitude) | is.na(longitude))
if (nrow(baseline_stns_invalid) > 0) {
  cat("Baseline stations without lat/lng:\n")
  warning("Some baseline stations are missing coordinates!")
  print(baseline_stns_invalid)
}


# find stations where all FSNs in a year have no baseline data and drop them
has_key_baseline_data <- baseline_clean |>
  select(station_id, year, fsn, all_of(key_measures)) |>
  pivot_longer(all_of(key_measures)) |>
  summarize(
    has_baseline = sum(!is.na(value)) > 0,
    .by = c(station_id, year, fsn)
  ) |>
  mutate(valid_year = any(has_baseline), .by = c(station_id, year)) |>
  filter(valid_year)

valid_fsn <- has_key_baseline_data$fsn

invalid_baseline_data <- baseline_data |>
  filter(!(fsn %in% valid_fsn))

# message says how many baseline fieldworks will be dropped due to lack of data
message(
  nrow(baseline_data) - length(valid_fsn),
  " fieldwork events will be dropped due to having no baseline data"
)


## Export baseline data ----

baseline_final <- baseline_clean |>
  arrange(station_id, date) |>
  filter(fsn %in% valid_fsn) |>
  drop_na(latitude, longitude)

baseline_final |>
  write_csv(data_dir("baseline_data.csv"), na = "")

macro_species_counts |>
  write_csv(data_dir("macro_species_counts.csv"), na = "")


## Validation report ----
# One row per problem a volunteer can fix in SWIMS, used to email volunteers
# (see data-validation/volunteer_emails.R). Built from baseline_validation, not
# baseline_final, so problems in fieldwork that was later dropped still show up.

measure_labels <- tribble(
  ~measure                     , ~label                       , ~units  ,
  "air_temp"                   , "Air temperature"            , "°C"    ,
  "water_temp"                 , "Water temperature"          , "°C"    ,
  "d_o"                        , "Dissolved oxygen"           , "mg/L"  ,
  "d_o_saturation"             , "DO saturation"              , "%"     ,
  "ph"                         , "pH"                         , ""      ,
  "specific_cond"              , "Specific conductance"       , "µS/cm" ,
  "transparency"               , "Transparency"               , "cm"    ,
  "stream_width"               , "Stream width"               , "ft"    ,
  "average_stream_depth"       , "Average stream depth"       , "ft"    ,
  "length_assessed"            , "Length assessed"            , "ft"    ,
  "cross_sectional_area"       , "Cross-sectional area"       , "sq ft" ,
  "average_surface_velocity"   , "Average surface velocity"   , "ft/s"  ,
  "corrected_surface_velocity" , "Corrected surface velocity" , "ft/s"  ,
  "entered_streamflow"         , "Streamflow (entered)"       , "cfs"   ,
  "calculated_streamflow"      , "Streamflow (calculated)"    , "cfs"   ,
  "corrected_streamflow"       , "Streamflow (calculated)"    , "cfs"   ,
  "streamflow"                 , "Streamflow"                 , "cfs"   ,
  "biotic_index_total_animals" , "Biotic index total animals" , ""      ,
  "biotic_index_score"         , "Biotic index score"         , ""      ,
  "do_consistency"             , "Dissolved oxygen"           , ""      ,
  "flow_calculation"           , "Streamflow"                 , ""
)

# SWIMS-calculated flow fields; "NaN"/"Infinity" there means missing inputs
swims_flow_calcs <- c(
  "calculated_streamflow",
  "corrected_streamflow",
  "corrected_surface_velocity",
  "cross_sectional_area",
  "average_surface_velocity"
)
flow_input_measures <- c(
  "stream_width",
  "average_stream_depth",
  "length_assessed",
  "average_surface_velocity"
)
swims_calc_text <- c("NaN", "Infinity", "-Infinity")

# what was actually typed into SWIMS for non-numeric entries
entered_text <- bind_rows(baseline_nan, flow_nan) |>
  distinct(fsn, measure = parameter, .keep_all = TRUE) |>
  select(fsn, measure, entered)

issues_all <- baseline_validation |>
  filter(!is.na(status)) |>
  left_join(entered_text, by = c("fsn", "measure")) |>
  mutate(
    swims_calc_failed = measure %in%
      swims_flow_calcs &
      status == "not_a_number" &
      (entered %in% swims_calc_text | measure != "average_surface_velocity")
  )

# SWIMS couldn't calculate flow: one issue per fieldwork, and only when the
# volunteer entered at least some flow inputs (otherwise flow just wasn't done)
flow_calc_fsn <- issues_all |>
  filter(swims_calc_failed) |>
  distinct(fsn) |>
  semi_join(
    baseline_clean |>
      filter(if_any(all_of(flow_input_measures), ~ !is.na(.x))),
    by = "fsn"
  )

issues_flow_calc <- baseline_clean |>
  filter(fsn %in% flow_calc_fsn$fsn) |>
  mutate(
    missing = pmap_chr(
      list(
        velocity_correction_factor,
        stream_width,
        average_stream_depth,
        length_assessed,
        average_surface_velocity
      ),
      \(vcf, w, d, l, v) {
        m <- c(
          "the velocity correction factor (0.8 for a rocky bottom or 0.9 for a muddy/sandy bottom)"[
            is.na(vcf)
          ],
          "stream width"[is.na(w) || w == 0],
          "stream depths"[is.na(d) || d == 0],
          "length assessed"[is.na(l) || l == 0],
          "float times"[is.na(v) || v == 0]
        )
        if (length(m) == 0) {
          return(NA_character_)
        }
        if (length(m) == 1) {
          m
        } else {
          paste(paste(m[-length(m)], collapse = ", "), "and", last(m))
        }
      }
    ),
    measure = "flow_calculation",
    status = "incomplete",
    problem = if_else(
      is.na(missing),
      "SWIMS couldn't calculate streamflow from the measurements entered. Please check the flow measurements.",
      paste0(
        "SWIMS couldn't calculate streamflow because ",
        missing,
        if_else(str_detect(missing, " and |depths$|times$"), " are", " is"),
        " missing or zero. Please fill in the missing values."
      )
    )
  ) |>
  select(fsn, measure, status, problem)

# DO % saturation doesn't match DO mg/L and water temp: one issue per fieldwork
issues_do <- issues_all |>
  filter(measure %in% c("calc_do_sat", "calc_do_sat_diff")) |>
  distinct(fsn) |>
  left_join(
    baseline_clean |>
      select(fsn, d_o, d_o_saturation, water_temp, calc_do_sat),
    by = "fsn"
  ) |>
  mutate(
    measure = "do_consistency",
    status = "flagged",
    problem = if_else(
      is.na(d_o_saturation),
      str_glue(
        "DO of {fmt_num(d_o)} mg/L at {fmt_num(water_temp)} °C works out to {fmt_num(calc_do_sat)}% saturation, which is unusual. Please double-check DO and water temperature."
      ),
      str_glue(
        "DO of {fmt_num(d_o)} mg/L at {fmt_num(water_temp)} °C works out to about {fmt_num(calc_do_sat)}% saturation, but {fmt_num(d_o_saturation)}% was entered. Please double-check DO (mg/L), DO saturation (%) and water temperature."
      )
    ),
    entered = str_glue(
      "{fmt_num(d_o)} mg/L, {coalesce(fmt_num(d_o_saturation), '-')}%, {fmt_num(water_temp)} °C"
    )
  ) |>
  select(fsn, measure, status, problem, entered)

# streamflow is derived from entered/corrected flow, so skip it if those are flagged
flow_flagged_fsn <- issues_all |>
  filter(
    measure %in%
      c("entered_streamflow", "corrected_streamflow") |
      fsn %in% flow_calc_fsn$fsn
  ) |>
  distinct(fsn)

issues_simple <- issues_all |>
  filter(
    !str_starts(measure, "calc_"),
    !swims_calc_failed,
    !(measure == "streamflow" & fsn %in% flow_flagged_fsn$fsn)
  ) |>
  left_join(measure_labels, by = "measure") |>
  mutate(
    val = str_squish(paste(fmt_num(value), units)),
    new_val = str_squish(paste(fmt_num(new_value), units)),
    problem = case_when(
      status == "not_a_number" & entered %in% swims_calc_text ~
        str_glue(
          "SWIMS couldn't calculate the {str_to_lower(label)} from the readings entered. Please check that all readings are filled in."
        ),
      status == "not_a_number" & !is.na(entered) ~
        str_glue(
          "Entered \"{entered}\", which isn't a number, so it isn't shown on the dashboard. Please enter a number only (no units or other text), or leave it blank if it wasn't measured."
        ),
      status == "not_a_number" ~
        "This value isn't a number, so it isn't shown on the dashboard.",
      status == "removed" & value == 0 ~
        str_glue(
          "A value of 0 isn't possible, so it isn't shown on the dashboard. If this wasn't measured, please leave it blank; otherwise please correct it."
        ),
      status == "removed" & fix %in% "f_to_c" ~
        str_glue(
          "{val} isn't possible, even if it was recorded in °F, so it isn't shown on the dashboard. Please check the value."
        ),
      status == "removed" ~
        str_glue(
          "{val} isn't possible (expected {range_text(hard_min, hard_max, units)}), so it isn't shown on the dashboard. Please check the value, or delete it if it wasn't measured."
        ),
      status == "converted" & fix %in% "f_to_c" ~
        str_glue(
          "{fmt_num(value)} looks like it was recorded in °F, so the dashboard shows it as {new_val}. Please change it to °C in SWIMS."
        ),
      status == "converted" & fix %in% "ms_to_us" ~
        str_glue(
          "{fmt_num(value)} looks like it was entered in mS/cm, so the dashboard shows it as {new_val}. Please enter specific conductance in µS/cm."
        ),
      status == "capped" ~
        str_glue(
          "{val} is more than the {fmt_num(tube_length)} cm tube length, so the dashboard shows {new_val}. Please check the transparency reading and tube length."
        ),
      status == "flagged" & value == 0 ~
        str_glue(
          "A value of 0 is unusual. If this wasn't measured, please leave it blank; otherwise please double-check it."
        ),
      status == "flagged" ~
        str_glue(
          "{val} is outside the usual range ({range_text(min, max, units)}). Please double-check it."
        )
    ),
    entered = coalesce(entered, val)
  ) |>
  select(fsn, measure, status, problem, entered, new_value)

validation_issues <- bind_rows(issues_simple, issues_do, issues_flow_calc) |>
  left_join(
    baseline_clean |>
      select(fsn, date, year, station_id, group_seq_no, group_desc),
    by = "fsn"
  ) |>
  left_join(
    stn_list |> select(station_id, station_name, station_seq_no),
    by = "station_id"
  ) |>
  left_join(
    measure_labels |> select(measure, parameter = label),
    by = "measure"
  ) |>
  mutate(
    problem = as.character(problem),
    entered = as.character(entered),
    dashboard_value = fmt_num(new_value),
    issue_id = paste(fsn, measure, entered, sep = "|"),
    fieldwork_url = as.character(swims_fw_url(fsn)),
    station_url = if_else(
      is.na(station_seq_no),
      NA,
      as.character(swims_stn_url(station_seq_no))
    ),
    in_dashboard = fsn %in% baseline_final$fsn
  ) |>
  select(
    issue_id,
    year,
    date,
    fsn,
    station_id,
    station_name,
    station_seq_no,
    group_seq_no,
    group_desc,
    measure,
    parameter,
    entered,
    dashboard_value,
    status,
    problem,
    fieldwork_url,
    station_url,
    in_dashboard
  ) |>
  arrange(desc(year), group_desc, date, station_id, parameter)

validation_issues |>
  write_excel_csv("data-validation/validation_issues.csv", na = "")

message(
  "Validation issues: ",
  nrow(validation_issues),
  " in ",
  n_distinct(validation_issues$fsn),
  " fieldwork events (",
  sum(validation_issues$year == year(MAX_DATE), na.rm = TRUE),
  " in ",
  year(MAX_DATE),
  ")"
)

# fieldwork that never makes it to the dashboard, for staff (not volunteers)
dropped_fieldwork <- bind_rows(
  baseline_clean |>
    filter(!(fsn %in% valid_fsn)) |>
    mutate(reason = "no baseline values at this station in this year"),
  baseline_clean |>
    filter(fsn %in% valid_fsn, is.na(latitude) | is.na(longitude)) |>
    mutate(reason = "station has no coordinates")
) |>
  select(fsn, date, station_id, station_name, group_desc, reason) |>
  arrange(desc(date))

dropped_fieldwork |>
  write_excel_csv("data-validation/dropped_fieldwork.csv", na = "")

# data check
if (FALSE) {
  validation_issues |> count(year, measure, status) |> print(n = 100)
  validation_issues |> filter(year == 2026) |> view()
  dropped_fieldwork |> count(reason)
}


# PEOPLE =======================================================================

ip_groups <- load_xl("wav_all_ip_groups.xlsx")
ip <- load_xl("wav_all_ip.xlsx")

submitter_list <- baseline_final |>
  distinct(year, fsn, group_seq_no) |>
  left_join(ip_groups, relationship = "many-to-many") |>
  drop_na(interested_party_seq_no) |>
  summarize(
    n_fieldwork = n(),
    fieldwork_ids = paste(unique(fsn), collapse = ", "),
    .by = c(year, interested_party_seq_no, last_name, first_name, primary_email)
  ) |>
  arrange(desc(year), last_name, first_name)

submitter_list |> write_csv("submitter_list.csv", na = "")

# group_descs <- ip_groups |>
#   summarize(
#     group_desc = paste(unique(str_to_title(paste(first_name, last_name))), collapse = ", "),
#     .by = group_seq_no
#   )

# NUTRIENT / PHOSPHORUS ========================================================

nutrient_xl <- load_xl("wav_nutrient.xlsx")
names(nutrient_xl)

# tp data in units of mg/L = ppm
nutrient_raw <- nutrient_xl |>
  select(
    fsn = fieldwork_seq_no,
    datetime = start_date_time,
    collector_name,
    result_value_no,
    station_id,
    plan_id,
    plan_name
  ) |>
  drop_na(result_value_no) |>
  mutate(across(c(fsn, station_id), as.integer)) |>
  mutate(
    result_value_no = result_value_no |>
      str_replace_all(fixed("*"), "") |>
      str_replace_all("ND", "0") |>
      to_numeric()
  ) |>
  rename(tp = result_value_no) |>
  mutate(
    datetime = with_tz(datetime, "America/Chicago"),
    date = as_date(datetime),
    year = year(date),
    month = month(date),
    month_name = month.name[month],
    .after = datetime
  ) |>
  arrange(datetime)

nutrient_obs <- nutrient_raw |>
  merge_by("fsn") |>
  filter(month %in% 5:10, date <= MAX_DATE) |>
  left_join_at(stn_list, "station_id") |>
  drop_na(latitude, longitude, tp)

message(
  "Total nutrient: ",
  nrow(nutrient_obs),
  ", most recent: ",
  max(nutrient_obs$date)
)

# data check
if (FALSE) {
  nutrient_obs |> count(year(datetime))

  # count of plan_ids
  nutrient_obs |> count(plan_id, sort = TRUE)

  # non-cbsm plans
  nutrient_obs |>
    filter(!str_detect(plan_id, "CBSM")) |>
    count(plan_id, sort = TRUE)

  # by month
  nutrient_obs |> count(month, month_name)

  # highest phosphorus
  nutrient_obs |> slice_max(tp, n = 5)

  hist(nutrient_obs$tp)
  hist(hour(nutrient_obs$datetime))
  tz(nutrient_obs$datetime)

  validate(log(nutrient_obs$tp))

  nutrient_obs |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    ggplot() +
    geom_sf(data = counties, fill = "grey", lwd = .25) +
    geom_sf(
      aes(fill = log1p(tp * 1000 + 1)),
      shape = 21,
      size = 2,
      alpha = .5
    ) +
    scale_fill_distiller(palette = "Spectral") +
    facet_wrap(~year) +
    theme(legend.position = "inside", legend.position.inside = c(.85, .15))
}

## Export nutrient data ----

tp_final <- nutrient_obs |>
  select(-datetime) |>
  arrange(station_id, date) |>
  filter(tp < 5) # for now to catch bad data

tp_final |> write_csv(data_dir("tp_data.csv"), na = "")


# THERMISTORS ==================================================================

# see Thermistors project

therm_inventory <- read_csv(data_dir("therm_inventory.csv"))
# hobo_data <- read_csv(data_dir("therm_data.csv.gz"))

# must be TRUE
# all(unique(hobo_data$station_id) %in% therm_inventory$station_id)
# all(unique(hobo_data$station_id) %in% stn_list$station_id)

# STATIONS EXPORT ==============================================================

# generate station lists
stn_ids <- sort(unique(c(
  baseline_final$station_id,
  tp_final$station_id,
  therm_inventory$station_id
)))

stn_list_keep <- stn_list |>
  filter(station_id %in% stn_ids) |>
  mutate(
    station_name = str_squish(str_replace_all(
      station_name,
      "[^[:alnum:][:punct:] ]",
      ""
    ))
  )

if (FALSE) {
  stn_list_keep |> filter(is.na(station_name))
  stn_list_keep |> validate_stns()
}

## Make sure there are no stations missing from the list ----
stopifnot(
  "baseline" = baseline_final |>
    count(station_id, sort = T) |>
    filter(!(station_id %in% stn_list$station_id)) |>
    nrow() ==
    0,
  "nutrient" = tp_final |>
    count(station_id, sort = T) |>
    filter(!(station_id %in% stn_list$station_id)) |>
    nrow() ==
    0,
  "therm" = therm_inventory |>
    count(station_id, sort = T) |>
    filter(!(station_id %in% stn_list$station_id)) |>
    nrow() ==
    0
)

## Plot stations on a map ----
if (FALSE) {
  stn_list_keep |>
    count(county_name, sort = T) |>
    print(n = 100)

  stn_list_keep |>
    mutate(label = paste0("[", station_id, "] ", station_name)) |>
    leaflet() |>
    addTiles() |>
    addCircleMarkers(
      label = ~label,
      radius = 1,
      opacity = 1,
      fill = F
    ) |>
    addMarkers(
      label = ~label,
      clusterOptions = markerClusterOptions()
    )
}

## Export station list ----
stn_list_keep |> write_csv(data_dir("stn_list.csv"), na = "")


# SAVE ----
save.image()
