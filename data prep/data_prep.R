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
library(leaflet)

# load(".RData")
# save.image()

# in case query returns newer stuff we don't want yet
MAX_DATE <- as_date("2025-12-31")

# load functions
source("functions.R")

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
     10060454 , 45.823530 , -92.012559
)


## Validation ----
stns_valid <- stns_in |>
  bind_rows(stn_corrections) |>
  merge_by("station_id") |>
  validate_stns()

stns_invalid <- validate_stns(stns_in, F)

# data check
if (FALSE) {
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

## Join shapefile data ----
stn_list.sf <- stns_valid |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) |>
  select(-county_name) |>
  # some sites are outside the strict state boundaries so we just join by nearest county
  st_join(
    counties |> select(CountyName, DnrRegion),
    join = st_nearest_feature
  ) |>
  st_join(huc12 |> select(-Area)) |>
  st_join(dnr_watersheds |> select(DnrWatershedCode, DnrWatershedName)) |>
  select(
    station_id,
    station_name,
    latitude,
    longitude,
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
  distinct(station_id, .keep_all = T)

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


# PEOPLE =======================================================================

# ip_groups <- load_xl("1_wav_all_ip_groups.xlsx")
#
# group_descs <- ip_groups |>
#   summarize(
#     group_desc = paste(unique(str_to_title(paste(first_name, last_name))), collapse = ", "),
#     .by = group_seq_no
#   )

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
  filter(date >= "2015-1-1")

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
  filter(date >= "2015-1-1")

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

# apply validation to select columns
validation_actions <- tribble(
  ~measure                     , ~min , ~max , ~action  ,
  "air_temp"                   ,  -40 ,   40 , "f_to_c" ,
  "water_temp"                 ,  -40 ,   40 , "f_to_c" ,
  "d_o"                        ,    0 ,   25 , "remove" ,
  "d_o_saturation"             ,    0 ,  150 , "remove" ,
  "ph"                         ,    4 ,   11 , "remove" ,
  "specific_cond"              ,    5 , 5000 , "keep"   ,
  "transparency"               ,    0 ,  120 , "cap"    ,
  "stream_width"               ,    0 ,  150 , "keep"   ,
  "average_stream_depth"       ,    0 ,   20 , "keep"   ,
  "average_surface_velocity"   ,    0 ,    6 , "keep"   ,
  "entered_streamflow"         ,    0 , 2000 , "keep"   ,
  # "calculated_streamflow", 0, 300, "keep",
  "corrected_streamflow"       ,    0 ,  250 , "keep"   ,
  "streamflow"                 ,    0 ,  300 , "keep"   ,
  "biotic_index_total_animals" ,    1 ,   22 , "keep"   ,
  "biotic_index_score"         ,    1 ,    4 , "keep"
)

# apply action to out of range values
baseline_validation <- baseline_data |>
  select(fsn, all_of(validation_actions$measure)) |>
  pivot_longer(cols = -fsn, names_to = "measure") |>
  left_join(validation_actions) |>
  mutate(
    valid = if_else(
      min == 0,
      value > 0 & value <= max,
      between(value, min, max)
    ),
    new_value = case_when(
      valid ~ value,
      is.nan(value) | is.infinite(value) ~ NA,
      # convert specific_cond when likely using incorrect units
      measure == "specific_cond" ~ case_when(
        value == 0 ~ NA,
        value < 5 ~ value * 1000,
        TRUE ~ value
      ),
      action == "f_to_c" ~ f_to_c(value),
      action == "cap" ~ clamp(value, min, max),
      action == "remove" ~ NA,
      action == "keep" ~ value
    ),
    msg = case_when(
      valid ~ NA,
      is.nan(value) ~ "was not a number",
      is.na(value) ~ NA,
      action == "f_to_c" ~ str_glue("converted from {value} to {new_value}"),
      measure == "specific_cond" ~ case_when(
        value == 0 ~ "bad value 0 removed",
        value < 5 ~ str_glue("value {value} converted to {new_value}"),
        !valid ~ str_glue("value {value} suspect")
      ),
      action == "cap" ~ str_glue("value {value} clamped to {new_value}"),
      action == "remove" ~ str_glue("bad value {value} removed"),
      action == "keep" ~ str_glue("value {value} suspect")
    ),
    msg = if_else(is.na(msg), NA, paste(measure, msg))
  )

# put it back together and swap in validated values
baseline_clean <- baseline_data |>
  select(-all_of(validation_actions$measure)) |>
  left_join({
    baseline_validation |>
      select(fsn, measure, new_value) |>
      pivot_wider(names_from = measure, values_from = new_value)
  }) |>
  left_join({
    baseline_validation |>
      summarize(
        data_validation = paste(na.omit(msg), collapse = "; "),
        .by = fsn
      ) |>
      mutate(across(data_validation, ~ if_else(.x == "", "OK", .x)))
  }) |>
  select(all_of(names(baseline_data)), everything())

# data check
if (FALSE) {
  baseline_validation |> filter(str_detect(msg, "not a number"))
  baseline_validation |> filter(str_detect(msg, "bad value"))

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
stopifnot(
  baseline_clean |>
    distinct(station_id, .keep_all = TRUE) |>
    filter(is.na(latitude) | is.na(longitude)) |>
    nrow() ==
    0
)


# find stations where all FSNs in a year have no baseline data and drop them
has_key_baseline_data <- baseline_clean |>
  select(station_id, year, fsn, all_of(validation_actions$measure)) |>
  pivot_longer(all_of(validation_actions$measure)) |>
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
  filter(month %in% 5:10) |>
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

# stn_list_keep |> filter(is.na(station_name))
# stn_list_keep |> validate_stns()

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
