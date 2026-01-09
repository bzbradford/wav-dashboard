
# data prep for WAV dashboard


library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(sf)
library(leaflet)



# Functions --------------------------------------------------------------------

data_dir <- function(f) {
  file.path("../data", f)
}

# update dir when new data
load_xl <- function(fname, clean = TRUE) {
  f <- file.path("swims", fname)
  f.mtime <- file.mtime(f)
  df <- read_excel(f, na = c("", "NA"))
  if (clean) df <- clean_names(df)
  str(df)
  message("Loaded data from '", f, "' which was last modified ", f.mtime, " (", format(now() - f.mtime), " ago)")
  df
}

# a left join but places all joined columns after the join column
left_join_at <- function(x, y, after = NULL, ...) {
  df <- left_join(x, y, ...)
  if (!is.null(after)) {
    nm <- setdiff(names(y), names(x))
    df <- relocate(df, all_of(nm), .after = after[1])
  }
  df
}

#' coalesce values within columns, grouping by a common variable like the fieldwork
#' sequence number. Used when merging data from different sources that may have partial
#' or overlapping data. Will retain the first row in each column by group that has a
#' valid value
merge_by <- function(df, by) {
  data.table::setDT(df) |>
    _[, lapply(.SD, \(x) x[!is.na(x)][1]), by = by] |>
    as_tibble()
}

# restrict a value to a given range
clamp <- function(x, lower = x, upper = x, na.rm = F) {
  pmax(lower, pmin(upper, x, na.rm = na.rm), na.rm = na.rm)
}

# replaces out of range values with NA
trim <- function(x, low = NULL, high = NULL, msg = FALSE) {
  if (!is.null(low)) x[x < low] <- NA
  if (!is.null(high)) x[x > high] <- NA
  x
}

# convert temperature
f_to_c <- function(x, digits = 1) {
  round((x - 32) * (5 / 9), digits)
}




# SHAPEFILES ===================================================================

quickmap <- function(shape) {
  message("Shape has ", nrow(shape), " objects and ", format(mapview::npts(shape), big.mark = ","), " vertices")
  leaflet() %>%
    addTiles() %>%
    addPolygons(
      data = shape,
      color = "black",
      weight = 2,
      opacity = .5,
      fillColor = "grey",
      fillOpacity = .1
    )
}

## Counties ----

counties <- read_sf("shp/wi-county-bounds.geojson") %>%
  clean_names("big_camel") %>%
  st_make_valid() %>%
  select(
    CountyName,
    DnrRegion = DnrRegionName,
    geometry
  )

counties.simp <- rmapshaper::ms_simplify(counties, .25)

if (F) {
  quickmap(counties)
  quickmap(counties.simp)
}


## NKEs ----

nkes <- read_sf("shp/wi-nke-plans-2022.geojson") %>%
  clean_names("big_camel") %>%
  st_make_valid() %>%
  drop_na(PlanId)

nkes.simp <- rmapshaper::ms_simplify(nkes, .25)

nke_data <- nkes %>%
  select(
    nke_plan_name = PlanName,
    nke_plan_purpose = PurposeDe,
    nke_plan_objective = Objective,
    nke_start = StartDate,
    nke_end = EndDate
  ) %>%
  mutate(across(where(is_character), ~ str_to_sentence(str_trim(gsub("[\r\n]", "", .x)))))

if (F) {
  quickmap(nkes)
  quickmap(nkes.simp)
}


## Watersheds ----
# transform to 3071 (WTM) for faster joining

# huc6 basins
huc6.wtm <- read_sf("shp/wi-huc06-basins.geojson") %>%
  clean_names(case = "big_camel") %>%
  st_make_valid() %>%
  st_transform(3071)

# load huc8 subbasins and join huc6 info
huc8.wtm <- read_sf("shp/wi-huc08-subbasins.geojson") %>%
  clean_names(case = "big_camel") %>%
  st_make_valid() %>%
  st_transform(3071) %>%
  select(-ShapeLeng) %>%
  st_join(huc6.wtm, largest = T) %>%
  select(
    Huc8Code, Huc8Name,
    MajorBasin,
    Area = ShapeArea,
    geometry
  )

# load huc10 watersheds and join huc8 info
huc10.wtm <- read_sf("shp/wi-huc10-watersheds.geojson") %>%
  clean_names(case = "big_camel") %>%
  st_make_valid() %>%
  st_transform(3071) %>%
  st_join(select(huc8.wtm, -Area), largest = T) %>%
  select(
    Huc10Code, Huc10Name,
    Huc8Code, Huc8Name,
    MajorBasin,
    Area = ShapeArea,
    geometry
  )

# load huc12 watersheds and join huc10 info
huc12.wtm <- read_sf("shp/wi-huc12-subwatersheds.geojson") %>%
  clean_names(case = "big_camel") %>%
  st_make_valid() %>%
  st_transform(3071) %>%
  st_join(select(huc10.wtm, -Area), largest = T) %>%
  select(
    Huc12Code, Huc12Name,
    Huc10Code, Huc10Name,
    Huc8Code, Huc8Name,
    MajorBasin,
    Area = ShapeArea,
    geometry
  )

# convert to WGS
huc6 <- st_transform(huc6.wtm, 4326)
huc8 <- st_transform(huc8.wtm, 4326)
huc10 <- st_transform(huc10.wtm, 4326)
huc12 <- st_transform(huc12.wtm, 4326)

# DNR watersheds (approx HUC10)
dnr_watersheds <- read_sf("shp/wi-dnr-watersheds.geojson") %>%
  clean_names(case = "big_camel") %>%
  st_make_valid() %>%
  select(
    DnrWatershedCode = WshedCode,
    DnrWatershedName = WshedName,
    SizeAcres = WatershedSizeAcresAmt,
    SizeSqMiles = WatershedSizeSqMilesAmt,
    TotalLakeAcres = TotalLakeAcresAmt,
    TotalWetlandAcres = TotalWetlandAcresAmt,
    geometry
  )

# simplify
huc8.simp <- rmapshaper::ms_simplify(huc8, .5)
huc10.simp <- rmapshaper::ms_simplify(huc10, .5)
huc12.simp <- rmapshaper::ms_simplify(huc12, .5)
dnr_watersheds.simp <- rmapshaper::ms_simplify(dnr_watersheds, .15)

# inspect
if (F) {
  quickmap(huc6)
  quickmap(huc8)
  quickmap(huc8.simp)
  quickmap(huc10)
  quickmap(huc10.simp)
  quickmap(huc12)
  quickmap(huc12.simp)
  quickmap(dnr_watersheds)
  quickmap(dnr_watersheds.simp)
}


## Major waterbodies ----
# Top 1000 waterbodies in the state by area, for use on the pdf reports

waterbodies <- read_sf("shp/wi-major-lakes.geojson")

if (F) {
  quickmap(waterbodies)
}


## Flowlines ----

flowlines <- read_sf("shp/wi-hydro-nhd-flowlines.gpkg")
head(flowlines)
str(flowlines)
sort(unique(flowlines$visibilityfilter))
flow2d <- flowlines %>%
  rename(geometry = geom) %>%
  st_zm() %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  arrange(desc(visibilityfilter)) %>%
  mutate(level = consecutive_id(visibilityfilter))

flowlines.simp <- flow2d %>%
  select(level, geometry) %>%
  filter(level <= 5) %>%
  rmapshaper::ms_simplify(.25)

rm(flowlines)
rm(flow2d)

if (F) {
  flow2d.simp %>%
    ggplot() +
    geom_sf(aes(color = factor(level))) +
    scale_color_brewer(palette = "Spectral", direction = -1)
}



## Export shapes ----

local({
  shapes <- list(
    counties = counties.simp,
    nkes = nkes.simp,
    huc8 = huc8.simp,
    huc10 = huc10.simp,
    huc12 = huc12.simp,
    dnr_watersheds = dnr_watersheds.simp,
    waterbodies = waterbodies,
    flowlines = flowlines.simp
  )
  for (shape in names(shapes)) {
    fname <- paste0(shape, ".rds")
    fpath <- data_dir(file.path("shp", fname))
    saveRDS(shapes[[shape]], fpath)
    message("Save shape => ", fpath)
  }
})



# STATIONS =====================================================================

validate_stns <- function(stns, return_valid = TRUE) {
  cat("Input stations:", nrow(stns), "\n")
  invalid <- list(
    missing_id = list(
      label = "Stations missing id",
      data = stns %>% filter(is.na(station_id))
    ),
    non_numeric_id = list(
      label = "Stations with letters in id",
      data = stns %>% filter(grepl("[a-zA-Z]+", station_id))
    ),
    missing_ll = list(
      label = "Stations missing latitude/longitude",
      data = stns %>% filter(is.na(latitude) | is.na(longitude))
    ),
    zero_ll = list(
      label = "Stations with zero latitude/longitude",
      data = stns %>% filter(latitude == 0 | longitude == 0)
    ),
    pos_ll = list(
      label = "Stations with positive longitude",
      data = stns %>% filter(longitude > 0)
    ),
    missing_wbic = list(
      label = "Stations missing WBIC",
      data = stns %>% filter(is.na(wbic))
    ),
    multiple_wbic = list(
      label = "Stations with multiple WBIC",
      data = stns %>% filter(grepl(",", wbic, fixed = T))
    )
  )

  # iterate each element of invalid and print the number of rows and the label
  for (inv in invalid) {
    if (nrow(inv$data) > 0) {
      cat(paste0("\n", inv$label, ": ", nrow(inv$data), "\n"))
      print(inv$data)
    }
  }

  # generate valid station list
  suppressWarnings({
    valid <- stns %>%
      filter(latitude != 0, longitude != 0) %>%
      mutate(longitude = -1 * abs(longitude)) %>%
      mutate(station_id = as.integer(station_id)) %>%
      mutate(wbic = as.integer(wbic)) %>%
      drop_na(station_id, latitude, longitude)
  })

  cat("\nValid stations:", nrow(valid), "\n")

  if (return_valid) valid else invalid
}


## Load ----

stn_xl <- load_xl("2_wav_all_stns.xlsx")

names(stn_xl)

# stn_xl %>% filter(!is.finite(as.numeric(station_id)))

stns_in <- stn_xl %>%
  select(
    station_id,
    station_name = primary_station_name,
    latitude = calc_ll_lat_dd_amt,
    longitude = calc_ll_long_dd_amt,
    wbic,
    waterbody = official_waterbody_name,
    station_type = station_type_code,
    county_name
  ) %>%
  mutate(station_id = as.integer(station_id)) %>%
  drop_na(station_id) %>%
  distinct(station_id, .keep_all = T) %>%
  arrange(station_id)

stn_corrections <- tribble(
  ~station_id, ~latitude, ~longitude,
  10055326, 44.048073, -88.688243,
  10060233, 45.998043, -89.404756,
  10060454, 45.823530, -92.012559
)


## Validation ----

stns_valid <- stns_in %>%
  bind_rows(stn_corrections) %>%
  merge_by("station_id") %>%
  validate_stns()

stns_invalid <- validate_stns(stns_in, F)

# optionally export invalid station lists
if (F) {
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
}

# leaflet
if (F) {
  valid_stns %>%
    leaflet() %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~longitude, lat = ~latitude,
      radius = 1, fillColor = "orange", weight = 0, fillOpacity = 1
    )
}


## Join shapefile data ----

stn_list.sf <- stns_valid %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>%
  select(-county_name) %>%
  # some sites are outside the strict state boundaries so we just join by nearest county
  st_join(counties %>% select(CountyName, DnrRegion), join = st_nearest_feature) %>%
  st_join(huc12 %>% select(-Area)) %>%
  st_join(dnr_watersheds %>% select(DnrWatershedCode, DnrWatershedName)) %>%
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
  ) %>%
  distinct(station_id, .keep_all = T)

stn_list <- stn_list.sf %>% st_set_geometry(NULL)

stn_list %>% filter(is.na(station_name))
stn_list %>% filter(is.na(county_name))


## Export full station list ----

stn_list %>% write_csv(data_dir("stn_list_full.csv"), na = "")



# PEOPLE =======================================================================

# ip_groups <- load_xl("1_wav_all_ip_groups.xlsx")
#
# group_descs <- ip_groups %>%
#   summarize(
#     group_desc = paste(unique(str_to_title(paste(first_name, last_name))), collapse = ", "),
#     .by = group_seq_no
#   )



# BASELINE =====================================================================

compare_fieldwork <- function(df1, df2) {
  message("Total: ", length(unique(c(df1$fsn, df2$fsn))))
  message("Shared: ", sum(df1$fsn %in% df2$fsn))
  message("Only in ", deparse(substitute(df1)), ": ", sum(!(df1$fsn %in% df2$fsn)))
  message("Only in ", deparse(substitute(df2)), ": ", sum(!(df2$fsn %in% df1$fsn)))
}

## Load baseline ----

baseline_xl <- load_xl("3_wav_baseline_fw.xlsx")
# names(baseline_xl)

baseline_obs <- baseline_xl %>%
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
  ) %>%
  mutate(datetime = with_tz(datetime, "America/Chicago")) %>%
  mutate(date = as_date(datetime), .after = datetime) %>%
  mutate(suppressWarnings(across(c(fsn, station_id), as.integer))) %>%
  mutate(suppressWarnings(across(c(air_temp, water_temp, d_o, d_o_saturation, ph, specific_cond, transparency, transparency_tube_length), as.numeric))) %>%
  distinct(fsn, station_id, date, .keep_all = T) %>%
  filter(date >= "2015-1-1")

# data check
baseline_obs %>% count(year(date))
max(baseline_obs$date)

# test
if (F) {
  hist(log10(baseline_obs$specific_cond))

  baseline_obs %>%
    # filter(between(specific_cond, 5, 100)) %>%
    # filter(between(specific_cond, 0, 5)) %>%
    ggplot(aes(specific_cond)) +
    geom_histogram()
}



## Load streamflow ----

flow_xl <- load_xl("4_wav_flow_fw.xlsx")
# names(flow_xl)

flow_obs <- flow_xl %>%
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
  ) %>%
  mutate(datetime = with_tz(datetime, "America/Chicago")) %>%
  mutate(date = as_date(datetime), .after = datetime) %>%
  mutate(across(c(fsn, station_id), as.integer)) %>%
  mutate(suppressWarnings(across(stream_width:entered_streamflow, as.numeric))) %>%
  # need to turn NaNs into Inf so they don't get treated as NA
  mutate(across(where(is.numeric), ~if_else(is.nan(.x), Inf, .x))) %>%
  mutate(
    streamflow = coalesce(entered_streamflow, corrected_streamflow),
    .after = entered_streamflow
  ) %>%
  distinct(fsn, station_id, date, .keep_all = T) %>%
  filter(date >= "2015-1-1")

# data check
flow_obs %>% count(year(datetime))
max(flow_obs$date)
compare_fieldwork(baseline_obs, flow_obs)

# test
if (F) {
  # check for infinity
  flow_obs %>% filter(is.infinite(streamflow))
  flow_obs %>% filter(is.nan(streamflow))
  flow_obs %>% filter(if_any(where(is.numeric), is.infinite)) %>% view()
  flow_obs %>% filter(if_any(where(is.numeric), is.nan)) %>% view()

  flow_obs %>% filter(station_id == 10059011) %>% view()

  # shows need to set timezone correctly
  tz(flow_obs$datetime)
  hist(hour(flow_obs$datetime))

  flow_obs$datetime %>%
    with_tz("America/Chicago") %>%
    hour() %>%
    hist()
}



## Load macros ----

macro_index <- read_csv("macro_species_index.csv")
macro_species <- macro_index %>%
  drop_na(group) %>%
  pull(dnr_parameter_description)

macro_xl <- load_xl("8_wav_macros_fw.xlsx", clean = F)
# names(macro_xl)

### Fieldwork and scores ----
macro_obs <- macro_xl %>%
  select(-all_of(macro_species)) %>%
  janitor::clean_names() %>%
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
  ) %>%
  mutate(datetime = with_tz(datetime, "America/Chicago")) %>%
  mutate(date = as_date(datetime), .after = datetime) %>%
  drop_na(biotic_index_score) %>%
  mutate(across(c(fsn, station_id, ends_with("_animals")), as.integer)) %>%
  mutate(across(biotic_index_score, as.numeric)) %>%
  arrange(datetime) %>%
  filter(date >= "2015-1-1") %>%
  # the form doesn't explicitly ask if you did a biotic index and if people click
  # and then unclick an animal it fills in zeros. We discard this as invalid data
  filter(biotic_index_total_animals > 0)

# data check
macro_obs %>% count(year(datetime))
max(macro_obs$date)
compare_fieldwork(baseline_obs, macro_obs)

# test
if (F) {
  str(macro_obs)

  macro_obs %>%
    filter(!(fsn %in% baseline_data$fsn)) %>%
    view()

  macro_obs %>%
    filter(biotic_index_total_animals == 0) %>% view()
}


### Individual species counts ----
macro_species_counts <- macro_xl %>%
  mutate(across(all_of(macro_species), ~ .x %in% c("Present", "YES"))) %>%
  select(fsn = FIELDWORK_SEQ_NO, all_of(macro_species)) %>%
  pivot_longer(
    all_of(macro_species),
    names_to = "species_name",
    values_to = "present"
  ) %>%
  left_join(macro_index, join_by(species_name == dnr_parameter_description)) %>%
  right_join({
    macro_obs %>%
      select(fsn, station_id, datetime, date)
  }) %>%
  relocate(station_id, datetime, date, .after = fsn) %>%
  mutate(year = year(date), .after = date) %>%
  mutate(species_name = factor(species_name, levels = macro_species)) %>%
  arrange(datetime, species_name)

macro_species_counts %>% write_csv(data_dir("macro_species_counts.csv"), na = "")

# data check
if (F) {
  macro_species_counts %>%
    select(-c(dnr_parameter_code, group)) %>%
    pivot_wider(names_from = species_name, values_from = present)

  macro_species_counts %>%
    filter(fsn %in% sample(fsn, 100)) %>%
    write_csv("macro_data_subset.csv")
}


## Combine datasets ----

baseline_merged <- bind_rows(baseline_obs, flow_obs, macro_obs) %>%
  merge_by("fsn")

# finalize baseline data
baseline_data <- baseline_merged %>%
  arrange(datetime) %>%
  mutate(
    year = year(date),
    month = month(date),
    day = day(date),
    yday = yday(date),
    .after = date
  ) %>%
  left_join_at(stn_list, "station_id") %>%
  left_join_at({
    macro_species_counts %>%
      select(fsn, species_name, present) %>%
      pivot_wider(names_from = species_name, values_from = present)
  }, "biotic_index_score") %>%
  mutate(across(c(weather_last_2_days, fieldwork_comment, additional_comments), ~ str_to_sentence(str_squish(.x)))) %>%
  mutate(weather_conditions = str_to_sentence(gsub("_", " ", weather_conditions))) %>%
  relocate(
    weather_conditions,
    weather_last_2_days,
    fieldwork_comment,
    additional_comments,
    group_seq_no,
    group_desc,
    .after = everything()
  ) %>%
  arrange(year, station_id, date)


## Data validation ----

validate <- function(x, low = NULL, high = NULL) {
  len <- length(x)
  nas <- sum(is.na(x))
  vals <- len - nas
  message("Values: ", len)
  message("NA: ", nas, sprintf(" (%.1f%%)", 100 * nas / len ))
  message("Numeric: ", vals, sprintf(" (%.1f%%)", 100 * vals / len))
  message(
    "Quantiles:\n  ",
    quantile(x, c(0, .01, .05, .5, .95, .99, .995, .999, 1), TRUE) %>%
      paste(names(.), ., sep = ": ", collapse = "\n  ")
  )
  if (!is.null(low)) message("x < ", low, ": ", sum(x < low, na.rm = T))
  if (!is.null(high)) message("x > ", high, ": ", sum(x > high, na.rm = T))
  x <- trim(x, low, high)
  hist(x)
  invisible(x)
}

# look at histograms of each parameter to determine appropriate ranges
if (F) {
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
  ~measure, ~min, ~max, ~action,
  "air_temp", -40, 40, "f_to_c",
  "water_temp", -40, 40, "f_to_c",
  "d_o", 0, 25, "remove",
  "d_o_saturation", 0, 150, "remove",
  "ph", 4, 10, "remove",
  "specific_cond", 5, 5000, "keep",
  "transparency", 0, 120, "cap",
  "stream_width", 0, 150, "keep",
  "average_stream_depth", 0, 20, "keep",
  "average_surface_velocity", 0, 6, "keep",
  "entered_streamflow", 0, 2000, "keep",
  # "calculated_streamflow", 0, 300, "keep",
  "corrected_streamflow", 0, 250, "keep",
  "streamflow", 0, 300, "keep",
  "biotic_index_total_animals", 1, 22, "keep",
  "biotic_index_score", 1, 4, "keep"
)

# apply action to out of range values
baseline_validation <- baseline_data %>%
  select(fsn, all_of(validation_actions$measure)) %>%
  pivot_longer(cols = -fsn, names_to = "measure") %>%
  left_join(validation_actions) %>%
  mutate(
    valid = if_else(
      min == 0,
      value > 0 & value <= max,
      between(value, min, max)
    ),
    new_value = case_when(
      valid ~ value,
      is.infinite(value) ~ NA,
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
      is.na(value) ~ NA,
      is.infinite(value) ~ "Inf or NaN",
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
baseline_clean <- baseline_data %>%
  select(-all_of(validation_actions$measure)) %>%
  left_join({
    baseline_validation %>%
      select(fsn, measure, new_value) %>%
      pivot_wider(names_from = measure, values_from = new_value)
  }) %>%
  left_join({
    baseline_validation %>%
      summarize(data_validation = paste(na.omit(msg), collapse = "; "), .by = fsn) %>%
      mutate(across(data_validation, ~if_else(.x == "", "OK", .x)))
  }) %>%
  select(all_of(names(baseline_data)), everything())

# test
if (F) {
  baseline_validation %>%
    filter(str_detect(msg, "NaN"))

  baseline_clean %>%
    filter(str_detect(data_validation, "specific_cond")) %>%
    pull(data_validation)

  baseline_clean %>%
    filter(specific_cond == 0)
}


## Data checks ----

# any missing lat/lng?
baseline_clean %>%
  distinct(station_id, .keep_all = TRUE) %>%
  filter(is.na(latitude) | is.na(longitude))

# find stations where all FSNs in a year have no baseline data and drop them
has_key_baseline_data <- baseline_clean %>%
  select(station_id, year, fsn, all_of(validation_actions$measure)) %>%
  pivot_longer(all_of(validation_actions$measure)) %>%
  summarize(has_baseline = sum(!is.na(value)) > 0, .by = c(station_id, year, fsn)) %>%
  mutate(valid_year = any(has_baseline), .by = c(station_id, year)) %>%
  filter(valid_year)

valid_fsn <- has_key_baseline_data$fsn

invalid_baseline_data <- baseline_data %>%
  filter(!(fsn %in% valid_fsn))

# message says how many baseline fieldworks will be dropped due to lack of data
message(nrow(baseline_data) - length(valid_fsn), " fieldwork events will be dropped due to having no baseline data")


# check for problems with baseline stations
baseline_data %>%
  distinct(station_id, latitude, longitude, wbic) %>%
  validate_stns()


## Export baseline data ----

baseline_final <- baseline_clean %>%
  arrange(station_id, date) %>%
  filter(fsn %in% valid_fsn) %>%
  drop_na(latitude, longitude)

baseline_final %>% write_csv(data_dir("baseline_data.csv"), na = "")




# NUTRIENT =====================================================================

## SWIMS ----
# apparently this query doesn't select dnr parameter code = 665???
nutrient_xl <- load_xl("5_wav_nutrient_fw.xlsx")
names(nutrient_xl)

# tp data in units of mg/L = ppm
tp_data_wav <- nutrient_xl %>%
  filter(dnr_parameter_code == 665) %>%
  select(
    fsn = fieldwork_seq_no,
    station_id,
    plan_id, plan_name,
    collector_name,
    datetime = start_date_time,
    result_value_no
  ) %>%
  drop_na(result_value_no) %>%
  mutate(across(c(fsn, station_id), as.integer)) %>%
  mutate(tp = suppressWarnings(
    result_value_no %>%
      str_replace_all(fixed("*"), "") %>%
      str_replace_all("ND", "0") %>%
      as.numeric()
  )) %>%
  select(-result_value_no) %>%
  mutate(datetime = with_tz(datetime, "America/Chicago"))

tp_data_wav %>% count(year(datetime))

# test
if (F) {
  tp_data_wav %>% count(plan_id, sort = T)
  tp_data_wav %>% count(fsn, sort = T)
  tp_data_wav %>% filter(fsn == 358886153)
  tp_data_wav %>%
    filter(n() > 1, .by = c(fsn, datetime))

  tp_data_wav %>% filter(is.na(tp))
  hist(tp_data_wav$tp)
  hist(hour(tp_data_wav$datetime))
  tz(tp_data_wav$datetime)

  # count of plan_ids
  tp_data_wav %>% count(plan_id, sort = T)
}

# whitelist specific data collectors for non-CBSM projects
tp_data_wav_clean <- tp_data_wav %>%
  filter(
    !(plan_id == "West_06_CMP25" & !(collector_name %in% c("RETTA ISAACSON", "HEATHER WOOD", "HEATHER WOOD_0"))),
    !(plan_id == "Ashipun River_South_TWA_1_2025" & !(collector_name == "DALE CIRA"))
  )
message(nrow(tp_data_wav) - nrow(tp_data_wav_clean), " rows removed due to filter")


## MRK & TWA ----
# MRK 2024 data
tp_data_mrk <- "nutrient/MilwaukeeRiverkeeper_TotalPhosphorusData_2024.xlsx" %>%
  read_excel(na = c("", "NA")) %>%
  clean_names() %>%
  select(
    fsn = fieldwork_seq_no,
    station_id,
    datetime = sample_start_date_time,
    collector_name,
    tp = result_value_no
  ) %>%
  drop_na(tp) %>%
  mutate(across(c(fsn, station_id), as.integer)) %>%
  mutate(tp = as.numeric(gsub("ND", 0, tp))) %>%
  mutate(across(datetime, ~ parse_date_time(.x, "mdy HMS p"))) %>%
  mutate(datetime = force_tz(datetime, "America/Chicago"))

# data check
if (F) {
  hist(tp_data_mrk$tp)
  hist(hour(tp_data_mrk$datetime))
  tz(tp_data_mrk$datetime) # need to force_tz
}


# East TWA 2024 data & West CMP
# result units mg/L
tp_data_twa <-
  bind_rows(
    read_excel("nutrient/East_TWA_1_2024.xlsx", na = c("", "NA")),
    read_excel("nutrient/West_06_CMP25.xlsx", na = c("", "NA"))
  ) %>%
  clean_names() %>%
  filter(sample_dnr_parameter == "665") %>%
  select(
    fsn = fieldwork_seqno,
    datetime = start_date_time,
    station_id = fieldwork_station_id,
    collector_name = data_collector,
    tp = result
  ) %>%
  drop_na(tp) %>%
  mutate(across(c(fsn, station_id), as.integer)) %>%
  mutate(tp = as.numeric(gsub("ND", 0, tp))) %>%
  filter(!(station_id %in% c(33223, 10037357, 10037358, 10056308, 173209, 183005))) %>%
  mutate(across(datetime, ~ parse_date_time(.x, "mdy HMS p"))) %>%
  mutate(datetime = force_tz(datetime, "America/Chicago"))

# data check
if (F) {
  tp_data_twa %>% count(station_id, sort = T)
  hist(tp_data_twa$tp)
  hist(hour(tp_data_twa$datetime))
  tz(tp_data_twa$datetime) # need to force_tz
}

## Merge data ----

tp_data <-
  bind_rows(
    tp_data_wav_clean,
    tp_data_mrk,
    tp_data_twa
  ) %>%
  merge_by("fsn") %>%
  mutate(
    across(collector_name, str_to_title),
    date = as_date(datetime),
    year = year(date),
    month = month(date),
    month_name = month.name[month],
    .after = datetime
  ) %>%
  filter(year >= 2015) %>%
  filter(month %in% 5:10) %>%
  left_join_at(stn_list, "station_id") %>%
  drop_na(latitude, longitude, tp)


## Data check ----

tp_data %>% count(year)

if (F) {
  tp_data %>% count(month)
  tp_data %>% slice_max(tp, n = 5)

  ggplot(tp_data) +
    geom_histogram(aes(x = tp)) +
    scale_x_sqrt()

  tp_data %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    ggplot() +
    geom_sf(data = counties, fill = "grey", lwd = .25) +
    geom_sf(aes(fill = log1p(tp * 1000 + 1)), shape = 24, size = 3) +
    scale_fill_distiller(palette = "Spectral") +
    facet_wrap(~year) +
    theme(legend.position = "inside", legend.position.inside = c(.85, .15))
}


## Export nutrient data ----

tp_final <- tp_data %>%
  select(-datetime) %>%
  arrange(station_id, date) %>%
  filter(tp < 5) # for now to catch bad data

tp_final %>% write_csv(data_dir("tp_data.csv"), na = "")



# THERMISTOR ===================================================================

# see thermistors.R

therm_inventory <- read_csv(data_dir("therm_inventory.csv"))
hobo_data <- read_csv(data_dir("therm_data.csv.gz"))

# must be TRUE
all(unique(hobo_data$station_id) %in% therm_inventory$station_id)
all(unique(hobo_data$station_id) %in% stn_list$station_id)



# Finalize station list ========================================================

# generate station lists
stn_ids <- sort(unique(c(
  baseline_final$station_id,
  tp_final$station_id,
  hobo_data$station_id
)))

stn_list_keep <- stn_list %>%
  filter(station_id %in% stn_ids) %>%
  mutate(station_name = str_squish(str_replace_all(station_name, "[^[:alnum:][:punct:] ]", "")))

stn_list_keep %>%
  filter(is.na(station_name))

if (F) {
  stn_list_keep %>% validate_stns()
}


## Check baseline ----

baseline_final %>%
  count(station_id, sort = T) %>%
  filter(!(station_id %in% stn_list$station_id))


## Check nutrient ----

tp_final %>%
  count(station_id, sort = T) %>%
  filter(!(station_id %in% stn_list$station_id))


## Check thermistor ----

hobo_data %>%
  count(station_id, sort = T) %>%
  filter(!(station_id %in% stn_list$station_id))


## Plot stations on a map ----

if (F) {
  stn_list_keep %>%
    count(county_name, sort = T) %>%
    print(n = 100)

  stn_list_keep %>%
    mutate(label = paste0("[", station_id, "] ", station_name)) %>%
    leaflet() %>%
    addTiles() %>%
    addCircleMarkers(
      label = ~label,
      radius = 1, opacity = 1, fill = F
    ) %>%
    addMarkers(
      label = ~label,
      clusterOptions = markerClusterOptions()
    )
}


## Export station list ----

stn_list_keep %>% write_csv(data_dir("stn_list.csv"), na = "")



# Landcover data ---------------------------------------------------------------

# calculated landcover by watershed. See landcover project


# Macroinvertebrate test -------------------------------------------------------

## data exploration ----

macro_obs %>%
  ggplot(aes(x = biotic_index_score)) +
  geom_histogram() +
  facet_grid(year(datetime)~.)

macro_obs %>%
  ggplot(aes(x = year(datetime), y = total_value)) +
  geom_boxplot(aes(group = year(datetime))) +
  stat_summary() +
  geom_smooth()

macro_obs %>%
  ggplot(aes(x = total_value / total_animals)) +
  geom_histogram()

macro_obs %>%
  filter(station_id == sample(station_id, 1)) %>%
  ggplot(aes(x = datetime, y = index_score)) +
  geom_boxplot(aes(group = year)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", color = "blue", fill = "blue", alpha = .1) +
  geom_smooth(method = "gam", color = "red", fill = "red", alpha = .1) +
  scale_y_continuous(limits = c(0, 4)) +
  scale_x_date(date_breaks = "year", date_labels = "%Y")

# number of samplings per year
macro_obs %>%
  filter(year == 2024) %>%
  summarize(n = n(), .by = c(year, station_id)) %>%
  summarize(stations = n(), .by = c(year, n)) %>%
  mutate(pct = stations / sum(stations), .by = year) %>%
  ggplot(aes(x = factor(n), y = pct)) +
  geom_col() +
  geom_text(aes(label = scales::label_percent(1)(pct)), vjust = -.5)



## Validation ----

# Duplicate FSN 351071051 == 357587224
# how should we handle duplicate data? Manually delete the fieldwork in swims?


## plotly prototype ----

library(plotly)

hex_to_rgba <- function(hex, alpha = 1) {
  rgb <- col2rgb(hex)
  paste0("rgba(", rgb[1], ",", rgb[2], ",", rgb[3], ",", alpha, ")")
}

# Logic: 1=Sensitive (Blue), 4=Tolerant (Red), Invasive (Purple)
macro_groups <- c("Group 1", "Group 2", "Group 3", "Group 4", "Invasive")
# macro_colors <- c("#2196F3", "#4CAF50", "#FF9800", "#F44336", "#9C27B0")
# macro_desc   <- c("Sensitive", "Moderately sensitive", "Moderately tolerant", "Tolerant", "Tolerant")
macro_colortable <- tibble(
  group = macro_groups,
  base_color = c("#2196F3", "#4CAF50", "#FF9800", "#F44336", "#9C27B0"),
  description = c(
    "Group 1 (Sensitive)",
    "Group 2 (Moderately sensitive)",
    "Group 3 (Moderately tolerant)",
    "Group 4 (Tolerant)",
    "Invasive"
  )
) %>%
  expand_grid(present = c(T, F)) %>%
  mutate(
    z = (1:10 - .5) / 10,
    alpha = if_else(present, 1, .1),
    color = mapply(hex_to_rgba, base_color, alpha, USE.NAMES = F)
  )

# build plotly heatmap colorscale
colorscale <- list()
for (i in 1:10) {
  row <- slice(macro_colortable, i)
  colorscale[[length(colorscale) + 1]] <- list((i - 1) / 10, row$color)
  colorscale[[length(colorscale) + 1]] <- list(i / 10, row$color)
}


add_missing_years <- function(df) {
  bind_rows(df, tibble(
    year = setdiff(2015:2025, unique(df$year)),
    date = as_date(paste(year, "-1-1")),
    date_label = paste("(", year, ")")
  )) %>%
    arrange(date) %>%
    mutate(date_label = fct_inorder(date_label))
}

# select a station
selected_data <- macro_species_counts %>%
  filter(station_id == sample(station_id, 1)) %>%
  mutate(
    species_name = factor(species_name, macro_species),
    group = factor(group, macro_groups),
    status = if_else(present, "Present", "Absent")
  ) %>%
  left_join(macro_colortable, join_by(group, present))


# all observations
plot_data <- selected_data %>%
  mutate(
    date_label = format(date, "%b %d, %Y"),
    tooltip_text = str_glue("
      <b>{species_name}</b>
      {description}
      {date_label}
      {status}
    ")
  ) %>%
  add_missing_years()

# or summarize by year
plot_data <- selected_data %>%
  summarize(
    present = any(present),
    .by = c(year, group, species_name)
  ) %>%
  left_join(macro_colortable, join_by(group, present)) %>%
  mutate(
    date = as_date(paste0(year, "-1-2")),
    date_label = as.character(year),
    species_name = factor(species_name, macro_species),
    group = factor(group, macro_groups),
    status = if_else(present, "Present", "Absent"),
    tooltip_text = paste0(
      "Species: ", species_name,
      "<br>Year: ", year,
      "<br>Group: ", group,
      "<br>Status: ", status
    )
  ) %>%
  add_missing_years()

# plot it
plot_ly() %>%
  add_trace(
    data = plot_data,
    type = "heatmap",
    x = ~date_label,
    y = ~species_name,
    z = ~z,
    text = ~tooltip_text,
    hoverinfo = "text",
    colorscale = colorscale,
    showscale = F, # Hide the rainbow bar
    xgap = 1, ygap = 1
  ) %>%
  layout(
    title = "Macroinvertebrate Sampling: Presence/Absence Heatmap",
    xaxis = list(
      title = "",
      type = "category",
      categoryarray = levels(plot_data$date_label),
      categoryorder = "array",
      tickangle = -45,
      showgrid = F
    ),
    yaxis = list(
      title = "",
      type = 'category',
      categoryarray = rev(levels(plot_data$species_name)),
      categoryorder = "array",
      showgrid = F
    ),
    plot_bgcolor = "white",
    margin = list(t = 50, r = 10, b = 10, l = 10),
    legend = list(title = list(text = "Species Group"))
  ) %>%
  config(displayModeBar = F)


## ggplot prototype ----

p <- plot_data %>%
  ggplot(aes(x = date_label, y = species_name, text = tooltip_text)) +
  geom_tile(
    aes(fill = base_color, alpha = alpha, color = base_color),
    lwd = 0.25, width = 0.9, height = 0.9
  ) +
  scale_y_discrete(limits = rev, na.translate = F) +
  scale_fill_identity(aes(color = base_color), na.value = "transparent") +
  scale_color_identity(aes(color = base_color), na.value = "transparent") +
  labs(
    title = "Macroinvertebrate Sampling: Presence/Absence Heatmap",
    x = NULL, y = NULL,
    fill = "Species group", color = "Species group"
  ) +
  guides(alpha = guide_none()) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", linewidth = .25)
  )

# Convert to Interactive Plotly Figure
ggplotly(p, tooltip = "text")



# Misc/Test --------------------------------------------------------------------

# baseline_stns <- all_stns %>%
#   filter(station_id %in% baseline_data$station_id)

# tp_stns <- all_stns %>%
#   filter(station_id %in% tp_data$station_id)

all_stns %>%
  filter(station_id == 223252)

all_stns %>%
  group_by(latitude, longitude) %>%
  filter(n() > 1) %>%
  arrange(latitude, longitude)

baseline_stn_dupes <- baseline_stns %>%
  group_by(latitude, longitude) %>%
  filter(n() > 1) %>%
  arrange(latitude, longitude) %>%
  select(station_id, station_name, latitude, longitude, everything())

baseline_stn_dupes %>% write_csv("baseline station duplicates.csv")

baseline_final %>%
  filter(station_id %in% baseline_stn_dupes$station_id) %>%
  arrange(latitude, longitude) %>%
  select(station_id, station_name, latitude, longitude, everything()) %>%
  write_csv("baseline data assigned to duplicate stations.csv")

tp_stns %>%
  group_by(latitude, longitude) %>%
  filter(n() > 1) %>%
  arrange(latitude, longitude)


baseline_data %>%
  filter(station_id == 10040536, year == 2022)


baseline_final %>%
  group_by(station_id, year) %>%
  tally()


# compare 2024 nutrient datsets
wav_nutrient <- read_excel("nutrient/wav_nutrient_RRC_RKeep_20241210.xlsx", na = c("", "NA")) %>%
  clean_names() %>%
  filter(year(start_date_time) == 2024) %>%
  mutate(source = "WAV") %>%
  rename(
    station_latitude = calc_ll_lat_dd_amt,
    station_longitude = calc_ll_long_dd_amt
  )
mrk_nutrient <- read_excel("nutrient/MilwaukeeRiverkeeper_TP_2024.xlsx", na = c("", "NA")) %>%
  clean_names() %>%
  rename(
    start_date_time = sample_start_date_time,
    plan_id = project_no
  ) %>%
  mutate(across(start_date_time, ~ parse_date_time(.x, "mdyHMSp")))

combined_nutrient <- bind_rows(wav_nutrient, mrk_nutrient) %>%
  mutate(across(result_value_no, ~ gsub("ND", 0, .x) %>% as.numeric())) %>%
  replace_na(list(result_value_no = 0)) %>%
  select(source, everything()) %>%
  arrange(start_date_time, station_id)

combined_nutrient %>%
  group_by(start_date_time, station_id, result_value_no) %>%
  filter(n() > 1) %>%
  mutate(dupe_id = cur_group_id(), .before = 1) %>%
  write_csv("nutrient dupes.csv", na = "")

combined_nutrient %>%
  filter(n() == 1, .by = c(start_date_time, station_id, result_value_no)) %>%
  filter(source == "Milwaukee Riverkeeper") %>%
  write_csv("nutrient MRK uniques.csv", na = "")


arrange(desc(n)) %>%
  pivot_wider(names_from = "source", values_from = "n") %>%
  view()
