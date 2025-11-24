
# data prep for WAV dashboard


library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(sf)
library(leaflet)


data_dir <- function(f) {
  file.path("../data", f)
}

# DASHBOARD_DIR <- "../WAV Dashboard/data/"

# EXPORT_DIRS <- c("~clean", "G:/Shared drives/Water Action Volunteers (WAV)/Data/Cleaned Dashboard Datasets")

# point to latest data
SWIMS_DIR <- "swims/2025-11-07"

load_xl <- function(fname, clean = TRUE) {
  f <- file.path(SWIMS_DIR, fname)
  cat("Loading", f, "...\n")
  df <- read_excel(f, na = c("", "NA"))
  if (clean) df <- clean_names(df)
  show(str(df))
  df
}


# 1 => Load Shapefiles =========================================================

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

quickmap(counties)
quickmap(counties.simp)


## NKEs ----

nkes <- read_sf("shp/wi-nke-plans-2022.geojson") %>%
  clean_names("big_camel") %>%
  st_make_valid() %>%
  drop_na(PlanId)

nkes.simp <- rmapshaper::ms_simplify(nkes, .25)

quickmap(nkes)
quickmap(nkes.simp)

nke_data <- nkes %>%
  select(
    nke_plan_name = PlanName,
    nke_plan_purpose = PurposeDe,
    nke_plan_objective = Objective,
    nke_start = StartDate,
    nke_end = EndDate
  ) %>%
  mutate(across(where(is_character), ~ str_to_sentence(str_trim(gsub("[\r\n]", "", .x)))))


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
quickmap(huc6)
quickmap(huc8)
quickmap(huc8.simp)
quickmap(huc10)
quickmap(huc10.simp)
quickmap(huc12)
quickmap(huc12.simp)
quickmap(dnr_watersheds)
quickmap(dnr_watersheds.simp)


## Major waterbodies ----
# Top 1000 waterbodies in the state by area, for use on the pdf reports

waterbodies <- read_sf("shp/wi-major-lakes.geojson")

quickmap(waterbodies)


## Flowlines ----

flowlines <- read_sf("D:/GIS/shapefiles/wi-hydro-nhd-flowlines.gpkg")
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

flow2d.simp <- flow2d %>%
  select(level, geometry) %>%
  filter(level <= 5) %>%
  rmapshaper::ms_simplify(.25)

flow2d.simp %>%
  ggplot() +
  geom_sf(aes(color = factor(level))) +
  scale_color_brewer(palette = "Spectral", direction = -1)


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
    flowlines = flow2d.simp
  )
  for (shape in names(shapes)) {
    fname <- paste0(shape, ".rds")
    fpath <- data_dir(file.path("shp", fname))
    saveRDS(shapes[[shape]], fpath)
    message("Save shape => ", fpath)
  }
})



# 2 => Load SWIMS Stations =====================================================

stn_xl <- load_xl("2_wav_all_stns.xlsx")

names(stn_xl)

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
  # mutate(across(wbic, as.integer)) %>%
  # bind_rows(read_csv("stations/extra-stations.csv")) %>%
  distinct(station_id, .keep_all = T) %>%
  arrange(station_id)

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

stn_master_list <- validate_stns(stns_in)

invalid_stns <- validate_stns(stns_in, F)

for (nm in names(invalid_stns)) {
  local({
    df <- invalid_stns[[nm]]$data
    print(nm)
    print(df)
    if (nrow(df) > 0) {
      write_excel_csv(df, sprintf("QC/invalid_stns_%s.csv", nm), na = "")
    }
  })
}

stn_master_list %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude, lat = ~latitude,
    radius = 1, fillColor = "orange", weight = 0, fillOpacity = 1
  )


## Export full station list ----

saveRDS(stn_master_list, data_dir("stn-master-list.rds"))



# 3 => Baseline data ===========================================================

## Baseline observations ----

baseline_xl <- load_xl("3_wav_baseline_fw.xlsx")

names(baseline_xl)

baseline_obs <- baseline_xl %>%
  select(
    fsn = fieldwork_seq_no,
    datetime = start_date_time,
    station_id,
    station_name = primary_station_name,
    latitude,
    longitude,
    wbic,
    waterbody = official_waterbody_name,
    station_type = station_type_code,
    air_temp = ambient_air_temp,
    air_temp_units = ambient_air_temp_units,
    water_temp,
    water_temp_units,
    d_o = do_mg,
    d_o_percent_saturation = do_pct,
    ph,
    specific_cond,
    transparency = transparency_avg,
    transparency_tube_length,
    weather_conditions,
    weather_last_2_days,
    current_stream_condition,
    group_desc,
    fieldwork_comments = fieldwork_comment,
    additional_comments
  ) %>%
  mutate(
    across(c(air_temp, water_temp, d_o, d_o_percent_saturation, ph, specific_cond, transparency, transparency_tube_length), as.numeric),
    across(c(weather_last_2_days, additional_comments, fieldwork_comments), ~ str_to_sentence(str_squish(.x))),
    weather_conditions = str_to_sentence(gsub("_", " ", weather_conditions)),
    across(c(fsn, station_id, wbic), as.integer)
    # across(datetime, ~ parse_date_time(.x, "ymdHMp"))
  ) %>%
  mutate(date = as.Date(datetime), .after = datetime) %>%
  arrange(datetime) %>%
  filter(datetime >= "2015-1-1") %>%
  distinct(fsn, station_id, date, .keep_all = T)
# ok if NAs were introduced

baseline_obs %>% count(year(datetime))


## Streamflow ----

flow_xl <- load_xl("4_wav_flow_fw.xlsx")

names(flow_xl)

flow_obs <- flow_xl %>%
  select(
    fsn = fieldwork_seq_no,
    datetime = start_date_time,
    station_id,
    station_name = primary_station_name,
    latitude,
    longitude,
    wbic,
    waterbody = official_waterbody_name,
    station_type = station_type_code,
    stream_width,
    average_stream_depth,
    average_surface_velocity,
    entered_streamflow = stream_flow_cfs,
    calculated_streamflow = calculated_streamflow_cfs,
    corrected_streamflow = calculated_corrected_streamflow_cfs,
    flow_method_used
  ) %>%
  mutate(
    # across(datetime, ~ parse_date_time(.x, "ymdHMp")),
    across(c(fsn, station_id, wbic), as.integer),
    across(stream_width:corrected_streamflow, as.numeric)
  ) %>%
  mutate(date = as.Date(datetime), .after = datetime) %>%
  mutate(streamflow = coalesce(entered_streamflow, corrected_streamflow, calculated_streamflow), .before = entered_streamflow) %>%
  distinct(fsn, station_id, date, .keep_all = T) %>%
  filter(date >= "2015-1-1")
# ok if NAs were introduced

flow_obs %>% count(year(datetime))


## Join baseline obs + flow ----

add_units <- function(.data, col, units) {
  mutate(.data, "{col}_units" := case_when(is.na(.data[[col]]) ~ "", T ~ units), .after = {{ col }})
}

baseline_data <- baseline_obs %>%
  left_join(flow_obs) %>%
  mutate(
    year = year(date),
    month = month(date),
    day = day(date),
    yday = yday(date),
    .after = date
  ) %>%
  add_units("d_o", "mg/L") %>%
  add_units("transparency", "cm") %>%
  add_units("stream_width", "ft") %>%
  add_units("average_stream_depth", "ft") %>%
  add_units("average_surface_velocity", "ft/s") %>%
  add_units("streamflow", "cfs") %>%
  relocate(contains("_comment"), .after = everything()) %>%
  arrange(year, station_id, date)


# any missing lat/lng?
baseline_data %>%
  distinct(station_id, .keep_all = TRUE) %>%
  filter(is.na(latitude) | is.na(longitude) | latitude == 0 | longitude == 0)


## Determine which fieldwork events are missing all of the key baseline parameters ----
key_baseline_vars <- c(
  "air_temp",
  "water_temp",
  "d_o",
  "ph",
  "specific_cond",
  "transparency",
  "streamflow"
)

# find stations where all FSNs in a year have no baseline data and drop them
has_key_baseline_data <- baseline_data %>%
  select(station_id, year, fsn, all_of(key_baseline_vars)) %>%
  pivot_longer(all_of(key_baseline_vars)) %>%
  summarize(has_baseline = sum(!is.na(value)) > 0, .by = c(station_id, year, fsn)) %>%
  mutate(valid_year = any(has_baseline), .by = c(station_id, year)) %>%
  filter(valid_year)

valid_fsn <- has_key_baseline_data$fsn

invalid_baseline_data <- baseline_data %>%
  filter(!(fsn %in% valid_fsn))

# message says how many baseline fieldworks will be dropped due to lack of data
message(nrow(baseline_data) - length(valid_fsn), " fieldwork events will be dropped due to having no key baseline data")

# check for problems with baseline stations
baseline_data %>%
  distinct(station_id, latitude, longitude, wbic) %>%
  validate_stns()


## Final baseline join and filter ----

baseline_final <- baseline_data %>%
  arrange(station_id, date) %>%
  filter(fsn %in% valid_fsn) %>%
  filter(station_id %in% stn_master_list$station_id)


## Export baseline data ----
local({
  df <- baseline_final
  f <- list(
    csv = data_dir(sprintf("baseline-data-%s-%s.csv", min(df$year), max(df$year))),
    rds = data_dir("baseline-data.rds")
  )
  message("Save baseline data => ", paste(f, collapse = ", "))
  write_csv(df, f$csv)
  saveRDS(df, f$rds)
})


# 3b => Macroinvertebrate data =================================================

macro_index <- read_csv("macro/macros.csv")
macro_species <- macro_index %>%
  drop_na(group) %>%
  pull(dnr_parameter_description)

macro_xl <- load_xl("8_wav_ibi_fw.xlsx", clean = F)

macro_fieldwork <- macro_xl %>%
  select(-all_of(macro_species)) %>%
  janitor::clean_names() %>%
  select(
    fsn = fieldwork_seq_no,
    station_id,
    group_seq_no,
    plan_name,
    datetime = start_date_time,
    group_1_animals = no_of_group_1_animals,
    group_2_animals = no_of_group_2_animals,
    group_3_animals = no_of_group_3_animals,
    group_4_animals = no_of_group_4_animals_present,
    total_animals,
    ends_with("total_value"),
    index_score,
    fieldwork_comment
  ) %>%
  mutate(datetime = force_tz(datetime, "America/Chicago")) %>%
  mutate(
    date = as_date(datetime),
    year = year(date),
    .after = datetime
  ) %>%
  arrange(datetime) %>%
  drop_na(total_value, index_score) %>%
  mutate(across(c(fsn, station_id, starts_with("group"), starts_with("total")), as.integer)) %>%
  mutate(across(index_score, as.numeric))

str(macro_fieldwork)

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
    macro_fieldwork %>%
      select(fsn, station_id, datetime, date, year)
  }) %>%
  relocate(station_id, datetime, date, year, .after = fsn)

macro_species_counts %>%
  filter(fsn %in% sample(fsn, 100)) %>%
  write_csv("macro_sampling.csv")


## data exploration ----

macro_fieldwork %>%
  ggplot(aes(x = index_score)) +
  geom_histogram() +
  facet_grid(year(datetime)~.)

macro_fieldwork %>%
  ggplot(aes(x = year(datetime), y = total_value)) +
  geom_boxplot(aes(group = year(datetime))) +
  stat_summary() +
  geom_smooth()

macro_fieldwork %>%
  ggplot(aes(x = total_value / total_animals)) +
  geom_histogram()

macro_fieldwork %>%
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
macro_fieldwork %>%
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


# 4 => Nutrient data ===========================================================

## From LPDES/SWIMS ----
# formatted as export from NPDES
# select total phosphorus parameter 665, should be all that's in here though
# tp data in units of mg/L = ppm

nutrient_xl <- load_xl("5_wav_nutrient_fw.xlsx")

tp_data_wav <- nutrient_xl %>%
  select(
    fsn = fieldwork_seq_no,
    station_id,
    station_name = primary_station_name,
    latitude,
    longitude,
    wbic,
    waterbody = official_waterbody_name,
    station_type = station_type_code,
    plan_id, plan_name,
    collector_name,
    datetime = start_date_time,
    tp = result_value_no
  ) %>%
  drop_na(tp) %>%
  mutate(
    across(c(fsn, station_id, wbic), as.integer),
    tp = as.numeric(gsub("ND", 0, tp))
  )

tp_data_wav %>% count(plan_id, sort = T)

# whitelist specific data collectors for non-CBSM projects
tp_data_wav_clean <- tp_data_wav %>%
  filter(
    grepl("CBSM-", plan_id) |
      ((plan_id == "West_06_CMP25") & (collector_name %in% c("RETTA ISAACSON", "HEATHER WOOD", "HEATHER WOOD_0"))) |
      ((plan_id == "Ashipun River_South_TWA_1_2025") & (collector_name == "DALE CIRA")) |
      plan_id == "North_01_CMP23"
  )

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
  mutate(
    across(c(fsn, station_id), as.integer),
    tp = as.numeric(gsub("ND", 0, tp)),
    across(datetime, ~ parse_date_time(.x, "mdy HMS p"))
  ) %>%
  left_join(stn_master_list)

hist(tp_data_mrk$tp)

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
  mutate(
    across(c(fsn, station_id), as.integer),
    tp = as.numeric(gsub("ND", 0, tp)),
    across(datetime, ~ parse_date_time(.x, "mdy HMS p"))
  ) %>%
  filter(!(station_id %in% c(33223, 10037357, 10037358, 10056308, 173209, 183005))) %>%
  left_join(stn_master_list)

tp_data_twa %>%
  filter(is.na(latitude))

# tp_data_twa %>% distinct(station_id) %>% write_csv("stns.csv")

hist(tp_data_twa$tp)
tp_data_twa %>% count(station_id, sort = T)

# merge and strip dupes
tp_data <-
  bind_rows(
    tp_data_wav_clean,
    tp_data_mrk,
    tp_data_twa
  ) %>%
  mutate(
    tp = gsub("ND", 0, tp),
    across(tp, as.numeric),
    across(collector_name, str_to_title),
    date = as.Date(datetime),
    year = year(date),
    month = month(date),
    month_name = month.name[month]
  ) %>%
  distinct(station_id, datetime, tp, .keep_all = TRUE) %>%
  mutate(
    num_obs = sum(!is.na(tp)),
    .by = c(year, station_id)
  ) %>%
  filter(month %in% 5:10) %>%
  filter(!is.na(latitude), !is.na(longitude))


## Data check ----

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

tp_final <- tp_data %>%
  select(-datetime) %>%
  arrange(station_id, date) %>%
  filter(tp < 5) # for now to catch bad data


## Export nutrient data ----
local({
  df <- tp_final
  f <- list(
    csv = data_dir(sprintf("tp-data-%s-%s.csv", min(df$year), max(df$year))),
    rds = data_dir("tp-data.rds")
  )
  message("Save nutrient data => ", paste(f, collapse = ", "))
  write_csv(df, f$csv)
  saveRDS(df, f$rds)
})


# 5 => Thermistor data =========================================================

# see thermistors.R

therm_inventory <- readRDS(data_dir("therm-inventory.rds"))
hobo_data <- readRDS(data_dir("therm-data.rds"))


# 6 => Finalize station list ===================================================

stn_attrib_cols <- c("station_id", "station_name", "latitude", "longitude", "wbic", "waterbody", "station_type")

# generate station lists
baseline_stns <- baseline_final %>%
  distinct(across(any_of(stn_attrib_cols))) %>%
  arrange(station_id) %>%
  validate_stns()
nutrient_stns <- tp_final %>%
  distinct(across(any_of(stn_attrib_cols))) %>%
  arrange(station_id) %>%
  validate_stns()
therm_stns <- therm_inventory %>%
  distinct(station_id) %>%
  drop_na() %>%
  left_join(stn_master_list) %>%
  arrange(station_id) %>%
  validate_stns()
stn_list <- bind_rows(baseline_stns, nutrient_stns, therm_stns) %>%
  drop_na(station_id, latitude, longitude) %>%
  distinct(station_id, .keep_all = T) %>%
  arrange(station_id) %>%
  validate_stns() %>%
  mutate(station_name = str_squish(str_replace_all(station_name, "[^[:alnum:][:punct:] ]", "")))


## Check baseline ----

# number of baseline stations
baseline_final %>% count(station_id, sort = T)

# count of fieldwork by station
stn_tally_baseline <- baseline_final %>%
  count(across(any_of(stn_attrib_cols)), sort = T)

# any baseline stations missing from list?
stn_tally_baseline %>%
  filter(!(station_id %in% stn_list$station_id))


## Check nutrient ----

# number of nutrient stations
tp_final %>% count(station_id, sort = T)

# any nutrient stations missing?
tp_final %>%
  count(station_id) %>%
  filter(!(station_id %in% stn_list$station_id))


## Check thermistor ----

# number of thermistor stations
hobo_data %>% count(station_id, sort = T)

# any thermistor inventory entries missing a station id?
hobo_data %>%
  count(station_id) %>%
  filter(!(station_id %in% stn_list$station_id))


## Stations to keep ----

keep_stns <- sort(unique(c(
  baseline_data$station_id,
  tp_final$station_id,
  hobo_data$station_id
)))


## Create station SF ----

stn_list.sf <- stn_list %>%
  filter(station_id %in% keep_stns) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>%
  st_join(select(counties, DnrRegion, CountyName)) %>%
  st_join(select(huc12, -Area)) %>%
  st_join(select(dnr_watersheds, DnrWatershedCode, DnrWatershedName)) %>%
  select(
    station_id, station_name,
    latitude, longitude,
    county_name = CountyName,
    dnr_region = DnrRegion,
    wbic, waterbody,
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


## Plot stations on a map ----

stn_list.sf %>%
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


## Export station list ----

all_stns <- stn_list.sf %>% st_set_geometry(NULL)

local({
  df <- all_stns
  f <- list(
    csv = data_dir("station-list.csv"),
    rds = data_dir("station-list.rds")
  )
  message("Saved station list => ", paste(f, collapse = ", "))
  write_csv(df, f$csv)
  saveRDS(df, f$rds)
})



# 7 => Landcover data ==========================================================

# see landcover project



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
