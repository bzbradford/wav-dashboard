## TESTING ##

library(tidyverse)
library(sf)
library(leaflet)
library(plotly)


# Data validation ----

# these stations appear in the data but don't have a location
all_coverage %>%
  filter(!(station_id %in% all_pts$station_id)) %>%
  write_csv("stations missing locations.csv")

test = c("2019", "2021")
all_coverage %>%
  rowwise() %>%
  filter(setequal(intersect(test, data_year_list), test)) %>%
  pull(station_id)

baseline_data %>%
  filter(station_id == station_id[1]) %>%
  clean_names(case = "title") %>%
  rownames_to_column() %>%
  mutate(rowname = paste("Obs", rowname)) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = -rowname, names_to = "Parameter") %>%
  pivot_wider(names_from = rowname)

names(df) <- paste("Obs", ncol(df))



# Some leaflet maps ----

station_pts %>%
  mutate(label = paste0(station_id, ": ", station_name)) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    label = ~label,
    radius = 1, opacity = 1, fill = F) %>%
  addMarkers(
    label = ~label,
    clusterOptions = markerClusterOptions())

baseline_data %>%
  slice_sample(n = 4) %>%
  clean_names(case = "title") %>%
  rownames_to_column() %>%
  mutate(label = paste0("Obs", rowname, "\n(", format(Date, "%b %d"), ")")) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = -label, names_to = "Parameter") %>%
  pivot_wider(names_from = label) %>%
  mutate(Parameter = gsub("D o", "D.O.", Parameter)) %>%
  mutate(Parameter = gsub("P h", "pH", Parameter))



# Dissolved oxygen plot ----

# library(RColorBrewer)

cur_baseline_data <- baseline_data %>%
  filter(station_id == 10016773, year == 2022)

cur_baseline_data <- baseline_data %>%
  filter(station_id == 10030403, year == 2022)


do_color <- function(do) {
  case_when(
    do <= 1 ~ "red",
    do <= 3 ~ "orange",
    do <= 5 ~ "darksalmon",
    do <= 7 ~ "lightblue",
    do <= 10 ~ "blue",
    T ~ "darkblue"
  )
}

do_color <- function(do) {
  case_when(
    do <= 1 ~ 1,
    do <= 3 ~ 2,
    do <= 5 ~ 3,
    do <= 7 ~ 4,
    do <= 10 ~ 5,
    T ~ 6
  )
}

do_color_pal <- c(
  "red", "orange", "darksalmon", "lightblue", "blue", "darkblue"
)

do_color(3)

colfunc <- colorRampPalette(c("red", "white", "blue"))(12)
pal <- colfunc(max(df$z))[df$z]

colfunc(12)



# Baseline plotly test ----

cur_baseline_data %>%
  drop_na(d_o) %>%
  rowwise() %>%
  mutate(do_color = brewer.pal(11, "RdBu")[min(d_o, 11)]) %>%
  plot_ly() %>%
  add_trace(
    x = ~date,
    y = ~d_o,
    text = ~paste(d_o, "mg/L"),
    marker = list(
      color = ~do_color,
      line = list(
        color = "black",
        width = 1
      )),
    type = "bar",
    hovertemplate = "%{y} mg/L<extra></extra>",
    showlegend = F
  ) %>%
  add_trace(
    x = ~date,
    y = ~d_o_percent_saturation,
    mode = "lines+markers",
    type = "scatter",
    yaxis = "y2",
    hovertemplate = "%{y} saturation<extra></extra>",
    showlegend = F
  ) %>%
  layout(
    title = "Dissolved Oxygen",
    xaxis = list(
      title = "",
      type = "date",
      range = ~c(min(date) - 15, max(date) + 15),
      fixedrange = T,
      dtick = "M1",
      ticklabelmode = "period",
      hoverformat = "%b %d, %Y"
    ),
    yaxis = list(
      title = "Dissolved oxygen (mg/L)",
      fixedrange = T),
    yaxis2 = list(
      title = "D.O. saturation",
      overlaying = "y",
      side = "right",
      range = ~c(0, max(100, d_o_percent_saturation) + 10),
      ticksuffix = "%",
      showgrid = F,
      fixedrange = T),
    hovermode = "x unified",
    margin = list(
      t = 50,
      r = 50
    )
  )



# Temperature plotly test ----

cur_baseline_data %>%
  plot_ly() %>%
  add_trace(
    x = ~date,
    y = ~air_temp_field,
    type = "scatter",
    mode = "lines+markers",
    name = "Water",
    hovertemplate = "%{y}&deg;C",
    showlegend = F
  ) %>%
  add_trace(
    x = ~date,
    y = ~water_temperature,
    type = "scatter",
    mode = "lines+markers",
    name = "Air",
    hovertemplate = "%{y}&deg;C",
    showlegend = F
  ) %>%
  layout(
    title = "Air and Water Temperature",
    hovermode = "x unified",
    xaxis = list(
      title = "",
      type = "date",
      range = ~c(min(date) - 15, max(date) + 15),
      fixedrange = T,
      dtick = "M1",
      ticklabelmode = "period",
      hoverformat = "%b %d, %Y"
    ),
    yaxis = list(
      title = "Temperature &deg;C",
      fixedrange = T),
    margin = list(
      t = 50,
      r = 50
    )
  )



# Combined plot test ----

df <- cur_baseline_data %>%
  distinct(date, .keep_all = T) %>%
  rowwise() %>%
  mutate(do_color = brewer.pal(11, "RdBu")[floor(min(d_o, 11))])

do_data <- df %>% filter(!(is.na(d_o) & is.na(d_o_percent_saturation)))
temp_data <- df %>% filter(!(is.na(water_temperature) & is.na(air_temp_field)))
trans_data <- df %>% filter(!is.na(transparency))
flow_data <- df %>% filter(!is.na(stream_flow_cfs))


df %>%
  plot_ly() %>%
  add_trace(
    data = do_data,
    name = "D.O.",
    x = ~date,
    y = ~d_o,
    text = ~paste0(d_o, " mg/L<br>", d_o_percent_saturation, "% sat"),
    marker = list(
      color = ~do_color,
      line = list(color = "black", width = 0.5)),
    type = "bar",
    width = 1000 * 60 * 60 * 24 * 15,
    hovertemplate = "%{y}"
  ) %>%
  add_trace(
    data = temp_data,
    name = "Water temp",
    x = ~date,
    y = ~water_temperature,
    type = "scatter",
    mode = "lines+markers",
    yaxis = "y2",
    marker = list(
      color = "lightblue",
      size = 10,
      line = list(color = "white", width = 1)
    ),
    line = list(
      color = "lightblue",
      width = 3
    )
  ) %>%
  add_trace(
    data = temp_data,
    name = "Air temp",
    x = ~date,
    y = ~air_temp_field,
    type = "scatter",
    mode = "lines+markers",
    yaxis = "y2",
    marker = list(
      color = "orange",
      size = 10,
      line = list(color = "white", width = 1)
    ),
    line = list(color = "orange", width = 3)
  ) %>%
  add_trace(
    data = trans_data,
    name = "Transparency",
    x = ~date,
    y = ~transparency,
    type = "scatter",
    mode = "lines+markers",
    yaxis = "y3",
    marker = list(
      color = "brown",
      size = 10,
      symbol = "square",
      line = list(color = "white", width = 1)
    ),
    line = list(color = "brown", width = 3)
  ) %>%
  add_trace(
    data = flow_data,
    name = "Stream flow",
    x = ~date,
    y = ~stream_flow_cfs,
    type = "scatter",
    mode = "lines+markers",
    yaxis = "y4",
    marker = list(
      color = "#48a67b",
      size = 10,
      symbol = "triangle-right",
      line = list(color = "white", width = 1)
    ),
    line = list(color = "#48a67b", width = 3)
  ) %>%
  layout(
    title = "Baseline Measurements",
    xaxis = list(
      title = "",
      type = "date",
      # range = ~c(min(date) - 15, max(date) + 30),
      fixedrange = T,
      dtick = "M1",
      ticklabelmode = "period",
      hoverformat = "%b %d, %Y",
      domain = c(.1, .9)
    ),
    yaxis = list(
      title = "Dissolved oxygen",
      ticksuffix = " mg/L",
      fixedrange = T),
    yaxis2 = list(
      title = "Temperature",
      overlaying = "y",
      side = "left",
      ticksuffix = "&deg;C",
      position = 0,
      showgrid = F,
      zeroline = F,
      fixedrange = T),
    yaxis3 = list(
      title = "Transparency",
      overlaying = "y",
      side = "right",
      ticksuffix = " cm",
      showgrid = F,
      zeroline = F,
      fixedrange = T),
    yaxis4 = list(
      title = "Stream flow",
      overlaying = "y",
      side = "right",
      ticksuffix = " cfs",
      position = 1,
      showgrid = F,
      zeroline = F,
      fixedrange = T),
    hovermode = "x unified",
    margin = list(t = 50, r = 50),
    legend = list(orientation = "h")
  )



# Baseline summaries ----

baseline_data %>%
  select(
    d_o,
    water_temperature,
    air_temp,
    transparency,
    stream_flow_cfs
  ) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  split(.$name) %>%
  map(summary)

df1 <- baseline_data %>%
  filter(station_id == sample(station_id, 1))

df2 <- df1 %>%
  filter(year == sample(year, 1))

#' Summary objectives:
#' - DO, water temp, air temp, transparency
#' - For each: hi/low value and date, current year and if multiple years all time

df1[which.max(df1$d_o), ]

df <- df1
var <- "d_o"
varname <- "Dissolved oxygen (mg/L)"



tibble(
  name = varname,
  max_val = df[which.max(df[[var]]), ][[var]],
  max_date = df[which.max(df[[var]]), ]$date,
  min_val = df[which.min(df[[var]]), ][[var]],
  min_date = df[which.min(df[[var]]), ]$date
)


make_min_max <- function(df, var) {
  tibble(
    min_val = df[which.min(df[[var]]), ][[var]],
    max_val = df[which.max(df[[var]]), ][[var]],
    min_date = df[which.min(df[[var]]), ]$date,
    max_date = df[which.max(df[[var]]), ]$date
  )
}

summary_vars <- tribble(
  ~var, ~name, ~units,
  "d_o", "Dissolved oxygen", "mg/L",
  "water_temperature", "Water temperature", "°C",
  "air_temp", "Air temperature", "°C",
  "transparency", "Transparency", "cm",
  "stream_flow_cfs", "Stream flow", "cfs"
) %>% rowwise()

summary_vars %>%
  summarize(cur_data(), make_min_max(df, var)) %>%
  mutate(across(c(min_val, max_val), ~paste(.x, units))) %>%
  mutate(across(c(min_date, max_date), ~format(.x, "%b %d, %Y")))



# Thermistor summary ----

therm_temp_units <- "F"

temp_col <- ifelse(therm_temp_units == "F", "temp_f", "temp_c")
therm_data %>%
  filter(station_id == station_id[1]) %>%
  mutate(temp = .[[temp_col]]) %>%
  mutate(month_name = fct_inorder(format(date, "%b"))) %>%
  summarize(
    days = n_distinct(date),
    obs = n(),
    min = min(temp, na.rm = T),
    q10 = quantile(temp, .1, na.rm = T),
    mean = mean(temp, na.rm = T),
    q90 = quantile(temp, .9, na.rm = T),
    max = max(temp, na.rm = T),
    .by = month_name
  )



# Map => Color by variable ----

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


# baseline means
# selected from the most recent year
# should select most recent n observations?
baseline_means <- baseline_data %>%
  slice_max(date, n = 10, by = station_id) %>%
  summarize(
    max_water_temp = max(water_temp, na.rm = T),
    mean_d_o = mean(d_o, na.rm = T),
    avg_transparency = mean(transparency, na.rm = T),
    avg_streamflow = mean(streamflow, na.rm = T),
    .by = station_id
  ) %>% {
    df <- .
    df[sapply(df, is.infinite)] <- NA
    df[sapply(df, is.nan)] <- NA
    df
  }

nutrient_means <- nutrient_data %>%
  drop_na(tp) %>%
  slice_max(date, n = 12, by = station_id) %>%
  summarize(
    mean_tp = mean(tp),
    log_mean_tp = mean(log10(tp)),
    .by = station_id)

stn_attr_totals <- stn_fieldwork_counts %>%
  left_join(baseline_means, join_by(station_id)) %>%
  left_join(nutrient_means, join_by(station_id))

summary(stn_attr_totals)

stn_color_opts <- tribble(
  ~label,            ~value,              ~domain,   ~reverse, ~pal,
  "Years of data",    "n_years",          c(0, 10),  F,        "viridis",
  "Fieldwork events", "n_fieldwork",      c(0, 100), F,        "viridis",
  "Max water temp",   "max_water_temp",   c(15, 30), T,        "RdYlBu",
  "Dissolved oxygen", "mean_d_o",         c(3, 12),  F,        "RdYlBu",
  "Transparency",     "avg_transparency", c(0, 120), F,        "BrBG",
  "Streamflow",       "avg_streamflow",   c(0, 50),  T,        "RdBu",
  "Total phosphorus", "mean_tp",          c(0, .25), T,        "Spectral",
)
stn_color_choices <- append(
  list("Station type" = "stn_type"),
  deframe(stn_color_opts[,1:2])
)




# Station report ----------------------------------------------------------


stn <- slice_sample(all_stns, n = 1)
baseline_coverage %>% filter(station_id == stn$station_id)
nutrient_coverage %>% filter(station_id == stn$station_id)
therm_coverage %>% filter(station_id == stn$station_id)

df <- baseline_data %>%
  filter(station_id == stn$station_id) %>%
  filter(year == max(year))


## Station Info ----

stn$station_name
stn$station_id

input <- list()
input$year <- 2023
selected_data <- list(
  baseline = baseline_data %>%
    filter(station_id == stn$station_id, year == input$year),
  nutrient = nutrient_data %>%
    filter(station_id == stn$station_id, year == input$year),
  thermistor = therm_data %>%
    filter(station_id == stn$station_id, year == input$year)
)

rows <- sapply(selected_data, function(df) nrow(df))
rows[rows > 0]

## Station data summary ----



# temperature
#' Pages:
#' - Intro / overview
#' - Temperature / thermistor?
#' - Dissolved oxygen
#' - Streamflow
#' - Transparency
#' - Nutrient
library(ggrepel)
water_label <- "Water (°C)"
air_label <- "Air (°C)"
flow_label <- "Streamflow (cfs)"
df %>%
  ggplot(aes(x = date)) +
  geom_line(
    aes(y = water_temp, color = water_label),
    linewidth = 2) +
  geom_point(
    aes(y = water_temp, color = water_label),
    size = 3) +
  geom_line(
    aes(y = air_temp, color = air_label),
    linewidth = 2) +
  geom_point(
    aes(y = air_temp, color = air_label),
    size = 3) +
  geom_text_repel(aes(y = water_temp, label = paste0(water_temp, "°C"))) +
  geom_text_repel(aes(y = air_temp, label = paste0(air_temp, "°C"))) +
  scale_x_date(
    name = "Date of observation",
    breaks = df$date,
    date_labels = "%b %d") +
  scale_y_continuous(expand = expansion(c(0, .1))) +
  scale_color_manual(
    breaks = c(air_label, water_label),
    values = c("orange", "lightblue")
  ) +
  scale_fill_distiller(palette = "RdBu", direction = 1, limits = c(0, 15)) +
  labs(
    y = "Measurement value",
    color = "Measurement",
    fill = "Dissolved\noxygen (mg/L)"
  ) +
  theme_classic()

df %>%
  ggplot(aes(x = date)) +
  geom_col(
    aes(y = d_o, fill = d_o),
    color = "black") +
  geom_text(aes(y = d_o, label = paste(d_o, "mg/L")), vjust = -.5) +
  geom_line(
    aes(y = d_o_percent_saturation / 10, color = "DO % Sat"),
    linewidth = 2) +
  geom_point(
    aes(y = d_o_percent_saturation / 10, color = "DO % Sat"),
    size = 3) +
  geom_text_repel(aes(y = d_o_percent_saturation / 10, label = paste0(d_o_percent_saturation, "%"))) +
  scale_x_date(
    breaks = df$date,
    date_labels = "%b %d\n%Y") +
  scale_y_continuous(
    name = "Measurement value",
    limits = c(0, max(df$d_o, 12, na.rm = T)),
    expand = expansion(c(0, .1)),
    sec.axis = sec_axis(
      name = "DO Saturation",
      trans = ~.*10
    )
  ) +
  scale_color_manual(
    breaks = c("DO % Sat"),
    values = c("navy")
  ) +
  scale_fill_distiller(palette = "Blues", direction = 1, limits = c(0, 15)) +
  labs(
    x = "Date of observation",
    fill = "Dissolved\noxygen (mg/L)"
  ) +
  theme_classic()

view(df)




# Thermistor plot ---------------------------------------------------------

test_therm <- therm_data %>%
  filter(station_id == 10011236) %>%
  mutate(dateskip = as.numeric(lead(date) - date))

gap_starts <- test_therm %>% filter(dateskip > 7)

tibble(date = gap_starts$date + gap_starts$dateskip / 2)

test_therm %>%
  bind_rows(tibble(date = gap_starts$date + gap_starts$dateskip / 2))
