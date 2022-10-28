library(leaflet)

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




# Dissolved oxygen plot ---------------------------------------------------

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



library(RColorBrewer)

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


# Temperature -------------------------------------------------------------


cur_baseline_data %>%
  plot_ly() %>%
  add_trace(
    x = ~date,
    y = ~ambient_air_temp_field,
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



# Combined plot -----------------------------------------------------------

df <- cur_baseline_data %>%
  distinct(date, .keep_all = T) %>%
  rowwise() %>%
  mutate(do_color = brewer.pal(11, "RdBu")[floor(min(d_o, 11))])

do_data <- df %>% filter(!(is.na(d_o) & is.na(d_o_percent_saturation)))
temp_data <- df %>% filter(!(is.na(water_temperature) & is.na(ambient_air_temp_field)))
trans_data <- df %>% filter(!is.na(transparency_average))
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
    y = ~ambient_air_temp_field,
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
    y = ~transparency_average,
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



# Baseline summaries ------------------------------------------------------

baseline_data %>%
  select(
    d_o,
    water_temperature,
    ambient_air_temp,
    transparency_average,
    stream_flow_cfs
  ) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  split(.$name) %>%
  map(summary)
