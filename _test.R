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
  filter(station_id == station_id[2], year == 2021)


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

cur_baseline_data %>%
  rowwise() %>%
  mutate(do_color = brewer.pal(11, "RdBu")[min(d_o, 11)]) %>%
  plot_ly() %>%
  add_trace(
    name = "D.O.",
    x = ~date,
    y = ~d_o,
    text = ~paste0(d_o, " mg/L<br>", d_o_percent_saturation, "% sat"),
    marker = list(
      color = ~do_color,
      line = list(color = "black", width = 0.5)),
    type = "bar",
    hovertemplate = "%{y}",
    showlegend = F
  ) %>%
  add_trace(
    x = ~date,
    y = ~water_temperature,
    type = "scatter",
    mode = "lines+markers",
    name = "Water",
    yaxis = "y2",
    marker = list(
      color = "lightblue",
      size = 10,
      line = list(color = "white", width = 1)
    ),
    line = list(
      color = "lightblue",
      width = 3
    ),
    showlegend = F
  ) %>%
  add_trace(
    x = ~date,
    y = ~ambient_air_temp_field,
    type = "scatter",
    mode = "lines+markers",
    name = "Air",
    yaxis = "y2",
    marker = list(
      color = "orange",
      size = 10,
      line = list(color = "white", width = 1)
    ),
    line = list(color = "orange", width = 3),
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
      title = "Dissolved oxygen",
      ticksuffix = " mg/L",
      fixedrange = T),
    yaxis2 = list(
      title = "Temperature",
      overlaying = "y",
      side = "right",
      ticksuffix = "&deg;C",
      showgrid = F,
      fixedrange = T),
    hovermode = "x unified",
    margin = list(t = 50, r = 50)
  )

