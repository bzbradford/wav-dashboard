# Create plotly for baseline data tab

#' @requires
#'  `do_color()` creates the color palette for DO
#'  `find_max()` finds top of an axis based on a default value and the data
#' @param df baseline data for a single station

makeBaselinePlot <- function(df) {
  require(dplyr)
  require(plotly)

  df <- df %>% distinct(date, .keep_all = T)

  # get y axis-ranges for plot
  yranges <- list(
    d_o = c(0, find_max(df$d_o, 12)),
    temp = c(0, find_max(c(df$water_temp, df$air_temp), 30)),
    trans = c(0, 125),
    cfs = c(0, find_max(df$streamflow, 10)))

  # modify and remove empties for each var
  do_data <- df %>%
    filter(!is.na(d_o)) %>%
    mutate(label = case_when(
      is.na(d_o_percent_saturation) ~ paste0(d_o, " mg/L"),
      T ~ paste0(d_o, " mg/L<br>", d_o_percent_saturation, "% sat"))) %>%
    rowwise() %>%
    mutate(do_color = do_color(d_o))
  temp_data <- filter(df, !(is.na(water_temp) & is.na(air_temp)))
  trans_data <- filter(df, !is.na(transparency))
  flow_data <- filter(df, !is.na(streamflow))

  # settings for longer date periods
  years <- as.numeric(max(df$date) - min(df$date)) / 365
  date_tick <- "M1"
  marker_opacity <- 1
  if (years > 3) {
    date_tick <- "M3"
    marker_opacity <- 0
  }
  if (years > 6) date_tick <- "M6"

  # create plot
  plot_ly() %>%
    add_trace(
      data = do_data,
      name = "D.O.",
      x = ~date,
      y = ~d_o,
      text = ~label,
      marker = list(
        color = ~do_color,
        line = list(color = "black", width = 0.5)),
      type = "bar",
      width = 1000 * 60 * 60 * 24 * 15,
      hovertemplate = "%{y}") %>%
    add_trace(
      data = temp_data,
      name = "Water temp",
      x = ~date,
      y = ~water_temp,
      type = "scatter",
      mode = "lines+markers",
      yaxis = "y2",
      marker = list(
        color = "lightblue",
        size = 10,
        line = list(color = "white", width = 1),
        opacity = marker_opacity),
      line = list(
        color = "lightblue",
        width = 3)) %>%
    add_trace(
      data = temp_data,
      name = "Air temp",
      x = ~date,
      y = ~air_temp,
      type = "scatter",
      mode = "lines+markers",
      yaxis = "y2",
      marker = list(
        color = "orange",
        size = 10,
        line = list(color = "white", width = 1),
        opacity = marker_opacity),
      line = list(color = "orange", width = 3)) %>%
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
        line = list(color = "white", width = 1),
        opacity = marker_opacity),
      line = list(color = "brown", width = 3)) %>%
    add_trace(
      data = flow_data,
      name = "Stream flow",
      x = ~date,
      y = ~streamflow,
      type = "scatter",
      mode = "lines+markers",
      yaxis = "y4",
      marker = list(
        color = "#48a67b",
        size = 10,
        symbol = "triangle-right",
        line = list(color = "white", width = 1),
        opacity = marker_opacity),
      line = list(color = "#48a67b", width = 3)) %>%
    layout(
      title = "Baseline Measurements",
      hovermode = "x unified",
      margin = list(t = 50, r = 50),
      legend = list(orientation = "h"),
      xaxis = list(
        title = "",
        type = "date",
        fixedrange = F, # allow user to zoom the axis?
        dtick = date_tick,
        ticklabelmode = "period",
        hoverformat = "%b %d, %Y",
        domain = c(.1, .9)),
      yaxis = list(
        title = "Dissolved oxygen",
        ticksuffix = " mg/L",
        range = yranges$d_o,
        fixedrange = T),
      yaxis2 = list(
        title = "Temperature",
        overlaying = "y",
        side = "left",
        ticksuffix = "&deg;C",
        position = 0,
        showgrid = F,
        zeroline = F,
        range = yranges$temp,
        fixedrange = T),
      yaxis3 = list(
        title = "Transparency",
        overlaying = "y",
        side = "right",
        ticksuffix = " cm",
        showgrid = F,
        zeroline = F,
        range = yranges$trans,
        fixedrange = T),
      yaxis4 = list(
        title = "Stream flow",
        overlaying = "y",
        side = "right",
        ticksuffix = " cfs",
        position = 1,
        showgrid = F,
        zeroline = F,
        range = yranges$cfs,
        fixedrange = T)) %>%
    config(displayModeBar = F)
}
