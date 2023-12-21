# Make thermistor plot

#' @requires
#'  `f_to_c()` converts temperature
#'  `rect()` creates rectangles on plotly
#' @param df_hourly hourly temperature data
#' @param df_daily daily min/max temperature data
#' @param units units 'f' or 'c'
#' @param annotations may be 'None', 'btrout', or 'wtemp' to add behind plot

makeThermistorPlot <- function(df_hourly, df_daily, units, annotations) {
  require(dplyr)
  require(plotly)

  # make sure daily values time is aligned to noon
  df_daily <- df_daily %>%
    mutate(date_time = as.POSIXct(paste(date, "12:00:00")))

  # handle units
  temp_col <- paste0("temp_", tolower(units))
  ytitle <- paste0("Temperature (°", units, ")")
  yrange <- ifelse(units == "F", c(30, 100), c(0, 37))

  plt <- plot_ly() %>%
    add_ribbons(
      data = df_daily,
      x = ~ date_time,
      ymin = ~ min,
      ymax = ~ max,
      line = list(
        color = "lightblue",
        width = 0.5,
        opacity = 0),
      fillcolor = "lightblue",
      opacity = 0.5,
      name = "Daily Range",
      hovertemplate = "Daily Range<extra></extra>") %>%
    add_lines(
      data = df_daily,
      x = ~ date_time,
      y = ~ min,
      line = list(
        color = "lightblue",
        width = 1,
        opacity = 0.5),
      name = "Daily Min",
      showlegend = F) %>%
    add_lines(
      data = df_daily,
      x = ~ date_time,
      y = ~ max,
      line = list(
        color = "lightblue",
        width = 1,
        opacity = 0.5),
      name = "Daily Max",
      showlegend = F) %>%
    add_trace(
      x = df_hourly$date_time,
      y = df_hourly[[temp_col]],
      name = "Hourly Temperature",
      type = "scatter",
      mode = "lines",
      line = list(
        color = "#1f77b4",
        width = 0.5,
        opacity = 0.8)) %>%
    add_trace(
      data = df_daily,
      x = ~ date_time,
      y = ~ mean,
      name = "Mean Daily Temp.",
      type = "scatter",
      mode = "lines",
      line = list(
        color = "orange")) %>%
    layout(
      title = "Stream Temperature",
      showlegend = TRUE,
      xaxis = list(title = "Date and Time"),
      yaxis = list(
        title = ytitle,
        range = yrange,
        zerolinecolor = "lightgrey"),
      hovermode = "x unified",
      legend = list(
        orientation = "h",
        x = 0.25,
        y = 1),
      margin = list(
        t = 50)) %>%
    config(displayModeBar = F)

  # add annotation color bands
  if (annotations != "none") {
    if (annotations == "btrout") {
      temps <- c(32, 52, 61, 72, 100) # F
      if (units == "C") temps <- f_to_c(temps)
      colors <- c("cornflowerblue", "green", "lightgreen", "darkorange")
    } else if (annotations == "wtemp") {
      temps <- c(32, 72, 77, 100) # F
      if (units == "C") temps <- f_to_c(temps)
      colors <- c("blue", "cornflowerblue", "darkorange")
    }

    plt <- plt %>%
      layout(
        shapes = lapply(1:length(colors), function(i) {
          rect(temps[i], temps[i + 1], colors[i])
        })
      )
  }

  plt
}
