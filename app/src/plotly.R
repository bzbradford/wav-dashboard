
# Plotly graphs and functions


# Helpers ----------------------------------------------------------------------

# plotly horizontal line annotation
# plotly_hline <- function(y = 0, color = "black") {
#   list(
#     type = "line",
#     x0 = 0,
#     x1 = 1,
#     xref = "paper",
#     y0 = y,
#     y1 = y,
#     line = list(color = color, dash = "dash")
#   )
# }

# plotly rectanglular annotation
plotly_rect <- function(ymin, ymax, color = "red") {
  list(
    type = "rect",
    fillcolor = color,
    line = list(color = color),
    opacity = 0.1,
    y0 = ymin,
    y1 = ymax,
    xref = "paper",
    x0 = 0,
    x1 = 1,
    layer = "below"
  )
}

# finds top of an axis based on a default value and the data
plotly_find_max <- function(vals, min_val) {
  vals <- na.omit(vals)
  if (length(vals) == 0) {
    return(min_val)
  }
  # ceiling(max(min_val, max(vals)) * 1.1)
  max(min_val, max(vals)) * 1.1
}



# Baseline data plot -----------------------------------------------------------

#' Create plotly for baseline data tab
#' @requires
#'  `do_color()` creates the color palette for DO
#' @param df baseline data for a single station

buildPlotlyBaseline <- function(df) {

  df <- distinct(df, date, .keep_all = T)

  # modify and remove empties for each var
  do_data <- df %>%
    filter(!is.na(d_o)) %>%
    mutate(label = case_when(
      is.na(d_o_percent_saturation) ~ paste0(d_o, " mg/L"),
      T ~ paste0(d_o, " mg/L<br>", d_o_percent_saturation, "% sat")
    )) %>%
    rowwise() %>%
    mutate(do_color = do_color(d_o))
  temp_data <- filter(df, !(is.na(water_temp) & is.na(air_temp)))
  trans_data <- filter(df, !is.na(transparency))
  flow_data <- filter(df, !is.na(streamflow))

  # get y-axis ranges for plot
  yranges <- list(
    d_o = c(0, plotly_find_max(df$d_o, 12)),
    temp = c(0, plotly_find_max(c(df$water_temp, df$air_temp), 30)),
    trans = c(0, 125),
    cfs = c(0, plotly_find_max(df$streamflow, 10))
  )

  # date settings
  dates <- unique(df$date)
  years <- as.numeric(max(dates) - min(dates)) / 365
  date_tick <- "M1"
  marker_opacity <- 1
  if (years < 1) {
    date_range <- set_report_date_range(dates)
  } else {
    date_range <- c(min(dates) - 15, max(dates) + 15)
  }
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
        line = list(color = "black", width = 0.5)
      ),
      type = "bar",
      width = 15 * (1000 * 60 * 60 * 24), # milliseconds per day
      hovertemplate = "%{y}"
    ) %>%
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
        opacity = marker_opacity
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
      y = ~air_temp,
      type = "scatter",
      mode = "lines+markers",
      yaxis = "y2",
      marker = list(
        color = "orange",
        size = 10,
        line = list(color = "white", width = 1),
        opacity = marker_opacity
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
        line = list(color = "white", width = 1),
        opacity = marker_opacity
      ),
      line = list(color = "brown", width = 3)
    ) %>%
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
        opacity = marker_opacity
      ),
      line = list(color = "#48a67b", width = 3)
    ) %>%
    layout(
      title = "Baseline Measurements",
      hovermode = "x unified",
      margin = list(t = 50, r = 50),
      legend = list(orientation = "h"),
      xaxis = list(
        title = "",
        type = "date",
        range = date_range,
        fixedrange = T, # allow user to zoom the axis?
        dtick = date_tick,
        ticklabelmode = "period",
        hoverformat = "%b %d, %Y",
        domain = c(.1, .9)
      ),
      yaxis = list(
        title = "Dissolved oxygen",
        ticksuffix = " mg/L",
        range = yranges$d_o,
        fixedrange = T,
        automargin = T
      ),
      yaxis2 = list(
        title = "Temperature",
        overlaying = "y",
        side = "left",
        ticksuffix = "&deg;C",
        position = 0,
        showgrid = F,
        zeroline = F,
        range = yranges$temp,
        fixedrange = T,
        automargin = T
      ),
      yaxis3 = list(
        title = "Transparency",
        overlaying = "y",
        side = "right",
        ticksuffix = " cm",
        showgrid = F,
        zeroline = F,
        range = yranges$trans,
        fixedrange = T,
        automargin = T
      ),
      yaxis4 = list(
        title = "Stream flow",
        overlaying = "y",
        side = "right",
        ticksuffix = " cfs",
        position = 1,
        showgrid = F,
        zeroline = F,
        range = yranges$cfs,
        fixedrange = T,
        automargin = T
      )
    ) %>%
    config(displayModeBar = F)
}



# Baseline trend plot ----------------------------------------------------------

#' Shows long term trends as scatterplot or boxplots grouped by month or year
#' @requires
#'   `OPTS$baseline_plot_opts`
#' @param df data frame derived from `baseline_data`
#' @param col name of the column to plot
#' @param type determines plot type

buildPlotlyBaselineTrend <- function(df, col, type = c("scatter", "month", "year")) {
  type <- match.arg(type)

  opts <- OPTS$baseline_plot_opts %>% filter(col == !!col)
  df <- df %>%
    select(year, month, date, all_of(c(value = col))) %>%
    drop_na(value) %>%
    mutate(month_name = factor(month.abb[month], levels = month.abb, ordered = T))

  req(nrow(df) > 0)

  # set x variable
  df <- switch(type,
    scatter = mutate(df, x = date),
    month = mutate(df, x = month_name),
    year = mutate(df, x = factor(year, ordered = T))
  )

  # set y axis range
  yrange <- c(min(df$value, opts$range_min), max(df$value, opts$range_max))
  yrange <- yrange + abs(yrange[2] - yrange[1]) * c(-.1, .1)
  if (min(df$value) >= 0 && yrange[1] < 0) yrange[1] <- 0

  plt <- plot_ly(df) %>%
    config(displayModeBar = F) %>%
    layout(
      title = list(text = opts$name),
      showlegend = F,
      hovermode = "unified",
      hoverdistance = 100,
      xaxis = list(
        title = NA,
        automargin = T,
        fixedrange = T
      ),
      yaxis = list(
        title = opts$label,
        automargin = T,
        fixedrange = T,
        range = yrange
      ),
      margin = list(b = 25)
    )

  plt <- if (type == "scatter") {
    plt %>%
      add_trace(
        name = "Observation",
        type = "scatter", mode = "lines+markers",
        x = ~date,
        y = ~value,
        text = ~ paste(signif(value), opts$unit),
        hovertemplate = paste("%{x}: %{y:.2f}", opts$unit)
      )
  } else {
    plt %>%
      add_trace(
        x = ~x,
        y = ~value,
        name = "Summary",
        type = "box",
        boxpoints = F,
        boxmean = T,
        hoverinfo = list(extras = "none")
      ) %>%
      add_trace(
        x = ~x,
        y = ~value,
        name = "Observation",
        type = "scatter",
        mode = "markers",
        hoverinfo = list(extras = "none"),
        text = ~ paste(month_name, year),
        hovertemplate = paste("%{text}: %{y}", opts$unit)
      )
  }

  # add annotations
  annot <- OPTS$baseline_trend_annot[[col]]
  if (!is.null(annot)) {
    values <- c(-1e6, annot$values, 1e6)
    shapes <- lapply(seq_along(annot$colors), function(i) {
      plotly_rect(values[i], values[i + 1], annot$colors[i])
    })
    plt <- layout(plt, shapes = shapes)
    for (i in seq_along(annot$values)) {
      plt <- plt %>%
        add_trace(
          type = "scatter",
          mode = "lines",
          x = df$x,
          y = annot$values[i],
          hovertemplate = paste(annot$labels[i], "<extra></extra>"),
          line = list(color = annot$colors[i]),
          opacity = 0
        )
    }
  }

  plt
}



# Baseline ribbon plot ---------------------------------------------------------

#' Shows heatmap of observations and measured values over time
#' @requires
#'   `OPTS$baseline_plot_opts`
#' @param df data frame derived from `baseline_data`

buildPlotlyBaselineRibbon <- function(df) {
  opts <- OPTS$baseline_plot_opts

  df <- df %>%
    mutate(
      year = factor(year, levels = 2015:year(Sys.Date())),
      month = factor(month, levels = 1:12)
    ) %>%
    complete(year, month) %>%
    pivot_longer(all_of(opts$col), names_to = "measure") %>%
    select(year, month, measure, value) %>%
    mutate(date = as_date(paste(year, month, 1, sep = "-"))) %>%
    filter(date < today()) %>%
    summarize(value = mean(value, na.rm = T), .by = c(date, measure)) %>%
    filter(!all(is.na(value)), .by = measure) %>%
    left_join(opts, join_by(measure == col)) %>%
    mutate(
      scaled = scales::rescale(value, from = c(min(c(value, range_min), na.rm = T), max(c(value, range_max), na.rm = T))),
      .by = measure
    )

  # param order for y axis
  yorder <- rev(intersect(opts$name, unique(df$name)))

  plot_ly(df) %>%
    add_trace(
      type = "heatmap",
      x = ~date,
      y = ~name,
      z = ~scaled,
      text = ~ if_else(is.na(value), "Not measured", paste(signif(value), unit)),
      colors = "Blues",
      hovertemplate = "%{x}<br>%{y}: %{text}<extra></extra>"
    ) %>%
    colorbar(
      title = list(text = NA),
      len = 1,
      thickness = 20,
      limits = c(0, 1),
      dtick = 1,
      outlinewidth = 0,
      tickmode = "array",
      tickvals = c(0, 1),
      ticktext = c("Low", "High")
    ) %>%
    layout(
      showlegend = F,
      margin = list(t = 0, b = 25),
      yaxis = list(
        title = NA,
        automargin = T,
        showgrid = F,
        fixedrange = T,
        tickmode = "linear",
        dtick = 1,
        categoryorder = "array",
        categoryarray = yorder
      ),
      xaxis = list(
        title = NA,
        automargin = T,
        showgrid = F,
        fixedrange = T,
        dtick = "M12"
      )
    ) %>%
    config(displayModeBar = F)
}

# baseline_data %>%
#   filter(station_id == sample(station_id, 1)) %>%
#   filter(year == sample(year, 1)) %>%
#   makeBaselineRibbonPlot()




# Nutrient/TP plot --------------------------------------------------------

#' @requires
#'  `rect()` creates a plotly rectangle annotation
#' @param df data frame containing nutrient data
#' @param phoslimit number representing the state phosphorus threshold, eg 0.075 mg/L
#' @param phos_estimate list with `n` observations and `lower` `upper` and `median` confidence interval values

buildPlotlyNutrient <- function(df, phoslimit, phos_estimate) {

  if (nrow(df) == 0) {
    return()
  }

  df <- df %>%
    summarize(tp = mean(tp, na.rm = T), .by = c(year, date)) %>%
    mutate(
      exceedance = factor(
        ifelse(is.na(tp), "No data", ifelse(tp >= phoslimit, "TP High", "TP OK")),
        levels = c("TP OK", "TP High", "No data")
      ),
      phoslimit = phoslimit
    ) %>%
    # determine column width, constrained to between 7 and 28 days
    arrange(date) %>%
    mutate(
      days_since_last = as.integer(date - lag(date)),
      days_to_next = as.integer(lead(date) - date)
    ) %>%
    rowwise() %>%
    mutate(bar_width = max(7, min(28, days_since_last, days_to_next, na.rm = T))) %>%
    replace_na(list(bar_width = 28))

  min_year <- min(df$year)
  max_year <- max(df$year)
  data_dates <- unique(df$date)
  date_range <- c(
    min(min(data_dates) - 15, new_date(min_year, 5, 1)),
    max(max(data_dates) + 15, new_date(max_year, 11, 1))
  )
  outer_months <- date_range + c(-15, 15)
  all_dates <- sort(unique(c(outer_months, data_dates)))
  yrange <- suppressWarnings(c(0, max(phoslimit * 1.2, max(df$tp, na.rm = T) * 1.2)))

  phos_params <- tibble(
    date = all_dates,
    lower = phos_estimate$lower,
    upper = phos_estimate$upper,
    median = phos_estimate$median
  )

  # no confidence invervals if only one month of data
  if (phos_estimate$n > 1) {
    ci_color <- ifelse(phos_estimate$lower >= phoslimit, "red", "teal")

    plt <- plot_ly(phos_params) %>%
      add_lines(
        x = ~date,
        y = ~phoslimit,
        name = "TP criteria",
        opacity = 0.75,
        line = list(color = "red", width = 2)
      ) %>%
      add_lines(
        x = ~date,
        y = ~lower,
        name = "Lower 90% CI",
        opacity = 0.5,
        line = list(color = ci_color, width = 0.5)
      ) %>%
      add_lines(
        x = ~date,
        y = ~median,
        name = "Median",
        opacity = 0.5,
        line = list(color = "black", dash = "dash", width = 1.5)
      ) %>%
      add_lines(
        x = ~date,
        y = ~upper,
        name = "Upper 90% CI",
        opacity = 0.5,
        line = list(color = ci_color, width = 0.5)
      )

    shapes <- list(
      plotly_rect(phos_estimate$lower, phos_estimate$upper, ci_color)
    )
  } else {
    plt <- plot_ly()
    shapes <- list()
  }

  plt <- plt %>%
    add_trace(
      data = df,
      x = ~date,
      y = ~tp,
      type = "bar",
      text = ~tp,
      textposition = "auto",
      color = ~exceedance,
      colors = "Set2",
      width = ~ 0.75 * 1000 * 60 * 60 * 24 * bar_width, # time in milliseconds
      marker = list(
        line = list(
          color = "rgb(8,48,107)",
          width = 1
        )
      ),
      textfont = list(color = "black"),
      hovertemplate = "Measured TP: %{y:.3f} mg/L<extra></extra>"
    ) %>%
    layout(
      title = "Total Phosphorus",
      xaxis = list(
        title = "",
        type = "date",
        hoverformat = "%B %d, %Y",
        tickformat = "%B<br>%Y",
        dtick = "M1",
        range = date_range
      ),
      yaxis = list(
        title = "Total phosphorus",
        ticksuffix = " mg/L",
        zerolinecolor = "lightgrey",
        range = yrange,
        fixedrange = T
      ),
      legend = list(
        traceorder = "reversed",
        orientation = "h",
        x = 0.25, y = 1
      ),
      hovermode = "x unified",
      margin = list(t = 50),
      shapes = shapes
    ) %>%
    config(displayModeBar = F)

  plt
}



# Thermistor plot --------------------------------------------------------------

#' @requires
#'  `f_to_c()` converts temperature
#'  `rect()` creates rectangles on plotly
#' @param df_hourly hourly temperature data
#' @param df_daily daily min/max temperature data
#' @param units units 'f' or 'c'
#' @param annotations may be 'None', 'btrout', or 'wtemp' to add behind plot

buildPlotlyThermistor <- function(df_hourly, df_daily, units, annotations) {

  # make sure daily values time is aligned to noon
  df_daily <- df_daily %>%
    mutate(date_time = as.POSIXct(paste(date, "12:00:00")))

  # handle units
  temp_col <- paste0("temp_", tolower(units))
  ytitle <- paste0("Temperature (°", units, ")")
  default_yrange <- if (units == "F") c(40, 90) else c(5, 30)
  data_yrange <- c(
    min(df_hourly[[temp_col]], na.rm = TRUE),
    max(df_hourly[[temp_col]], na.rm = TRUE)
  )
  yrange <- c(
    floor(min(default_yrange[1], data_yrange[1])),
    ceiling(max(default_yrange[2], data_yrange[2]))
  )

  plt <- plot_ly() %>%
    add_ribbons(
      data = df_daily,
      x = ~date_time,
      ymin = ~min,
      ymax = ~max,
      line = list(
        color = "lightblue",
        width = 0.5,
        opacity = 0
      ),
      fillcolor = "lightblue",
      opacity = 0.5,
      name = "Daily Range",
      connectgaps = FALSE,
      fill = "tozeroy",
      hovertemplate = "Daily Range<extra></extra>"
    ) %>%
    add_lines(
      data = df_daily,
      x = ~date_time,
      y = ~min,
      line = list(
        color = "lightblue",
        width = 1,
        opacity = 0.5
      ),
      name = "Daily Min",
      showlegend = FALSE
    ) %>%
    add_lines(
      data = df_daily,
      x = ~date_time,
      y = ~max,
      line = list(
        color = "lightblue",
        width = 1,
        opacity = 0.5
      ),
      name = "Daily Max",
      showlegend = FALSE
    ) %>%
    add_trace(
      x = df_hourly$date_time,
      y = df_hourly[[temp_col]],
      name = "Hourly Temperature",
      type = "scatter",
      mode = "lines",
      line = list(
        color = "#1f77b4",
        width = 0.5,
        opacity = 0.8
      )
    ) %>%
    add_trace(
      data = df_daily,
      x = ~date_time,
      y = ~mean,
      name = "Mean Daily Temp.",
      type = "scatter",
      mode = "lines",
      line = list(
        color = "orange"
      )
    ) %>%
    layout(
      title = list(
        text = "Stream Temperature",
        y = .99,
        yanchor = "top"
      ),
      showlegend = TRUE,
      xaxis = list(title = "Date and Time"),
      yaxis = list(
        title = ytitle,
        range = yrange,
        zerolinecolor = "lightgrey"
      ),
      hovermode = "x unified",
      legend = list(
        orientation = "h",
        yanchor = "bottom",
        x = 0.25,
        y = 1
      ),
      margin = list(t = 50)
    ) %>%
    config(displayModeBar = FALSE)

  # add annotation color bands
  if (annotations != "none") {
    if (annotations == "btrout") {
      temps <- c(32, 52, 61, 72, 100) # F
      if (units == "C") temps <- f_to_c(temps)
      colors <- c("cornflowerblue", "green", "lightgreen", "darkorange")
    } else if (annotations == "wtemp") {
      temps <- c(-40, 69.3, 72.5, 76.3, 150) # F
      if (units == "C") temps <- f_to_c(temps)
      colors <- c("blue", "cornflowerblue", "lightsteelblue", "darkorange")
    }

    plt <- plt %>%
      layout(
        shapes = lapply(seq_along(colors), function(i) {
          plotly_rect(temps[i], temps[i + 1], colors[i])
        })
      )
  }

  plt
}



# Landscape pie chart -----------------------------------------------------

#' @param landscape landscape data for current watershed

buildPlotlyLandscapePie <- function(landscape) {
  landscape %>%
    plot_ly() %>%
    add_trace(
      type = "pie",
      labels = ~class_name,
      values = ~pct_area,
      marker = list(
        colors = ~hex,
        line = list(color = "#fff", width = 0.5)
      ),
      textposition = "inside",
      texttemplate = "<b>%{label}</b><br>%{percent}",
      hovertemplate = "<b>%{label}</b><br>%{percent}<extra></extra>",
      sort = F
    ) %>%
    layout(
      showlegend = F,
      margin = list(l = 0, r = 0, t = 0, b = 0),
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    ) %>%
    config(displayModeBar = F)
}



# Landscape diff plot -----------------------------------------------------

#' @param landscape1 first landscape, against which to compare
#' @param landscape2 gets compared against first by percent of each class

buildPlotlyLandscapeDiff <- function(landscape1, landscape2) {

  df <- landscape1 %>%
    left_join(select(landscape2, class_name, pct_area2 = pct_area), by = "class_name") %>%
    replace_na(list(pct_area2 = 0)) %>%
    mutate(diff = pct_area2 - pct_area) %>%
    mutate(label = scales::percent(diff, .1)) %>%
    mutate(label = if_else(substr(label, 1, 1) == "-", label, paste0("+", label))) %>%
    mutate(label_pos = -1 * sign(diff) * .00001) %>%
    mutate(hovertext = paste0(
      "Current watershed: ",
      scales::label_percent(.1)(pct_area2),
      "<br>State average: ",
      scales::label_percent(.1)(pct_area),
      "<br>Difference: ",
      label
    )) %>%
    droplevels()

  xrange <- with(df, c(min(diff) * 1.2, max(diff) * 1.2))

  df %>%
    plot_ly() %>%
    add_bars(
      y = ~class_name,
      x = ~label_pos,
      marker = list(
        opacity = 0
      ),
      text = ~class_name,
      textposition = "outside",
      texttemplate = "<b>%{text}</b>",
      hoverinfo = "none"
    ) %>%
    add_bars(
      y = ~class_name,
      x = ~diff,
      text = ~label,
      marker = list(
        opacity = 0
      ),
      textposition = "outside",
      texttemplate = "<b>%{text}</b>"
    ) %>%
    add_bars(
      y = ~class_name,
      x = ~diff,
      text = ~hovertext,
      marker = list(
        color = ~hex,
        line = list(color = "#000", width = 1)
      ),
      textposition = "none",
      hovertemplate = "<b>%{y}<br></b>%{text}<extra></extra>"
    ) %>%
    layout(
      barmode = "overlay",
      xaxis = list(
        title = "Difference from state average",
        tickformat = ",.0%",
        ticks = "outside",
        fixedrange = T,
        range = xrange,
        zerolinewidth = 1.5
      ),
      yaxis = list(
        visible = F,
        fixedrange = T
      ),
      showlegend = F,
      margin = list(l = 10, r = 10),
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    ) %>%
    config(displayModeBar = F)
}

