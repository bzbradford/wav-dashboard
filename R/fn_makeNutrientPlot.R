# Make plotly for nutrient data

#' @requires
#'  `rect()` creates a plotly rectangle annotation
#' @param df data frame containing nutrient data
#' @param phoslimit number representing the state phosphorus threshold, eg 0.075 mg/L
#' @param phos_estimate list with `n` observations and `lower` `upper` and `median` confidence interval values
makeNutrientPlot <- function(df, phoslimit, phos_estimate) {
  require(dplyr)
  require(plotly)

  if (nrow(df) == 0) return()

  df <- df %>%
    mutate(exceedance = factor(
      ifelse(is.na(tp), "No data", ifelse(tp >= phoslimit, "TP High", "TP OK")),
      levels = c("TP OK", "TP High", "No data"))) %>%
    drop_na(tp) %>%
    mutate(phoslimit = phoslimit)

  min_year <- min(df$year)
  max_year <- max(df$year)
  data_dates <- unique(df$date)
  date_range <- c(
    min(min(data_dates) - 15, newDate(min_year, 5, 1)),
    max(max(data_dates) + 15, newDate(max_year, 11, 1))
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
        line = list(color = "red", width = 2)) %>%
      add_lines(
        x = ~date,
        y = ~lower,
        name = "Lower 90% CI",
        opacity = 0.5,
        line = list(color = ci_color, width = 0.5)) %>%
      add_lines(
        x = ~date,
        y = ~median,
        name = "Median",
        opacity = 0.5,
        line = list(color = "black", dash = "dash", width = 1.5)) %>%
      add_lines(
        x = ~date,
        y = ~upper,
        name = "Upper 90% CI",
        opacity = 0.5,
        line = list(color = ci_color, width = 0.5))

    shapes <- list(
      rect(phos_estimate$lower, phos_estimate$upper, ci_color)
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
      width = 0.5 * 1000 * 60 * 60 * 24 * 30, # time in milliseconds
      marker = list(
        line = list(
          color = "rgb(8,48,107)",
          width = 1)),
      textfont = list(color = "black"),
      hovertemplate = "Measured TP: %{y:.3f} mg/L<extra></extra>") %>%
    layout(
      title = "Total Phosphorus",
      xaxis = list(
        title = "",
        type = "date",
        hoverformat = "%B %d, %Y",
        tickformat = "%B<br>%Y",
        dtick = "M1",
        range = date_range,
        fixedrange = T),
      yaxis = list(
        title = "Total phosphorus",
        ticksuffix = " mg/L",
        zerolinecolor = "lightgrey",
        range = yrange,
        fixedrange = T),
      legend = list(
        traceorder = "reversed",
        orientation = "h",
        x = 0.25, y = 1),
      hovermode = "x unified",
      margin = list(t = 50),
      shapes = shapes) %>%
    config(displayModeBar = F)

  plt
}
