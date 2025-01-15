
makeBaselineScatterplot <- function(.data, col) {
  df <- .data %>%
    select(date, year, month, yday, all_of(c(value = col))) %>%
    drop_na(value)

  opts <- OPTS$baseline_plot_opts %>% filter(col == !!col)
  hovertemplate <- paste("%{x}: %{y:.2f}", opts$unit)
  yrange <- c(
    min(df$value, opts$range_min),
    max(df$value, opts$range_max)
  )
  yrange <- yrange + yrange * c(-.1, .1)

  plt <- df %>%
    plot_ly() %>%
    add_trace(
      name = "Observation",
      type = "scatter", mode = "markers",
      x = ~date,
      y = ~value,
      text = ~paste(signif(value), opts$unit),
      hovertemplate = hovertemplate
    )

  if (nrow(df) > 1) {
    plt <- plt %>%
      add_lines(
        name = "Linear fit",
        x = ~date,
        y = ~fitted(lm(value ~ date, data = df)),
        hovertemplate = hovertemplate
      )
  }

  if (nrow(df) > 6) {
    plt <- plt %>%
      add_lines(
        name = "Smoothed fit",
        x = ~date,
        y = ~predict(loess(value ~ as.numeric(date), data = df)),
        hovertemplate = hovertemplate
      )
  }

  plt %>%
    layout(
      title = list(text = opts$name),
      legend = list(orientation = "h"),
      xaxis = list(
        title = NA,
        automargin = T,
        fixedrange = T
      ),
      yaxis = list(
        title = opts$label,
        automargin = T,
        fixedrange = T
        # range = yrange
      )
    ) %>%
    config(displayModeBar = F)
}

# makeBaselineScatterplot(df, "water_temp")
#
# test <- df %>%
#   select(date, year, month, yday, all_of(c(value = "water_temp"))) %>%
#   drop_na(value)
#
# testlm <- lm(value ~ date, test)
# anova(testlm)[["Pr(>F)"]]
#
#
# predict(loess(value ~ as.numeric(date), data = test))
