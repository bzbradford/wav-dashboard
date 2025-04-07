
#' @param .data data frame derived from `baseline_data`
#' @param col name of the column to plot
#' @param type determines plot type

makeBaselineTrendPlot <- function(.data, col, type = c("scatter", "month", "year")) {
  type <- match.arg(type)

  opts <- OPTS$baseline_plot_opts %>% filter(col == !!col)
  df <- .data %>%
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
  if (min(df$value) >= 0 & yrange[1] < 0) yrange[1] <- 0

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
        text = ~paste(signif(value), opts$unit),
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
        text = ~paste(month_name, year),
        hovertemplate = paste("%{text}: %{y}", opts$unit)
      )
  }

  # add annotations
  annot <- OPTS$baseline_trend_annot[[col]]
  if (!is.null(annot)) {
    values <- c(-1e6, annot$values, 1e6)
    shapes <- lapply(1:length(annot$colors), function(i) {
      rect(values[i], values[i + 1], annot$colors[i])
    })
    plt <- layout(plt, shapes = shapes)
    for (i in 1:length(annot$values)) {
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

