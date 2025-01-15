
#' @param .data data frame derived from `baseline_data`
#' @param opts single row from `OPTS$baseline_plot_opts`

makeBaselineBoxplot <- function(.data, col, type = c("month", "year")) {
  type <- match.arg(type)

  df <- .data %>%
    select(year, month, all_of(c(value = col))) %>%
    drop_na(value) %>%
    mutate(month_name = factor(month.abb[month], levels = month.abb, ordered = T))

  opts <- OPTS$baseline_plot_opts %>% filter(col == !!col)

  df <- if (type == "month") {
    mutate(df, x = month_name)
  } else {
    mutate(df, x = factor(year, ordered = T))
  }

  # yrange <- c(
  #   min(.data$value, opts$range_min),
  #   max(.data$value, opts$range_max)
  # )
  # yrange <- yrange + yrange * c(-.1, .1)

  plot_ly(df) %>%
    add_trace(
      x = ~x,
      y = ~value,
      name = "Summary",
      type = "box",
      boxpoints = F,
      boxmean = T,
      hoverinfo = list(extras = "none")
      # line = list(color = alpha(opts$color, .8)),
      # fillcolor = alpha(opts$color, .25)
    ) %>%
    add_trace(
      x = ~x,
      y = ~value,
      name = "Observation",
      type = "scatter",
      mode = "markers",
      hoverinfo = list(extras = "none"),
      # marker = list(color = opts$color),
      text = ~paste(month_name, year),
      hovertemplate = paste("%{text}: %{y}", opts$unit)
    ) %>%
    layout(
      title = opts$name,
      hovermode = "unified",
      showlegend = F,
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
    )
}
