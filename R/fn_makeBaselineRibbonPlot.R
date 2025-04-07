
#' @requires
#' - OPTS$baseline_plot_opts
#' @param .data data frame derived from `baseline_data`

makeBaselineRibbonPlot <- function(.data) {
  opts <- OPTS$baseline_plot_opts

  df <- .data %>%
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
      text = ~if_else(is.na(value), "Not measured", paste(signif(value), unit)),
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

baseline_data %>%
  filter(station_id == sample(station_id, 1)) %>%
  filter(year == sample(year, 1)) %>%
  makeBaselineRibbonPlot()
