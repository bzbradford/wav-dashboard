
#' @param landscape1 first landscape, against which to compare
#' @param landscape2 gets compared against first by percent of each class

makeLandscapeDiffPlot <- function(landscape1, landscape2) {
  require(dplyr)
  require(plotly)

  df <- landscape1 %>%
    left_join(select(landscape2, class_name, pct_area2 = pct_area), by = "class_name") %>%
    replace_na(list(pct_area2 = 0)) %>%
    mutate(diff = pct_area2 - pct_area) %>%
    mutate(label = scales::percent(diff, .1)) %>%
    mutate(label = if_else(substr(label, 1, 1) == "-", label, paste0("+", label))) %>%
    mutate(label_pos = -1 * sign(diff) * .00001) %>%
    mutate(hovertext = paste0(
      "Current watershed: ",
      scales::percent(pct_area2, .1),
      "<br>State average: ",
      scales::percent(pct_area, .1),
      "<br>Difference: ",
      label)
    ) %>%
    droplevels()

  xrange <- with(df, c(min(diff) * 1.2, max(diff) * 1.2))

  df %>%
    plot_ly() %>%
    add_bars(
      y = ~class_name,
      x = ~label_pos,
      marker = list(
        opacity = 0),
      text = ~class_name,
      textposition = "outside",
      texttemplate = "<b>%{text}</b>",
      hoverinfo = "none") %>%
    add_bars(
      y = ~class_name,
      x = ~diff,
      text = ~label,
      marker = list(
        opacity = 0),
      textposition = "outside",
      texttemplate = "<b>%{text}</b>") %>%
    add_bars(
      y = ~class_name,
      x = ~diff,
      text = ~hovertext,
      marker = list(
        color = ~hex,
        line = list(color = "#000", width = 1)),
      textposition = "none",
      hovertemplate = "<b>%{y}<br></b>%{text}<extra></extra>") %>%
    layout(
      barmode = "overlay",
      xaxis = list(
        title = "Difference from state average",
        tickformat = ",.0%",
        ticks = "outside",
        fixedrange = T,
        range = xrange,
        zerolinewidth = 1.5),
      yaxis = list(
        visible = F,
        fixedrange = T),
      showlegend = F,
      margin = list(l = 10, r = 10),
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
    config(displayModeBar = F)
}
