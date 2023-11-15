
makeLandscapePieChart <- function(landscape) {
  landscape %>%
    plot_ly() %>%
    add_trace(
      type = "pie",
      labels = ~class_name,
      values = ~pct_area,
      marker = list(
        colors = ~hex,
        line = list(color = "#fff", width = 0.5)),
      textposition = "inside",
      texttemplate = "<b>%{label}</b><br>%{percent}",
      hovertemplate = "<b>%{label}</b><br>%{percent}<extra></extra>",
      sort = F) %>%
    layout(
      showlegend = F,
      margin = list(l = 0, r = 0, t = 0, b = 0),
      paper_bgcolor = "rgba(0, 0, 0, 0)") %>%
    config(displayModeBar = F)
}
