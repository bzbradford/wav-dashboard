library(leaflet)

station_pts %>%
  mutate(label = paste0(station_id, ": ", station_name)) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    label = ~label,
    radius = 1, opacity = 1, fill = F) %>%
  addMarkers(
    label = ~label,
    clusterOptions = markerClusterOptions())

baseline_data %>%
  slice_sample(n = 4) %>%
  clean_names(case = "title") %>%
  rownames_to_column() %>%
  mutate(label = paste0("Obs", rowname, "\n(", format(Date, "%b %d"), ")")) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = -label, names_to = "Parameter") %>%
  pivot_wider(names_from = label) %>%
  mutate(Parameter = gsub("D o", "D.O.", Parameter)) %>%
  mutate(Parameter = gsub("P h", "pH", Parameter))
