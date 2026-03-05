# Macroinvertebrates ----

## data exploration ----

macro_obs |>
  ggplot(aes(x = biotic_index_score)) +
  geom_histogram() +
  facet_grid(year(datetime) ~ .)

macro_obs |>
  ggplot(aes(x = year(datetime), y = total_value)) +
  geom_boxplot(aes(group = year(datetime))) +
  stat_summary() +
  geom_smooth()

macro_obs |>
  ggplot(aes(x = total_value / total_animals)) +
  geom_histogram()

macro_obs |>
  filter(station_id == sample(station_id, 1)) |>
  ggplot(aes(x = datetime, y = index_score)) +
  geom_boxplot(aes(group = year)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", color = "blue", fill = "blue", alpha = .1) +
  geom_smooth(method = "gam", color = "red", fill = "red", alpha = .1) +
  scale_y_continuous(limits = c(0, 4)) +
  scale_x_date(date_breaks = "year", date_labels = "%Y")

# number of samplings per year
macro_obs |>
  filter(year == 2024) |>
  summarize(n = n(), .by = c(year, station_id)) |>
  summarize(stations = n(), .by = c(year, n)) |>
  mutate(pct = stations / sum(stations), .by = year) |>
  ggplot(aes(x = factor(n), y = pct)) +
  geom_col() +
  geom_text(aes(label = scales::label_percent(1)(pct)), vjust = -.5)


## Validation ----

# Duplicate FSN 351071051 == 357587224
# how should we handle duplicate data? Manually delete the fieldwork in swims?

## plotly prototype ----

library(plotly)

hex_to_rgba <- function(hex, alpha = 1) {
  rgb <- col2rgb(hex)
  paste0("rgba(", rgb[1], ",", rgb[2], ",", rgb[3], ",", alpha, ")")
}

# Logic: 1=Sensitive (Blue), 4=Tolerant (Red), Invasive (Purple)
macro_groups <- c("Group 1", "Group 2", "Group 3", "Group 4", "Invasive")
# macro_colors <- c("#2196F3", "#4CAF50", "#FF9800", "#F44336", "#9C27B0")
# macro_desc   <- c("Sensitive", "Moderately sensitive", "Moderately tolerant", "Tolerant", "Tolerant")
macro_colortable <- tibble(
  group = macro_groups,
  base_color = c("#2196F3", "#4CAF50", "#FF9800", "#F44336", "#9C27B0"),
  description = c(
    "Group 1 (Sensitive)",
    "Group 2 (Moderately sensitive)",
    "Group 3 (Moderately tolerant)",
    "Group 4 (Tolerant)",
    "Invasive"
  )
) |>
  expand_grid(present = c(T, F)) |>
  mutate(
    z = (1:10 - .5) / 10,
    alpha = if_else(present, 1, .1),
    color = mapply(hex_to_rgba, base_color, alpha, USE.NAMES = F)
  )

# build plotly heatmap colorscale
colorscale <- list()
for (i in 1:10) {
  row <- slice(macro_colortable, i)
  colorscale[[length(colorscale) + 1]] <- list((i - 1) / 10, row$color)
  colorscale[[length(colorscale) + 1]] <- list(i / 10, row$color)
}


add_missing_years <- function(df) {
  bind_rows(
    df,
    tibble(
      year = setdiff(2015:2025, unique(df$year)),
      date = as_date(paste(year, "-1-1")),
      date_label = paste("(", year, ")")
    )
  ) |>
    arrange(date) |>
    mutate(date_label = fct_inorder(date_label))
}

# select a station
selected_data <- macro_species_counts |>
  filter(station_id == sample(station_id, 1)) |>
  mutate(
    species_name = factor(species_name, macro_species),
    group = factor(group, macro_groups),
    status = if_else(present, "Present", "Absent")
  ) |>
  left_join(macro_colortable, join_by(group, present))


# all observations
plot_data <- selected_data |>
  mutate(
    date_label = format(date, "%b %d, %Y"),
    tooltip_text = str_glue(
      "
      <b>{species_name}</b>
      {description}
      {date_label}
      {status}
    "
    )
  ) |>
  add_missing_years()

# or summarize by year
plot_data <- selected_data |>
  summarize(
    present = any(present),
    .by = c(year, group, species_name)
  ) |>
  left_join(macro_colortable, join_by(group, present)) |>
  mutate(
    date = as_date(paste0(year, "-1-2")),
    date_label = as.character(year),
    species_name = factor(species_name, macro_species),
    group = factor(group, macro_groups),
    status = if_else(present, "Present", "Absent"),
    tooltip_text = paste0(
      "Species: ",
      species_name,
      "<br>Year: ",
      year,
      "<br>Group: ",
      group,
      "<br>Status: ",
      status
    )
  ) |>
  add_missing_years()

# plot it
plot_ly() |>
  add_trace(
    data = plot_data,
    type = "heatmap",
    x = ~date_label,
    y = ~species_name,
    z = ~z,
    text = ~tooltip_text,
    hoverinfo = "text",
    colorscale = colorscale,
    showscale = F, # Hide the rainbow bar
    xgap = 1,
    ygap = 1
  ) |>
  layout(
    title = "Macroinvertebrate Sampling: Presence/Absence Heatmap",
    xaxis = list(
      title = "",
      type = "category",
      categoryarray = levels(plot_data$date_label),
      categoryorder = "array",
      tickangle = -45,
      showgrid = F
    ),
    yaxis = list(
      title = "",
      type = 'category',
      categoryarray = rev(levels(plot_data$species_name)),
      categoryorder = "array",
      showgrid = F
    ),
    plot_bgcolor = "white",
    margin = list(t = 50, r = 10, b = 10, l = 10),
    legend = list(title = list(text = "Species Group"))
  ) |>
  config(displayModeBar = F)


## ggplot prototype ----

p <- plot_data |>
  ggplot(aes(x = date_label, y = species_name, text = tooltip_text)) +
  geom_tile(
    aes(fill = base_color, alpha = alpha, color = base_color),
    lwd = 0.25,
    width = 0.9,
    height = 0.9
  ) +
  scale_y_discrete(limits = rev, na.translate = F) +
  scale_fill_identity(aes(color = base_color), na.value = "transparent") +
  scale_color_identity(aes(color = base_color), na.value = "transparent") +
  labs(
    title = "Macroinvertebrate Sampling: Presence/Absence Heatmap",
    x = NULL,
    y = NULL,
    fill = "Species group",
    color = "Species group"
  ) +
  guides(alpha = guide_none()) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", linewidth = .25)
  )

# Convert to Interactive Plotly Figure
ggplotly(p, tooltip = "text")


# Misc old stuff ----

baseline_stns <- stn_list |>
  filter(station_id %in% baseline_data$station_id)

tp_stns <- stn_list |>
  filter(station_id %in% tp_data$station_id)

stn_list |>
  filter(station_id == 223252)

stn_list |>
  group_by(latitude, longitude) |>
  filter(n() > 1) |>
  arrange(latitude, longitude)

baseline_stn_dupes <- baseline_stns |>
  group_by(latitude, longitude) |>
  filter(n() > 1) |>
  arrange(latitude, longitude) |>
  select(station_id, station_name, latitude, longitude, everything())

baseline_stn_dupes |> write_csv("baseline station duplicates.csv")

baseline_final |>
  filter(station_id %in% baseline_stn_dupes$station_id) |>
  arrange(latitude, longitude) |>
  select(station_id, station_name, latitude, longitude, everything()) |>
  write_csv("baseline data assigned to duplicate stations.csv")

tp_stns |>
  group_by(latitude, longitude) |>
  filter(n() > 1) |>
  arrange(latitude, longitude)


baseline_data |>
  filter(station_id == 10040536, year == 2022)


baseline_final |>
  group_by(station_id, year) |>
  tally()


# compare 2024 nutrient datsets
wav_nutrient <- read_excel(
  "nutrient/wav_nutrient_RRC_RKeep_20241210.xlsx",
  na = c("", "NA")
) |>
  clean_names() |>
  filter(year(start_date_time) == 2024) |>
  mutate(source = "WAV") |>
  rename(
    station_latitude = calc_ll_lat_dd_amt,
    station_longitude = calc_ll_long_dd_amt
  )
mrk_nutrient <- read_excel(
  "nutrient/MilwaukeeRiverkeeper_TP_2024.xlsx",
  na = c("", "NA")
) |>
  clean_names() |>
  rename(
    start_date_time = sample_start_date_time,
    plan_id = project_no
  ) |>
  mutate(across(start_date_time, ~ parse_date_time(.x, "mdyHMSp")))

combined_nutrient <- bind_rows(wav_nutrient, mrk_nutrient) |>
  mutate(across(result_value_no, ~ gsub("ND", 0, .x) |> as.numeric())) |>
  replace_na(list(result_value_no = 0)) |>
  select(source, everything()) |>
  arrange(start_date_time, station_id)

combined_nutrient |>
  group_by(start_date_time, station_id, result_value_no) |>
  filter(n() > 1) |>
  mutate(dupe_id = cur_group_id(), .before = 1) |>
  write_csv("nutrient dupes.csv", na = "")

combined_nutrient |>
  filter(n() == 1, .by = c(start_date_time, station_id, result_value_no)) |>
  filter(source == "Milwaukee Riverkeeper") |>
  write_csv("nutrient MRK uniques.csv", na = "")


arrange(desc(n)) |>
  pivot_wider(names_from = "source", values_from = "n") |>
  view()
