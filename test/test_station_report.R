# Station report test
library(tidyverse)

test_stn <- slice_sample(all_pts, n = 1)
test_baseline <- baseline_data %>%
  filter(station_id == test_stn$station_id) %>%
  filter(year == max(year)) %>%
  mutate(formatted_date = format(date, "%b %d"))


test_baseline %>%
  select(
    `Date` = formatted_date,
    `Air temp (°C)` = air_temp,
    `Water temp (°C)` = water_temp,
    `DO (mg/L)` = d_o,
    `DO % sat.` = d_o_percent_saturation,
    `pH` = ph,
    `Transparency (cm)` = transparency,
    `Streamflow (cfs)` = streamflow,
    `Weather` = weather_last_2_days
  )

test_baseline %>%
  select(
    `Date` = formatted_date,
    `Stream width (ft)` = stream_width,
    `Average depth (ft)` = average_stream_depth,
    `Surface velocity (ft/s)` = average_surface_velocity,
    `Streamflow (cfs)` = streamflow,
    `Flow method` = flow_method_used
  )

test_baseline %>%
  select(
    `Date` = formatted_date,
    `Group names` = group_desc,
    `Weather conditions` = weather_conditions,
    `Weather last 2 days` = weather_last_2_days,
    `Fieldwork comments` = fieldwork_comment,
    `Streamflow comments` = streamflow_comments
  )


makeReportPlots(test_baseline, type = "temp")
makeReportPlots(test_baseline, type = "do")
makeReportPlots(test_baseline, type = "trans")
makeReportPlots(test_baseline, type = "flow")



test_stn <- all_pts %>%
  filter(station_id %in% nutrient_data$station_id) %>%
  slice_sample(n = 1)
test_nutrient <- nutrient_data %>%
  filter(station_id == test_stn$station_id) %>%
  filter(year == max(year)) %>%
  mutate(formatted_date = format(date, "%b %d"))

est <- getPhosEstimate(test_nutrient$tp)
# getPhosExceedanceText(test_phos_estimate)

df <- test_nutrient %>%
  select(date, tp) %>%
  drop_na(tp) %>%
  mutate(exceeds = tp > phoslimit) %>%
  mutate(label = paste(signif(tp, 3), "mg/L"))

est_labels <- tribble(
  ~value, ~label,
  est$lower, "Lower 90% CI",
  est$median, "Median value",
  est$upper, "Upper 90% CI",
  est$limit, "State limit",
) %>%
  mutate(date = as.Date("2022-12-1"))

df %>%
  ggplot(aes(x = date, y = tp)) +
  geom_rect(
    aes(fill = est$median > phoslimit),
    xmin = -Inf, xmax = Inf,
    ymin = est$lower, ymax = est$upper,
    alpha = .05) +
  geom_hline(yintercept = phoslimit, linewidth = 1, color = "red") +
  geom_hline(yintercept = est$lower) +
  geom_hline(yintercept = est$upper) +
  geom_hline(yintercept = est$median, linetype = "dashed") +
  geom_col(aes(fill = exceeds), color = "black") +
  ggrepel::geom_text_repel(
    data = est_labels,
    aes(y = value, label = label),
    nudge_y = .001,
    min.segment.length = unit(0, "lines"),
    seed = 1
  ) +
  ggrepel::geom_text_repel(
    aes(label = label),
    nudge_y = .001,
    nudge_x = 1,
    seed = 1) +
  scale_x_date(
    breaks = df$date,
    date_labels = "%b\n%Y") +
  scale_y_continuous(
    breaks = scales::pretty_breaks(),
    limits = c(0, find_max(df$tp, .2)),
    expand = expansion()) +
  scale_fill_manual(
    breaks = c(T, F),
    values = c("#ff8e68", "#52c2a6"),
    labels = c("Yes", "No")) +
  labs(x = NULL, y = "Total phosphorus (mg/L)", fill = "Exceeds 0.075 mg/L limit?") +
  theme_classic() +
  theme(legend.position = "top")



makeReportPlots(test_nutrient, type = "nutrient")



# Thermistor ----

test_stn <- all_pts %>%
  filter(station_id == 10031935)
test_stn <- all_pts %>%
  filter(station_id %in% therm_data$station_id) %>%
  slice_sample(n = 1)
test_therm <- therm_data %>%
  filter(station_id == test_stn$station_id) %>%
  filter(year == max(year)) %>%
  mutate(formatted_date = format(date, "%b %d"))

daily_min <- test_therm %>%
  slice_min(order_by = temp_c, by = date) %>%
  select(date_time, min = temp_c)
daily_max <- test_therm %>%
  slice_max(order_by = temp_c, by = date) %>%
  select(date_time, max = temp_c)

daily_range <- bind_rows(daily_min, daily_max) %>%
  arrange(date_time) %>%
  mutate(across(c(min, max), ~zoo::na.spline(.x))) %>%
  mutate(mean = (min + max) / 2) %>%
  na.omit()

daily_mean <- test_therm %>%
  summarize(mean = mean(temp_c), .by = date) %>%
  mutate(date_time = as.POSIXct(paste(date, "12:00:00")))

add_rect <- function(ymin, ymax, color) {
  annotate("rect",
    xmin = as.POSIXct(-Inf), xmax = as.POSIXct(Inf),
    ymin = ymin, ymax = ymax, fill = color,
    alpha = .08
  )
}

test_therm %>%
  slice_max(temp_c, n = 24) %>%
  pull(temp_c) %>% mean()

library(ggrepel)

daily_range %>%
  ggplot(aes(x = date_time)) +
  add_rect(-Inf, 22.2, "blue") +
  add_rect(22.2, 25, "cornflowerblue") +
  add_rect(25, Inf, "darkorange") +
  geom_hline(yintercept = 22.2, color = alpha("blue", .25)) +
  geom_hline(yintercept = 25, color = alpha("cornflowerblue", .25)) +
  geom_text_repel(
    data = tibble(
      x = as.POSIXct(Inf),
      y = c(22.2, 25),
      label = c(
        "Cold-cool transition\n(22.2°C / 72°F)",
        "Cool-warm transition\n(25°C / 77°F)")),
    aes(x, y, label = label),
    seed = 1) +
  geom_ribbon(
    aes(ymin = min, ymax = max),
    color = NA, fill = alpha("lightblue", .1)) +
  geom_line(
    data = test_therm,
    aes(y = temp_c),
    color = alpha("#1f77b4", .5),
    linewidth = .25) +
  geom_ribbon(
    aes(ymin = min, ymax = max),
    color = alpha("#2590da", .25), fill = NA) +
  geom_line(
    aes(y = mean),
    color = "orange",
    linewidth = 1) +
  scale_x_datetime(
    breaks = "weeks",
    date_labels = "%b %d") +
  scale_y_continuous(
    breaks = scales::pretty_breaks(),
    labels = ~sprintf("%s°C\n(%s°F)", .x, c_to_f(.x))) +
  labs(x = NULL, y = "Water temperature (°C)") +
  theme_classic()

makeReportPlots(test_therm, "thermistor")

max_temp = 27
case_when(
  max_temp < 22.2 ~ "coldwater",
  max_temp < 25 ~ "coolwater",
  .default = "warmwater"
  )



stn <- slice_sample(all_stns, n = 1)
yr <- stn$max_fw_year
baseline <- baseline_data %>% filter(station_id == stn$station_id, year == yr)
nutrient <- nutrient_data %>% filter(station_id == stn$station_id, year == yr)
thermistor <- therm_data %>% filter(station_id == stn$station_id, year == yr)
params <- list(
  year = yr,
  stn = stn,
  data = list(
    baseline = baseline,
    nutrient = nutrient,
    thermistor = thermistor
  )
)

report_summary <- buildReportSummary(params)
has <- report_summary$has



stn <- all_pts %>%
  slice_sample(n = 1)


bbox <- with(stn, c(
  xmin = longitude - .3,
  xmax = longitude + .3,
  ymin = latitude - .2,
  ymax = latitude + .2
))

crop_counties <- st_crop(counties, bbox)
crop_wsheds <- st_crop(huc10, bbox)
crop_water <- st_crop(waterbodies, bbox)
wshed_labels <- st_centroid(crop_wsheds)

plt1 <- ggplot() +
  geom_sf(data = counties) +
  geom_sf_text(
    data = counties,
    aes(label = CountyNam),
    size = 1.5,
    alpha = .5) +
  geom_sf(
    data = st_as_sfc(st_bbox(bbox, crs = 4326)),
    fill = NA,
    color = "#c5050c") +
  geom_sf(data = stn, fill = "red", size = 4, shape = 24) +
  theme_void()

plt2 <- ggplot() +
  geom_sf(data = crop_counties, color = NA) +
  geom_sf(data = crop_water, color = NA, fill = "lightsteelblue") +
  geom_sf(data = crop_counties, fill = NA, linetype = "dashed") +
  geom_sf(data = crop_wsheds, alpha = .25) +
  geom_sf_text(
    data = crop_wsheds,
    aes(label = Huc10Name),
    alpha = .5,
    size = 3,
    check_overlap = T) +
  geom_sf(data = stn, fill = "red", size = 4, shape = 24) +
  theme_void()

gridExtra::grid.arrange(plt1, plt2, nrow = 1)

