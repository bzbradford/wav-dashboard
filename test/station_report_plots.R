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
    `Air temp (°C)` = ambient_air_temp,
    `Water temp (°C)` = water_temp,
    `DO (mg/L)` = d_o,
    `DO % sat.` = d_o_percent_saturation,
    `pH` = ph,
    `Transparency (cm)` = transparency_average,
    `Streamflow (cfs)` = streamflow_cfs,
    `Weather` = weather_last_2_days
  )

test_baseline %>%
  select(
    `Date` = formatted_date,
    `Stream width (ft)` = stream_width,
    `Average depth (ft)` = average_stream_depth,
    `Surface velocity (ft/s)` = average_surface_velocity,
    `Streamflow (cfs)` = streamflow_cfs,
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

