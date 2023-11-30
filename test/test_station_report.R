# Station report test
library(tidyverse)



# Report summary ----------------------------------------------------------

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

summarizeReportCols(baseline, report_baseline_cols)


baseline %>%
  select(all_of(names(baseline_count_cols))) %>%
  pivot_longer(everything()) %>%
  summarize(count = sum(!is.na(value)), .by = name) %>%
  left_join(enframe(baseline_count_cols), join_by(name)) %>%
  mutate(text = paste(count, value, if_else(count == 1, "measurement", "measurements"))) %>%
  filter(count != 0) %>%
  arrange(desc(count))

baseline %>%
  select(all_of(names(baseline_count_cols))) %>%
  pivot_longer(everything()) %>%
  summarize(count = sum(!is.na(value)), .by = name) %>%
  filter(count != 0) %>%
  left_join(enframe(baseline_count_cols), join_by(name)) %>%
  summarize(text = knitr::combine_words(value), .by = count) %>%
  mutate(text = paste(text, if_else(count == 1, "measurement", "measurements")))


baseline %>%
  mutate(formatted_date = date) %>%
  makeReportBaselineTable()



# Station map -------------------------------------------------------------

all_pts %>%
  slice_sample(n = 1) %>%
  makeReportMap()



# Baseline data -----------------------------------------------------------

test_stn <- slice_sample(all_pts, n = 1)
test_baseline <- baseline_data %>%
  filter(station_id == test_stn$station_id) %>%
  filter(year == max(year)) %>%
  mutate(formatted_date = format(date, "%b %d"))


test_baseline_ph <- baseline_data %>%
  filter(!is.na(ph)) %>%
  filter(ph <= 14, ph > 0)
test_stn <- all_pts %>%
  filter(station_id %in% test_baseline_ph$station_id) %>%
  slice_sample(n = 1)
test_baseline <- test_baseline_ph %>%
  filter(station_id == test_stn$station_id) %>%
  filter(year == max(year))

ph_colors <- c("#FF0000", "#FFA500", "#FFFF00", "#008000", "#9999ff", "#000066")
ph_colors <- c("#ff4331", "#ffd43a", "#00b82b", "#0099f7", "#844cbf")



makeReportPlots(test_baseline, "temp")
makeReportPlots(test_baseline, "do")
makeReportPlots(test_baseline, "trans")
makeReportPlots(test_baseline, "ph")
makeReportPlots(test_baseline, "flow")


baseline_data %>%
  filter(station_id == 10033880, year == 2023) %>%
  mutate(across(c(air_temp, water_temp), c_to_f)) %>%
  makeReportPlots("temp")



# Nutrient data -----------------------------------------------------------

test_stn <- all_pts %>%
  filter(station_id %in% nutrient_data$station_id) %>%
  slice_sample(n = 1)
test_nutrient <- nutrient_data %>%
  filter(station_id == test_stn$station_id) %>%
  filter(year == max(year)) %>%
  mutate(formatted_date = format(date, "%b %d"))

makeReportPlots(test_nutrient, type = "nutrient")



# Thermistor ---------------------------------------------------------------

test_stn <- all_pts %>%
  filter(station_id == 10031935)
test_stn <- all_pts %>%
  filter(station_id %in% therm_data$station_id) %>%
  slice_sample(n = 1)
test_therm <- therm_data %>%
  filter(station_id == test_stn$station_id) %>%
  filter(year == max(year)) %>%
  mutate(formatted_date = format(date, "%b %d"))

makeReportPlots(test_therm, "thermistor")



