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

str_to_sentence("PARTLY_SUNNY")

makeReportMap(test_stn)

makeReportTempPlot(test_baseline)

makeReportDOPlot(test_baseline)

makeReportTransPlot(test_baseline)

get_date_range <- function(dates) {
  yr <- format(dates[1], "%Y")
  default_range <- as.Date(paste0(yr, c("-05-1", "-10-1")))
  final_range <- c(
    min(dates, default_range[1]),
    max(dates, default_range[2])
  )
  # seq(format(final_range, "%m"))
  final_range
}

get_date_range(test_baseline$date)

makeReportPlots(test_baseline, type = "temp")
makeReportPlots(test_baseline, type = "do")
makeReportPlots(test_baseline, type = "trans")
makeReportPlots(test_baseline, type = "flow")
