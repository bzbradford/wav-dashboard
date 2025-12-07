
#' Thermistor (hobo) data processing and validation for the WAV dashboard

library(tidyverse)



# Setup ----

data_dir <- function(f) {
  file.path("../data", f)
}

stn_master_list <- read_csv(data_dir("stn_master_list.csv"))



# Define functions ----

# reads in all raw hobo data csv files and parses them
read_hobos <- function(dir, yr) {
  require(tidyverse)
  require(lubridate)
  require(janitor)
  require(tools)

  c_to_f <- function(c) {
    c * 1.8 + 32
  }
  f_to_c <- function(f) {
    (f - 32) * 5 / 9.0
  }

  temp_check <- function(temp, unit) {
    if_else(
      unit == "F",
      between(temp, 23, 86),
      between(temp, -5, 30)
    )
  }

  errors <- c()

  warn <- function(sn, msg) {
    msg <- paste(sn, "==>", msg)
    message(msg)
    errors <<- c(errors, msg)
  }

  files <- list.files(dir, "*.csv", full.names = T)
  if (length(files) == 0) {
    stop("No csv files found in directory '", dir, "'")
  }
  files <- files[order(nchar(files), files)] # sort files

  raw_data <- lapply(files, function(file) {
    # check file name and get logger SN
    if (file_ext(file) != "csv") stop("File '", file, "' is not a csv!")
    message("\nReading ", basename(file), "...")
    sn <- gsub(".csv", "", basename(file))

    first_line <- readLines(file, n = 1)
    skip <- 0

    if (grepl("Plot", first_line)) skip <- 1

    import <- read_csv(file, skip = skip, col_select = 1:3, col_types = "ccc")
    message(paste0("SN: ", sn))

    if (grepl("(*F)", names(import)[3], useBytes = T)) {
      unit <- "F"
    } else if (grepl("(*C)", names(import)[3], useBytes = T)) {
      unit <- "C"
    } else {
      stop("FATAL: Unable to determine temperature units! Require (°F) or (°C) in temperature column name!")
    }

    data <- import %>%
      select(DateTime = 2, Temp = 3) %>%
      mutate(Temp = as.numeric(Temp)) %>%
      drop_na(Temp) %>%
      mutate(Unit = unit) %>%
      mutate(LoggerSN = as.numeric(sn), .before = 1) %>%
      mutate(DateTime = parse_date_time2(DateTime, c(
        "%Y-%m-%d %H:%M:%S",
        "%m-%d-%Y %H:%M:%S",
        "%m-%d-%y %H:%M:%S",
        "%m/%d/%y %I:%M:%S %p",
        "%m/%d/%y %H:%M:%S",
        "%m/%d/%Y %H:%M",
        "%m/%d/%Y %H:%M:%S"
      ), exact = T, tz = "America/Chicago")) %>%
      mutate(Date = as.Date(DateTime), Year = lubridate::year(Date), .after = DateTime) %>%
      mutate(TempOK = temp_check(Temp, Unit))

    print(data)

    cat(paste0(
      " => ", nrow(data), " obs\n",
      " => ", as.Date(min(data$Date)), " - ", as.Date(max(data$Date)), "\n",
      " => ", min(data$Temp), " - ", max(data$Temp), " °", unit, "\n"
    ))

    if (length(unique(data$Year)) > 1) {
      before <- nrow(data)
      years <- paste(sort(unique(data$Year)), collapse = ", ")
      # data <- filter(data, Year == yr)
      # after <- nrow(data)
      after <- filter(data, Year == yr) %>% nrow()
      warn(sn, paste("Multiple years in data range: ", years))
      warn(sn, paste("Note: ", before - after, "values are not from the inventory year."))
    }

    if (!all(data$TempOK)) {
      before <- nrow(data)
      bad_temps <- filter(data, !TempOK)
      data <- filter(data, TempOK)
      after <- nrow(data)
      warn(sn, paste("Removed ", before - after, "temperature value(s) out of range"))
      print(bad_temps)
    }

    data %>%
      mutate(
        TempF = ifelse(Unit == "F", Temp, round(c_to_f(Temp), 2)),
        TempC = ifelse(Unit == "C", Temp, round(f_to_c(Temp), 2))
      ) %>%
      select(-Temp, -Unit, -TempOK)
  })

  cat("\n")
  lapply(errors, function(err) message(err))

  clean_data <- bind_rows(raw_data) %>%
    clean_names() %>%
    mutate(inventory_year = yr)
  attr(clean_data, "spec") <- NULL
  clean_data
}


# cleans hobo data by trimming dates before/after deployment
# checks for temperatures out of range and issues warnings
# prints a status table summarizing all the loggers
# requires the `therm_inventory` table
clean_hobos <- function(hobodata, return_status = FALSE) {
  require(dplyr)

  n_loggers <- n_distinct(hobodata$logger_sn)
  message("Total loggers: ", n_loggers)

  before_counts <- hobodata %>%
    summarize(
      date_min = min(date),
      date_max = max(date),
      days = n_distinct(date),
      .by = logger_sn
    )

  cleaned <- hobodata %>%
    left_join(therm_inventory, join_by(logger_sn, inventory_year == year)) %>%
    select(-inventory_year) %>%
    mutate(
      after_deployed = if_else(!is.na(date_deployed), date > date_deployed, TRUE),
      before_removed = if_else(!is.na(date_removed), date < date_removed, TRUE)
    ) %>%
    filter(!is.na(station_id) & after_deployed & before_removed) %>%
    select(-c(after_deployed, before_removed)) %>%
    mutate(
      month = month(date),
      day = day(date),
      hour = hour(date_time),
      yday = yday(date),
      .after = year
    ) %>%
    arrange(logger_sn, date_time)

  print(cleaned)

  after_counts <- cleaned %>%
    summarize(
      deployed = first(date_deployed),
      removed = first(date_removed),
      clean_days = n_distinct(date),
      station_id = first(station_id),
      latitude = as.character(first(latitude)),
      longitude = as.character(first(longitude)),
      station_name = first(station_name),
      .by = logger_sn
    )

  counts <- before_counts %>%
    left_join(after_counts, join_by(logger_sn)) %>%
    mutate(
      days_rm = days - clean_days,
      status = case_when(
        is.na(station_id) ~ "missing station id",
        is.na(deployed) & is.na(removed) ~ "missing deploy/removal",
        is.na(deployed) ~ "missing deploy",
        is.na(removed) ~ "missing removal",
        T ~ "OK"
      ),
      .after = clean_days
    )

  print(counts, n = 100)
  message("Total loggers after cleaning: ", n_distinct(cleaned$logger_sn))
  message("Loggers missing station id: ", sum(counts$status == "missing station id"))
  message("Loggers missing deployment dates: ", sum(is.na(counts$deployed)))
  message("Loggers missing removal dates: ", sum(is.na(counts$removed)))

  if (return_status) {
    counts
  } else {
    select(cleaned, -c(contact_name, date_deployed, date_removed))
  }
}


# plotly showing temperature readout for a logger
# adds weather data behind to compare and look for anomalies
make_thermistor_plot <- function(df_hourly, weather = NULL) {
  require(dplyr)
  require(plotly)

  opts <- as.list(first(df_hourly))
  title <- with(opts, paste(
    "Year:", year, "|",
    "Logger SN:", logger_sn, "|",
    "Station ID:", station_id, "|",
    "Coords:", paste0(latitude, ", ", longitude)
  ))
  img_name <- with(opts, paste0(
    year, " thermistors - SN ", logger_sn, " - Stn ", station_id,
    sprintf(" (%.4f, %.4f)", latitude, longitude)
  ))

  # make sure daily values time is aligned to noon
  df_daily <- df_hourly %>%
    summarize(
      min = min(temp_f),
      mean = mean(temp_f),
      max = max(temp_f),
      .by = date
    ) %>%
    mutate(date_time = as.POSIXct(paste(date, "12:00:00")))
  daterange <- c(min(df_daily$date_time), max(df_daily$date_time))

  plt <- plot_ly()

  if (!is.null(weather)) {
    weather <- weather %>%
      mutate(date_time = as.POSIXct(paste(date, "12:00:00")))
    daily_ranges <- df_daily %>%
      summarize(water_range = max - mean, .by = date_time) %>%
      left_join(
        summarize(weather, air_range = max_temp - min_temp, .by = date_time),
        join_by(date_time)
      ) %>%
      mutate(ratio = water_range / air_range, valid = water_range < air_range)
    range_pal <-
      plt <- plt %>%
      add_bars(
        data = daily_ranges,
        x = ~date_time,
        y = ~ratio,
        marker = list(
          color = ~ratio,
          colorscale = "Viridis",
          cmin = 0, cmax = 1,
          reversescale = T
        ),
        yaxis = "y2",
        name = "Water:air ratio",
        hovertemplate = "%{y:.2f}"
      ) %>%
      add_ribbons(
        data = weather,
        x = ~date_time,
        ymin = ~min_temp,
        ymax = ~max_temp,
        line = list(color = "orange", width = 0.5, opacity = 0.1),
        fillcolor = "orange",
        opacity = 0.1,
        name = "Air temperature",
        hovertemplate = "%{y:.1f}"
      ) %>%
      add_trace(
        data = weather,
        x = ~date_time,
        y = ~avg_temp,
        name = "Mean air temp.",
        type = "scatter",
        mode = "lines",
        line = list(color = "orange", opacity = .1),
        hovertemplate = "%{y:.1f}"
      )
  }

  plt %>%
    add_ribbons(
      data = df_daily,
      x = ~date_time,
      ymin = ~min,
      ymax = ~max,
      line = list(color = "lightblue", width = 0.5, opacity = 0),
      fillcolor = "lightblue",
      opacity = 0.5,
      name = "Daily water temp range",
      hovertemplate = "%{y:.1f}"
    ) %>%
    add_trace(
      data = df_hourly,
      x = ~date_time,
      y = ~temp_f,
      name = "Hourly Water temp (F)",
      type = "scatter",
      mode = "lines",
      line = list(color = "#1f77b4", width = 0.5, opacity = 0.8)
    ) %>%
    add_trace(
      data = df_daily,
      x = ~date_time,
      y = ~mean,
      name = "Mean daily water temp.",
      type = "scatter",
      mode = "lines",
      line = list(color = "blue"),
      hovertemplate = "%{y:.1f}"
    ) %>%
    layout(
      title = title,
      showlegend = TRUE,
      xaxis = list(
        title = "Date and time",
        range = daterange
      ),
      yaxis = list(
        title = "Temperature (F)",
        range = c(32, 90),
        zerolinecolor = "lightgrey"
      ),
      yaxis2 = list(
        title = "Water:air temperature ratio",
        side = "right", overlaying = "y",
        showgrid = F, zeroline = F, range = c(0, 4),
        automargin = T
      ),
      hovermode = "x unified",
      legend = list(
        orientation = "h",
        x = 0.25,
        y = 1
      ),
      margin = list(t = 50)
    ) %>%
    config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      modeBarButtons = list(list("toImage")),
      toImageButtonOptions = list(
        format = "png",
        filename = img_name,
        height = 500,
        width = 1000,
        scale = 1.25
      )
    ) %>%
    hide_colorbar()
}


# cycles through hobo data and plots them each in turn
# can give one or more serial numbers to inspect, or it goes through all of them
inspect_hobos <- function(hobodata, serials = sort(unique(hobodata$logger_sn))) {
  i <- 1
  n <- length(serials)
  for (i in 1:n) {
    sn <- serials[i]
    message("Logger ", i, "/", n, ": SN ", sn)
    hobo <- hobodata %>% filter(logger_sn == sn)
    info <- last(hobo)
    url <- build_agweather_url(hobo)
    weather <- get_agweather_data(url)
    make_thermistor_plot(hobo, weather) %>% print()
    if (n == 1 || i == n) break
    resp <- readline("[Enter] for next, q to quit > ")
    if (resp != "") break
  }
}

# create a URL to get weather data from AgWeather
build_agweather_url <- function(df) {
  lat <- df$latitude[1]
  lng <- df$longitude[1]
  start_date <- min(df$date)
  end_date <- max(df$date)
  if (any(is.na(c(lat, lng, start_date, end_date)))) {
    return(NULL)
  }
  str_glue("https://agweather.cals.wisc.edu/api/weather?lat={lat}&lng={lng}&start_date={start_date}&end_date={end_date}")
}

# pulls and formats weather data from AgWeather
get_agweather_data <- function(url) {
  if (is.null(url)) {
    return(NULL)
  }
  httr::GET(url) %>%
    httr::content() %>%
    pluck("data") %>%
    enframe() %>%
    unnest_wider("value")
}

# saves all the hobo files individually for SWIMS upload
export_hobos <- function(clean_data, yr = clean_data$year[1], logger_serials = unique(clean_data$logger_sn), out_dir = "cleaned") {
  out_dir <- file.path(out_dir, yr)
  dir.create(out_dir, showWarnings = F)
  for (sn in logger_serials) {
    df <- clean_data %>%
      filter(logger_sn == sn) %>%
      mutate(across(date_time, ~ format(.x, "%Y-%m-%d %H:%M:%S")))
    stn_id <- df$station_id[1]
    fname <- glue::glue("Hobo data {yr} - SN {sn} - Stn {stn_id}.csv")
    fpath <- file.path(out_dir, fname)
    message("Writing '", fpath, "'")
    write_csv(df, fpath)
  }
}


# Read and check Hobo data ----

# Load thermistor inventory, matching SNs with WAV Stns
# update the inventory with correct deploy/retrieve dates
# then re-run the cleaning
therm_inventory <- read_csv("combined-hobo-inventory.csv") %>%
  left_join({
    stn_master_list %>%
      select(station_id, station_name, latitude, longitude)
  })


#' File format expectations:
#' - Row 1 may be skipped, often contained 'plot title' or other heading.
#' - Obs # in column 1
#' - Date in column 2
#' - Temperature in column 3
#'   - Units must be specified in column name

# Read in raw hobo csv files. Indicate inventory year
hobos_in_2020 <- read_hobos("hobo/2020", 2020)
hobos_in_2021 <- read_hobos("hobo/2021", 2021)
hobos_in_2022 <- read_hobos("hobo/2022", 2022)
hobos_in_2023 <- read_hobos("hobo/2023", 2023)
hobos_in_2023_mrk <- read_hobos("hobo/2023_mrk", 2023)
hobos_in_2024 <- read_hobos("hobo/2024", 2024)
# these two hobos were found, having been deployed for multiple years
hobos_in_2024_extra <- read_hobos("hobo/2024_extra", 2024)
hobos_in_2024_mrk <- read_hobos("hobo/2024_mrk", 2024)

# clean the hobo data using the deployment dates in the inventory
hobos_2020 <- clean_hobos(hobos_in_2020)
hobos_2021 <- clean_hobos(hobos_in_2021)
hobos_2022 <- clean_hobos(hobos_in_2022)
hobos_2023_wav <- clean_hobos(hobos_in_2023)
hobos_2023_mrk <- clean_hobos(hobos_in_2023_mrk)
hobos_2024_wav <- clean_hobos(hobos_in_2024)
hobos_2024_extra <- clean_hobos(hobos_in_2024_extra)
hobos_2024_mrk <- clean_hobos(hobos_in_2024_mrk)


# generate interactive charts to inspect the data and compare to air temperatures
# currently no way to easily trim internal dates when a logger becomes exposed to the air
# can use this to confirm deployment and retrieval dates
# update dates in the inventory file if desired, then re-run the cleaning
inspect_hobos(hobos_2020)
inspect_hobos(hobos_2021)
inspect_hobos(hobos_2022)
inspect_hobos(hobos_2023_wav)
inspect_hobos(hobos_2023_mrk)
inspect_hobos(hobos_2024_wav, 20361490)
inspect_hobos(hobos_2024_extra)
inspect_hobos(hobos_2024_mrk)

# merge sets, exclude logger(s) with very dubious data
hobos_2023 <- bind_rows(hobos_2023_wav, hobos_2023_mrk) %>% filter(!(logger_sn %in% c(20820405)))
hobos_2024 <- bind_rows(hobos_2024_wav, hobos_2024_extra, hobos_2024_mrk)

# save these cleaned datasets
hobos_2020 %>% write_csv("cleaned/hobos-cleaned-2020.csv.gz")
hobos_2021 %>% write_csv("cleaned/hobos-cleaned-2021.csv.gz")
hobos_2022 %>% write_csv("cleaned/hobos-cleaned-2022.csv.gz")
hobos_2023 %>% write_csv("cleaned/hobos-cleaned-2023.csv.gz")
hobos_2024 %>% write_csv("cleaned/hobos-cleaned-2024.csv.gz")

# export individual CSVs, indicate output year folder
export_hobos(hobos_2020, 2020)
export_hobos(hobos_2021, 2021)
export_hobos(hobos_2022, 2022)
export_hobos(hobos_2023, 2023)
export_hobos(hobos_2024, 2024, logger_serials = 20361490)


# Merge hobodata ----

hobo_data <-
  bind_rows(
    hobos_2020,
    hobos_2021,
    hobos_2022,
    hobos_2023,
    hobos_2024
  ) %>%
  filter(!is.na(station_id), !is.na(date_time))

# loggers per year
hobo_data %>%
  count(logger_sn, year) %>%
  count(year)

tz(hobo_data$date_time)

# Collect list of SNs by year
hobo_serials <- hobo_data %>%
  distinct(year, logger_sn) %>%
  mutate(have_data = T)

# Are we missing data for loggers in the inventory?
therm_info <- therm_inventory %>%
  left_join(hobo_serials) %>%
  replace_na(list(have_data = F)) %>%
  arrange(year)

# save list of loggers without inventory
local({
  df <- therm_info %>%
    filter(!have_data)
  show(df)
  df %>%
    write_csv("QC/Thermistors - Inventory entries without matching data.csv")
})


therm_info_export <- therm_info %>%
  filter(have_data) %>%
  select(
    year,
    logger_sn,
    device_type,
    contact_name,
    date_deployed,
    date_removed,
    station_id,
    station_name,
    latitude,
    longitude
  )


# Export data ----

therm_info_export %>% write_csv(data_dir("therm_inventory.csv"))
hobo_data %>% write_csv(data_dir("therm_data.csv.gz"))
