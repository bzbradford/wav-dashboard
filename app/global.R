##  GLOBAL  ##

library(sf) # spatial

suppressPackageStartupMessages({
  # core
  library(rlang) # walrus operator
  library(markdown) # includeMarkdown
  library(tidyverse) # core
  library(lubridate) # date functions
  library(janitor) # name cleaning
  library(glue) # string interpolation

  # shiny
  library(shiny)
  library(bslib) # accordion panels
  library(shinyjs) # javascript
  library(shinyWidgets) # radioGroupButtons
  library(htmltools) # tagList

  # interactive
  library(DT) # data tables
  library(leaflet) # map
  library(leaflet.extras) # map JS buttons
  library(plotly) # plots

  # reports
  library(knitr)
})

# load data created in setup.R
load(".RData")

# Development ------------------------------------------------------------------

if (FALSE) {
  # load/reset data
  source("setup.R")

  # this got removed from CRAN
  remotes::install_github("trafficonese/leaflet.extras")
  renv::install("sf@1.0-24") # v1.1 failed to install

  # RENV
  renv::activate()
  renv::init()
  renv::status()
  renv::restore()
  renv::update()
  renv::snapshot()
  renv::clean()

  # enable development mode
  shiny::devmode(TRUE)

  # turn warnings into errors
  options(warn = 2)
}

# reproject spatial data?
# print(sf::sf_extSoftVersion())
# c("counties", "waterbodies", "nkes", "huc8", "huc10", "huc12", "all_pts") |>
#   lapply(function(var) {
#     shape <- eval(parse(text = var))
#     message("TEST >> ", var, " crs: ", st_crs(shape)$proj4string)
#     # assign(var, st_transform(shape, "+proj=longlat +datum=WGS84 +ellps=WGS84 +no_defs"))
#   })

# install xelatex

# Definitions ------------------------------------------------------------------

## stn_colors ----
stn_colors <- list(
  baseline = "green",
  nutrient = "orange",
  thermistor = "purple",
  current = "deepskyblue"
)

## tab_names ----
tab_names <- list(
  baseline = "Baseline data",
  nutrient = "Nutrient data",
  thermistor = "Thermistor data",
  watershed = "Watershed & landscape context",
  reports = "Downloadable reports",
  more = "Learn more"
)

stn_type_choices <- list(
  "Baseline (stream monitoring)" = "baseline",
  "Nutrient (total phosphorus)" = "nutrient",
  "Thermistor (temperature loggers)" = "thermistor"
)

stn_data_years <- rev(as.character(sort(unique(all_stn_years$year))))

stn_year_choices <- append(
  setNames(stn_data_years[1:4], stn_data_years[1:4]),
  setNames(
    stn_data_years[5],
    paste0(last(stn_data_years), "-", stn_data_years[5])
  )
)

map_color_choices <- list(
  "Station type" = "type",
  "Years of data" = "n_years",
  "Fieldwork events" = "n_fieldwork",
  "Measured value:" = "measure"
)

map_measure_choices <- data_opts |>
  filter(map_color_menu == TRUE) |>
  select(name, col) |>
  deframe()


# Functions --------------------------------------------------------------------

## Utility ----

# message and print an object to the console for testing
echo <- function(x) {
  message(deparse(substitute(x)))
  print(x)
}

# swaps names and values in a list or vector
invert <- function(x) {
  y <- as(names(x), class(x))
  names(y) <- x
  y
}

# return the first truthy argument
first_truthy <- function(...) {
  for (arg in list(...)) {
    if (shiny::isTruthy(arg) & length(arg) > 0) {
      return(arg)
    }
  }
  NULL
}

c_to_f <- function(c, d = 1) {
  round(c * 1.8 + 32, d)
}

f_to_c <- function(f, d = 1) {
  round((f - 32) * 5.0 / 9.0, d)
}

clamp <- function(x, lower = x, upper = x, na.rm = F) {
  pmax(lower, pmin(upper, x, na.rm = na.rm), na.rm = na.rm)
}

new_date <- function(y, m, d) {
  as.Date(paste(y, m, d, sep = "-"))
}

rnd_stn <- function(df) {
  stns <- unique(df$station_id)
  df |>
    filter(station_id == stns[sample(seq_along(stns), 1)])
}


## UI ----

# adds "All" to end of years list
year_choices <- function(years) {
  if (length(years) > 1) {
    c(years, "All")
  } else {
    years
  }
}

colorize <- function(text, color = tolower(text)) {
  HTML(paste0(
    "<span style='font-weight:bold; color:",
    color,
    "'>",
    text,
    "</span>"
  ))
}

set_page_url <- function(id) {
  if (!is.null(id)) {
    url <- sprintf(
      "window.location.origin + window.location.pathname + '?stn=%s'",
      id
    )
    runjs(sprintf("window.history.replaceState(null, null, %s)", url))
  } else {
    runjs(
      "window.history.replaceState(null, null, window.location.origin + window.location.pathname)"
    )
  }
}

set_page_title <- function(label) {
  if (!is.null(label)) {
    title <- sprintf("Station %s - WAV Dashboard", str_trunc(label, 40))
    shinyjs::runjs(sprintf("document.title = '%s'", title))
  } else {
    shinyjs::runjs("document.title = 'WAV Data Dashboard'")
  }
}

with_spinner <- function(ui, ...) {
  shinycssloaders::withSpinner(ui, type = 8, color = "#30a67d", ...)
}

build_plot_download_btn <- function(id, filename, text = "Download plot") {
  a(
    class = "btn btn-default btn-sm",
    style = "cursor: pointer;",
    title = paste0("Save a copy of this plot as '", filename, "'"),
    onclick = sprintf(
      "html2canvas(document.querySelector('%s'), {scale: 3}).then(canvas => {saveAs(canvas.toDataURL(), '%s')})",
      id,
      filename
    ),
    icon("save"),
    " ",
    text
  )
}

build_notice_ui <- function(content, type = c("ok", "error")) {
  type <- match.arg(type)
  class <- paste0("notice notice-", type)
  renderUI({
    div(
      class = class,
      div(
        class = "notice-close",
        "✕",
        onclick = "document.querySelector('#notice').remove()"
      ),
      div(class = "notice-text", content)
    )
  })
}

build_link <- function(text, href, title = text, ...) {
  a(
    text,
    href = href,
    title = href,
    .noWS = "outside",
    target = "_blank",
    ...
  )
}


## Plot helpers ----

# get the min and max of a vector for plotly axis ranges
# min_max <- function(v) {
#   possibly(
#     return(c(floor(min(v, na.rm = TRUE)), ceiling(max(v, na.rm = TRUE)))),
#     return(c(NA, NA))
#   )
# }

# get the color for the dissolved oxygen bars on plots
do_color <- function(do) {
  i <- min(max(round(do), 1), 11)
  RColorBrewer::brewer.pal(11, "RdBu")[i]
}


# Baseline data ----------------------------------------------------------------

get_baseline_param_choices <- function(df) {
  opts <- data_opts

  df |>
    pivot_longer(any_of(opts$col), names_to = "col") |>
    summarize(n = sum(!is.na(value)), .by = col) |>
    filter(n > 0) |>
    left_join(opts, join_by(col)) |>
    select(name, col) |>
    deframe()
}

# identify min and max values and returns the dates for each
stn_summary_min_max <- function(df, var) {
  v <- df[[var]]
  if (length(v) == 0) {
    return(tibble())
  }
  tibble(
    observations = length(na.omit(v)),
    min = df[which.min(v), ][[var]],
    mean = round(mean(df[[var]], na.rm = TRUE), 1),
    max = df[which.max(v), ][[var]],
    date_of_min = df[which.min(v), ]$date,
    date_of_max = df[which.max(v), ]$date
  )
}

if (F) {
  baseline_data |>
    slice_sample(n = 1, by = station_id) |>
    stn_summary_min_max("water_temp")
}

# creates the summary table for the baseline tab
build_baseline_summary <- function(df) {
  date_fmt <- ifelse(length(unique(df$year)) > 1, "%b %e, %Y", "%b %e")
  data_opts |>
    filter(source == "baseline") |>
    select(col, name, units) |>
    rowwise() |>
    reframe(pick(everything()), stn_summary_min_max(df, col)) |>
    mutate(across(
      c(min, mean, max),
      ~ if_else(is.na(units), as.character(.x), paste(.x, units))
    )) |>
    mutate(across(c(date_of_min, date_of_max), ~ format(.x, date_fmt))) |>
    select(-c(col, units)) |>
    clean_names("title")
}

if (F) {
  baseline_data |>
    slice_sample(n = 1, by = station_id) |>
    build_baseline_summary()
}


# for baseline and nutrient data downloads
format_for_dt <- function(df, transpose = TRUE, hide_empty = FALSE) {
  df <- df |> arrange(date)

  if (hide_empty) {
    df <- select(df, where(~ !all(is.na(.x))))
  }

  df <- df |>
    mutate(across(any_of(c("latitude", "longitude")), ~ round(.x, 6))) |>
    clean_names(
      case = "title",
      abbreviations = c("ID", "DNR", "WBIC", "HUC", "DO", "pH", "TP"),
      replace = c("d_o" = "DO", "tp" = "Total Phosphorus")
    )

  if (transpose) {
    df <- df |>
      mutate(date_n = n(), .by = Date) |>
      mutate(
        label = format(Date, "%b %d, %Y"),
        label = if_else(
          date_n > 1,
          paste0(label, " (", row_number(), ")"),
          label
        ),
        .by = Date
      ) |>
      select(-date_n) |>
      mutate(across(everything(), as.character)) |>
      pivot_longer(cols = -label, names_to = "Parameter") |>
      pivot_wider(names_from = label)
  }

  df
}

if (F) {
  baseline_data |> rnd_stn() |> format_for_dt()
  baseline_data |> rnd_stn() |> format_for_dt(FALSE)
  baseline_data |>
    filter(station_id == 10040926) |>
    format_for_dt() |>
    print(n = 50)
}

# used before formatting for display
merge_unit_cols <- function(df, units_suffix = "_units") {
  units_cols <- names(df)[grepl(paste0(units_suffix, "$"), names(df))]
  data_cols <- sub(paste0(units_suffix, "$"), "", units_cols)

  valid_pairs <- data_cols %in% names(df)
  units_cols <- units_cols[valid_pairs]
  data_cols <- data_cols[valid_pairs]

  for (i in seq_along(data_cols)) {
    df[[data_cols[i]]] <- paste(df[[data_cols[i]]], df[[units_cols[i]]])
    df[[data_cols[i]]][df[[data_cols[i]]] == "NA NA"] <- NA_character_
  }

  df[, !(names(df) %in% units_cols), drop = FALSE]
}

# baseline_data |>
#   rnd_stn() |>
#   merge_unit_cols() |>
#   format_for_dt() |> view()

# data structure for plotly background annotation rectangles
PlotAnnotOpts <- function(
  values = numeric(),
  labels = character(),
  colors = character()
) {
  stopifnot(is.numeric(values), length(values) > 0)
  stopifnot(is.character(labels), length(labels) == length(values))
  stopifnot(is.character(labels), length(colors) == length(values) + 1)

  lst(values, labels, colors)
}

## baseline_plot_annot ----
baseline_plot_annot <- lst(
  water_temp = PlotAnnotOpts(
    values = c(20.7, 22.5, 24.6),
    labels = c(
      "Cold/Cool-cold transition",
      "Cool-cold/Cool-warm transition",
      "Cool-warm/Warm transition"
    ),
    colors = c("blue", "cornflowerblue", "lightsteelblue", "darkorange")
  ),
  air_temp = PlotAnnotOpts(
    values = c(0, 10, 20, 30),
    labels = c(
      "Freezing weather",
      "Cold weather",
      "Moderate weather",
      "Warm weather"
    ),
    colors = c(
      "blue",
      "steelblue",
      "cornflowerblue",
      "lightsteelblue",
      "darkorange"
    )
  ),
  d_o = PlotAnnotOpts(
    values = c(1, 3, 5, 6, 7),
    labels = c(
      "Aquatic life minimum\n(1 mg/L) ",
      "Limited forage fish\n(>3 mg/L) ",
      "Warmwater fish\n(>5 mg/L) ",
      "Coldwater fish\n(>6 mg/L) ",
      "Coldwater spawning\n(>7 mg/L) "
    ),
    colors = c(
      "red",
      "orange",
      "gold",
      "lightblue",
      "steelblue",
      "cornflowerblue"
    )
  ),
  transparency = PlotAnnotOpts(
    values = c(55, 90, 120),
    labels = c(
      "Low transparency",
      "Moderate transparency",
      "High transparency"
    ),
    colors = c("khaki", "lightgreen", "lightblue", "lightblue")
  ),
  ph = PlotAnnotOpts(
    values = c(6, 7.5, 9),
    labels = c(
      "Minimum water quality\nstandard (pH 6.0) ",
      "Optimal for fish\n(pH 7.5) ",
      "Maximum water quality\nstandard (pH 9.0) "
    ),
    colors = c("orange", "lightgreen", "lightgreen", "purple")
  ),
  specific_cond = PlotAnnotOpts(
    values = c(150, 800, 1500, Inf),
    labels = c(
      "Low conductivity (<150 µs/cm)",
      "Normal conductivity (150-800 µs/cm)",
      "High conductivity (800-1500 µs/cm)",
      "Very high conductivity (>1500 µs/cm)"
    ),
    colors = c("steelblue", "lightblue", "pink", "orchid", "purple")
  ),
  streamflow = PlotAnnotOpts(
    values = c(.03, 3, 150),
    labels = c(
      "Headwater stream (0.03-3 cfs)\nEphemeral stream (< 0.03 cfs)",
      "Mainstem stream (3-150 cfs)\nHeadwater stream (0.03-3 cfs)",
      "Large river (> 150 cfs)\nMainstem stream (3-150 cfs)"
    ),
    colors = c("#915119", "#e3c283", "#73cdc1", "#09968e")
  ),
  biotic_index_score = PlotAnnotOpts(
    values = c(2, 2.5, 3.5),
    labels = c(
      "Poor (1-2) ",
      "Fair (2-2.5)",
      "Good (2.5-3.5) "
    ),
    colors = c("red", "orange", "green", "green")
  ),
)


# baseline_data |>
#   slice_sample(n = 1, by = station_id) |>
#   get_baseline_param_choices()

# Nutrient tab -----------------------------------------------------------------

#' @param vals vector of phosphorus readings
get_phos_estimate <- function(vals) {
  suppressWarnings({
    vals <- na.omit(vals)
    n <- length(vals)
    log_vals <- log(vals + .001)
    log_mean <- mean(log_vals)
    se <- sd(log_vals) / sqrt(n)
    tval <- qt(p = 0.80, df = n - 1)
  })
  params <- list(
    mean = log_mean,
    median = median(log_vals),
    lower = log_mean - tval * se,
    upper = log_mean + tval * se
  ) |>
    lapply(exp) |>
    lapply(signif, 3)
  params$n <- n
  params$limit <- phoslimit
  params
}

#' @param vals
#'   `n` number of observations
#'   `median` median value
#'   `lower` lower confidence limit
#'   `upper` upper confidence limit
#' @param limit state phosphorus exceedance limit
get_phos_exceedance_text <- function(vals, limit = phoslimit) {
  median <- vals$median
  lower <- vals$lower
  upper <- vals$upper

  msg <- "Insufficient data to determine phosphorus exceedance language based on the data shown above."
  if (anyNA(c(median, lower, upper))) {
    return(msg)
  }

  msg <- case_when(
    lower >=
      limit ~ "Total phosphorus levels clearly exceed the DNR's criteria (median and entire confidence interval above phosphorus standard) and the stream is likely impaired.",
    (lower <= limit) &
      (median >=
        limit) ~ "Total phosphorus levels may exceed the DNR's criteria (median greater than the standard, but lower confidence interval below the standard).",
    (upper >= limit) &
      (median <=
        limit) ~ "Total phosphorus levels may meet the DNR's criteria (median below phosphorus standard, but upper confidence interval above standard).",
    upper <=
      limit ~ "Total phosphorus levels clearly meet the DNR's criteria (median and entire confidence interval below phosphorus standard).",
    .default = msg
  )

  if (vals$n < 6) {
    msg <- paste(
      msg,
      "However, less than the required 6 monthly measurements were taken at this station."
    )
  }
}


# Thermistor tab ----------------------------------------------------------

build_therm_daily <- function(df, units, stn) {
  temp_col <- ifelse(tolower(units) == "f", "temp_f", "temp_c")

  df |>
    group_by(date) |>
    summarise(
      hours = n(),
      min = min(!!sym(temp_col)),
      max = max(!!sym(temp_col)),
      mean = round(mean(!!sym(temp_col)), 2),
      units = units,
      latitude = latitude[1],
      longitude = longitude[1]
    ) |>
    mutate(
      station_id = stn$station_id,
      station_name = stn$station_name,
      .before = latitude
    )
}

build_therm_summary <- function(df, units) {
  temp_col <- ifelse(tolower(units) == "f", "temp_f", "temp_c")

  daily <- df |>
    mutate(temp = !!sym(temp_col)) |>
    drop_na(temp) |>
    arrange(date) |>
    summarize(temp = mean(temp), .by = c(date, month))

  monthly <- daily |>
    mutate(name = fct_inorder(format(date, "%B"))) |>
    summarize(
      days = n_distinct(date),
      min = round(min(temp), 1),
      mean = round(mean(temp), 1),
      max = round(max(temp), 1),
      .by = c(month, name)
    ) |>
    clean_names("title")

  total <- daily |>
    summarize(
      name = "Total",
      days = n_distinct(date),
      min = round(min(temp), 1),
      mean = round(mean(temp), 1),
      max = round(max(temp), 1)
    ) |>
    clean_names("title")

  bind_rows(monthly, total)
}


# Source files in /R ----

for (file in list.files("src", pattern = "\\.[Rr]$", full.names = TRUE)) {
  source(file)
}
