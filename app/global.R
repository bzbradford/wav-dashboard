##  global.R  ##

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


# Development ------------------------------------------------------------------

# Reload/reset data
# source("setup.R")

# renv::status()
# renv::init()         # initiate renv if not already
# renv::dependencies() # show project dependencies
# renv::clean()        # remove unused packages
# renv::update()       # update project libraries
# renv::snapshot()     # save updated lock file to project
# renv::restore()      # restore versions from lockfile

# shiny::devmode(TRUE)
# shiny::devmode(FALSE)

# reproject spatial data?
# print(sf::sf_extSoftVersion())
# c("counties", "waterbodies", "nkes", "huc8", "huc10", "huc12", "all_pts") %>%
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

## OPTS ----
OPTS <- lst(
  baseline_plot_type_choices = list(
    "Annual" = "annual",
    "Long-term" = "trend"
  ),
  baseline_trend_type_choices = list(
    "None" = "scatter",
    "Month" = "month",
    "Year" = "year"
  ),
  baseline_plot_opts = tribble(
    ~col, ~name, ~unit, ~range_min, ~range_max, ~color,
    "water_temp", "Water temperature", "°C", 5, 25, "steelblue",
    "air_temp", "Air temperature", "°C", 5, 35, "orange",
    "d_o", "Dissolved oxygen", "mg/L", 2, 14, "navy",
    "transparency", "Transparency", "cm", 0, 120, "brown",
    "streamflow", "Streamflow", "cfs", 0, 10, "#48a67b",
    "ph", "pH", "pH", 6, 9, "orchid",
    "specific_cond", "Conductivity", "µS/cm", 100, 1000, "pink",
  ) %>% mutate(label = sprintf("%s (%s)", name, unit)),

  #' each option should have a value list and color list.
  #' the value list will be expanded with the plot limits on either side
  baseline_trend_annot = list(
    water_temp = list(
      values = c(20.7, 22.5, 24.6),
      labels = c("Cold/Cool-cold transition", "Cool-cold/Cool-warm transition", "Cool-warm/Warm transition"),
      colors = c("blue", "cornflowerblue", "lightsteelblue", "darkorange")
    ),
    air_temp = list(
      values = c(0, 10, 20, 30),
      labels = c("Freezing weather", "Cold weather", "Moderate weather", "Warm weather"),
      colors = c("blue", "steelblue", "cornflowerblue", "lightsteelblue", "darkorange")
    ),
    d_o = list(
      values = c(1, 3, 5, 6, 7),
      labels = c(
        "Aquatic life minimum\n(1 mg/L) ",
        "Limited forage fish\n(>3 mg/L) ",
        "Warmwater fish\n(>5 mg/L) ",
        "Coldwater fish\n(>6 mg/L) ",
        "Coldwater spawning\n(>7 mg/L) "
      ),
      colors = c("red", "orange", "gold", "lightblue", "steelblue", "cornflowerblue")
    ),
    transparency = list(
      values = c(55, 90, 120),
      labels = c("Low transparency", "Moderate transparency", "High transparency"),
      colors = c("khaki", "lightgreen", "lightblue", "lightblue")
    ),
    streamflow = list(
      values = c(.03, 3, 150),
      labels = c(
        "Headwater stream (0.03-3 cfs)\nEphemeral stream (< 0.03 cfs)",
        "Mainstem stream (3-150 cfs)\nHeadwater stream (0.03-3 cfs)",
        "Large river (> 150 cfs)\nMainstem stream (3-150 cfs)"
      ),
      colors = c("#915119", "#e3c283", "#73cdc1", "#09968e")
    ),
    ph = list(
      values = c(6, 7.5, 9),
      labels = c(
        "Minimum water quality\nstandard (pH 6.0) ",
        "Optimal for fish\n(pH 7.5) ",
        "Maximum water quality\nstandard (pH 9.0) "
      ),
      colors = c("orange", "lightgreen", "lightgreen", "purple")
    ),
    specific_cond = list(
      values = c(150, 800, 1500),
      labels = c("Low conductivity (<150 µs/cm)", "Normal conductivity (150-800 µs/cm)", "High conductivity (800-1500 µs/cm)", "Very high conductivity (>1500 µs/cm)"),
      colors = c("steelblue", "lightblue", "pink", "orchid")
    )
  ),
  baseline_trend_captions = list(
    "scatter" = "All observations for the selected parameter are shown above.",
    "month" = "Measurements from each month across all years are summarized using boxplots, which illustrate the median value (solid central bar), mean value (dashed central bar), Q1-Q3 interquartile range (main box) and full value range (whiskers). Individual observations are overlaid as points.",
    "year" = "Measurements from each year are summarized using boxplots, which illustrate the median value, mean value, interquartile range (main box), and full value range (whiskers). Individual observations are overlaid as points."
  ) %>% lapply(function(txt) paste(txt, " Interpretive ranges are illustrated to contextualize the observations.")),
  baseline_summary_vars = tribble(
    ~var, ~parameter, ~units,
    "d_o", "Dissolved oxygen", "mg/L",
    "water_temp", "Water temperature", "°C",
    "air_temp", "Air temperature", "°C",
    "transparency", "Transparency", "cm",
    "streamflow", "Stream flow", "cfs",
    "average_stream_depth", "Stream depth", "ft",
  ) %>% rowwise()
)


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
    if (shiny::isTruthy(arg)) {
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

clamp <- function(x, lower = x, upper = x) {
  pmax(lower, pmin(upper, x))
}

new_date <- function(y, m, d) {
  as.Date(paste(y, m, d, sep = "-"))
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
  HTML(paste0("<span style='font-weight:bold; color:", color, "'>", text, "</span>"))
}

set_page_url <- function(id) {
  if (!is.null(id)) {
    url <- sprintf("window.location.origin + window.location.pathname + '?stn=%s'", id)
    runjs(sprintf("window.history.replaceState(null, null, %s)", url))
  } else {
    runjs("window.history.replaceState(null, null, window.location.origin + window.location.pathname)")
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
    onclick = sprintf("html2canvas(document.querySelector('%s'), {scale: 3}).then(canvas => {saveAs(canvas.toDataURL(), '%s')})", id, filename),
    icon("save"), " ", text
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



# Nutrient tab -----------------------------------------------------------------

phoslimit <- 0.075 # mg/L or ppm

#' @param vals vector of phosphorus readings
getPhosEstimate <- function(vals) {
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
  ) %>%
    lapply(exp) %>%
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
getPhosExceedanceText <- function(vals, limit = phoslimit) {
  median <- vals$median
  lower <- vals$lower
  upper <- vals$upper

  msg <- "Insufficient data to determine phosphorus exceedance language based on the data shown above."
  if (anyNA(c(median, lower, upper))) {
    return(msg)
  }

  msg <- case_when(
    lower >= limit ~ "Total phosphorus levels clearly exceed the DNR's criteria (median and entire confidence interval above phosphorus standard) and the stream is likely impaired.",
    (lower <= limit) & (median >= limit) ~ "Total phosphorus levels may exceed the DNR's criteria (median greater than the standard, but lower confidence interval below the standard).",
    (upper >= limit) & (median <= limit) ~ "Total phosphorus levels may meet the DNR's criteria (median below phosphorus standard, but upper confidence interval above standard).",
    upper <= limit ~ "Total phosphorus levels clearly meet the DNR's criteria (median and entire confidence interval below phosphorus standard).",
    .default = msg
  )
  msg <- paste(msg, ifelse(vals$n < 6, "However, less than the required 6 monthly measurements were taken at this station.", ""))
}



# Thermistor tab ----------------------------------------------------------

buildDailyThermData <- function(df, units, stn) {
  temp_col <- ifelse(tolower(units) == "f", "temp_f", "temp_c")

  df %>%
    group_by(date) %>%
    summarise(
      hours = n(),
      min = min(!!sym(temp_col)),
      max = max(!!sym(temp_col)),
      mean = round(mean(!!sym(temp_col)), 2),
      units = units,
      lat = latitude[1],
      long = longitude[1]
    ) %>%
    mutate(
      station_id = stn$station_id,
      station_name = stn$station_name,
      .before = lat
    )
}

buildThermSummary <- function(df, units) {
  temp_col <- ifelse(tolower(units) == "f", "temp_f", "temp_c")

  daily <- df %>%
    mutate(temp = !!sym(temp_col)) %>%
    drop_na(temp) %>%
    arrange(date) %>%
    summarize(temp = mean(temp), .by = c(date, month))

  monthly <- daily %>%
    mutate(name = fct_inorder(format(date, "%B"))) %>%
    summarize(
      days = n_distinct(date),
      min = round(min(temp), 1),
      mean = round(mean(temp), 1),
      max = round(max(temp), 1),
      .by = c(month, name)
    ) %>%
    clean_names("title")

  total <- daily %>%
    summarize(
      name = "Total",
      days = n_distinct(date),
      min = round(min(temp), 1),
      mean = round(mean(temp), 1),
      max = round(max(temp), 1)
    ) %>%
    clean_names("title")

  bind_rows(monthly, total)
}



# Reports ----------------------------------------------------------------------

# baseline temperature data normally stored in C, must be converted to F
report_baseline_cols <- c(
  `Air temp (°C)` = "air_temp",
  `Water temp (°C)` = "water_temp",
  `DO (mg/L)` = "d_o",
  `DO (% sat.)` = "d_o_percent_saturation",
  `pH` = "ph",
  `Specific conductance (μS/cm)` = "specific_cond",
  `Transparency (cm)` = "transparency",
  `Streamflow (cfs)` = "streamflow"
)

# will be excluded if all NA
report_baseline_optional_cols <- c("ph", "specific_cond")

# will be included if any streamflow cfs data
report_streamflow_cols <- c(
  `Stream width (ft)` = "stream_width",
  `Average depth (ft)` = "average_stream_depth",
  `Surface velocity (ft/s)` = "average_surface_velocity"
)

# creates a paragraph of text describing the data
buildReportSummary <- function(params) {
  yr <- params$year
  data <- params$data

  counts <- list(
    baseline = nrow(data$baseline),
    nutrient = sum(!is.na(data$nutrient$tp)),
    thermistor = n_distinct(data$thermistor$date)
  )

  baseline_count_cols <- c(
    air_temp = "air temperature",
    water_temp = "water temperature",
    d_o = "dissolved oxygen",
    ph = "ph",
    specific_cond = "specific conductivity",
    transparency = "water transparency",
    streamflow = "streamflow"
  )

  for (var in names(baseline_count_cols)) {
    counts[[var]] <- sum(!is.na(data$baseline[[var]]))
  }

  has <- sapply(counts, function(n) {
    n > 0
  }, simplify = F)

  # generate summary paragraph
  base_counts <- tribble(
    ~count, ~text,
    counts$baseline, "baseline water quality measurements",
    counts$nutrient, "total phosphorus samples",
    counts$thermistor, "days of continuous water temperature logging"
  ) %>%
    filter(count > 0) %>%
    mutate(text = paste(count, text)) %>%
    pull(text) %>%
    combine_words()

  msg <- str_glue("This report covers monitoring data collected between Jan 1 and Dec 31, {yr}, and includes {base_counts}.")

  if (has$baseline) {
    baseline_counts <- data$baseline %>%
      select(all_of(names(baseline_count_cols))) %>%
      pivot_longer(everything()) %>%
      summarize(count = sum(!is.na(value)), .by = name) %>%
      filter(count != 0) %>%
      left_join(enframe(baseline_count_cols), join_by(name)) %>%
      summarize(text = combine_words(value), .by = count) %>%
      arrange(desc(count)) %>%
      mutate(text = paste(count, text, if_else(count == 1, "measurement", "measurements"))) %>%
      pull(text) %>%
      combine_words()
    msg <- paste0(msg, " Baseline water quality monitoring included ", baseline_counts, ".")
    msg <- paste0(msg, " Report downloaded on ", format(Sys.Date(), "%b %d, %Y"), ".")
  }

  list(counts = counts, has = has, message = msg)
}

# min/max etc for data cols
summarizeReportCols <- function(df, cols) {
  df %>%
    rename(all_of(cols)) %>%
    pivot_longer(all_of(names(cols)), names_to = "Parameter") %>%
    mutate(Parameter = factor(Parameter, levels = names(cols))) %>%
    drop_na(value) %>%
    summarize(
      across(value, list(
        N = ~ n(),
        Min = min,
        Max = max,
        Median = median,
        Mean = mean,
        SD = sd
      ), .names = "{.fn}"),
      .by = Parameter
    ) %>%
    mutate(CV = scales::percent(SD / Mean, accuracy = 1)) %>%
    mutate(across(Min:SD, ~ if_else(is.na(.x), NA, as.character(signif(.x, 3)))))
}

# summary table
buildReportBaselineTable <- function(baseline) {
  df <- baseline
  for (col in report_baseline_optional_cols) {
    if (all(is.na(df[[col]]))) df[[col]] <- NULL
  }
  df <- df %>% select(`Date` = formatted_date, any_of(report_baseline_cols))
  names(df) <- gsub(" (", "\\\n(", names(df), fixed = T) # add line breaks
  df
}

# summary table
buildReportStreamflowTable <- function(baseline) {
  baseline %>%
    mutate(across(flow_method_used, ~ gsub(" Method", "", .x))) %>%
    select(
      `Date` = formatted_date,
      all_of(report_streamflow_cols),
      `Streamflow (cfs)` = streamflow,
      `Flow method` = flow_method_used
    )
}

# creates some paragraphs with fieldwork details for the report
buildReportFieldworkComments <- function(baseline) {
  baseline %>%
    select(
      date,
      fsn = fieldwork_seq_no,
      names = group_desc,
      wx = weather_conditions,
      rec_wx = weather_last_2_days,
      com1 = fieldwork_comments,
      com2 = additional_comments
    ) %>%
    mutate(across(where(is.character), xtable::sanitize)) %>%
    rowwise() %>%
    mutate(comments = paste(na.omit(com1, com2), collapse = ". ")) %>%
    mutate(fieldwork_desc = str_glue(
      "* **{format(date, '%b %d, %Y')}** - ",
      "SWIMS fieldwork number: {fsn}. ",
      if_else(is.na(wx), "", " Weather: {wx}."),
      if_else(is.na(rec_wx), "", " Weather past 2 days: {rec_wx}."),
      if_else(nchar(comments) == 0, "", " Fieldwork comments: {comments}."),
      if_else(is.na(names), "", " Submitted by: {names}.")
    )) %>%
    pull(fieldwork_desc) %>%
    gsub("..", ".", ., fixed = T)
}






# Source files in /R ----

for (file in list.files("src", pattern = "\\.[Rr]$", full.names = TRUE)) {
  source(file)
}
