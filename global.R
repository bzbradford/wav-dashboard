##  global.R  ##

# Load Dependencies ----

library(sf) # spatial

suppressMessages({
  # core
  library(rlang) # walrus operator
  library(markdown) # includeMarkdown
  library(tidyverse) # core
  library(lubridate) # date functions
  library(janitor) # name cleaning
  library(glue) # string interpolation

  # shiny
  library(shiny)
  library(shinyBS) # bscollapse
  library(shinyjs) # javascript
  library(shinythemes) # theme
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


# Definitions ----

stn_colors <- list(
  baseline = "green",
  nutrient = "orange",
  thermistor = "purple",
  current = "deepskyblue"
)

tab_names <- list(
  baseline = "Baseline data",
  nutrient = "Nutrient data",
  thermistor = "Thermistor data",
  watershed = "Watershed & landscape context",
  reports = "Downloadable reports",
  more = "Learn more"
)


# Functions ---------------------------------------------------------------

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
  for (arg in list(...)) if (shiny::isTruthy(arg)) return(arg)
  NULL
}

c_to_f <- function(c, d = 1) {
  round(c * 1.8 + 32, d)
}

f_to_c <- function(f, d = 1) {
  round((f - 32) * 5.0 / 9.0, d)
}

clamp <- function(x, lower = x, upper = x) {
  if_else(
    is.na(x) | is.null(x), x,
    if_else(x < lower, lower,
      if_else(x > upper, upper, x)))
}

newDate <- function(y, m, d) {
  as.Date(paste(y, m, d, sep = "-"))
}



## HTML / JS ----

colorize <- function(text, color = tolower(text)) {
  shiny::HTML(paste0("<span style='font-weight:bold; color:", color, "'>", text, "</span>"))
}

set_page_url <- function(id) {
  if (!is.null(id)) {
    shinyjs::runjs(sprintf("window.history.replaceState(null, null, window.location.origin + window.location.pathname + '?stn=%s')", id))
  } else {
    shinyjs::runjs("window.history.replaceState(null, null, window.location.origin + window.location.pathname)")
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

withSpinnerProxy <- function(ui, ...) {
  ui %>% shinycssloaders::withSpinner(type = 8, color = "#30a67d", ...)
}

buildPlotDlBtn <- function(id, filename, text = "Download plot") {
  a(
    class = "btn btn-default btn-sm",
    style = "cursor: pointer;",
    onclick = sprintf("html2canvas(document.querySelector('%s'), {scale: 3}).then(canvas => {saveAs(canvas.toDataURL(), '%s')})", id, filename),
    icon("save"), " ", text
  )
}


## Plot helpers ----

# plotly horizontal line annotation
hline <- function(y = 0, color = "black") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color, dash = "dash")
  )
}

# plotly rectanglular annotation
rect <- function(ymin, ymax, color = "red") {
  list(
    type = "rect",
    fillcolor = color,
    line = list(color = color),
    opacity = 0.1,
    y0 = ymin,
    y1 = ymax,
    xref = "paper",
    x0 = 0,
    x1 = 1,
    layer = "below"
  )
}

# get the min and max of a vector for plotly axis ranges
min_max <- function(v) {
  possibly(
    return(c(floor(min(v, na.rm = T)), ceiling(max(v, na.rm = T)))),
    return(c(NA, NA))
  )
}

# get the color for the dissolved oxygen bars on the baseline plotly
do_color <- function(do) {
  i <- min(max(round(do), 1), 11)
  RColorBrewer::brewer.pal(11, "RdBu")[i]
}

find_max <- function(vals, min_val) {
  vals <- na.omit(vals)
  if (length(vals) == 0) return(min_val)
  # ceiling(max(min_val, max(vals)) * 1.1)
  max(min_val, max(vals)) * 1.1
}

# used for x axis in plots
setReportDateRange <- function(dates, pad_right = FALSE) {
  yr <- format(dates[1], "%Y")
  default_range <- as.Date(paste0(yr, c("-05-1", "-10-1")))
  lims <- c(
    min(dates - 10, default_range[1]),
    max(dates + 10, default_range[2])
  )
  if (pad_right) lims[2] <- lims[2] + 30
  lims
}

setAxisLimits <- function(vals, lower, upper) {
  lims <- c(
    min(vals, lower, na.rm = T),
    max(vals, upper, na.rm = T)
  )
  lims + abs(lims) * c(-.1, .1)
}

addRectDate <- function(ymin, ymax, color) {
  gg <- annotate("rect",
    xmin = as.Date(-Inf), xmax = as.Date(Inf),
    ymin = ymin, ymax = ymax, fill = alpha(color, .05)
  )
  if (!is.infinite(ymax))
    gg <- c(gg, geom_hline(yintercept = ymax, color = alpha(color, .25)))
  gg
}

addRectDatetime <- function(ymin, ymax, color) {
  gg <- annotate("rect",
    xmin = as.POSIXct(-Inf), xmax = as.POSIXct(Inf),
    ymin = ymin, ymax = ymax, fill = alpha(color, .05)
  )
  if (!is.infinite(ymax))
    gg <- c(gg, geom_hline(yintercept = ymax, color = alpha(color, .2)))
  gg
}


## Server ----

# adds "All" to end of years list
year_choices <- function(years) {
  if (length(years) > 1) {
    c(years, "All")
  } else {
    years
  }
}



# Settings ----------------------------------------------------------------

OPTS <- lst(

  ## Baseline tab ----

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
        "Coldwater spawning\n(>7 mg/L) "),
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
        "Large river (> 150 cfs)\nMainstem stream (3-150 cfs)"),
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


# Dev ----

# reproject spatial data?
# print(sf::sf_extSoftVersion())
# c("counties", "waterbodies", "nkes", "huc8", "huc10", "huc12", "all_pts") %>%
#   lapply(function(var) {
#     shape <- eval(parse(text = var))
#     message("TEST >> ", var, " crs: ", st_crs(shape)$proj4string)
#     # assign(var, st_transform(shape, "+proj=longlat +datum=WGS84 +ellps=WGS84 +no_defs"))
#   })
