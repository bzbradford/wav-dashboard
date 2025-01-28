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
  library(shinycssloaders) # withSpinner

  # interactive
  library(DT) # data tables
  library(leaflet) # map
  library(leaflet.extras) # map JS buttons
  library(plotly) # plots

  # reports
  library(knitr)
})


# Settings ----

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
    "scatter" = "All observations for the selected parameter are shown above. Interpretive ranges are illustrated to contextualize the observations.",
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
