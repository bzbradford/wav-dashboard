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

  # Baseline tab
  baseline_plot_opts = tribble(
    ~col, ~name, ~unit, ~range_min, ~range_max, ~color,
    "water_temp", "Temperature, water", "°C", 5, 25, "steelblue",
    "air_temp", "Temperature, air", "°C", 5, 35, "orange",
    "d_o", "Dissolved oxygen", "mg/L", 3, 12, "navy",
    "transparency", "Transparency", "cm", 0, 120, "brown",
    "streamflow", "Streamflow", "cfs", 0, 25, "#48a67b",
    "ph", "pH", "pH", 6, 9, "orchid",
    "specific_cond", "Conductivity", "µS/cm", 100, 1000, "pink",
  ) %>% mutate(label = sprintf("%s (%s)", name, unit)),

  baseline_plot_type_choices = list(
    "Annual" = "annual",
    "Long-term" = "trend"
  ),

  baseline_trend_type_choices = list(
    "Month" = "box_month",
    "Year" = "box_year",
    "None" = "scatter"
  ),

  baseline_trend_captions = list(
    "scatter" = "All measurements are shown as points above, with a best-fit linear model and a smoothed trend line (LOESS) shown to illustrate any trends in the data over time. Click on an item in the legend to hide or show it on the plot.",
    "box_month" = "Measurements from each month across all years are summarized using boxplots, which illustrate the median value (solid central bar), mean value (dashed central bar), Q1-Q3 interquartile range (main box) and full value range (whiskers). Individual observations are overlaid as points. Change the grouping options and the measurement type using the buttons above the plot.",
    "box_year" = "Measurements from each year are summarized using boxplots, which illustrate the median value, mean value, interquartile range (main box), and full value range (whiskers). Individual observations are overlaid as points. Change the grouping options and the measurement type using the buttons above the plot."
  ),

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
