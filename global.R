##  global.R  ##

# Load Dependencies ----

suppressMessages({
  # core
  library(rlang) # walrus operator
  library(sf) # spatial
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
