##  global.R  ##

# Load Dependencies ----

suppressMessages({
  # core
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(sf)
  library(rlang)
  library(markdown)

  # shiny
  library(shiny)
  library(shinyBS)
  library(shinyjs)
  library(shinythemes)
  library(shinyWidgets)
  library(htmltools)

  # display
  library(DT)
  library(leaflet)
  library(leaflet.extras)
  library(plotly)
  library(RColorBrewer)
})
