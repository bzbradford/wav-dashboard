# ui.R

library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(shiny)
library(shinyBS)
library(DT)
library(shinyjs)

ui <- fluidPage(
  title = "WAV Data Dashboard",
  useShinyjs(),

  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    includeHTML("google-analytics.html"),
    tags$style(HTML("
      body {
        font-family: 'Lato', sans-serif;
      }

      .container-fluid {
        max-width: 1000px;
        margin: auto;
      }

      .leaflet-control-layers-list::before {
        content: 'Basemap:';
        font-weight: bold;
      }

      .leaflet-control-layers-overlays::before {
        content: 'Layers:';
        font-weight: bold;
      }
    "))
  ),

  div(
    align = "center",
    style = "margin-top: 1em;",
    a(img(src = "wav-logo-color.png", height = "100px"), href = "https://wateractionvolunteers.org", target = "_blank")
  ),

  br(),

  h2("Water Action Volunteers - Data Dashboard", align = "center"),

  br(),

  # station select

  bsCollapse(
    bsCollapsePanel(
      title = "Station criteria",
      value = "opts",
      checkboxGroupInput(
        "years",
        "Show data from:",
        choices = data_years,
        selected = data_years,
        inline = T
      ),
      checkboxGroupInput(
        "stn_types",
        "Station type:",
        choices = station_types,
        selected = station_types,
        inline = TRUE
      ),
      uiOutput("total_stns_ui")
    ),
    open = "opts"
  ),

  fluidRow(
    column(12,
      selectInput(
        inputId = "station",
        label = "Select monitoring station:",
        choices = list("Select a station" = NULL),
        width = "100%"
      ),
      style = "z-index: 1001;"
    )
  ),


  # Map container

  br(),

  bsCollapse(
    bsCollapsePanel(
      title = "Station map",
      value = "map",
      div(
        style = "max-width: 1000px; margin: auto; border: 1px solid lightgrey;",
        leafletOutput("map", width = "100%", height = "700px")
      ),
      div(style = "margin: 0.5em 1em; 0.5em 1em;", align = "center",
        p(em(HTML(paste0("Baseline stations are shown in ", colorize(stn_colors$baseline), ", thermistor stations in ", colorize(stn_colors$thermistor), ", and nutrient stations in ", colorize(stn_colors$nutrient), ". Currently selected station is shown in ", colorize(stn_colors$current), ". Click on any startion to select it, or choose from the list above.")))),
        p(
          actionButton("zoom_in", "Zoom to selected site"),
          actionButton("reset_zoom", "Zoom out to all sites"),
          actionButton("random_site", "Random site")
        )
      )
    ),
    open = "map"
  ),

  h3("Station Lists:"),
  uiOutput("stnLists"),

  h3("More Information:"),
  p("Visit the Water Action Volunteers website at", HTML("<a href='https://wateractionvolunteers.org' target='_blank'>wateractionvolunteers.org</a>.")),

  br(),
  hr(),
  p(
    style = "color: grey; font-size: smaller; font-style: italic;",
    align = "center",
    "Dashboard developed by",
    a("Ben Bradford", href = "https://github.com/bzbradford", target = "_blank", .noWS = "after"),
    ", UW-Madison Entomology", br(),
    paste("Last updated:", format(file.info(".")$mtime, "%Y-%m-%d")), br(),
    a("Source code", href = "https://github.com/bzbradford/wav-dashboard", target = "_blank")
  )
)
