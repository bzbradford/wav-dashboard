# ui.R

suppressMessages({
  library(tidyverse)
  library(sf)
  library(leaflet)
  library(leaflet.extras)
  library(htmltools)
  library(shiny)
  library(shinyBS)
  library(DT)
  library(shinyjs)
})


# Styles ------------------------------------------------------------------

head_css <- "
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
"

data_tab_css <- "
  min-height: 300px;
  margin-top: 1em;
"


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  title = "WAV Data Dashboard",
  useShinyjs(),

  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    includeHTML("google-analytics.html"),
    tags$style(HTML(head_css))
  ),

  div(
    align = "center",
    style = "margin-top: 1em;",
    a(img(src = "wav-logo-color.png", height = "100px"), href = "https://wateractionvolunteers.org", target = "_blank")
  ),

  br(),

  h2("Water Action Volunteers - Data Dashboard", align = "center"),

  br(),
  br(),

# Map caption -----------------------------------------------------------

  div(style = "margin: 0.5em 1em; 0.5em 1em;", align = "center",
    p(em(HTML(paste0("Baseline stations are shown in ", colorize(stn_colors$baseline), ", thermistor stations in ", colorize(stn_colors$thermistor), ", and nutrient stations in ", colorize(stn_colors$nutrient), ". Currently selected station is shown in ", colorize("blue", stn_colors$current), ". Click on any station to select it, or choose from the list below the map"))))
  ),


# Map sidebar layout ------------------------------------------------------

  sidebarLayout(
    sidebarPanel(
      p(strong("Stations with data from:")),
      fluidRow(
        column(5,
          checkboxGroupInput(
            inputId = "years",
            label = NULL,
            choices = data_years,
            selected = data_years
          )
        ),
        column(7,
          radioButtons(
            inputId = "year_exact_match",
            label = NULL,
            choices = list(
              "ANY selected year" = FALSE,
              "ALL selected years" = TRUE
            )
          )
        )
      ),
      hr(),
      checkboxGroupInput(
        inputId = "stn_types",
        label = "Station data types:",
        choices = station_types,
        selected = station_types
      ),
      hr(),
      div(
        style = "text-align: center; font-weight: bold;",
        textOutput("total_stns_text")
      ),
      hr(),
      div(
        style = "text-align: center; line-height: 3em;",
        actionButton("zoom_in", "Zoom to selected site"), br(),
        actionButton("reset_zoom", "Zoom out to all sites"), br(),
        actionButton("random_site", "Random site")
      )
    ),
    mainPanel(
      div(
        style = "max-width: 1000px; margin: auto; border: 1px solid lightgrey;",
        leafletOutput("map", width = "100%", height = "700px")
      ),

    ),
    position = "right"
  ),






# Station dropdown --------------------------------------------------------

  h4("Current station:"),
  selectInput(
    inputId = "station",
    label = NULL,
    choices = list("Select a station" = NULL),
    width = "100%"
  ),


# Station info ------------------------------------------------------------

  uiOutput("stn_info_ui"),


# Data tabs ---------------------------------------------------------------

  tabsetPanel(
    tabPanel(
      title = "Baseline data",
      div(
        style = data_tab_css,
        uiOutput("baseline_tab")
      )
    ),
    tabPanel(
      title = "Thermistor data",
      div(
        style = data_tab_css,
        uiOutput("thermistor_tab")
      )
    ),
    tabPanel(
      title = "Nutrient data",
      div(
        style = data_tab_css,
        uiOutput("nutrient_tab")
      )
    ),
    tabPanel(
      title = "Station lists",
      div(
        style = data_tab_css,
        uiOutput("station_lists")
      )
    )
  ),


# Footer ------------------------------------------------------------------

  br(),
  hr(),
  h4("More information:"),
  p("Visit the Water Action Volunteers website at", HTML("<a href='https://wateractionvolunteers.org' target='_blank'>wateractionvolunteers.org</a>.")),
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
