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

  div(
    style = "max-width: 1000px; margin: auto; border: 1px solid grey;",
    leafletOutput("map", width = "100%", height = "800px")
  ),

  br(),

  h3("Monitoring Stations"),
  p("The sites on the map above show where in the state Water Action Volunteers made water quality monitoring measurements during the 2021 season."),
  p(strong("Baseline monitoring:"), "Volunteers enter the WAV program by training to do baseline stream monitoring. Each year, baseline volunteers journey to their monitoring sites once per month from May to October to collect four baseline parameters: dissolved oxygen, instantaneous temperature, transparency and streamflow. During at least two of these months (May/June and September/October), volunteers also collect macroinvertebrates to calculate a biotic index score. Once per season, some advanced volunteers also conduct a habitat assessment. In 2020, volunteers collected this baseline data at 284 unique monitoring sites. In 2021, these data were collected at 279 unique sites."),
  p(strong("Nutrient monitoring:"), "After at least one season of baseline monitoring, some WAV volunteers will support special projects monitoring. Special projects monitoring is designed to either use the same methods as DNR professionals for data collection or to meet specific data needs. Recently these special projects have included monitoring with meters, aquatic invasive species monitoring, nutrient monitoring, and deploying continuous temperature monitors. Nutrient monitoring is the most widespread of the special projects. Volunteers sample for total phosphorus concentrations in rivers and streams. In some instances, volunteers also collect suspended solids samples and/or nitrogen panels."),
  p(strong("Temperature loggers:"), "Across the state there are a number of automatic, deployed temperature loggers that continuously monitor water temperature in streams. This data can be useful for understanding seasonal stream dynamics, as lower temperatures can indicate higher flow rates, more oxygen-rich water, and overall healther stream systems. You can see more detailed stream temperature data from these stations on our ", HTML("<a href='https://data-viz.it.wisc.edu/wav-temp-loggers/' target = '_blank'>temperature logger data dashboard</a>.")),

  h3("Map Layers"),
  p(strong("DNR Regions:"), "The Department of Natural Resources has grouped Wisconsin's 72 counties into five different regions, which are shown on the map as a light color fill."),
  p(strong("Nine Key Elements Plans:"), "These are long-term plans for specific watersheds that provide a framework for improving water quality in a holistic manner. The nine elements help assess the contributing causes and sources of nonpoint source pollution, involve key stakeholders and prioritize restoration and protection strategies to address water quality problems. Learn more about NKEs at the", HTML("<a href='https://dnr.wisconsin.gov/topic/Nonpoint/9keyElement' target='_blank'>Wisconsin DNR</a>.")),
  p(strong("HUC8, HUC10, and HUC12 watersheds:"), "HUC stands for Hydrologic Unit Code and is a sequence of numbers or letters that identify a hydrological feature like a river, lake, or drainage basin. For this map, we are including HUC8 boundaries (subbasins), HUC10 boundaries (watersheds), and HUC12 boundaries (subwatersheds) as optional layers so you can better understand the hydrology of Wisconsin. HUC8 is the largest of these classifications, and HUC12 the smallest."),

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
    a("Source code", href = "https://github.com/bzbradford/wav-station-map", target = "_blank")
  )
)
