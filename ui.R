# ui.R

# UI ----------------------------------------------------------------------

ui <- fluidPage(

  title = "WAV Data Dashboard",
  theme = shinytheme("flatly"),
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

  h2("Stream Monitoring Data Dashboard", align = "center"),

  br(),
  br(),

# Map caption -----------------------------------------------------------

  div(style = "margin: 0.5em 1em; 0.5em 1em;", align = "center",
    p(em(HTML(paste0("Baseline stations are shown in ", colorize(stn_colors$baseline), ", thermistor stations in ", colorize(stn_colors$thermistor), ", and nutrient stations in ", colorize(stn_colors$nutrient), ". Currently selected station is shown in ", colorize("blue", stn_colors$current), ". Click on any station to select it, or choose from the list below the map."))))
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
            selected = data_years[1]
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
        style = "line-height: 3em;",
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

  br(),




# Station dropdown --------------------------------------------------------

  div(
    class = "well",
    style = "padding-top: 0.5em;",
    h4("Current station:"),
    selectInput(
      inputId = "station",
      label = NULL,
      choices = list("Select a station" = NULL),
      width = "100%"
    ),
    em("To search for a station by name, delete the text above and start typing.")
  ),

  bsCollapse(
    bsCollapsePanel(
      title = "Station information",
      div(
        style = flex_row,
        div(style = flex_col, uiOutput("stn_info")),
        div(style = flex_col, uiOutput("stn_coverage"))
      )
    )
  ),


# Data tabs ---------------------------------------------------------------

  tabsetPanel(
    tabPanel(
      title = "Baseline data",
      div(style = tab_css, uiOutput("baseline_tab"))
    ),
        tabPanel(
      title = "Nutrient data",
      div(style = tab_css, uiOutput("nutrient_tab"))
    ),
    tabPanel(
      title = "Thermistor data",
      div(style = tab_css, uiOutput("therm_tab"))
    ),
    tabPanel(
      title = "Station lists",
      div(style = tab_css, uiOutput("station_lists"))
    ),
    tabPanel(
      title = "More information",
      div(
        style = tab_css,
        h3("Monitoring Stations"),
        p("The sites on the map above show where in the state Water Action Volunteers made water quality monitoring measurements during the 2021 season."),
        p(strong("Baseline monitoring:"), "Volunteers enter the WAV program by training to do baseline stream monitoring. Each year, baseline volunteers journey to their monitoring sites once per month from May to October to collect four baseline parameters: dissolved oxygen, instantaneous temperature, transparency and streamflow. During at least two of these months (May/June and September/October), volunteers also collect macroinvertebrates to calculate a biotic index score. Once per season, some advanced volunteers also conduct a habitat assessment. In 2020, volunteers collected this baseline data at 284 unique monitoring sites. In 2021, these data were collected at 279 unique sites."),
        p(strong("Nutrient monitoring:"), "After at least one season of baseline monitoring, some WAV volunteers will support special projects monitoring. Special projects monitoring is designed to either use the same methods as DNR professionals for data collection or to meet specific data needs. Recently these special projects have included monitoring with meters, aquatic invasive species monitoring, nutrient monitoring, and deploying continuous temperature monitors. Nutrient monitoring is the most widespread of the special projects. Volunteers sample for total phosphorus concentrations in rivers and streams. In some instances, volunteers also collect suspended solids samples and/or nitrogen panels."),
        p(strong("Temperature loggers:"), "Across the state there are a number of automatic, deployed temperature loggers that continuously monitor water temperature in streams. This data can be useful for understanding seasonal stream dynamics, as lower temperatures can indicate higher flow rates, more oxygen-rich water, and overall healther stream systems. You can see more detailed stream temperature data from these stations on our ", HTML("<a href='https://data-viz.it.wisc.edu/wav-temp-loggers/' target = '_blank'>temperature logger data dashboard</a>.")),

        h3("Map Layers"),
        p(strong("DNR Regions:"), "The Department of Natural Resources has grouped Wisconsin's 72 counties into five different regions, which are shown on the map as a light color fill."),
        p(strong("Nine Key Elements Plans:"), "These are long-term plans for specific watersheds that provide a framework for improving water quality in a holistic manner. The nine elements help assess the contributing causes and sources of nonpoint source pollution, involve key stakeholders and prioritize restoration and protection strategies to address water quality problems. Learn more about NKEs at the", HTML("<a href='https://dnr.wisconsin.gov/topic/Nonpoint/9keyElement' target='_blank'>Wisconsin DNR</a>.")),
        p(strong("HUC8, HUC10, and HUC12 watersheds:"), "HUC stands for Hydrologic Unit Code and is a sequence of numbers or letters that identify a hydrological feature like a river, lake, or drainage basin. For this map, we are including HUC8 boundaries (subbasins), HUC10 boundaries (watersheds), and HUC12 boundaries (subwatersheds) as optional layers so you can better understand the hydrology of Wisconsin. HUC8 is the largest of these classifications, and HUC12 the smallest."),

        h3("More information"),
        p("Visit the Water Action Volunteers website at", HTML("<a href='https://wateractionvolunteers.org' target='_blank'>wateractionvolunteers.org</a>."))
      )
    )
  ),


# Footer ------------------------------------------------------------------

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
