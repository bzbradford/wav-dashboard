# ui.R

# UI ----------------------------------------------------------------------

ui <- fluidPage(

  title = "WAV Data Dashboard",
  theme = shinytheme("flatly"),
  useShinyjs(),

  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "description", content = "An online data dashboard for viewing stream monitoring data collected by volunteers across Wisconsin"),
    tags$meta(name = "keywords", content = "wav, water action volunteers, wisconsin, water, data, dashboard, uw"),
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    includeHTML("google-analytics.html"),
    tags$script(src = "html2canvas.js"),
    tags$script(src = "saveAs.js")
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


  # Map UI ----

  div(style = "margin: 0.5em 1em;", align = "center",
      p(em(HTML(paste0("Baseline stations are shown in ", colorize(stn_colors$baseline), ", thermistor stations in ", colorize(stn_colors$thermistor), ", and nutrient stations in ", colorize(stn_colors$nutrient), ". Currently selected station is shown in ", colorize("blue", stn_colors$current), ". Click on any station to select it, or choose from the list below the map."))))
  ),

  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "stn_types",
        label = "Station data types:",
        choices = station_types,
        selected = station_types
      ),
      p(strong("Stations with data from:")),
      fluidRow(
        column(5,
          checkboxGroupInput(
            inputId = "stn_years",
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
      hr(),
      uiOutput("total_stns_text"),
      hr(),
      div(
        style = "line-height: 3.5em;",
        align = "center",
        actionButton("zoom_in", "Zoom to selected site", width = "100%"), br(),
        actionButton("reset_zoom", "Zoom out to all sites", width = "100%")
      )
    ),
    mainPanel(
      div(
        style = "max-width: 1000px; margin: auto; border: 1px solid lightgrey;",
        leafletOutput("map", width = "100%", height = "700px")
      )
    ),
    position = "right"
  ),

  br(),


  # Station dropdown ----

  div(
    class = "well",
    style = "padding: 15px;",
    tabsetPanel(
      type = "pills",
      tabPanel(
        title = "Current Station",
        div(
          class = "flex-row stn-select",
          div(
            class = "flex-col",
            title = "Currently selected station",
            selectInput(
              inputId = "station",
              label = NULL,
              choices = list()
            ),
          ),
          div(
            class = "stn-btns",
            actionButton("prev_stn", "<", class = "stn-btn", title = "Previous station"),
            actionButton("next_stn", ">", class = "stn-btn", title = "Next station"),
            actionButton("rnd_stn", "?", class = "stn-btn", title = "Random station")
          )
        ),
        em("To search for a station by name, delete the text above and start typing.")
      ),
      tabPanel("Recently Viewed Stations", recentStationsUI())
    )
  ),


  # Data tabs ----

  tabsetPanel(
    id = "data_tabs",
    type = "pills",
    tabPanel("Baseline data", baselineDataUI()),
    tabPanel("Nutrient data", nutrientDataUI()),
    tabPanel("Thermistor data", thermistorDataUI()),
    tabPanel("Station Details", stationInfoUI()),
    tabPanel("Station Lists", stationListUI()),
    tabPanel("More information", moreInfoUI())
  ),


  # Footer ----

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
