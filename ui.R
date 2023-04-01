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

  mapUI(),

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
