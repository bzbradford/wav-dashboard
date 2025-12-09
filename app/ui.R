##  MAIN UI  ##

ui <- fluidPage(

  # Settings ----
  title = "WAV Data Dashboard",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),

  # Head ----
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "description", content = "An online data dashboard for viewing stream monitoring data collected by volunteers across Wisconsin"),
    tags$meta(name = "keywords", content = "wav, water action volunteers, wisconsin, water, data, dashboard, uw"),
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    includeHTML("google-analytics.html"),
    tags$script(src = "html2canvas.min.js"),
    tags$script(src = "scripts.js"),
    useShinyjs(),
  ),

  # Body ----
  div(
    id = "main-content",

    ## Page heading ----
    tags$header(
      div(
        align = "center",
        style = "margin-top: 1em;",
        a(img(src = "wav-logo-color.png", height = "100px", title = "Visit the Water Action Volunteers website"), href = "https://wateractionvolunteers.org", target = "_blank")
      ),
      br(),
      h2("Stream Monitoring Data Dashboard", align = "center"),
      br(),
      uiOutput("notice"),
    ),

    ## Info text above map ----
    div(
      style = "margin: 0.5em 1em;",
      align = "center",
      p(em(HTML(paste0(
        "Baseline stations are shown in ",
        colorize(stn_colors$baseline),
        ", total phosphorus monitoring stations in ",
        colorize(stn_colors$nutrient),
        ", and temperature logging stations in ",
        colorize(stn_colors$thermistor),
        " (stations may have more than one type of data). Currently selected station is shown in ",
        colorize("blue", stn_colors$current),
        ". Click on any station to select it, or choose from the list below the map."
      ))))
    ),

    ## Map and sidebar ----
    div(
      id = "map-content",
      style = "margin-bottom: 1em;",
      mapUI(),
    ),

    ## Station tabs ----
    div(
      id = "stn-selector-content",
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
                actionButton("prev_stn", icon("arrow-left"), class = "stn-btn", title = "Next closest station West"),
                actionButton("next_stn", icon("arrow-right"), class = "stn-btn", title = "Next closes station East"),
                actionButton("rnd_stn", icon("random"), class = "stn-btn", title = "Random station"),
                uiOutput("bookmark_btn", inline = TRUE)
              )
            ),
            div(class = "note", style = "margin-top: 5px;", "To search for a station by name or ID, delete the text above and start typing. Click the bookmark icon above to show the station ID in the browser URL and page title (for sharing a site or bookmarking the page to easily return to it). You can also search for stations in the Station Lists tab.")
          ),
          tabPanel("Recently Viewed Stations", recentStationsUI()),
          tabPanel("Station Details", stationInfoUI()),
          tabPanel("Station Lists", stationListUI())
        )
      ),
    ),

    ## Data tabs ----
    div(
      id = "data-tab-content",
      style = "margin-top: 30px;",
      tabsetPanel(
        id = "data_tabs",
        type = "pills",
        tabPanel(tab_names$baseline, baselineDataUI()),
        tabPanel(tab_names$nutrient, nutrientDataUI()),
        tabPanel(tab_names$thermistor, thermistorDataUI()),
        tabPanel(tab_names$watershed, watershedInfoUI()),
        tabPanel(tab_names$reports, stnReportUI()),
        tabPanel(
          title = tab_names$more,
          includeMarkdown("md/learn_more.md"),
          accordion(
            accordion_panel(
              title = "Changelog",
              includeMarkdown("CHANGELOG.md")
            ),
            open = FALSE
          )
        )
      )
    )
  ),

  # Footer ----
  tags$footer(
    id = "footer-content",
    br(),
    hr(),
    p(
      style = "color: grey; font-size: smaller; font-style: italic;",
      align = "center",
      "Dashboard developed by",
      a("Ben Bradford", href = "https://entomology.wisc.edu/directory/ben-bradford/", target = "_blank", .noWS = "after"),
      ", UW-Madison Entomology", br(),
      paste(
        "Last updated:", format(file.info(".")$mtime, "%Y-%m-%d"), " | ",
        "Most recent fieldwork:", as.character(max(baseline_data$date))
      ), br(),
      a("Source code", href = "https://github.com/bzbradford/wav-dashboard", target = "_blank")
    )
  )
)
