##  ui.R  ##


# UI ----

stnSelectorUI <- function() {
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
            actionButton("prev_stn", "<", class = "stn-btn", title = "Next closest station West"),
            actionButton("next_stn", ">", class = "stn-btn", title = "Next closes station East"),
            actionButton("rnd_stn", "?", class = "stn-btn", title = "Random station"),
            uiOutput("bookmark_btn", inline = TRUE)
          )
        ),
        div(class = "note", "To search for a station by name or ID, delete the text above and start typing. To show the station ID in the browser URL and page title (for bookmarking a site to easily return to it), click the star button. You can also search for stations in the Station Lists tab.")
      ),
      tabPanel("Recently Viewed Stations", recentStationsUI()),
      tabPanel("Station Details", stationInfoUI()),
      tabPanel("Station Lists", stationListUI())
    )
  )
}

ui <- fluidPage(

  ## Setup ----

  title = "WAV Data Dashboard",
  theme = shinytheme("flatly"),
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "description", content = "An online data dashboard for viewing stream monitoring data collected by volunteers across Wisconsin"),
    tags$meta(name = "keywords", content = "wav, water action volunteers, wisconsin, water, data, dashboard, uw"),
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    includeHTML("google-analytics.html"),
    tags$script(src = "html2canvas.min.js"),
    tags$script(src = "saveAs.js"),
  ),
  useShinyjs(),

  div(
    id = "main-content",

    ## Page heading ----
    tags$header(
      div(
        align = "center",
        style = "margin-top: 1em;",
        a(img(src = "wav-logo-color.png", height = "100px"), href = "https://wateractionvolunteers.org", target = "_blank")
      ),
      br(),
      h2("Stream Monitoring Data Dashboard", align = "center"),
      br(),
      uiOutput("notice"),
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
      stnSelectorUI(),
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
        # temporarily disable the reports tab on the wisc server
        {if (Sys.getenv("REPORT_DISABLED") == "") tabPanel(tab_names$reports, stnReportUI())},
        tabPanel(tab_names$more, learnMoreUI())
      ),
    ),

  ),


  # Footer ----

  tags$footer(
    id = "footer-content",
    br(),
    hr(),
    # div(
    #   align = "center",
    #   actionButton(
    #     "screenshot",
    #     "Download a screenshot of the entire page (except map)",
    #     title = "Screenshot does not currently include the map",
    #     class = "btn-xs",
    #     onclick = "this.disabled = true; document.querySelector('#screenshot-msg').style.display = null;"
    #   ),
    #   div(id = "screenshot-msg", style = "padding: 5px; font-style: italic; font-size: small; display: none;", "Generating screenshot, please wait...")
    # ),
    # br(),
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
      # glue("
      #   Pandoc {rmarkdown::pandoc_version()},
      #   GEOS {sf::sf_extSoftVersion()[['GEOS']]},
      #   GDAL {sf::sf_extSoftVersion()[['GDAL']]},
      #   proj.4 {sf::sf_extSoftVersion()[['proj.4']]},
      #   PROJ {sf::sf_extSoftVersion()[['PROJ']]}
      # "), br(),
      a("Source code", href = "https://github.com/bzbradford/wav-dashboard", target = "_blank")
    )
  )
)
