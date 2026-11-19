##  MAIN UI  ##

ui <- fluidPage(
  # Settings ----
  title = "WAV Data Dashboard",
  lang = "en",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    # Accessibility: Flatly's default link color (#18bc9c, 2.4:1) and secondary
    # button color (#95a5a6, 2.6:1 with white text) fail WCAG 1.4.3 contrast.
    # These darker values keep the palette but pass AA (>= 4.5:1).
    secondary = "#5d6d6e",
    "link-color" = "#11745a",
    "link-hover-color" = "#0b5240"
  ),

  # Head ----
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(
      name = "description",
      content = "An online data dashboard for viewing stream monitoring data collected by volunteers across Wisconsin"
    ),
    tags$meta(
      name = "keywords",
      content = "wav, water action volunteers, wisconsin, water, data, dashboard, uw"
    ),
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    includeHTML("www/google-analytics.html"),
    tags$script(src = "html2canvas.min.js"),
    tags$script(src = "scripts.js"),
    useShinyjs(),
  ),

  # Native Shiny busy indicators (replaces shinycssloaders): a spinner on
  # each output while it recalculates, plus a page-level pulse bar
  useBusyIndicators(spinners = TRUE, pulse = TRUE),
  busyIndicatorOptions(spinner_color = "#30a67d"),

  # Body ----
  # skip link lets keyboard users bypass the header (WCAG 2.4.1)
  a(class = "skip-link", href = "#main-content", "Skip to main content"),

  # screen-reader announcements (station changes etc), see scripts.js
  div(
    id = "sr-announcer",
    class = "visually-hidden",
    `aria-live` = "polite",
    role = "status"
  ),

  tags$header(
    tags$nav(
      `aria-label` = "UW-Madison Extension and WAV websites",
      style = "display: inline-flex; gap: 1rem;",
      build_link(
        "University of Wisconsin-Madison Extension",
        href = "https://extension.wisc.edu/"
      ),
      span("|", `aria-hidden` = "true"),
      build_link(
        "Water Action Volunteers Program",
        href = "https://www.wateractionvolunteers.org"
      )
    ),
  ),

  tags$main(
    id = "main-content",
    tabindex = "-1",

    div(
      div(
        align = "center",
        style = "margin: 1rem 0;",
        build_link(
          img(
            src = "wav-logo-round.png",
            height = "150px",
            alt = "Water Action Volunteers logo. Visit the WAV website."
          ),
          href = "https://www.wateractionvolunteers.org"
        )
      ),
    ),

    ## Page heading ----
    h1("Stream Monitoring Data Dashboard", class = "page-title"),

    ## Notices on load ----
    uiOutput("notice"),

    ## Info text above map ----
    div(
      style = "margin: 0.5em 1em;",
      align = "center",
      p(em(
        "Welcome to the WAV Dashboard! Click on any station on the map to select it, or choose from the list below the map. "
      ))
    ),

    ## Map and sidebar ----
    div(
      id = "map-content",
      style = "margin-bottom: 1em;",
      mapUI(),
    ),

    ## Station tabs ----
    tags$section(
      id = "stn-selector-content",
      `aria-label` = "Station selection",
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
                selectInput(
                  inputId = "station",
                  label = span(
                    class = "visually-hidden",
                    "Currently selected station"
                  ),
                  choices = list()
                ),
              ),
              div(
                class = "stn-btns",
                actionButton(
                  "prev_stn",
                  icon("arrow-left", `aria-hidden` = "true"),
                  class = "stn-btn",
                  title = "Next closest station west",
                  `aria-label` = "Next closest station west"
                ),
                actionButton(
                  "next_stn",
                  icon("arrow-right", `aria-hidden` = "true"),
                  class = "stn-btn",
                  title = "Next closest station east",
                  `aria-label` = "Next closest station east"
                ),
                actionButton(
                  "rnd_stn",
                  icon("random", `aria-hidden` = "true"),
                  class = "stn-btn",
                  title = "Random station",
                  `aria-label` = "Random station"
                ),
                actionButton(
                  "bookmarking",
                  icon("bookmark", `aria-hidden` = "true"),
                  class = "stn-btn",
                  title = "Show station in URL and page title so you can share or bookmark this page",
                  `aria-label` = "Show station in URL and page title",
                  `aria-pressed` = "false"
                )
              )
            ),
            div(
              class = "note",
              style = "margin-top: 5px;",
              "To search for a station by name or ID, delete the text above and start typing. Click the bookmark icon above to show the station ID in the browser URL and page title (for sharing a site or bookmarking the page to easily return to it). You can also search for stations in the Station Lists tab."
            )
          ),
          tabPanel("Recently Viewed Stations", recentStationsUI()),
          tabPanel("Station Details", stationInfoUI()),
          tabPanel("Station Lists", stationListUI())
        )
      ),
    ),

    ## Data tabs ----
    tags$section(
      id = "data-tab-content",
      `aria-label` = "Station data",
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
      class = "footer-text",
      "Dashboard developed by ",
      build_link(
        "Ben Bradford",
        href = "https://entomology.wisc.edu/directory/ben-bradford/"
      ),
      ", UW-Madison Entomology",
      br(),
      paste(
        "Last updated:",
        format(file.info(".")$mtime, "%Y-%m-%d"),
        " | ",
        "Most recent fieldwork:",
        as.character(max(baseline_data$date))
      ),
      br(),
      build_link(
        "Source code",
        href = "https://github.com/bzbradford/wav-dashboard"
      )
    )
  )
)
