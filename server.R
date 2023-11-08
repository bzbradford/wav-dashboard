##  server.R  ##

# Server ----

server <- function(input, output, session) {

  # Reactives values ----

  ## first_run ----
  # becomes F after initial station selection
  first_run <- reactiveVal(TRUE)

  ## initial_stn ----
  # determines station to select on app load
  initial_stn <- reactiveVal(random_baseline_stn())

  ## bookmarking ----
  # enable/disable bookmarking features
  bookmarking <- reactiveVal(FALSE)

  ## avail_stns ----
  # reacts to map sidebar selections
  avail_stns <- reactive({
    mapReturn()$avail_stns
  })

  ## stns_avail ----
  # are any stations available?
  stns_avail <- reactive({
    nrow(avail_stns()) > 0
  })

  ## stn_list ----
  # creates a list for selectInput based on avail_stns
  stn_list <- reactive({
    if (!stns_avail()) return(list())
    all_stn_list[all_stn_list %in% avail_stns()$station_id]
  })

  ## last_valid_stn ----
  last_valid_stn <- reactiveVal()

  ## cur_stn ----
  # single line data frame with station info for currently selected station
  cur_stn <- reactive({
    req(stns_avail())
    req(input$station)

    stn <- filter(all_pts, station_id == input$station)
    last_valid_stn(stn)
    stn
  })

  # Event reactives ----

  ## startup => set initial station ----
  # Checks for URL query string, set initial station to random or requested
  observeEvent(TRUE, once = TRUE, {
    # check for a url query
    query <- parseQueryString(session$clientData$url_search)[['stn']]
    if (!is.null(query)) {
      if (query %in% all_stns$station_id) {
        selected <- query
        stn_name <- all_stns[all_stns$station_id == selected,]$station_name
        bookmarking(TRUE)
        initial_stn(selected)
        output$notice <- renderUI({
          div(
            class = "notice notice-ok",
            div(class = "notice-close", "✕", onclick = "document.querySelector('#notice').style.display = 'none'"),
            div(class = "notice-text", sprintf("Dashboard loaded with station '%s: %s' selected.", query, stn_name))
          )
        })
      } else {
        output$notice <- renderUI({
          div(
            class = "notice notice-error",
            div(class = "notice-close", "✕", onclick = "document.querySelector('#notice').style.display = 'none'"),
            div(class = "notice-text", sprintf("Station ID specified in URL ('?stn=%s') does not match a station in our list. Loading random station instead.", query))
          )
        })
      }
      delay(10000, { output$notice <- NULL })
    }
  })

  ## stn_list => updateSelectInput ----
  # if the previously selected station is still in the list, keep it selected
  # otherwise pick a random station from the list
  observeEvent(stn_list(), {
    stations <- stn_list()
    selected <- ""

    # pick the initial station, either from URL or random, on load
    if (first_run()) {
      selected <- initial_stn()
      first_run(FALSE)

    # if current station is still in the list, keep it selected
    } else if (input$station %in% stations) {
      selected <- input$station

    # pick the geographically nearest station
    } else if (length(stations) > 0) {
      last_pt <- last_valid_stn()
      if (nrow(last_pt) == 1) {
        # find geographically nearest station
        avail_pts <- all_pts %>% filter(station_id %in% stations)
        selected <- avail_pts[st_nearest_feature(last_pt, avail_pts),]$station_id
      } else {
        selected <- stations[sample(1:length(stations), 1)]
      }
    } else {
      # keep the current station selected in a one-item list
      stations <- all_stn_list[all_stn_list %in% last_valid_stn()$station_id]
      selected <- stations
    }

    updateSelectInput(
      inputId = "station",
      choices = stations,
      selected = selected
    )
  })

  ## cur_stn & bookmarking => update URL ----
  # Set URL and page title when bookmarking enabled
  observeEvent(list(last_valid_stn(), bookmarking()), {
    if (bookmarking()) {
      setURL(last_valid_stn()$station_id)
      setTitle(last_valid_stn()$label)
    } else {
      setURL(NULL)
      setTitle(NULL)
    }
  })


  # Button handlers ----

  ## next_stn => Next station button ----
  # go to the next station east
  observeEvent(input$next_stn, {
    req(length(stn_list()) > 0)
    avail_pts <- filter(all_pts, station_id %in% avail_stns()$station_id)
    stns_east <- filter(avail_pts, longitude > cur_stn()$longitude)
    if (nrow(stns_east) == 0) {
      # circle back around
      selected <- avail_pts %>%
        filter(longitude == min(longitude)) %>%
        pull(station_id)
    } else {
      # pick the next easterly station
      selected <- stns_east %>%
        slice(st_nearest_feature(cur_stn(), stns_east)) %>%
        pull(station_id)
    }

    updateSelectInput(inputId = "station", selected = selected)
  })

  ## prev_stn => Prev station button ----
  # go to the next station west
  observeEvent(input$prev_stn, {
    req(length(stn_list()) > 0)
    avail_pts <- all_pts %>% filter(station_id %in% avail_stns()$station_id)
    stns_west <- avail_pts %>% filter(longitude < cur_stn()$longitude)
    if (nrow(stns_west) == 0) {
      # circle back around
      selected <- avail_pts %>%
        filter(longitude == max(longitude)) %>%
        pull(station_id)
    } else {
      selected <- stns_west %>%
        slice(st_nearest_feature(cur_stn(), stns_west)) %>%
        pull(station_id)
    }

    updateSelectInput(inputId = "station", selected = selected)
  })

  ## rnd_stn => Random station button ----
  # select a random station
  observeEvent(input$rnd_stn, {
    req(length(stn_list()) > 0)
    stn_id <- stn_list()[sample(1:length(stn_list()), 1)]
    updateSelectInput(inputId = "station", selected = stn_id)
  })

  ## bookmarking => toggle state ----
  observeEvent(input$bookmarking, bookmarking(!bookmarking()))

  ## recent_stn => select a recent station ----
  # see 'recent_stations.R'
  # modify map selections to ensure the station shows up in the available stations
  observeEvent(input$recent_stn, {
    id <- input$recent_stn
    stn <- all_stns %>% filter(station_id == id)

    if (!(id %in% stn_list())) {
      # desired station not in list, need to remove restrictions
      # keep current year select, add the most recent year from the desired station
      new_years <- union(
        max(c(stn$max_fw_year, last(stn_year_choices))),
        input$`map-stn_years`
      )
      updateCheckboxGroupInput(inputId = "map-stn_types", selected = station_types)
      updateCheckboxGroupInput(inputId = "map-stn_years", selected = new_years)
      updateRadioButtons(inputId = "map-year_exact_match", selected = FALSE)
    }
    updateSelectInput(inputId = "station", selected = id)
    leafletProxy("map-map") %>%
      setView(
        lat = stn$latitude,
        lng = stn$longitude,
        zoom = 10
      )
  })

  ## screenshot => download pdf ----
  observeEvent(stns_avail(), {
    if (stns_avail()) enable("screenshot") else disable("screenshot")
  })

  #' use html2canvas to screenshot the main page content
  #' have to remove the map div for now because leaflet is using svg instead of canvas
  #' map polygons render in the incorrect location with html2canvas
  #' once cloned the radio buttons are modified because they didn't appear correctly
  #' after rendering, the screenshot button is re-enabled
  buildScreenshotFilename <- function() {
    stn_id <- cur_stn()$station_id
    tab_name <- input$data_tabs
    suffix <- ""
    if (grepl("Baseline", tab_name)) suffix <- baselineReturn()$year
    else if (grepl("Nutrient", tab_name)) suffix <- NutrientReturn()$year
    else if (grepl("Thermistor", tab_name)) suffix <- ThermistorReturn()$year
    else if (grepl("Watershed", tab_name)) suffix <- watershedReturn()$huc
    fname <- paste0("WAV Dashboard - Station ", cur_stn()$station_id, " - ", input$data_tabs)
    if (!is.null(suffix)) fname <- paste(fname, suffix)
    fname
  }

  observeEvent(input$screenshot, {
    fname <- buildScreenshotFilename()
    runjs(sprintf("
      html2canvas(
        document.querySelector('#main-content'),
        {
          scale: 1,
          crossOrigin: 'anonymous',
          useCORS: true,
          imageTimeout: 5000,
          onclone: (cloneDoc) => {
            cloneDoc.querySelector('#map-content').style.display = 'none';
            const style = cloneDoc.createElement('style');
            style.innerHTML = 'input[type=\"radio\"] { appearance: none !important; };'
            cloneDoc.body.appendChild(style);
          }
        }
      ).then(canvas => {
        saveAs(canvas.toDataURL(), '%s.png')
      });
    ", fname))
    enable("screenshot")
    runjs("document.querySelector('#screenshot-msg').style.display = 'none';")
  })


  # Rendered UIs ----

  ## bookmark_btn ----
  # enable/disable URL and title to show current station

  output$bookmark_btn <- renderUI({
    if (bookmarking()) {
      actionButton("bookmarking", "★", class = "stn-btn", style = "background: gold;", title = "Disable showing station in URL and page title")
    } else {
      actionButton("bookmarking", "☆", class = "stn-btn", title = "Show station in URL and page title so you can share or bookmark this page")
    }
  })


  # Module servers ----

  ## Map ----
  # returns the station that was clicked
  mapReturn <- mapServer(
    cur_stn = reactive(cur_stn()),
    main_session = session
  )

  ## Recent stations ----
  recentStationsServer(
    cur_stn = reactive(last_valid_stn()),
    stn_list = reactive(stn_list())
  )

  ## Station info tab ----
  stationInfoServer(
    cur_stn = reactive(last_valid_stn())
  )

  ## Station list tab ----
  stationListServer()

  ## Baseline data tab ----
  baselineReturn <- baselineDataServer(
    cur_stn = reactive(last_valid_stn())
  )

  ## Nutrient data tab ----
  nutrientReturn <- nutrientDataServer(
    cur_stn = reactive(last_valid_stn())
  )

  ## Thermistor data tab ----
  thermistorReturn <- thermistorDataServer(
    cur_stn = reactive(last_valid_stn())
  )

  ## Watershed info tab ----
  watershedReturn <- watershedInfoServer(
    cur_stn = reactive(last_valid_stn())
  )

}
