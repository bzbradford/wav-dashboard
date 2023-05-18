## MAIN SERVER ##

# Server ----

server <- function(input, output, session) {

  # Reactives values ----

  ## first_run // becomes F after initial station selection ----
  first_run <- reactiveVal(TRUE)

  ## initial_stn // determines station to select on app load ----
  initial_stn <- reactiveVal(random_baseline_stn())

  ## bookmarking // enable/disable bookmarking features ----
  bookmarking <- reactiveVal(FALSE)

  ## avail_stns // reacts to map sidebar selections ----
  avail_stns <- reactive({
    ids <- list(0)
    coverage = list(
      "baseline" = baseline_coverage,
      "nutrient" = nutrient_coverage,
      "thermistor" = therm_coverage
    )

    for (stn_type in input$stn_types) {
      if (input$year_exact_match) {
        ids[[stn_type]] <- coverage[[stn_type]] %>%
          filter(all(input$stn_years %in% data_year_list)) %>%
          pull(station_id)
      } else {
        ids[[stn_type]] <- coverage[[stn_type]] %>%
          filter(any(input$stn_years %in% data_year_list)) %>%
          pull(station_id)
      }
    }

    avail_ids <- sort(reduce(ids, union))

    all_stn_years %>%
      filter(station_id %in% avail_ids)
  })

  ## stn_list ----
  # creates a list for selectInput based on avail_stns
  stn_list <- reactive({
    if (nrow(avail_stns()) > 0)
      return(all_stn_list[all_stn_list %in% avail_stns()$station_id])
    return(list())
  })

  ## cur_stn ----
  # single line data frame with station info for currently selected station
  cur_stn <- reactive({
    req(input$station)
    req(nrow(avail_stns()) > 0)

    filter(all_pts, station_id == input$station)
  })


  # Event reactives ----

  # On startup // Set initial station ----
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

  ## Update the station selectInput on stn_list() change ----
  # if the previously selected station is still in the list, keep it selected
  # otherwise pick a random station from the list
  observeEvent(stn_list(), {
    stations <- stn_list()
    if (first_run()) {
      selected <- initial_stn()
      first_run(FALSE)
    } else if (input$station %in% stations) {
      selected <- input$station
    } else {
      selected <- stations[sample(1:length(stations), 1)]
    }
    updateSelectInput(
      inputId = "station",
      choices = stations,
      selected = selected
    )
  })

  ## Set URL and page title when bookmarking enabled ----
  observeEvent(list(cur_stn(), bookmarking()), {
    if (bookmarking()) {
      setURL(cur_stn()$station_id)
      setTitle(cur_stn()$label)
    } else {
      setURL(NULL)
      setTitle(NULL)
    }
  })


  # Button handlers ----

  ## Prev station button ----
  # select the next station in the list
  observeEvent(input$next_stn, {
    stn <- cur_stn()$station_id
    stns <- stn_list()
    i <- which(stn == stns)[[1]]
    selected <- ifelse(i < length(stns), stns[i + 1], stns[1])
    updateSelectInput(inputId = "station", selected = selected)
  })

  ## Next station button ----
  # select the previous station in the list
  observeEvent(input$prev_stn, {
    stn <- cur_stn()$station_id
    stns <- stn_list()
    i <- which(stn == stns)[[1]]
    selected <- ifelse(i > 1, stns[i - 1], stns[length(stns)])
    updateSelectInput(inputId = "station", selected = selected)
  })

  ## Random station button ----
  # select a random station
  observeEvent(input$rnd_stn, {
    req(length(stn_list()) > 0)
    stn_id <- stn_list()[sample(1:length(stn_list()), 1)]
    stn <- all_pts %>% filter(station_id == stn_id)
    updateSelectInput(inputId = "station", selected = stn_id)
  })

  ## Bookmark button ----
  # enable/disable URL and title to show current station
  observeEvent(input$toggle_bookmarking, bookmarking(!bookmarking()))
  output$bookmark_btn <- renderUI({
    if (bookmarking()) {
      actionButton("toggle_bookmarking", "★", class = "stn-btn", style = "background: gold;", title = "Disable showing station in URL and page title")
    } else {
      actionButton("toggle_bookmarking", "☆", class = "stn-btn", title = "Show station in URL and page title")
    }
  })



  # Module servers ----

  ## Map ----

  map_click <- mapServer(
    cur_stn = reactive(cur_stn()),
    avail_stns = reactive(avail_stns())
  )

  # select a station when clicked on the map
  observeEvent(map_click(), {
    updateSelectInput(
      inputId = "station",
      selected = map_click()
    )
    cur_zoom <- input$`map-map_zoom`
    leafletProxy("map-map") %>%
      setView(
        lat = map_click()$lat,
        lng = map_click()$lng,
        zoom = max(cur_zoom, 10) # don't zoom out
      )
  })


  ## Recent stations ----
  recentStationsServer(
    cur_stn = reactive(cur_stn()),
    stn_list = reactive(stn_list())
  )

  ## Station info tab ----
  stationInfoServer(cur_stn = reactive(cur_stn()))

  ## Station list tab ----
  stationListServer()

  ## Baseline data tab ----
  baselineDataServer(cur_stn = reactive(cur_stn()))

  ## Nutrient data tab ----
  nutrientDataServer(cur_stn = reactive(cur_stn()))

  ## Thermistor data tab ----
  thermistorDataServer(cur_stn = reactive(cur_stn()))

  ## Watershed info tab ----
  watershedInfoServer(cur_stn = reactive(cur_stn()))

}
