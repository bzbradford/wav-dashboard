##  MAIN SERVER  ##

server <- function(input, output, session) {
  # Defs ----

  # stations with data from most recent year
  initial_avail_stns <- all_stns |>
    filter(max_fw_year == max(data_years))

  # picks a baseline station from the most recent year
  initial_stn <- initial_avail_stns |>
    filter(baseline_stn == TRUE) |>
    slice_sample() |>
    pull(station_id)

  # Reactives values ----

  rv <- reactiveValues(
    ## rv$init ----
    # becomes FALSE after initial station selection
    init = TRUE,

    ## rv$bookmarking ----
    # enable/disable bookmarking features where station id shown in url
    bookmarking = NULL,

    ## rv$avail_stns ----
    avail_stns = initial_avail_stns,

    ## rv$initial_stn ----
    initial_stn = NULL,

    ## rv$stn_list ----
    stn_list = list(),

    ## rv$cur_stn ----
    # single row data frame with station info for currently selected station
    cur_stn = NULL
  )

  observe({
    stns <- rv$avail_stns
    rv$stn_list <- if (any(is.null(stns), nrow(stns) == 0)) {
      list()
    } else {
      all_stn_list[all_stn_list %in% stns$station_id]
    }
  })

  # set cur_stn based on input$station
  observe({
    req(rv$avail_stns)
    stn <- req(input$station)

    rv$cur_stn <- filter(all_pts, station_id == stn)
  })

  ## stn_list() ----
  # creates a list for selectInput based on avail_stns
  stn_list <- reactive({
    stns <- req(rv$avail_stns)
    if (nrow(stns) > 0) {
      all_stn_list[all_stn_list %in% stns$station_id]
    } else {
      list()
    }
  })

  # Event reactives ----

  ## startup => set initial station ----
  # Checks for URL query string, set initial station to random or requested
  observeEvent(TRUE, once = TRUE, {
    # check for a url query
    id <- parseQueryString(session$clientData$url_search)[["stn"]]

    if (is.null(id)) {
      rv$bookmarking <- FALSE
      rv$initial_stn <- initial_stn
      return()
    }

    rv$bookmarking <- TRUE

    opts <- if (id %in% all_stns$station_id) {
      selected <- id
      stn_name <- all_stns[all_stns$station_id == selected, ]$station_name
      rv$initial_stn <- selected

      # BUG FIX: if the station has no data in the default map years it isn't
      # in the station list, so the bookmarked station silently failed to load
      # (affected ~half of all stations). Expand the year filter to include it.
      stn_max_yr <- all_stns[all_stns$station_id == selected, ]$max_fw_year
      if (!(selected %in% initial_avail_stns$station_id)) {
        new_years <- union(
          stn_year_choices[1:4],
          max(c(as.character(stn_max_yr), last(stn_year_choices)))
        )
        updateCheckboxGroupInput(inputId = "map-stn_years", selected = new_years)
      }

      list(
        msg = sprintf(
          "Dashboard loaded with station '%s: %s' selected.",
          id,
          stn_name
        ),
        type = "ok"
      )
    } else {
      rv$initial_stn <- initial_stn
      list(
        msg = sprintf(
          "Station ID specified in URL ('?stn=%s') does not match a station in our list. Loading random station instead.",
          id # BUG FIX: was `query`, an undefined variable, so this notice errored
        ),
        type = "error"
      )
    }

    output$notice <- build_notice_ui(opts$msg, opts$type)

    delay(10000, {
      output$notice <- NULL
    })
  })

  ## stn_list => updateSelectInput ----
  # if the previously selected station is still in the list, keep it selected
  # otherwise pick a random station from the list
  observeEvent(rv$stn_list, ignoreInit = TRUE, {
    stations <- rv$stn_list
    req(length(stations) > 0)
    selected <- ""
    cur_stn <- rv$cur_stn

    # pick the initial station, either from URL or random, on load
    if (rv$init) {
      selected <- req(rv$initial_stn)
      # wait until the map filters include the initial station (see startup)
      req(selected %in% stations)
      rv$init <- FALSE

      # if current station is still in the list, keep it selected
    } else if (input$station %in% stations) {
      selected <- input$station

      # pick the geographically nearest station
    } else if (length(stations) > 0) {
      # last_pt <- last_valid_stn()
      if (!is.null(cur_stn)) {
        # find geographically nearest station
        avail_pts <- all_pts |> filter(station_id %in% stations)
        selected <- avail_pts[
          st_nearest_feature(cur_stn, avail_pts),
        ]$station_id
      } else {
        selected <- stations[sample(seq_along(stations), 1)]
      }
    } else {
      # keep the current station selected in a one-item list
      stations <- all_stn_list[all_stn_list %in% cur_stn$station_id]
      selected <- stations
    }

    updateSelectInput(
      inputId = "station",
      choices = stations,
      selected = selected
    )
  })

  ## cur_stn => announce to screen readers ----
  # visual users see the map/plots update; screen reader users get a polite
  # status message (WCAG 4.1.3 Status Messages)
  observeEvent(rv$cur_stn, {
    stn <- req(rv$cur_stn)
    session$sendCustomMessage(
      "announce",
      paste("Selected station", stn$label)
    )
  })

  ## cur_stn & bookmarking => update URL ----
  # Set URL and page title when bookmarking enabled
  observeEvent(list(rv$cur_stn, rv$bookmarking), {
    opt <- rv$bookmarking
    req(!is.null(opt))
    if (opt) {
      stn <- rv$cur_stn
      set_page_url(stn$station_id)
      set_page_title(stn$label)
    } else {
      set_page_url(NULL)
      set_page_title(NULL)
    }
  })

  # Button handlers ----

  ## next_stn => Next station button ----
  # go to the next station east
  observeEvent(input$next_stn, {
    stn_list <- rv$stn_list
    req(length(stn_list) > 0)

    avail_stns <- rv$avail_stns
    cur_stn <- rv$cur_stn
    avail_pts <- filter(all_pts, station_id %in% avail_stns$station_id)
    stns_east <- filter(avail_pts, longitude > cur_stn$longitude)

    if (nrow(stns_east) == 0) {
      # circle back around
      selected <- avail_pts |>
        filter(longitude == min(longitude)) |>
        pull(station_id)
    } else {
      # pick the next easterly station
      selected <- stns_east |>
        slice(st_nearest_feature(cur_stn, stns_east)) |>
        pull(station_id)
    }

    updateSelectInput(inputId = "station", selected = selected)
  })

  ## prev_stn => Prev station button ----
  # go to the next station west
  observeEvent(input$prev_stn, {
    stn_list <- rv$stn_list
    req(length(stn_list) > 0)

    avail_stns <- rv$avail_stns
    cur_stn <- rv$cur_stn
    avail_pts <- filter(all_pts, station_id %in% avail_stns$station_id)
    stns_west <- filter(avail_pts, longitude < cur_stn$longitude)

    if (nrow(stns_west) == 0) {
      # circle back around
      selected <- avail_pts |>
        filter(longitude == max(longitude)) |>
        pull(station_id)
    } else {
      selected <- stns_west |>
        slice(st_nearest_feature(cur_stn, stns_west)) |>
        pull(station_id)
    }

    updateSelectInput(inputId = "station", selected = selected)
  })

  ## rnd_stn => Random station button ----
  # select a random station
  observeEvent(input$rnd_stn, {
    stn_list <- rv$stn_list
    req(length(stn_list) > 0)

    stn_id <- stn_list[sample(seq_along(stn_list), 1)]

    updateSelectInput(inputId = "station", selected = stn_id)
  })

  ## bookmarking => toggle state ----
  observeEvent(input$bookmarking, {
    rv$bookmarking <- !rv$bookmarking
  })

  ## recent_stn => select a recent station ----
  # see 'recent_stations.R'
  # modify map selections to ensure the station shows up in the available stations
  observeEvent(input$recent_stn, {
    id <- input$recent_stn
    stn <- all_stns |> filter(station_id == id)

    if (!(id %in% rv$stn_list)) {
      # desired station not in list, need to remove restrictions
      # keep current year select, add the most recent year from the desired station
      new_years <- union(
        max(c(stn$max_fw_year, last(stn_year_choices))),
        input$`map-stn_years`
      )
      updateCheckboxGroupInput(
        inputId = "map-stn_types",
        selected = station_types
      )
      updateCheckboxGroupInput(inputId = "map-stn_years", selected = new_years)
      updateRadioButtons(inputId = "map-year_exact_match", selected = FALSE)
    }
    updateSelectInput(inputId = "station", selected = id)
    leafletProxy("map-map") |>
      setView(
        lat = stn$latitude,
        lng = stn$longitude,
        zoom = 10
      )
  })

  # Rendered UIs ----

  ## bookmark_btn ----
  # enable/disable URL and title to show current station. The button is static
  # in ui.R and only its state is toggled here, so keyboard focus is not lost
  # by re-rendering it (WCAG 2.4.3), and aria-pressed conveys the state (4.1.2)
  observeEvent(rv$bookmarking, {
    on <- isTRUE(rv$bookmarking)
    runjs(sprintf(
      "(function(b){ b.classList.toggle('stn-btn-active', %1$s); b.setAttribute('aria-pressed', '%1$s'); b.title = %2$s; })(document.getElementById('bookmarking'))",
      tolower(on),
      jsonlite::toJSON(
        if (on) {
          "Disable showing station in URL and page title"
        } else {
          "Show station in URL and page title so you can share or bookmark this page"
        },
        auto_unbox = TRUE
      )
    ))
  })

  # Module servers ----

  mapServer(rv, session)

  recentStationsServer(rv)
  stationInfoServer(rv)
  stationListServer(rv)

  baselineDataServer(rv)
  nutrientDataServer(rv)
  thermistorDataServer(rv)
  watershedInfoServer(rv)
  stnReportServer(rv)
}
