## MAIN SERVER ##

# Server ----

server <- function(input, output, session) {

  # Reactives ----

  ## first_run ----
  # becomes F after a random station gets selected
  first_run <- reactiveVal(T)

  ## avail_stns ----
  # reacts to map sidebar selections
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


  # Observers ----

  ## on stn_list change ----
  # it updates the station selectInput
  observeEvent(stn_list(), {
    stations <- stn_list()

    if (input$station %in% stations) {
      # if the previously selected station is still in the list, keep it selected
      selected <- input$station
    } else {
      if (first_run()) {
        # always pick a random baseline station on app load
        selected <- random_baseline_stn()
        first_run(F)
      } else {
        # otherwise pick any random station
        selected <- stations[sample(1:length(stations), 1)]
      }
    }

    updateSelectInput(
      inputId = "station",
      choices = stations,
      selected = selected
    )
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


  # Module servers ----

  ## Map ----

  map_click <- mapServer(
    cur_stn = reactive(cur_stn()),
    avail_stns = reactive(avail_stns())
  )

  # select a station when clicked on the map
  observe({
    updateSelectInput(
      inputId = "station",
      selected = map_click()
    )
  })


  ## Recent stations ----
  recentStationsServer(
    cur_stn = reactive(cur_stn()),
    stn_list = reactive(stn_list())
  )

  ## Baseline data tab ----
  baselineDataServer(cur_stn = reactive(cur_stn()))

  ## Nutrient data tab ----
  nutrientDataServer(cur_stn = reactive(cur_stn()))

  ## Thermistor data tab ----
  thermistorDataServer(cur_stn = reactive(cur_stn()))

  ## Station info tab ----
  stationInfoServer(cur_stn = reactive(cur_stn()))

  ## Station list tab ----
  stationListServer()


  # PDF Reports (pending) ----

  # output$baseline_report <- downloadHandler(
  #   filename = paste0(
  #     input$baseline_year,
  #     " Baseline Report for Station ",
  #     as.character(cur_stn()$station_id),
  #     ".pdf"),
  #   content = function(file) {
  #     temp_dir <- tempdir()
  #     temp_file <- tempfile(tmpdir = temp_dir, fileext = ".Rmd")
  #     file.copy("baseline-report.Rmd", temp_file)
  #     print(temp_file)
  #     rmarkdown::render(
  #       temp_file,
  #       output_file = file,
  #       output_options = list(self_contained = TRUE),
  #       envir = new.env(parent = globalenv()),
  #       params = list(
  #         id = cur_stn()$station_id,
  #         name = cur_stn()$station_name,
  #         year = input$baseline_year,
  #         plot = baseline_plot()
  #         )
  #     )
  #   }
  # )


  # Gracefully exit ----

  session$onSessionEnded(function() { stopApp() })

}
