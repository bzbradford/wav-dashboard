# server.R

server <- function(input, output, session) {


  # Reactive values ----

  first_run <- reactiveVal(T)


  # Station select ----

  ## Available stations ----

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


  ## Station list for selector ----

  stn_list <- reactive({
    if (nrow(avail_stns()) > 0)
      return(all_stn_list[all_stn_list %in% avail_stns()$station_id])
    return(list())
  })


  ## Current station ----

  cur_stn <- reactive({
    req(input$station)
    req(nrow(avail_stns()) > 0)

    filter(all_pts, station_id == input$station)
  })


  ## When list changes ----
  observeEvent(stn_list(), {
    stations <- stn_list()

    if (input$station %in% stations) {
      selected <- input$station
    } else {
      if (first_run()) {
        selected <- random_baseline_stn()
        first_run(F)
      } else {
        selected <- stations[sample(1:length(stations), 1)]
      }
    }

    updateSelectInput(
      inputId = "station",
      choices = stations,
      selected = selected
    )
  })


  ## Prev/Next/Rnd station buttons ----

  observeEvent(input$next_stn, {
    stn <- cur_stn()$station_id
    stns <- stn_list()
    i <- which(stn == stns)[[1]]
    selected <- ifelse(i < length(stns), stns[i + 1], stns[1])
    updateSelectInput(inputId = "station", selected = selected)
  })

  observeEvent(input$prev_stn, {
    stn <- cur_stn()$station_id
    stns <- stn_list()
    i <- which(stn == stns)[[1]]
    selected <- ifelse(i > 1, stns[i - 1], stns[length(stns)])
    updateSelectInput(inputId = "station", selected = selected)
  })

  observeEvent(input$rnd_stn, {
    req(length(stn_list()) > 0)
    stn_id <- stn_list()[sample(1:length(stn_list()), 1)]
    stn <- all_pts %>% filter(station_id == stn_id)
    updateSelectInput(inputId = "station", selected = stn_id)
  })


  # Module servers ----

  map_click <- mapServer(
    avail_stns = reactive(avail_stns()),
    cur_stn = reactive(cur_stn())
  )

  # select a station when clicked on the map
  observe({
    updateSelectInput(
      inputId = "station",
      selected = map_click()
    )
  })

  recentStationsServer(
    cur_stn = reactive(cur_stn()),
    stn_list = reactive(stn_list())
  )

  baselineDataServer(cur_stn = reactive(cur_stn()))

  nutrientDataServer(cur_stn = reactive(cur_stn()))

  thermistorDataServer(cur_stn = reactive(cur_stn()))

  stationInfoServer(cur_stn = reactive(cur_stn()))

  stationListServer()


  # Gracefully exit ----

  session$onSessionEnded(function() { stopApp() })


  # PDF Reports ----

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

}
