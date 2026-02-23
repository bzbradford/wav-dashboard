## STATION LISTS TAB ##

stationListUI <- function() {
  ns <- NS("station-list")

  # createStnPanel <- function(title, btn_id, tbl_id) {
  #   accordion_panel(
  #     title = title,
  #     style = "primary",
  #     p(downloadButton(ns(btn_id), "Download this list")),
  #     dataTableOutput(ns(tbl_id))
  #   )
  # }

  tagList(
    radioButtons(
      inputId = ns("stn_set"),
      label = "Select which set of stations to view:",
      choices = list(
        `Shown on map` = "map",
        `All baseline` = "baseline",
        `All nutrient` = "nutrient",
        `All thermistor` = "therm",
        `All stations` = "all"
      ),
      inline = T
    ),
    div(
      id = "resize_wrapper",
      style = "margin: 1em 0px; padding: 10px; border: 1px solid grey; border-radius: 5px; background: white; min-height: 550px;",
      dataTableOutput(ns("stn_tbl")) |> with_spinner(proxy.height = 530)
    ),
    p(uiOutput(ns("dl_btns"))),
    em(
      "Use the buttons at the top of this section to select which set of stations to show and download. 'Shown on map' means any stations shown on the map above; use the map options to change which stations to show, such as selecting a specific year or type of station. If a station is not shown on the map, the 'Select' action button will not be available for that station. You can also download the station lists as a CSV, as a KML for viewing in Google Earth, or as a GeoJSON for viewing in a GIS application such as QGIS."
    )
  )
}

#' @requires `all_stns`
#' @param main_rv reactive values object from main server session
stationListServer <- function(main_rv) {
  moduleServer(
    id = "station-list",
    function(input, output, session) {
      ns <- session$ns

      stn_list <- reactive({
        req(main_rv$stn_list)
      })

      # Station table ----------------------------------------------------------

      ## cur_stns() ----
      cur_stns <- reactive({
        req(input$stn_set)

        stns <- switch(
          input$stn_set,
          "map" = all_stns |> filter(station_id %in% stn_list()),
          "baseline" = all_stns |> filter(baseline_stn),
          "nutrient" = all_stns |> filter(nutrient_stn),
          "therm" = all_stns |> filter(therm_stn),
          all_stns
        )

        stns |>
          select(-c(label, baseline_stn, therm_stn, nutrient_stn, map_label)) |>
          clean_names(case = "big_camel")
      })

      ## dt_data() ----
      dt_data <- reactive({
        stn_list <- req
        cur_stns() |>
          mutate(
            Action = if_else(
              StationId %in% stn_list(),
              sprintf(
                "<a class='btn btn-default btn-sm' style='cursor: pointer; text-decoration: none;' id=%s onclick=\"Shiny.setInputValue('recent_stn', this.id, {priority: 'event'}); Shiny.setInputValue('station', this.id);\">Select</a>",
                StationId
              ),
              ""
            ) |>
              lapply(HTML),
            .before = 1
          )
      })

      ## stn_tbl // renderDataTable ----
      # handle initial rendering of the data table
      # later, an observer is used to inject new data into the existing DT
      output$stn_tbl <- renderDataTable(
        isolate(dt_data()),
        rownames = F,
        selection = "none",
        extensions = "FixedColumns",
        options = list(
          dom = "iftrp",
          scrollResize = T,
          scrollX = T,
          scrollY = 400,
          scrollCollapse = T,
          pageLength = 20,
          fixedColumns = list(leftColumns = 1),
          columnDefs = list(list(
            targets = 2:24,
            render = JS(
              "
              function(data, type, row, meta) {
                return (type === 'display' && data && data.length > 30) ?
                  '<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;
              }
            "
            )
          ))
        )
      )

      ## stn_tbl observer ----
      # update data table when source data changes
      observe({
        dataTableProxy("stn_tbl") |>
          replaceData(dt_data(), rownames = F)
      })

      # Download handlers ------------------------------------------------------

      ## filename() ----
      # sets file name for downloads
      filename <- reactive({
        switch(
          req(input$stn_set),
          "map" = "WAV Selected Station List",
          "baseline" = "WAV Baseline Stations",
          "nutrient" = "WAV Nutrient Stations",
          "therm" = "WAV Thermistor Stations",
          "all" = "WAV Station List"
        )
      })

      ## dl_btns // renderUI ----
      output$dl_btns <- renderUI({
        validate(
          need(nrow(cur_stns()) > 0, "No stations in list.")
        )

        div(
          strong("Download list as:", style = "margin-right: 10px;"),
          downloadButton(ns("dl_csv"), "CSV"),
          downloadButton(ns("dl_kml"), "KML"),
          downloadButton(ns("dl_geojson"), "GeoJSON")
        )
      })

      ## dl_csv handler ----
      output$dl_csv <- downloadHandler(
        paste0(filename(), ".csv"),
        function(file) {
          write_csv(cur_stns(), file)
        }
      )

      ## dl_kml handler ----
      output$dl_kml <- downloadHandler(
        paste0(filename(), ".kml"),
        function(file) {
          cur_stns() |>
            mutate(Name = paste(StationId, StationName), .before = 1) |>
            st_as_sf(
              coords = c("Longitude", "Latitude"),
              crs = 4326,
              remove = F
            ) |>
            write_sf(file, layer = "")
        }
      )

      ## dl_geojson handler ----
      output$dl_geojson <- downloadHandler(
        paste0(filename(), ".geojson"),
        function(file) {
          cur_stns() |>
            mutate(Name = paste(StationId, StationName), .before = 1) |>
            st_as_sf(
              coords = c("Longitude", "Latitude"),
              crs = 4326,
              remove = F
            ) |>
            write_sf(file)
        }
      )
    }
  )
}
