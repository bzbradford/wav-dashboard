## STATION LISTS TAB ##

stationListUI <- function() {
  ns <- NS("station-list")

  createStnPanel <- function(title, btn_id, tbl_id) {
    bsCollapsePanel(
      title = title,
      style = "primary",
      p(downloadButton(ns(btn_id), "Download this list")),
      dataTableOutput(ns(tbl_id))
    )
  }

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
      dataTableOutput(ns("stn_tbl")) %>% withSpinnerProxy(proxy.height = 530)
    ),
    p(uiOutput(ns("dl_btns"))),
    em("Use the buttons at the top of this section to select which set of stations to show and download. 'Shown on map' means any stations shown on the map above; use the map options to change which stations to show, such as selecting a specific year or type of station. If a station is not shown on the map, the 'Select' action button will not be available for that station. You can also download the station lists as a CSV, as a KML for viewing in Google Earth, or as a GeoJSON for viewing in a GIS application such as QGIS.")
  )
}


#' requires global data frame 'all_stns'
#' @param avail_stns list of stations currently shown on the map

stationListServer <- function(stn_list) {
  moduleServer(
    id = "station-list",
    function(input, output, session) {
      ns <- session$ns

      fmtStns <- function(df) {
        df %>%
          select(-c(label, baseline_stn, therm_stn, nutrient_stn, map_label)) %>%
          clean_names(case = "big_camel")
      }

      # Reactives ----

      ## cur_stns // reactive ----
      cur_stns <- reactive({
        req(input$stn_set)
        set <- input$stn_set

        if (set == "map") {
          all_stns %>% filter(station_id %in% stn_list()) %>% fmtStns()
        } else if (set == "baseline") {
          all_stns %>% filter(baseline_stn) %>% fmtStns()
        } else if (set == "nutrient") {
          all_stns %>% filter(nutrient_stn) %>% fmtStns()
        } else if (set == "therm") {
          all_stns %>% filter(therm_stn) %>% fmtStns()
        } else {
          fmtStns(all_stns)
        }
      })

      ## dt_data // reactive ----
      dt_data <- reactive({
        cur_stns() %>%
          mutate(Action = if_else(
            StationId %in% stn_list(),
            paste0("<a class='btn-default btn-sm' style='cursor: pointer; text-decoration: none;' id=", StationId, " onclick=\"Shiny.setInputValue('recent_stn', this.id, {priority: 'event'}); Shiny.setInputValue('station', this.id);\">Select</a>"),
            ""
          ) %>% lapply(shiny::HTML), .before = 1)
      })

      ## filename // reactive ----
      # sets file name for downloads
      filename <- reactive({
        req(input$stn_set)
        case_match(
          input$stn_set,
          "map" ~ "WAV Selected Station List",
          "baseline" ~ "WAV Baseline Stations",
          "nutrient" ~ "WAV Nutrient Stations",
          "therm" ~ "WAV Thermistor Stations",
          "all" ~ "WAV Station List"
        )
      })


      # Data table ----

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
            render = JS("
              function(data, type, row, meta) {
                return (type === 'display' && data && data.length > 30) ?
                  '<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;
              }
            ")
          ))
        )
      )

      ## observe // update data table when source data changes ----
      observe({
        dataTableProxy("stn_tbl") %>%
          replaceData(dt_data(), rownames = F)
      })


      # Downloads ----

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


      ## Download handlers per file type ----

      output$dl_csv <- downloadHandler(
        filename = paste0(filename(), ".csv"),
        content = function(file) {
          write_csv(cur_stns(), file)
        }
      )

      output$dl_kml <- downloadHandler(
        filename = paste0(filename(), ".kml"),
        content = function(file) {
          cur_stns() %>%
            mutate(Name = paste(StationId, StationName), .before = 1) %>%
            st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F) %>%
            write_sf(file, layer = "")
        }
      )

      output$dl_geojson <- downloadHandler(
        filename = paste0(filename(), ".geojson"),
        content = function(file) {
          cur_stns() %>%
            mutate(Name = paste(StationId, StationName), .before = 1) %>%
            st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F) %>%
            write_sf(file)
        }
      )

    }
  )
}
