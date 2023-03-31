# server.R

server <- function(input, output, session) {


# Reactive values ---------------------------------------------------------

  recent_stns <- reactiveVal(c())
  first_run <- reactiveVal(T)

# Station select ----------------------------------------------------------

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


  ## Recent stations ----

  observeEvent(cur_stn(), {
    cur_id <- cur_stn()$station_id

    if (!(cur_id %in% recent_stns())) {
      new_list <- c(cur_id, recent_stns())
      if (length(new_list) > 5) new_list <- new_list[1:5]
      recent_stns(new_list)
    }
  })


  ## Available points (sf) ----

  avail_pts <- reactive({
    all_pts %>%
      filter(station_id %in% avail_stns()$station_id)
  })

  avail_baseline_pts <- reactive({
    baseline_pts %>%
      filter(station_id %in% avail_stns()$station_id)
  })

  avail_therm_pts <- reactive({
    therm_pts %>%
      filter(station_id %in% avail_stns()$station_id)
  })

  avail_nutrient_pts <- reactive({
    nutrient_pts %>%
      filter(station_id %in% avail_stns()$station_id)
  })


  ## Total stations text ----

  output$total_stns_text <- renderUI({
    div(
      style = "text-align: center; font-weight: bold; padding: 5px; border: 2px solid grey; border-radius: 5px; width: 100%;",
      paste("Showing", nrow(avail_pts()), "out of", nrow(all_pts), "total stations")
    )
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

  ## Center map when station changes ----
  observeEvent(cur_stn(), {
    if (!is.null(input$map_zoom)) {
      if (input$map_zoom > 7) {
        leafletProxy("map") %>%
          setView(
            lat = cur_stn()$latitude,
            lng = cur_stn()$longitude,
            zoom = input$map_zoom
          )
      }
    }
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
    if (input$map_zoom > 8) {
      leafletProxy("map") %>%
        setView(
          lat = stn$latitude,
          lng = stn$longitude,
          zoom = input$map_zoom
        )
    }
    updateSelectInput(inputId = "station", selected = stn_id)
  })



# Map ---------------------------------------------------------------------

  basemaps <- list(
    one = "ESRI Topo",
    two = "Grey Canvas",
    three = "OpenStreetMap"
  )

  layers <- list(
    counties = "Counties/Regions",
    nkes = paste0("NKE Plans"),
    huc8 = paste0("HUC8 Subbasins"),
    huc10 = paste0("HUC10 Watersheds"),
    huc12 = paste0("HUC12 Subwatersheds"),
    baseline = paste0("Baseline stations (", colorize(stn_colors$baseline), ")"),
    nutrient = paste0("Nutrient stations (", colorize(stn_colors$nutrient), ")"),
    therm = paste0("Thermistor stations (", colorize(stn_colors$thermistor), ")"),
    pins = "Station clusters (groups and pins)"
  )

  hidden_layers <- c(layers$nkes, layers$huc8, layers$huc10, layers$huc12, layers$pins)


  ## Render initial map ----

  output$map <- renderLeaflet({
    leaflet() %>%
      fitBounds(
        lat1 = 42.4,
        lat2 = 47.1,
        lng1 = -92.9,
        lng2 = -86.8
      ) %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = basemaps$one) %>%
      addProviderTiles(providers$CartoDB.Positron, group = basemaps$two) %>%
      addProviderTiles(providers$OpenStreetMap, group = basemaps$three) %>%
      addMapPane("counties", 410) %>%
      addMapPane("huc8", 420) %>%
      addMapPane("huc10", 421) %>%
      addMapPane("huc12", 422) %>%
      addMapPane("nkes", 423) %>%
      addMapPane("baseline", 430) %>%
      addMapPane("thermistor", 431) %>%
      addMapPane("nutrient", 432) %>%
      addMapPane("pins", 440) %>%
      addMapPane("cur_point", 450) %>%
      hideGroup(hidden_layers) %>%
      addLayersControl(
        baseGroups = unlist(basemaps, use.names = FALSE),
        overlayGroups = unlist(layers, use.names = FALSE),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addFullscreenControl(pseudoFullscreen = TRUE) %>%
      addEasyButtonBar(
        easyButton(
          position = "topleft",
          icon = "fa-crosshairs",
          title = "Get my location",
          onClick = JS("
            function(btn, map) {
              map.locate({
                setView: true,
                enableHighAccuracy: false,
                maxZoom: 12
              })
            }
          ")
        ),
        easyButton(
          position = "topleft",
          icon = "fa-globe",
          title = "Reset map view",
          onClick = JS("
            function(btn, map) {
              map.fitBounds([[47.1, -86.8], [42.4, -92.9]])
            }
          ")
        )
      ) %>%
      suspendScroll(
        sleepTime = 0,
        wakeTime = 1000,
        hoverToWake = T,
        sleepNote = F,
        sleepOpacity = 1
      ) %>%
      addPolygons(
        data = counties,
        group = layers$counties,
        label = ~ lapply(paste0("<b>", CountyNam, " County</b><br>", DnrRegion), HTML),
        fillOpacity = 0.1,
        color = "grey",
        opacity = 0.5,
        fillColor = ~ colorFactor("Dark2", counties$DnrRegion)(DnrRegion),
        weight = 1,
        options = pathOptions(pane = "counties")
      )
  })


  ## Render additional map layers ----

  observeEvent(TRUE, {

    map <- leafletProxy("map")
    color <- "blue"
    fill_color <- "lightblue"

    # Nine Key Elements
    delay(500, {
      map %>%
        addPolygons(
          data = nkes,
          group = layers$nkes,
          label = ~ lapply(paste0("<b>", PlanName, "</b><br>Ends: ", EndDate, "<br>Objective: ", Objective), HTML),
          weight = 1,
          color = "blue",
          fillColor = "blue",
          fillOpacity = 0.1,
          options = pathOptions(pane = "nkes"),
          labelOptions = labelOptions(style = list("width" = "300px", "white-space" = "normal"))
        )
    })

    # HUC8
    delay(750, {
      map %>%
        addPolygons(
          data = huc8,
          group = layers$huc8,
          label = ~ lapply(
            paste0(
              "<b>", Huc8Name, " Subbasin</b>",
              "<br>HUC8 Code: ", Huc8Code,
              "<br>Area: ", formatC(ShapeArea / 1e6, format = "f", big.mark = ",", digits = 2), " sq km"),
            HTML),
          weight = 1.5,
          color = color,
          fillColor = fill_color,
          fillOpacity = 0.15,
          options = pathOptions(pane = "huc8")
        )
    })

    # HUC10
    delay(1000, {
      map %>%
        addPolygons(
          data = huc10,
          group = layers$huc10,
          label = ~ lapply(
            paste0(
              "<b>", Huc10Name, " Watershed</b>",
              "<br>HUC10 Code: ", Huc10Code,
              "<br>Area: ", formatC(ShapeArea / 1e6, format = "f", big.mark = ",", digits = 2), " sq km"),
            HTML),
          weight = 1,
          color = color,
          fillColor = fill_color,
          fillOpacity = 0.1,
          options = pathOptions(pane = "huc10")
        )
    })


    # HUC12
    delay(1250, {
      map %>%
        addPolygons(
          data = huc12,
          group = layers$huc12,
          label = ~ lapply(
            paste0(
              "<b>", Huc12Name, " Subwatershed</b>",
              "<br>HUC12 Code: ", Huc12Code,
              "<br>Area: ", formatC(ShapeArea / 1e6, format = "f", big.mark = ",", digits = 2), " sq km"),
            HTML),
          weight = 0.5,
          color = color,
          fillColor = fill_color,
          fillOpacity = 0.05,
          options = pathOptions(pane = "huc12")
        )
    })

    # Hide the legend after a delay
    delay(3000, {
      map %>%
        addLayersControl(
          baseGroups = unlist(basemaps, use.names = FALSE),
          overlayGroups = unlist(layers, use.names = FALSE),
          options = layersControlOptions(collapsed = TRUE)
        )
    })

  })


  ## Render map points ----

  observeEvent(avail_pts(), {

    leafletProxy("map") %>%
      clearGroup(layers$baseline) %>%
      clearGroup(layers$therm) %>%
      clearGroup(layers$nutrient) %>%
      clearGroup(layers$pins)

    if (nrow(avail_pts()) > 0) {
      pts <- avail_pts()
      labels <- all_labels[names(all_labels) %in% pts$station_id] %>% setNames(NULL)
      popups <- all_popups[names(all_popups) %in% pts$station_id] %>% setNames(NULL)

      baseline <- avail_baseline_pts()
      therm <- avail_therm_pts()
      nutrient <- avail_nutrient_pts()

      leafletProxy("map") %>%
        addCircleMarkers(
          data = baseline,
          group = layers$baseline,
          label = setNames(all_labels[names(all_labels) %in% baseline$station_id], NULL),
          # popup = setNames(all_popups[names(all_popups) %in% baseline$station_id], NULL),
          layerId = ~station_id,
          radius = 4,
          color = "black",
          weight = 0.5,
          fillColor = stn_colors$baseline,
          fillOpacity = 0.75,
          options = markerOptions(pane = "baseline", sticky = F)
        ) %>%
        addCircleMarkers(
          data = therm,
          group = layers$therm,
          label = setNames(all_labels[names(all_labels) %in% therm$station_id], NULL),
          # popup = setNames(all_popups[names(all_popups) %in% therm$station_id], NULL),
          layerId = ~station_id,
          radius = 4,
          color = "black",
          weight = 0.5,
          fillColor = stn_colors$thermistor,
          fillOpacity = 0.75,
          options = markerOptions(pane = "thermistor", sticky = F)
        ) %>%
        addCircleMarkers(
          data = nutrient,
          group = layers$nutrient,
          label = setNames(all_labels[names(all_labels) %in% nutrient$station_id], NULL),
          # popup = setNames(all_popups[names(all_popups) %in% nutrient$station_id], NULL),
          layerId = ~station_id,
          radius = 4,
          color = "black",
          weight = 0.5,
          fillColor = stn_colors$nutrient,
          fillOpacity = 0.75,
          options = markerOptions(pane = "nutrient", sticky = F)
        ) %>%
        addMarkers(
          data = pts,
          group = layers$pins,
          label = labels,
          popup = popups,
          layerId = ~station_id,
          clusterOptions = markerClusterOptions()
        )
    } else {
      leafletProxy("map") %>%
        clearGroup("cur_point")
    }
  })


  ## Get clicked station and select it ----

  observe({
    updateSelectInput(
      inputId = "station",
      selected = input$map_marker_click
    )
  })


  ## Handle displaying current station ----

  observeEvent(list(avail_stns(), cur_stn()), {
    leafletProxy("map") %>%
      clearGroup("cur_point")

    if (nrow(avail_stns()) == 0) return()

    label <- all_labels[names(all_labels) == cur_stn()$station_id] %>% setNames(NULL)
    popup <- all_popups[names(all_popups) == cur_stn()$station_id] %>% setNames(NULL)

    leafletProxy("map") %>%
      addCircleMarkers(
        data = cur_stn(),
        lat = ~latitude,
        lng = ~longitude,
        label = label,
        popup = popup,
        layerId = ~station_id,
        group = "cur_point",
        options = pathOptions(pane = "cur_point"),
        radius = 5,
        weight = 0.75,
        color = "black",
        fillColor = stn_colors$current,
        fillOpacity = 1
      ) %>%
      addMarkers(
        data = cur_stn(),
        lat = ~latitude,
        lng = ~longitude,
        label = label,
        popup = popup,
        layerId = ~station_id,
        group = "cur_point",
        options = pathOptions(pane = "cur_point")
      )
  })


  ## Map action buttons ----

  ### Zoom in button ----

  observeEvent(input$zoom_in, {
    leafletProxy("map") %>%
      setView(
        lat = cur_stn()$latitude,
        lng = cur_stn()$longitude,
        zoom = 10
      )
  })


  ### Reset zoom button ----

  observeEvent(input$reset_zoom, {
    if (nrow(avail_pts()) > 0) {
      leafletProxy("map") %>%
        fitBounds(
          lat1 = min(avail_pts()$latitude),
          lat2 = max(avail_pts()$latitude),
          lng1 = min(avail_pts()$longitude),
          lng2 = max(avail_pts()$longitude)
        )
    } else {
      leafletProxy("map") %>%
        fitBounds(
          lat1 = min(all_pts$latitude),
          lat2 = max(all_pts$latitude),
          lng1 = min(all_pts$longitude),
          lng2 = max(all_pts$longitude)
        )
    }

  })



# Station info ---------------------------------------------------------------

  output$stn_info <- renderUI({
    list(
      h4("Station Information"),
      renderTable(
        {
          cur_stn() %>%
            select(station_id:geometry) %>%
            st_set_geometry(NULL) %>%
            mutate(across(everything(), as.character)) %>%
            clean_names(case = "title") %>%
            pivot_longer(
              cols = everything(),
              names_to = "Property",
              values_to = "Value") %>%
            na.omit()
        }
      )
    )
  })

  output$stn_coverage <- renderUI({
    list(
      h4("Station Data Coverage"),
      renderTable(
        {
          all_stn_data %>%
            filter(station_id == cur_stn()$station_id) %>%
            select(-station_id) %>%
            clean_names(case = "title")
        },
        align = "c"
      )
    )
  })


  # Station lists -----------------------------------------------------------

  stationListServer()





# Recent stations ---------------------------------------------------------

  ## Layout ----
  output$recent_stn_ui <- renderUI({
    list(
      dataTableOutput("recent_stn_tbl"),
      p(actionButton("clear_recent_stns", "Clear list"), align = "right", style = "margin-top: 1em;")
    )
  })

  ## Table ----
  output$recent_stn_tbl <- renderDataTable({
    ids <- recent_stns()
    cur_id <- cur_stn()$station_id

    tibble(station_id = ids) %>%
      left_join(all_stns, by = "station_id") %>%
      select(id = station_id, name = station_name, baseline = baseline_stn, nutrient = nutrient_stn, thermistor = therm_stn) %>%
      mutate(across(where(is_logical), ~ ifelse(.x, "\u2705", "\u274c"))) %>%
      mutate(action = lapply(ids, function(id) {
        paste0("<a style='cursor: pointer;' id=", id, " onclick=\"Shiny.setInputValue('recent_stn', this.id, {priority: 'event'}); Shiny.setInputValue('station', this.id);\">Select</a>")
      }), .before = everything()) %>%
      mutate(current = ifelse(id == cur_id, "\u27a4", ""), .before = everything()) %>%
      clean_names("title")
    },
    server = F,
    rownames = T,
    selection = "none",
    options = list(
      paging = F,
      bFilter = F,
      bSort = F,
      bInfo = F,
      columnDefs = list(list(targets = c(0:1, 3:5), className = "dt-center")))
  )

  ## Observers ----
  observeEvent(input$recent_stn, {
    id <- input$recent_stn

    if (id %in% stn_list()) {
      updateSelectInput(
        inputId = "station",
        selected = id
      )
    } else {
      updateSelectInput(
        inputId = "stn_types",
        selected = station_types
      )
      updateCheckboxGroupInput(
        inputId = "stn_years",
        selected = data_years[1]
      )
    }
  })

  observeEvent(input$clear_recent_stns, {
    recent_stns(cur_stn()$station_id)
  })


# Baseline data ----

  baselineServer(cur_stn = reactive(cur_stn()))


# Nutrient data ----

  nutrientServer(cur_stn = reactive(cur_stn()))


# Thermistor data ----

  thermistorServer(cur_stn = reactive(cur_stn()))


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
