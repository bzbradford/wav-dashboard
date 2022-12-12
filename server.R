# server.R

server <- function(input, output, session) {


# Helper functions --------------------------------------------------------

  year_choices <- function(years) {
    if (length(years) > 1) {
      c(years, "All")
    } else {
      years
    }
  }

  min_max <- function(v) {
    possibly(
      return(c(floor(min(v, na.rm = T)), ceiling(max(v, na.rm = T)))),
      return(c(NA, NA))
    )
  }


# Reactive values ---------------------------------------------------------

  recent_stns <- reactiveVal(value = c())

# Station select ----------------------------------------------------------

  ## Random station on load ----

  random_stn <- all_pts %>%
    filter(baseline_stn) %>%
    filter(max_fw_year == max(data_years)) %>%
    pull(station_id) %>%
    sample(1)


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
    if (nrow(avail_stns()) > 0) {
      ids <- avail_stns()$station_id
      all_stn_list[all_stn_list %in% ids]
    } else {
      list("No stations available" = NULL)
    }
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
      selected <- stations[sample(1:length(stations), 1)]
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

  ## Prev/Next station buttons ----
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


  ### Random site button ----

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

    updateSelectInput(
      inputId = "station",
      selected = stn_id
    )
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

  ## Layout ----

  output$station_lists <- renderUI({
    bsCollapse(
      bsCollapsePanel(
        title = "Baseline monitoring stations",
        p(downloadButton("baseline_stn_dl", "Download this list")),
        dataTableOutput("baseline_stn_tbl")
      ),
      bsCollapsePanel(
        title = "Nutrient monitoring locations",
        p(downloadButton("nutrient_stn_dl", "Download this list")),
        dataTableOutput("nutrient_stn_tbl")
      ),
      bsCollapsePanel(
        title = "Thermistor station locations",
        p(downloadButton("therm_stn_dl", "Download this list")),
        dataTableOutput("therm_stn_tbl")
      ),
      bsCollapsePanel(
        title = "Complete station list",
        p(downloadButton("all_stns_dl", "Download this list")),
        dataTableOutput("all_stn_tbl")
      )
    )
  })

  output$baseline_stn_tbl <- renderDataTable({
    all_stns %>%
      filter(baseline_stn) %>%
      clean_names(case = "big_camel")
  })

  output$nutrient_stn_tbl <- renderDataTable({
    all_stns %>%
      filter(nutrient_stn) %>%
      clean_names(case = "big_camel")
  })

  output$therm_stn_tbl <- renderDataTable({
    all_stns %>%
      filter(therm_stn) %>%
      clean_names(case = "big_camel")
  })

  output$all_stn_tbl <- renderDataTable({
    all_stns %>%
      clean_names(case = "big_camel")
  })


  ## Download handlers ----

  output$baseline_stn_dl <- downloadHandler(
    "wav-baseline-stations.csv",
    function(file) {
      all_stns %>%
        filter(baseline_stn) %>%
        write_csv(file)
    }
  )

  output$therm_stn_dl <- downloadHandler(
    "wav-thermistor-stations.csv",
    function(file) {
      all_stns %>%
        filter(therm_stn) %>%
        write_csv(file)
    }
  )

  output$nutrient_stn_dl <- downloadHandler(
    "wav-nutrient-stations.csv",
    function(file) {
      all_stns %>%
        filter(nutrient_stn) %>%
        write_csv(file)
    }
  )

  output$all_stns_dl <- downloadHandler(
    "wav-station-list.csv",
    function(file) {
      all_stns %>%
        write_csv(file)
    }
  )



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


# Baseline data -----------------------------------------------------------

  ## Vars ----

  cur_baseline_data <- reactive({
    baseline_data %>%
      filter(station_id == cur_stn()$station_id)
  })

  cur_baseline_years <- reactive({
    sort(unique(cur_baseline_data()$year))
  })

  selected_baseline_data <- reactive({
    if (input$baseline_year == "All") {
      cur_baseline_data()
    } else {
      cur_baseline_data() %>%
        filter(year == input$baseline_year)
    }
  })


  ## Layout ----

  output$baseline_tab <- renderUI({
    validate(
      need(
        nrow(cur_baseline_data()) > 0,
        "This station has no baseline data. Choose another station or view the thermistor or nutrient data associated with this station."
      )
    )

    list(
      div(
        class = "well flex-row year-btns",
        div(class = "year-btn-text", em("Choose year:")),
        radioGroupButtons(
          inputId = "baseline_year",
          label = NULL,
          choices = year_choices(cur_baseline_years()),
          selected = last(cur_baseline_years())
        )
      ),
      div(
        id = "baseline-plot-container",
        h3(cur_stn()$label, align = "center"),
        plotlyOutput("baseline_plot")
      ),
      uiOutput("baseline_plot_export"),
      p(strong("Dissolved Oxygen."), "The amount of dissolved oxygen (D.O.) in a stream is critical for aquatic life, particularly larger animals like fish. 5 mg/L is considered the minimum level for fish, while 7 mg/L is the minimum required by trout in the spawning season. Colder waters can support higher concentrations of dissolved oxygen than warmer waters. The percent saturation refers to the equilibrium amount of oxygen that can dissolve into the water from the atmosphere. Higher than 100% D.O. saturation means oxygen is being actively added to the water, either by aquatic life or by air-water mixing. Lower than 100% D.O. saturation means the dissolved oxygen has been depleted below equilibrium level by plant or algal respiration or decomposition."),
      p(strong("Temperature."), "The chart shows both the recorded air temperature and water temperature. Cold streams generally provide better habitat because they can contain higher levels of dissolved oxygen, and higher water temperatures may indicate shallow, pooled, or stagnant water. Learn more about water temperature on the", strong("Thermistor"), "data tab."),
      p(strong("Transparency."), "These measurements reflect the turbidity of the stream water. Lower transparency means the water is cloudier/murkier and could indicate a recent storm event kicking up silt and mud in the stream. Lower transparency isn't necessarily bad but can be associated with warm waters with low dissolved oxygen and lots of suspended algae."),
      p(strong("Stream flow."), "Stream flow measurements give you an idea of the general size of the stream. Periods of higher than normal stream flow would suggest recent rains in the watershed. Most streams have a consistent and predictable stream flow based on the size of the watershed that they drain, but stream flow will 'pulse' after rain and storm events, returning to baseflow after several days. For more stream flow data check out the", a("USGS Water Dashboard", href = "https://dashboard.waterdata.usgs.gov/app/nwd/?aoi=state-wi", target = "_blank", .noWS = "after"), "."),
      br(),
      uiOutput("baseline_data")
    )
  })

  ## Plot export ----

  output$baseline_plot_export <- renderUI({
    p(
      style = "margin-left: 2em; margin-right: 2em; font-size: smaller;",
      align = "center",
      em("Click on any of the plot legend items to show or hide it in the plot.",
        a("Click here to download this plot as a PNG.",
          style = "cursor: pointer;",
          onclick = paste0(
            "html2canvas(document.querySelector('",
            "#baseline-plot-container",
            "'), {scale: 3}).then(canvas => {saveAs(canvas.toDataURL(), '",
            paste("baseline-plot", cur_stn()$station_id, input$baseline_year, sep = "-"),
            ".png",
            "')})")
        )
      )
    )
  })


  ## Plot ----

  oxy_color <- function(d_o) {
    i <- min(max(round(d_o), 1), 11)
    brewer.pal(11, "RdBu")[i]
  }

  find_max <- function(vals, min_val) {
    vals <- na.omit(vals)
    if (length(vals) == 0) return(min_val)
    ceiling(max(min_val, max(vals)) * 1.1)
  }

  output$baseline_plot <- renderPlotly({
    req(input$baseline_year)
    req(nrow(selected_baseline_data()) > 0)

    df <- selected_baseline_data() %>%
      distinct(date, .keep_all = T)

    yranges <- list(
      d_o = c(0, find_max(df$d_o, 12)),
      temp = c(0, find_max(c(df$water_temperature, df$ambient_air_temp), 30)),
      trans = c(0, 125),
      cfs = c(0, find_max(df$stream_flow_cfs, 10))
    )

    do_data <- df %>%
      filter(!is.na(d_o)) %>%
      mutate(label = ifelse(
        is.na(d_o_percent_saturation),
        paste0(d_o, " mg/L"),
        paste0(d_o, " mg/L<br>", d_o_percent_saturation, "% sat"))) %>%
      rowwise() %>%
      mutate(do_color = oxy_color(d_o))
    temp_data <- df %>% filter(!(is.na(water_temperature) & is.na(ambient_air_temp)))
    trans_data <- df %>% filter(!is.na(transparency_average))
    flow_data <- df %>% filter(!is.na(stream_flow_cfs))

    df %>%
      plot_ly() %>%
      layout(
        title = "Baseline Measurements",
        hovermode = "x unified",
        margin = list(t = 50, r = 50),
        legend = list(orientation = "h"),
        xaxis = list(
          title = "",
          type = "date",
          fixedrange = T,
          dtick = "M1",
          ticklabelmode = "period",
          hoverformat = "%b %d, %Y",
          domain = c(.1, .9))
      ) %>%
      add_trace(
        data = do_data,
        name = "D.O.",
        x = ~date,
        y = ~d_o,
        text = ~paste0(d_o, " mg/L<br>", d_o_percent_saturation, "% sat"),
        marker = list(
          color = ~do_color,
          line = list(color = "black", width = 0.5)),
        type = "bar",
        width = 1000 * 60 * 60 * 24 * 15,
        hovertemplate = "%{y}"
      ) %>%
      add_trace(
        data = temp_data,
        name = "Water temp",
        x = ~date,
        y = ~water_temperature,
        type = "scatter",
        mode = "lines+markers",
        yaxis = "y2",
        marker = list(
          color = "lightblue",
          size = 10,
          line = list(color = "white", width = 1)
        ),
        line = list(
          color = "lightblue",
          width = 3
        )
      ) %>%
      add_trace(
        data = temp_data,
        name = "Air temp",
        x = ~date,
        y = ~ambient_air_temp,
        type = "scatter",
        mode = "lines+markers",
        yaxis = "y2",
        marker = list(
          color = "orange",
          size = 10,
          line = list(color = "white", width = 1)
        ),
        line = list(color = "orange", width = 3)
      ) %>%
      add_trace(
        data = trans_data,
        name = "Transparency",
        x = ~date,
        y = ~transparency_average,
        type = "scatter",
        mode = "lines+markers",
        yaxis = "y3",
        marker = list(
          color = "brown",
          size = 10,
          symbol = "square",
          line = list(color = "white", width = 1)
        ),
        line = list(color = "brown", width = 3)
      ) %>%
      add_trace(
        data = flow_data,
        name = "Stream flow",
        x = ~date,
        y = ~stream_flow_cfs,
        type = "scatter",
        mode = "lines+markers",
        yaxis = "y4",
        marker = list(
          color = "#48a67b",
          size = 10,
          symbol = "triangle-right",
          line = list(color = "white", width = 1)
        ),
        line = list(color = "#48a67b", width = 3)
      ) %>%
      layout(
        yaxis = list(
          title = "Dissolved oxygen",
          ticksuffix = " mg/L",
          range = yranges$d_o,
          fixedrange = T
        ),
        yaxis2 = list(
          title = "Temperature",
          overlaying = "y",
          side = "left",
          ticksuffix = "&deg;C",
          position = 0,
          showgrid = F,
          zeroline = F,
          range = yranges$temp,
          fixedrange = T
        ),
        yaxis3 = list(
          title = "Transparency",
          overlaying = "y",
          side = "right",
          ticksuffix = " cm",
          showgrid = F,
          zeroline = F,
          range = yranges$trans,
          fixedrange = T
        ),
        yaxis4 = list(
          title = "Stream flow",
          overlaying = "y",
          side = "right",
          ticksuffix = " cfs",
          position = 1,
          showgrid = F,
          zeroline = F,
          range = yranges$cfs,
          fixedrange = T
        )
      ) %>%
      config(displayModeBar = F)
  })


  ## Data ----

  output$baseline_data <- renderUI({
    bsCollapse(
      bsCollapsePanel(
        title = "View/download baseline data",
        p(
          downloadButton("baseline_data_dl", paste("Download", input$baseline_year, "data")),
          downloadButton("baseline_data_all_dl", "Download all years of baseline data for this site")
        ),
        div(style = "overflow: auto;", dataTableOutput("baseline_data_table"))
      )
    )
  })

  output$baseline_data_table <- renderDataTable({

    df <- selected_baseline_data() %>%
      arrange(date) %>%
      distinct(date, .keep_all = TRUE) %>%
      clean_names(case = "title") %>%
      mutate(label = format(Date, "%b %d")) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = -label, names_to = "Parameter") %>%
      pivot_wider(names_from = label) %>%
      mutate(Parameter = gsub("D o", "D.O.", Parameter)) %>%
      mutate(Parameter = gsub("P h", "pH", Parameter))

    datatable(df, options = list(paging = F))
  },
    server = F)

  output$baseline_data_dl <- downloadHandler(
    paste0("stn-", cur_stn()$station_id, "-baseline-data-", input$baseline_year, ".csv"),
    function(file) {write_csv(selected_baseline_data(), file)}
  )

  output$baseline_data_all_dl <- downloadHandler(
    paste0("stn-", cur_stn()$station_id, "-baseline-data.csv"),
    function(file) {write_csv(cur_baseline_data(), file)}
  )



  # Nutrient data -----------------------------------------------------------

  ## Vars ----

  phoslimit <- 0.075 # mg/L, ppm

  cur_nutrient_data <- reactive({
    nutrient_data %>%
      filter(station_id == cur_stn()$station_id)
  })

  cur_nutrient_years <- reactive({
    sort(unique(cur_nutrient_data()$year))
  })

  selected_nutrient_data <- reactive({
    if (input$nutrient_year == "All") {
      cur_nutrient_data()
    } else {
      cur_nutrient_data() %>%
        filter(year == input$nutrient_year)
    }
  })

  phos_estimate <- reactive({
    vals <- na.omit(selected_nutrient_data()$tp)
    log_vals <- log(vals)
    n <- length(vals)
    meanp <- mean(log_vals)
    se <- sd(log_vals) / sqrt(n)
    suppressWarnings({
      tval <- qt(p = 0.90, df = n - 1)
    })

    params <- list(
      mean = meanp,
      median = median(log_vals),
      lower = meanp - tval * se,
      upper = meanp + tval * se
    )

    params <- lapply(params, exp)
    params <- lapply(params, round, 3)
    params["n"] <- n
    params
  })


  ## Layout ----

  output$nutrient_tab <- renderUI({
    validate(
      need(
        nrow(cur_nutrient_data()) > 0,
        "This station has no nutrient data. Choose another station or view the baseline or thermistor data associated with this station."
      )
    )

    list(
      div(
        class = "well flex-row year-btns",
        div(class = "year-btn-text", em("Choose year:")),
        radioGroupButtons(
          inputId = "nutrient_year",
          label = NULL,
          choices = year_choices(cur_nutrient_years()),
          selected = last(cur_nutrient_years())
        )
      ),
      div(
        id = "nutrient-plot-container",
        h3(cur_stn()$label, align = "center"),
        plotlyOutput("nutrient_plot"),
        p(
          style = "margin-left: 2em; margin-right: 2em; font-size: smaller;",
          align = "center",
          em("The dashed line on this plot indicates the total phosphorus state exceedance level of 0.075 mg/L (ppm). If more than one month of data was collected, the median and 90% confidence interval for the true total phosphorus level are displayed as a horizontal band.")
        ),
      ),
      uiOutput("nutrient_plot_export"),
      br(),
      p("The shaded horizontal band on the plot represents the 90% confidence interval for the median total phosphorus (TP) at this site (if more than one month of data was collected). This means that, given the TP concentrations measured this year, there is about an 90% chance that the true median total phosphorus concentration falls somewhere between those lines. We know that TP in streams varies quite a bit, so individual samples could be higher or lower than the confidence interval."),
      p(strong("Exceedance criteria."), HTML(paste0("A stream site is considered 'Criteria Exceeded' and the confidence interval band will be shaded ", strong(colorize("red")), " if: 1) the lower 90% confidence limit of the sample median exceeds the state TP criterion of ", phoslimit, " mg/L or 2) there is corroborating WDNR biological data to support an adverse response in the fish or macroinvertebrate communities. If there is insufficient data for either of these requirements, more data will need to be collected in subsequent years before a decision can be made. A site is designated as 'Watch Waters' if the total phosphorus state criterion concentration falls within the confidence limit or additional data are required, and a site is considered to have 'Met Criteria' if the upper limit of the confidence interval does not exceed the criterion (shaded confidence interval band will be ", strong(colorize("teal")), "). Some sites are assigned 'Watch Waters' because fewer than six samples were collected. Nevertheless, these total phosphorus measurements will still improve our understanding of stream health at this site."))),
      p(strong("Why phosphorus?"), "Phosphorus is an essential nutrient responsible for plant growth, but it is also the most visible, widespread water pollutant in lakes. Small increases in phosphorus levels can bring about substantial increases in aquatic plant and algae growth, which in turn can reduce the recreational use and biodiversity. When the excess plants die and are decomposed, oxygen levels in the water drop dramatically which can lead to fish kills. Additionally, one of the most common impairments in Wisconsin’s streams is excess sediment that covers stream bottoms. Since phosphorus moves attached to sediments, it is intimately connected with this source of pollution in our streams. Phosphorus originates naturally from rocks, but its major sources in streams and lakes today are usually associated with human activities: soil erosion, human and animal wastes, septic systems, and runoff from farmland or lawns. Phosphorus-containing contaminants from urban streets and parking lots such as food waste, detergents, and paper products are also potential sources of phosphorus pollution from the surrounding landscape. The impact that phosphorus can have in streams is less apparent than in lakes due to the overall movement of water, but in areas with low velocity, where sediment can settle and deposit along the bottom substrate, algae blooms can result."),
      p(strong("Volunteer monitoring protocol."), "To assess in-stream phosphorus levels, WAV volunteers collected water samples that were analyzed for total phosphorus (TP) at the State Lab of Hygiene during the growing season. Following Wisconsin Department of Natural Resources (WDNR) methods, four to six phosphorus water samples were collected at each monitoring site - one per month for up to each of the six months during the growing season. The monthly water samples were collected approximately 30 days apart and no samples were collected within 15 days of one another. Samples at several sites were collected every two weeks. The monthly values are an average of the biweekly sample results."),
      br(),
      uiOutput("nutrient_data")
    )
  })


  ## Plot export ----

  output$nutrient_plot_export <- renderUI({
    p(
      style = "margin-left: 2em; margin-right: 2em; font-size: smaller;",
      align = "center",
      em(
        a("Click here to download this plot as a PNG.",
          style = "cursor: pointer;",
          onclick = paste0(
            "html2canvas(document.querySelector('",
            "#nutrient-plot-container",
            "'), {scale: 3}).then(canvas => {saveAs(canvas.toDataURL(), '",
            paste("nutrient-plot", cur_stn()$station_id, input$nutrient_year, sep = "-"),
            ".png",
            "')})"
          )
        )
      )
    )
  })


  ## Plot ----

  output$nutrient_plot <- renderPlotly({
    req(nrow(selected_nutrient_data()) > 0)

    df <- selected_nutrient_data() %>%
      mutate(exceedance = factor(
        ifelse(is.na(tp), "No data", ifelse(tp >= phoslimit, "TP High", "TP OK")),
        levels = c("TP OK", "TP High", "No data"))) %>%
      drop_na(tp) %>%
      mutate(phoslimit = phoslimit)

    min_year <- min(df$year)
    max_year <- max(df$year)
    date_range <- c(ISOdate(min_year, 5, 1), ISOdate(max_year, 10, 31))
    outer_months <- c(ISOdate(min_year, 4, 30), ISOdate(max_year, 11, 1))
    data_dates <- unique(df$date)
    all_dates <-  c(outer_months, data_dates)
    yrange <- suppressWarnings(c(0, max(phoslimit * 1.1, max(df$tp, na.rm = T) * 1.1)))

    phos_params <- tibble(
      date = all_dates,
      lower = phos_estimate()$lower,
      upper = phos_estimate()$upper,
      median = phos_estimate()$median
    )

    # no confidence invervals if only one month of data
    if (phos_estimate()$n > 1) {
      ci_color <- ifelse(phos_estimate()$upper >= phoslimit, "red", "teal")

      plt <- plot_ly(phos_params) %>%
        add_lines(
          x = ~date,
          y = ~phoslimit,
          name = "TP limit",
          xperiod = "M1",
          xperiodalignment = "middle",
          opacity = 0.75,
          line = list(color = "black", dash = "dash", width = 1.5)
        ) %>%
        add_lines(
          x = ~date,
          y = ~lower,
          name = "Lower 90% CI",
          xperiod = "M1",
          xperiodalignment = "middle",
          opacity = 0.5,
          line = list(color = ci_color, width = 0.5)
        ) %>%
        add_lines(
          x = ~date,
          y = ~median,
          name = "Median",
          xperiod = "M1",
          xperiodalignment = "middle",
          opacity = 0.5,
          line = list(color = "darkblue", width = 2)
        ) %>%
        add_lines(
          x = ~date,
          y = ~upper,
          name = "Upper 90% CI",
          xperiod = "M1",
          xperiodalignment = "middle",
          opacity = 0.5,
          line = list(color = ci_color, width = 0.5)
        )

      shapes <- list(
        rect(phos_estimate()$lower, phos_estimate()$upper, ci_color)
      )
    } else {
      plt <- plot_ly()
      shapes <- list()
    }

    plt <- plt %>%
      add_trace(
        data = df,
        x = ~date,
        y = ~tp,
        type = "bar",
        text = ~tp,
        textposition = "auto",
        color = ~exceedance,
        colors = "Set2",
        width = 0.5 * 1000 * 60 * 60 * 24 * 30,
        xperiod = "M1",
        xperiodalignment = "middle",
        marker = list(
          line = list(color = "rgb(8,48,107)", width = 1)
        ),
        textfont = list(color = "black"),
        hovertemplate = "Measured TP: %{y:.3f} ppm<extra></extra>"
      ) %>%
      layout(
        title = "Total Phosphorus",
        xaxis = list(
          title = "",
          type = "date",
          tickformat = "%B<br>%Y",
          dtick = "M1",
          ticklabelmode = "period",
          range = date_range),
        yaxis = list(
          title = "Total phosphorus",
          ticksuffix = " ppm",
          zerolinecolor = "lightgrey",
          range = yrange),
        legend = list(
          traceorder = "reversed",
          orientation = "h",
          x = 0.25, y = 1
        ),
        hovermode = "x unified",
        margin = list(t = 50),
        shapes = shapes
      ) %>%
      config(displayModeBar = F)

    plt
  })


  ## Data ----

  output$nutrient_data <- renderUI({
    bsCollapse(
      bsCollapsePanel(
        title = "View or download nutrient data data",

        p(
          downloadButton("nutrient_data_dl", paste("Download", input$nutrient_year, "data")),
          downloadButton("nutrient_data_all_dl", paste("Download all years of nutrient data for this site"))
        ),
        div(style = "overflow: auto;", dataTableOutput("nutrient_data_table")),
        p(em("Total phosphorus is shown in units of mg/L (ppm)."))
      )
    )
  })

  output$nutrient_data_table <- renderDataTable({
    selected_nutrient_data() %>%
      drop_na(tp) %>%
      clean_names(case = "big_camel")
  })

  output$nutrient_data_dl <- downloadHandler(
    paste0("stn-", cur_stn()$station_id, "-nutrient-data-", input$nutrient_year, ".csv"),
    function(file) {write_csv(selected_nutrient_data(), file)}
  )

  output$nutrient_data_all_dl <- downloadHandler(
    paste0("stn-", cur_stn()$station_id, "-nutrient-data.csv"),
    function(file) {write_csv(cur_nutrient_data(), file)}
  )



# Thermistor data ---------------------------------------------------------

  ## Vars ----

  cur_therm_data <- reactive({
    therm_data %>%
      filter(station_id == cur_stn()$station_id)
  })

  cur_therm_years <- reactive({
    sort(unique(cur_therm_data()$year))
  })

  selected_therm_data <- reactive({
    if (input$therm_year == "All") {
      cur_therm_data()
    } else {
      cur_therm_data() %>%
        filter(year == input$therm_year)
    }
  })

  # create station daily totals
  therm_daily <- reactive({
    df <- selected_therm_data()
    req(nrow(df) > 0)
    req(input$therm_temp_units)
    temp_col <- paste0("temp_", tolower(input$therm_temp_units))

    df %>%
      group_by(date) %>%
      summarise(
        hours = n(),
        min = min(!!sym(temp_col)),
        max = max(!!sym(temp_col)),
        mean = round(mean(!!sym(temp_col)), 2),
        units = input$therm_temp_units,
        lat = latitude[1],
        long = longitude[1]
      ) %>%
      mutate(
        station_id = cur_stn()$station_id,
        station_name = cur_stn()$station_name,
        .before = everything()
      )
  })


  ## Layout ----

  output$therm_tab <- renderUI({
    validate(
      need(
        nrow(cur_therm_data()) > 0,
        "This station has no thermistor data. Choose another station or view the baseline or nutrient data associated with this station."
      )
    )

    list(
      div(
        class = "well flex-row year-btns",
        div(class = "year-btn-text", em("Choose year:")),
        radioGroupButtons(
          inputId = "therm_year",
          label = NULL,
          choices = year_choices(cur_therm_years()),
          selected = last(cur_therm_years())
        )
      ),
      uiOutput("therm_plot_opts"),
      div(
        id = "therm-plot-container",
        h3(cur_stn()$label, align = "center"),
        plotlyOutput("therm_plot"),
        uiOutput("therm_plot_caption")
      ),
      uiOutput("therm_plot_export"),
      br(),
      h4("What does water temperature tell us?"),
      p("Temperature is often referred to as a 'master variable' in aquatic ecosystems because temperature determines the speed of important processes, from basic chemical reactions to the growth and metabolism of fishes and other organisms. In addition, temperature determines the type of fish community that the stream can support. It is especially important to gather stream temperature information over many years in order to track how quickly our streams are warming due to climate change. The continuous data loggers you have deployed and maintained are a 21st-century approach to monitoring stream temperature, providing accurate, high-resolution temperature data as we monitor the health of streams across the state."),
      p("Tips for understanding and interacting with the temperature plot:"),
      tags$ul(
        tags$li("Hover over the chart to see hourly and daily temperature measurement details."),
        tags$li("Click on the legend to show / hide the time series of hourly or daily temperature."),
        tags$li("Drag the mouse over a time period to zoom in, and double click to return to the original view."),
        tags$li("While hovering over the plot, click the camera icon along the top to download a picture of the plot."),
        tags$li("Anomalous temperature readings at the very beginning and end of the data may reflect air temperatures before the logger was deployed into the stream. It's also possible that the logger because exposed to the air during deployment if water levels dropped.")
      ),
      br(),
      uiOutput("therm_data")
    )
  })


  ## Plot export ----

  output$therm_plot_export <- renderUI({
    p(
      style = "margin-left: 2em; margin-right: 2em; font-size: smaller;",
      align = "center",
      em(
        a("Click here to download this plot as a PNG.",
          style = "cursor: pointer;",
          onclick = paste0(
            "html2canvas(document.querySelector('",
            "#therm-plot-container",
            "'), {scale: 3}).then(canvas => {saveAs(canvas.toDataURL(), '",
            paste("thermistor-plot", cur_stn()$station_id, input$therm_year, sep = "-"),
            ".png",
            "')})"
          )
        )
      )
    )
  })


  ## Plot options ----

  output$therm_plot_opts <- renderUI({
    list(
      p(
        div(
          style = "float: left; margin-right: 1em;",
          strong("Temperature units:")
        ),
        radioButtons(
          inputId = "therm_temp_units",
          label = NULL,
          inline = T,
          choices = list("Fahrenheit" = "F", "Celsius" = "C")
        ),
        div(
          style = "float: left; margin-right: 1em;",
          strong("Optional plot annotations:")
        ),
        radioButtons(
          inputId = "therm_plot_annotations",
          label = NULL,
          inline = T,
          choices = list(
            "Brook trout temperature range" = "btrout",
            "Warm/cool/coldwater classification" = "wtemp",
            "None"
          )
        )
      ),
      hr()
    )
  })


  ## Plot ----

  output$therm_plot <- renderPlotly({
    req(input$therm_temp_units)
    req(input$therm_plot_annotations)
    req(nrow(therm_daily()) > 0)
    req(nrow(selected_therm_data()) > 0)

    units <- input$therm_temp_units
    annotation <- input$therm_plot_annotations

    df_daily <- therm_daily() %>%
      mutate(date_time = as.POSIXct(paste(date, "12:00:00")))
    df_hourly <- selected_therm_data()

    # if (input$therm_year == "All") {
    #   dates <- df_hourly %>%
    #     group_by(year) %>%
    #     summarize(
    #       first_date = min(date) - 1
    #       # ,
    #       # last_date = max(date) + 2
    #     )
    #   # dates <- tibble(date = c(dates$first_date, dates$last_date)) %>%
    #   #   arrange(date)
    #   dates <- tibble(date = dates$first_date) %>%
    #     arrange(date)
    #   # years <- cur_therm_years()[1:length(cur_therm_years()) - 1]
    #   # dates <- tibble(date = as.Date(paste0(years, "-12-31")))
    #   print(dates)
    #   df_daily <- df_daily %>%
    #     bind_rows(dates) %>%
    #     arrange(date)
    #
    #   df_hourly <- df_hourly %>%
    #     bind_rows(dates) %>%
    #     arrange(date)
    # }
    #
    # df_daily <- df_daily %>%
    #   mutate(date_time = as.POSIXct(paste(date, "12:00:00")))
    # print(head(df_daily))

    # handle units
    temp_col <- paste0("temp_", tolower(units))
    ytitle <- paste0("Temperature (°", units, ")")
    yrange <- ifelse(units == "F", c(30, 100), c(0, 37))

    plt <- plot_ly() %>%
      add_ribbons(
        data = df_daily,
        x = ~ date_time,
        ymin = ~ min,
        ymax = ~ max,
        line = list(
          color = "lightblue",
          width = 0.5,
          opacity = 0),
        fillcolor = "lightblue",
        opacity = 0.5,
        name = "Daily Range",
        hovertemplate = "Daily Range<extra></extra>"
      ) %>%
      add_lines(
        data = df_daily,
        x = ~ date_time,
        y = ~ min,
        line = list(
          color = "lightblue",
          width = 1,
          opacity = 0.5),
        name = "Daily Min",
        showlegend = F
      ) %>%
      add_lines(
        data = df_daily,
        x = ~ date_time,
        y = ~ max,
        line = list(
          color = "lightblue",
          width = 1,
          opacity = 0.5),
        name = "Daily Max",
        showlegend = F
      ) %>%
      add_trace(
        x = df_hourly$date_time,
        y = df_hourly[[temp_col]],
        name = "Hourly Temperature",
        type = "scatter",
        mode = "lines",
        line = list(
          color = "#1f77b4",
          width = 0.5,
          opacity = 0.8
        )) %>%
      add_trace(
        data = df_daily,
        x = ~ date_time,
        y = ~ mean,
        name = "Mean Daily Temp.",
        type = "scatter",
        mode = "lines",
        line = list(
          color = "orange"
        )) %>%
      layout(
        title = "Stream Temperature",
        showlegend = TRUE,
        xaxis = list(title = "Date and Time"),
        yaxis = list(
          title = ytitle,
          range = yrange,
          zerolinecolor = "lightgrey"),
        hovermode = "x unified",
        legend = list(
          orientation = "h",
          x = 0.25,
          y = 1
        ),
        margin = list(t = 50)
      ) %>%
      config(displayModeBar = F)

    # add annotation color bands
    if (annotation != "None") {
      if (annotation == "btrout") {
        temps <- c(32, 52, 61, 72, 100) # F
        if (units == "C") temps <- f_to_c(temps)
        colors <- c("cornflowerblue", "green", "lightgreen", "darkorange")
      } else if (annotation == "wtemp") {
        temps <- c(32, 72, 77, 100) # F
        if (units == "C") temps <- f_to_c(temps)
        colors <- c("blue", "cornflowerblue", "darkorange")
      }

      plt <- plt %>%
        layout(
          shapes = lapply(1:length(colors), function(i) {
            rect(temps[i], temps[i + 1], colors[i])
          })
        )
    }

    plt
  })


  ## Plot caption ----

  output$therm_plot_caption <- renderUI({
    req(input$therm_temp_units)
    req(input$therm_plot_annotations)

    units <- input$therm_temp_units
    unit_text <- paste0("°", units)
    annotation <- input$therm_plot_annotations

    overlay_caption <- ""

    if (annotation == "btrout") {
      temps <- c(52, 61, 72)
      if (units == "C") temps <- f_to_c(temps)

      overlay_caption <- paste0(
        "Optimal brook trout temperatures are shown shaded ", colorize("dark green", "darkgreen"),
        " (", temps[1], "-", temps[2], unit_text, "), acceptable temperatures in ", colorize("light green", "darkseagreen"),
        " (", temps[2], "-", temps[3], unit_text, "), too hot in ", colorize("orange", "orange"),
        " and too cold in ", colorize("blue", "blue"), ".")

    } else if (annotation == "wtemp") {
      temps <- c(72, 77)
      if (units == "C") temps <- f_to_c(temps)

      overlay_caption <- paste0(
        "The DNR classifies streams as ", colorize("coldwater", "blue"), " when maximum summer temperatures are below ",
        temps[1], unit_text, ", as ", colorize("coolwater", "deepskyblue"), " streams when maximum temperatures are between ",
        temps[1], " and ", temps[2], unit_text, ", and as ", colorize("warmwater", "orange"),
        " streams when maximum temperatures are above ", temps[2], unit_text, ".")
    }

    p(
      style = "margin-left: 2em; margin-right: 2em; font-size: smaller;",
      em(HTML(paste(overlay_caption, "High or widely fluctuating temperatures may indicate that the logger became exposed to the air, either before/after deployment, or when stream levels dropped below the point where the logger was anchored.")))
    )
  })


  ## Data table ----

  output$therm_data <- renderUI({
    req(nrow(selected_therm_data()) > 0)

    min_date <- min(selected_therm_data()$date)
    max_date <- max(selected_therm_data()$date)

    bsCollapse(
      bsCollapsePanel(
        title = "View or download stream temperature data",
        p(
          strong("Station ID:"), cur_stn()$station_id, br(),
          strong("Station Name:"), cur_stn()$station_name, br(),
          strong("Waterbody:"), cur_stn()$waterbody, br(),
          strong("Date range:"),
          paste0(
            format(min_date, "%B %d"), " - ",
            format(max_date, "%B %d, %Y"),
            " (", max_date - min_date, " days)"
          )
        ),
        tabsetPanel(
          tabPanel(
            title = "Daily temperature data",
            class = "data-tab",
            p(downloadButton("therm_daily_dl", "Download this data")),
            renderDataTable({ clean_names(therm_daily(), case = "big_camel") })
          ),
          tabPanel(
            title = "Hourly temperature data",
            class = "data-tab",
            p(downloadButton("therm_hourly_dl", "Download this data")),
            renderDataTable({ clean_names(selected_therm_data(), case = "big_camel") })
          )
        )
      )
    )
  })

  output$therm_daily_dl <- downloadHandler(
    paste0("stn-", cur_stn()$station_id, "-therm-daily-data-", input$therm_year, ".csv"),
    function(file) {write_csv(therm_daily(), file)}
  )

  output$therm_hourly_dl <- downloadHandler(
    paste0("stn-", cur_stn()$station_id, "-therm-hourly-data-", input$therm_year, ".csv"),
    function(file) {write_csv(selected_therm_data(), file)}
  )

}
