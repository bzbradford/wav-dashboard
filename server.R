# server.R

suppressMessages({
  library(tidyverse)
  library(sf)
  library(leaflet)
  library(leaflet.extras)
  library(htmltools)
  library(shiny)
  library(shinyBS)
  library(DT)
  library(shinyjs)
})

server <- function(input, output, session) {


# Station select ----------------------------------------------------------

  random_stn <- all_pts %>%
    filter(max_fw_year == max(data_years)) %>%
    pull(station_id) %>%
    sample(1)

  avail_stns <- reactive({

    if (input$year_exact_match) {
      if (is.null(input$years)) {
        year_stns <- NULL
      } else {
        year_stns <- all_coverage %>%
          rowwise() %>%
          filter(setequal(intersect(input$years, data_year_list), input$years)) %>%
          pull(station_id)
      }
    } else {
      year_stns <- all_stn_years %>%
        filter(year %in% input$years) %>%
        pull(station_id) %>%
        unique()
    }

    if (input$stn_exact_match) {
      types <- str_to_title(paste(input$stn_types, collapse = ", "))
      type_stns <- all_coverage %>%
        filter(data_sources == types) %>%
        pull(station_id)
    } else {
      type_stns <- all_stn_years %>%
        filter(
          (baseline_stn & ("baseline" %in% input$stn_types)) |
            (therm_stn & ("thermistor" %in% input$stn_types)) |
            (nutrient_stn & ("nutrient" %in% input$stn_types))
        ) %>%
        pull(station_id) %>%
        unique()
    }

    all_stn_years %>%
      filter((station_id %in% year_stns) & (station_id %in% type_stns))
  })

  avail_pts <- reactive({
    if (nrow(avail_stns()) > 0) {
      stns <- avail_stns()$station_id
      all_pts %>%
        filter(station_id %in% stns)
    } else {
      tibble()
    }
  })

  stn_list <- reactive({
    if (nrow(avail_stns()) > 0) {
      ids <- avail_stns()$station_id
      all_stn_list[all_stn_list %in% ids]
    } else {
      list("No stations available" = NULL)
    }
  })

  cur_stn <- reactive({
    req(input$station)

    all_pts %>%
      filter(station_id == input$station)
  })

  output$total_stns_ui <- renderUI({
    p(strong("Selected", nrow(avail_pts()), "out of", nrow(all_pts), "stations."), align = "center")
  })

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



# Map ---------------------------------------------------------------------

  basemaps <- list(
    one = "ESRI Topo",
    two = "Grey Canvas",
    three = "OpenStreetMap"
  )

  layers <- list(
    counties = "Counties/Regions",
    nkes = paste0("NKE Plans (", colorize("blue"), ")"),
    huc8 = paste0("HUC8 Subbasins (", colorize("blue"), ")"),
    huc10 = paste0("HUC10 Watersheds (", colorize("blue"), ")"),
    huc12 = paste0("HUC12 Subwatersheds (", colorize("blue"), ")"),
    points = "Station points",
    pins = "Station clusters (groups and pins)"
  )

  hidden_layers <- c(layers$nkes, layers$huc8, layers$huc10, layers$huc12)


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
      addMapPane("points", 430) %>%
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
      )
  })


  ## Render additional map layers ----

  observeEvent(TRUE, {
    map <- leafletProxy("map")
    color <- "blue"
    fill_color <- "lightblue"

    # Counties
    map %>%
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

    # Nine Key Elements
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

    # HUC8
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

    # HUC10
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

    # HUC12
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

  observeEvent(TRUE, {
    # Hide the legend after a delay
    delay(3000, {
      leafletProxy("map") %>%
        addLayersControl(
          baseGroups = unlist(basemaps, use.names = FALSE),
          overlayGroups = unlist(layers, use.names = FALSE),
          options = layersControlOptions(collapsed = TRUE)
        )
    })
  })


  ## Render map points ----

  observeEvent(avail_pts(), {

    if (nrow(avail_pts()) > 0) {
      pts <- avail_pts()
      labels <- all_labels[names(all_labels) %in% pts$station_id] %>% setNames(NULL)
      popups <- all_popups[names(all_popups) %in% pts$station_id] %>% setNames(NULL)

      leafletProxy("map") %>%
        clearGroup(layers$points) %>%
        clearGroup(layers$pins) %>%
        addCircleMarkers(
          data = pts,
          group = layers$points,
          label = labels,
          popup = popups,
          layerId = ~station_id,
          radius = 4,
          color = "black",
          weight = 0.5,
          fillColor = ~stn_color,
          fillOpacity = 0.75,
          options = markerOptions(pane = "points", sticky = F)
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
        clearGroup(layers$points) %>%
        clearGroup(layers$pins)
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

  observeEvent(list(cur_stn(), input$map_collapse), {
    req(cur_stn())

    label <- all_labels[names(all_labels) == cur_stn()$station_id] %>% setNames(NULL)
    popup <- all_popups[names(all_popups) == cur_stn()$station_id] %>% setNames(NULL)

    leafletProxy("map") %>%
      clearGroup("cur_point") %>%
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
        fillColor = "orange",
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

  observeEvent(input$zoom_in, {
    leafletProxy("map") %>%
      setView(
        lat = cur_stn()$latitude,
        lng = cur_stn()$longitude,
        zoom = 10
      )
  })

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

  observeEvent(input$random_site, {
    stn_id <- stn_list()[sample(1:length(stn_list()), 1)]
    stn <- all_pts %>% filter(station_id == stn_id)
    leafletProxy("map") %>%
      setView(
        lat = stn$latitude,
        lng = stn$longitude,
        zoom = 10
      )
    updateSelectInput(
      inputId = "station",
      selected = stn_id
    )
  })




# Baseline data -----------------------------------------------------------

  cur_baseline_data <- reactive({
    baseline_data %>%
      filter(station_id == cur_stn()$station_id)
  })

  cur_baseline_years <- reactive({
    unique(cur_baseline_data()$year)
  })

  output$baseline_tab <- renderUI({
    validate(
      need(
        nrow(cur_baseline_data()) > 0,
        "This station has no baseline data. Choose another station or view the thermistor or nutrient data associated with this station."
      )
    )

    list(
      radioButtons(
        inputId = "baseline_year",
        label = "Choose year:",
        choices = cur_baseline_years(),
        inline = T
      ),
      hr(),
      bsCollapse(
        bsCollapsePanel(
          title = "Baseline data table",
          renderDataTable({
            req(input$baseline_year)

            cur_baseline_data() %>%
              filter(year == input$baseline_year) %>%
              clean_names(case = "title")
          })
        )
      )
    )
  })



# Thermistor data ---------------------------------------------------------

  cur_therm_data <- reactive({
    therm_data %>%
      filter(station_id == cur_stn()$station_id)
  })

  cur_therm_years <- reactive({
    unique(cur_therm_data()$year)
  })

  output$thermistor_tab <- renderUI({
    validate(
      need(
        nrow(cur_therm_data()) > 0,
        "This station has no thermistor data. Choose another station or view the baseline or nutrient data associated with this station."
      )
    )

    list(
      radioButtons(
        inputId = "therm_year",
        label = "Choose year:",
        choices = cur_therm_years(),
        inline = T
      ),
      hr(),
      bsCollapse(
        bsCollapsePanel(
          title = "Thermistor data table",
          renderDataTable({
            req(input$therm_year)

            cur_therm_data() %>%
              filter(year == input$therm_year) %>%
              clean_names(case = "title")
          })
        )
      )
    )
  })



  ## Nutrient data ----

  cur_nutrient_data <- reactive({
    nutrient_data %>%
      filter(station_id == cur_stn()$station_id)
  })

  cur_nutrient_years <- reactive({
    unique(cur_nutrient_data()$year)
  })

  output$nutrient_tab <- renderUI({
    validate(
      need(
        nrow(cur_nutrient_data()) > 0,
        "This station has no nutrient data. Choose another station or view the baseline or thermistor data associated with this station."
      )
    )

    list(
      radioButtons(
        inputId = "nutrient_year",
        label = "Choose year:",
        choices = cur_nutrient_years(),
        inline = T
      ),
      hr(),
      bsCollapse(
        bsCollapsePanel(
          title = "Nutrient data table",
          renderDataTable({
            req(input$nutrient_year)

            cur_nutrient_data() %>%
              filter(year == input$nutrient_year) %>%
              clean_names(case = "title")
          })
        )
      )
    )
  })




  ## Station lists ----

  output$station_lists <- renderUI({
    bsCollapse(
      bsCollapsePanel(
        title = "Baseline monitoring stations",
        div(style = "overflow: auto;", renderDataTable(baseline_stns)),
        downloadButton("baseline_stn_dl")
      ),
      bsCollapsePanel(
        title = "Nutrient monitoring stations",
        div(style = "overflow: auto;", renderDataTable(nutrient_stns)),
        downloadButton("nutrient_stn_dl")
      ),
      bsCollapsePanel(
        title = "Temperature logging stations",
        div(style = "overflow: auto;", renderDataTable(therm_stns)),
        downloadButton("thermistor_stn_dl")
      ),
      bsCollapsePanel(
        title = "All WAV stations",
        div(style = "overflow: auto;", renderDataTable(all_stns)),
        downloadButton("all_stns_dl")
      )
    )
  })

  output$baseline_stn_dl <- downloadHandler(
    filename = "wav-baseline-stations.csv",
    content = function(file) {write_csv(baseline_stns, file)}
  )

  output$nutrient_stn_dl <- downloadHandler(
    filename = "wav-nutrient-stations.csv",
    content = function(file) {write_csv(nutrient_stns, file)}
  )

  output$thermistor_stn_dl <- downloadHandler(
    filename = "wav-temperature-loggers.csv",
    content = function(file) {write_csv(therm_stns, file)}
  )

  output$all_stns_dl <- downloadHandler(
    filename = "wav-station-list.csv",
    content = function(file) {write_csv(all_stns, file)}
  )

}
