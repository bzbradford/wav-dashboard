# server.R

server <- function(input, output, session) {

  # Logger select ----

  random_stn <- all_pts %>%
    filter(max_fw_year == max(data_years)) %>%
    pull(station_id) %>%
    sample(1)



  # Map ----

  # output$mapUI <- renderUI({
  #   div(
  #     style = "max-width: 1000px; margin: auto; border: 1px solid grey;",
  #     leafletOutput("map", width = "100%", height = "800px")
  #   )
  # })

  basemaps <- list(
    one = "ESRI Topo",
    two = "Grey Canvas",
    three = "OpenStreetMap"
  )

  layers <- list(
    counties = "Counties/Regions",
    nkes = "NKE Plans (<span style='color: blue;'>blue</span>)",
    huc8 = "HUC8 Subbasins (<span style='color: blue;'>blue</span>)",
    huc10 = "HUC10 Watersheds (<span style='color: blue;'>blue</span>)",
    huc12 = "HUC12 Subwatersheds (<span style='color: blue;'>blue</span>)",
    points = "Station points (<span style='color: green;'>green</span>)",
    pins = "Station clusters (groups and pins)"
  )

  create_popup <- function(data, title) {
    data %>% {
      cols <- names(.)
      lapply(1:nrow(.), function(r) {
        row <- .[r,]
        details <-
          lapply(1:length(cols), function(c) {
            paste0("<br><b>", cols[c], ":</b> ", row[c])
          }) %>%
          paste0(collapse = "")
        paste0(title, details)
      }) %>% paste0()
    }
  }


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
      hideGroup(c(layers$huc8, layers$huc10, layers$huc12)) %>%
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

    # Points
    map %>%
      addCircleMarkers(
        data = all_pts,
        group = layers$points,
        label = ~lapply(paste0("<b>WAV Monitoring Site</b><br>Station ID: ", station_id, "<br>Name: ", station_name), HTML),
        popup = ~create_popup(baseline_stns, "<b>WAV Monitoring Site</b><br>"),
        radius = 4,
        color = "black",
        weight = 0.5,
        fillColor = "green",
        fillOpacity = 0.75,
        options = markerOptions(pane = "points", sticky = F)
      ) %>%
      addMarkers(
        data = all_pts,
        group = layers$pins,
        label = ~lapply(paste0("<b>WAV Monitoring Site</b><br>Station ID: ", station_id, "<br>Name: ", station_name), HTML),
        popup = ~create_popup(all_stns, "<b>WAV Monitoring Site</b><br>"),
        clusterOptions = markerClusterOptions()
      )

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

    # Hide the legend after a delay
    delay(2000, {
      map %>%
        addLayersControl(
          baseGroups = unlist(basemaps, use.names = FALSE),
          overlayGroups = unlist(layers, use.names = FALSE),
          options = layersControlOptions(collapsed = TRUE)
        )
    })

  })



  # Table outputs ----

  output$stnLists <- renderUI({
    bsCollapse(
      bsCollapsePanel(
        title = "Baseline monitoring stations",
        div(style = "overflow: auto;", renderDataTable(baseline_stns)),
        downloadButton("baselineDL")
      ),
      bsCollapsePanel(
        title = "Nutrient monitoring stations",
        div(style = "overflow: auto;", renderDataTable(nutrient_stns)),
        downloadButton("nutrientDL")
      ),
      bsCollapsePanel(
        title = "Temperature logging stations",
        div(style = "overflow: auto;", renderDataTable(therm_stns)),
        downloadButton("thermistorDL")
      ),
      bsCollapsePanel(
        title = "All WAV stations",
        div(style = "overflow: auto;", renderDataTable(all_stns)),
        downloadButton("allDL")
      )
    )
  })



  # Download handlers ----

  output$baselineDL <- downloadHandler(
    filename = "wav-baseline-stations.csv",
    content = function(file) {write_csv(baseline_stns, file)}
  )

  output$nutrientDL <- downloadHandler(
    filename = "wav-nutrient-stations.csv",
    content = function(file) {write_csv(nutrient_stns, file)}
  )

  output$thermistorDL <- downloadHandler(
    filename = "wav-temperature-loggers.csv",
    content = function(file) {write_csv(thermistor_stns, file)}
  )

  output$allDL <- downloadHandler(
    filename = "wav-station-list.csv",
    content = function(file) {write_csv(all_pts, stns)}
  )

}
