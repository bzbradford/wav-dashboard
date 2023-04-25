## MAP ##

# UI ----

#' requires globals:
#' - station_types
#' - stn_colors
#' - data_years
#' - all_pts
#' - all_labels

mapUI <- function() {
  ns <- NS("map")

  tagList(
    div(
      style = "margin: 0.5em 1em;", align = "center",
      p(em(HTML(paste0("Baseline stations are shown in ", colorize(stn_colors$baseline), ", thermistor stations in ", colorize(stn_colors$thermistor), ", and nutrient stations in ", colorize(stn_colors$nutrient), ". Currently selected station is shown in ", colorize("blue", stn_colors$current), ". Click on any station to select it, or choose from the list below the map."))))
    ),

    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          inputId = "stn_types",
          label = "Station data types:",
          choices = station_types,
          selected = station_types
        ),
        p(strong("Stations with data from:")),
        fluidRow(
          column(
            width = 5,
            checkboxGroupInput(
              inputId = "stn_years",
              label = NULL,
              choices = data_years,
              selected = data_years[1]
            )
          ),
          column(
            width = 7,
            radioButtons(
              inputId = "year_exact_match",
              label = NULL,
              choices = list(
                "ANY selected year" = FALSE,
                "ALL selected years" = TRUE
              )
            )
          )
        ),
        hr(),
        uiOutput(ns("totalStationsText")),
        hr(),
        div(
          style = "line-height: 3.5em;",
          align = "center",
          actionButton(ns("zoomInBtn"), "Zoom to selected site", width = "100%"), br(),
          actionButton(ns("zoomAllBtn"), "Zoom out to all sites", width = "100%")
        )
      ),
      mainPanel(
        div(
          class = "map-container",
          leafletOutput(ns("map"), width = "100%", height = "700px")
        )
      ),
      position = "right"
    ),
  )
}


# Server ----

#' @param cur_stn a `reactive()` expression containing the currently selected station
#' @param avail_stns a `reactive()` expression containing the list of available stations

mapServer <- function(cur_stn, avail_stns) {
  moduleServer(
    id = "map",
    function(input, output, session) {
      ns <- session$ns

      ## Reactives ----

      any_stns <- reactive({
        nrow(avail_stns()) > 0
      })

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


      ## Map point size ----

      pt_size <- reactiveVal(4)

      getPtSize <- function(z) {
        if (is.null(z)) return(4)
        if (z >= 14) return(8)
        if (z >= 12) return(6)
        if (z >= 10) return(5)
        4
      }

      observe({
        z <- input$map_zoom
        if (is.null(z)) return()
        new_size <- getPtSize(z)
        if (pt_size() != new_size) pt_size(new_size)
      })


      ## Observers ----

      # center map when station changes
      observe({
        zoom <- input$map_zoom
        if (is.null(zoom)) return()

        if (zoom > 7) {
          leafletProxy(ns("map")) %>%
            setView(
              lat = cur_stn()$latitude,
              lng = cur_stn()$longitude,
              zoom = zoom
            )
        }
      }) %>% bindEvent(cur_stn())


      ## Station text output ----

      output$totalStationsText <- renderUI({
        div(
          style = "text-align: center; font-weight: bold; padding: 5px; border: 2px solid grey; border-radius: 5px; width: 100%;",
          paste("Showing", nrow(avail_pts()), "out of", nrow(all_pts), "total stations")
        )
      })


      ## Render initial map ----

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


      ## Render additional map layers after a delay ----

      observe({
        map <- leafletProxy(ns("map"))
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
      }) %>% bindEvent(T, once = T)


      ## Render map points ----

      drawPts <- function() {
        pts <- avail_pts()
        pt_size <- pt_size()
        labels <- all_labels[names(all_labels) %in% pts$station_id] %>% setNames(NULL)
        popups <- all_popups[names(all_popups) %in% pts$station_id] %>% setNames(NULL)

        baseline <- avail_baseline_pts()
        therm <- avail_therm_pts()
        nutrient <- avail_nutrient_pts()

        leafletProxy(ns("map")) %>%
          clearGroup(layers$baseline) %>%
          addCircleMarkers(
            data = baseline,
            group = layers$baseline,
            label = setNames(all_labels[names(all_labels) %in% baseline$station_id], NULL),
            layerId = ~station_id,
            radius = pt_size,
            color = "black",
            weight = 0.5,
            fillColor = stn_colors$baseline,
            fillOpacity = 0.75,
            options = markerOptions(pane = "baseline", sticky = F)
          ) %>%
          clearGroup(layers$therm) %>%
          addCircleMarkers(
            data = therm,
            group = layers$therm,
            label = setNames(all_labels[names(all_labels) %in% therm$station_id], NULL),
            layerId = ~station_id,
            radius = pt_size,
            color = "black",
            weight = 0.5,
            fillColor = stn_colors$thermistor,
            fillOpacity = 0.75,
            options = markerOptions(pane = "thermistor", sticky = F)
          ) %>%
          clearGroup(layers$nutrient) %>%
          addCircleMarkers(
            data = nutrient,
            group = layers$nutrient,
            label = setNames(all_labels[names(all_labels) %in% nutrient$station_id], NULL),
            layerId = ~station_id,
            radius = pt_size,
            color = "black",
            weight = 0.5,
            fillColor = stn_colors$nutrient,
            fillOpacity = 0.75,
            options = markerOptions(pane = "nutrient", sticky = F)
          ) %>%
          clearGroup(layers$pins) %>%
          addMarkers(
            data = pts,
            group = layers$pins,
            label = labels,
            popup = popups,
            layerId = ~station_id,
            clusterOptions = markerClusterOptions()
          )
      }

      observe({
        if (nrow(avail_pts()) > 0) {
          drawPts()
        } else {
          leafletProxy(ns("map")) %>%
            clearGroup(layers$baseline) %>%
            clearGroup(layers$therm) %>%
            clearGroup(layers$nutrient) %>%
            clearGroup(layers$pins)
            clearGroup("cur_point")
        }
      }) %>% bindEvent(list(avail_pts(), pt_size()))


      ## Show current station ----

      observe({
        leafletProxy(ns("map")) %>%
          clearGroup("cur_point")

        if (!any_stns()) return()

        label <- all_labels[names(all_labels) == cur_stn()$station_id] %>% setNames(NULL)
        popup <- all_popups[names(all_popups) == cur_stn()$station_id] %>% setNames(NULL)

        leafletProxy(ns("map")) %>%
          addCircleMarkers(
            data = cur_stn(),
            lat = ~latitude,
            lng = ~longitude,
            label = label,
            popup = popup,
            layerId = ~station_id,
            group = "cur_point",
            options = pathOptions(pane = "cur_point"),
            radius = pt_size() + 1,
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
      }) %>%
        bindEvent(list(avail_stns(), cur_stn(), pt_size()))


      ## Map action buttons ----

      ### Zoom in button ----

      observeEvent(input$zoomInBtn, {
        leafletProxy(ns("map")) %>%
          setView(
            lat = cur_stn()$latitude,
            lng = cur_stn()$longitude,
            zoom = 10
          )
      })


      ### Reset zoom button ----

      observeEvent(input$zoomAllBtn, {
        if (any_stns()) {
          leafletProxy(ns("map")) %>%
            fitBounds(
              lat1 = min(avail_pts()$latitude),
              lat2 = max(avail_pts()$latitude),
              lng1 = min(avail_pts()$longitude),
              lng2 = max(avail_pts()$longitude)
            )
        } else {
          leafletProxy(ns("map")) %>%
            fitBounds(
              lat1 = min(all_pts$latitude),
              lat2 = max(all_pts$latitude),
              lng1 = min(all_pts$longitude),
              lng2 = max(all_pts$longitude)
            )
        }
      })


      ## Return values ----

      # return the clicked point to the main session
      return(reactive(input$map_marker_click))
    }
  )
}
