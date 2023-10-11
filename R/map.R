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
      p(em(HTML(paste0("Baseline stations are shown in ", colorize(stn_colors$baseline), ", total phosphorus monitoring stations in ", colorize(stn_colors$nutrient), ", and temperature logging stations in ", colorize(stn_colors$thermistor), ". Currently selected station is shown in ", colorize("blue", stn_colors$current), ". Click on any station to select it, or choose from the list below the map."))))
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
              selected = head(data_years, 4)
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
          id = "map",
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
        if (z >= 12) return(7)
        if (z >= 10) return(6)
        4
      }

      observe({
        z <- input$map_zoom
        if (is.null(z)) return()
        new_size <- getPtSize(z)
        if (pt_size() != new_size) pt_size(new_size)
      })


      ## Observers ----

      # center map when station changes but only if zoomed in
      observeEvent(cur_stn(), {
        req(input$map_zoom)
        z <- input$map_zoom
        if (z >= 8) zoomToSite()
      })


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
          addMapPane("cur_huc", 420) %>%
          addMapPane("huc8", 421) %>%
          addMapPane("huc10", 422) %>%
          addMapPane("huc12", 423) %>%
          addMapPane("nkes", 424) %>%
          addMapPane("baseline", 430) %>%
          addMapPane("thermistor", 431) %>%
          addMapPane("nutrient", 432) %>%
          addMapPane("cur_point", 440) %>%
          addMapPane("pins", 450) %>%
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


      ## Render watersheds ----

      observeEvent(T, {
        map <- leafletProxy(ns("map"))
        color <- "blue"
        fill_color <- "lightblue"

        # Nine Key Elements
        delay(500, {
          map %>%
            addPolygons(
              data = nkes,
              group = layers$nkes,
              label = ~ lapply(Label, HTML),
              popup = ~ lapply(Label, HTML),
              weight = 1,
              color = "blue",
              fillColor = "blue",
              fillOpacity = 0.1,
              options = pathOptions(pane = "nkes"),
              labelOptions = labelOptions(style = list("width" = "300px", "white-space" = "normal"))
            )
        })

        # HUC8
        delay(500, {
          map %>%
            addPolygons(
              data = huc8,
              group = layers$huc8,
              label = ~ lapply(Label, HTML),
              popup = ~ lapply(Label, HTML),
              weight = 1.5,
              color = color,
              fillColor = fill_color,
              fillOpacity = 0.15,
              options = pathOptions(pane = "huc8")
            )
        })

        # HUC10
        delay(500, {
          map %>%
            addPolygons(
              data = huc10,
              group = layers$huc10,
              label = ~ lapply(Label, HTML),
              popup = ~ lapply(Label, HTML),
              weight = 1,
              color = color,
              fillColor = fill_color,
              fillOpacity = 0.1,
              options = pathOptions(pane = "huc10")
            )
        })


        # HUC12
        delay(500, {
          map %>%
            addPolygons(
              data = huc12,
              group = layers$huc12,
              label = ~ lapply(Label, HTML),
              popup = ~ lapply(Label, HTML),
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

      drawPts <- function() {
        map <- leafletProxy(ns("map"))
        pts <- avail_pts()
        pt_size <- pt_size()
        labels <- all_labels[names(all_labels) %in% pts$station_id] %>% setNames(NULL)
        popups <- all_popups[names(all_popups) %in% pts$station_id] %>% setNames(NULL)

        # pins
        map %>% clearGroup(layers$pins)
        if (nrow(pts) > 0) {
          map %>%
            addMarkers(
              data = pts,
              group = layers$pins,
              label = labels,
              popup = popups,
              layerId = ~station_id,
              clusterOptions = markerClusterOptions()
            )
        }

        # baseline points
        baseline <- avail_baseline_pts()
        map %>% clearGroup(layers$baseline)
        if (nrow(baseline) > 0) {
          map %>%
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
            )
        }

        # nutrient points
        nutrient <- avail_nutrient_pts()
        map %>% clearGroup(layers$nutrient)
        if (nrow(nutrient) > 0) {
          map %>%
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
            )
        }

        # thermistor points
        therm <- avail_therm_pts()
        map %>% clearGroup(layers$therm)
        if (nrow(therm) > 0) {
          map %>%
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
            )
        }
      }

      observe({
        if (nrow(avail_pts()) > 0) {
          drawPts()
        } else {
          leafletProxy(ns("map")) %>%
            clearGroup(layers$baseline) %>%
            clearGroup(layers$therm) %>%
            clearGroup(layers$nutrient) %>%
            clearGroup(layers$pins) %>%
            clearGroup("cur_point")
        }
      }) %>% bindEvent(list(avail_pts(), pt_size()))


      ## Show current station ----

      observe({
        map <- leafletProxy(ns("map"))
        map %>% clearGroup("cur_point")

        if (!any_stns()) return()

        label <- all_labels[names(all_labels) == cur_stn()$station_id] %>% setNames(NULL)
        popup <- all_popups[names(all_popups) == cur_stn()$station_id] %>% setNames(NULL)

        map %>%
          addCircleMarkers(
            data = cur_stn(),
            lat = ~latitude,
            lng = ~longitude,
            label = label,
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

      zoomToSite <- function() {
        cur_zoom <- input$map_zoom
        leafletProxy(ns("map")) %>%
          setView(
            lat = cur_stn()$latitude,
            lng = cur_stn()$longitude,
            zoom = max(cur_zoom, 10)
          )
      }

      observeEvent(input$zoomInBtn, {
        zoomToSite()
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


      ## Toggle watershed visibility ----

      observeEvent(input$showWatersheds, {
        leafletProxy(ns("map")) %>%
          showGroup(layers$huc8) %>%
          showGroup(layers$huc10) %>%
          showGroup(layers$huc12)
        zoomToSite()
      })


      ## Render selected watersheds ----

      cur_huc12 <- reactive({
        huc12 %>% filter(Huc12Code == cur_stn()$huc12)
      })

      cur_huc10 <- reactive({
        huc10 %>% filter(Huc10Code == cur_stn()$huc10)
      })

      cur_huc8 <- reactive({
        huc8 %>% filter(Huc8Code == cur_stn()$huc8)
      })

      observeEvent(cur_stn(), {
        leafletProxy(ns("map")) %>%
          removeShape("curHuc12") %>%
          addPolygons(
            data = cur_huc12(),
            group = layers$huc12,
            layerId = "curHuc12",
            weight = 2,
            color = "red",
            fillOpacity = 0,
            options = pathOptions(pane = "cur_huc")
          )
      })

      observeEvent(cur_stn(), {
        leafletProxy(ns("map")) %>%
          removeShape("curHuc10") %>%
          addPolygons(
            data = cur_huc10(),
            group = layers$huc10,
            layerId = "curHuc10",
            weight = 3,
            color = "red",
            fillOpacity = 0,
            options = pathOptions(pane = "cur_huc")
          )
      })

      observeEvent(cur_stn(), {
        leafletProxy(ns("map")) %>%
          removeShape("curHuc8") %>%
          addPolygons(
            data = cur_huc8(),
            group = layers$huc8,
            layerId = "curHuc8",
            weight = 4,
            color = "red",
            fillOpacity = 0,
            options = pathOptions(pane = "cur_huc")
          )
      })


      ## Return values ----

      # return the clicked point to the main session
      return(reactive(input$map_marker_click))
    }
  )
}
