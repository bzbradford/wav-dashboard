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
      p(em(HTML(paste0("Baseline stations are shown in ", colorize(stn_colors$baseline), ", total phosphorus monitoring stations in ", colorize(stn_colors$nutrient), ", and temperature logging stations in ", colorize(stn_colors$thermistor), " (stations may have more than one type of data). Currently selected station is shown in ", colorize("blue", stn_colors$current), ". Click on any station to select it, or choose from the list below the map."))))
    ),

    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          inputId = ns("stn_types"),
          label = "Station data types:",
          choices = station_types,
          selected = station_types
        ),
        p(strong("Stations with data from:")),
        fluidRow(
          column(
            width = 5,
            checkboxGroupInput(
              inputId = ns("stn_years"),
              label = NULL,
              choices = stn_year_choices,
              selected = stn_year_choices[1:4]
            ),
            style = "white-space: nowrap;"
          ),
          column(
            width = 7,
            radioButtons(
              inputId = ns("year_exact_match"),
              label = NULL,
              choices = list(
                "ANY selected year" = FALSE,
                "ALL selected years" = TRUE
              )
            )
          )
        ),
        radioButtons(
          inputId = ns("stn_color_by"),
          label = "Color station markers by:",
          choices = stn_color_choices
        ),
        div(
          class = "total-stns-text",
          textOutput(ns("totalStnsText")),
        ),
        div(
          style = "line-height: 3.5em;",
          align = "center",
          actionButton(ns("zoom_in_btn"), "Zoom to selected site", width = "100%"), br(),
          actionButton(ns("zoom_all_btn"), "Zoom out to all sites", width = "100%"),
        )
      ),
      mainPanel(
        div(
          class = "map-container",
          id = "map",
          leafletOutput(ns("map"), width = "100%", height = "720px") %>% withSpinnerProxy(hide.ui = TRUE)
        )
      ),
      position = "right"
    )
  )
}


# Server ----

#' @param cur_stn a `reactive()` expression containing the currently selected station
#' @param main_session main server session

mapServer <- function(cur_stn, main_session) {
  moduleServer(
    id = "map",
    function(input, output, session) {
      ns <- session$ns

      # Static vars ----

      ## stn_coverages ----
      stn_coverages <- list(
        "baseline" = baseline_coverage,
        "nutrient" = nutrient_coverage,
        "thermistor" = therm_coverage
      )


      # Reactive vals ----

      ## user_loc_shown ----
      user_loc_shown <- reactiveVal(FALSE)

      ## avail_stns ----
      avail_stns <- reactive({
        if (is.null(input$stn_types) | is.null(input$stn_years)) {
          return(head(all_stn_years, 0))
        }

        # expand list of years if last option is selected
        match_years <- input$stn_years
        if (last(stn_year_choices) %in% match_years) {
          match_years <- union(match_years, last(match_years):last(data_years))
        }

        # handle any/all matching types
        if (input$year_exact_match) {
          ids <- sapply(input$stn_types, function(stn_type) {
            stn_coverages[[stn_type]] %>%
              filter(all(match_years %in% data_year_list)) %>%
              pull(station_id)
          })
        } else {
          ids <- sapply(input$stn_types, function(stn_type) {
            stn_coverages[[stn_type]] %>%
              filter(any(match_years %in% data_year_list)) %>%
              pull(station_id)
          })
        }

        avail_ids <- sort(reduce(ids, union))
        filter(all_stn_years, station_id %in% avail_ids)
      })

      ## any_stns ----
      any_stns <- reactive({
        nrow(avail_stns()) > 0
      })

      ## avail_pts ----
      avail_pts <- reactive({
        filter(all_pts, station_id %in% avail_stns()$station_id)
      })

      ## avail_baseline_pts ----
      avail_baseline_pts <- reactive({
        all_pts %>%
          filter(baseline_stn, station_id %in% avail_stns()$station_id)
      })

      ## avail_nutrient_pts ----
      avail_nutrient_pts <- reactive({
        all_pts %>%
          filter(nutrient_stn, station_id %in% avail_stns()$station_id)
      })

      ## avail_therm_pts ----
      avail_therm_pts <- reactive({
        all_pts %>%
          filter(therm_stn, station_id %in% avail_stns()$station_id)
      })

      ## pt_size => Map point size ----
      pt_size <- reactiveVal(4)

      getPtSize <- function(z) {
        if (is.null(z)) return(4)
        if (z >= 14) return(8)
        if (z >= 10) return(6)
        4
      }

      observe({
        z <- input$map_zoom
        if (is.null(z)) return()
        new_size <- getPtSize(z)
        if (pt_size() != new_size) pt_size(new_size)
      })


      # Event handlers ----
      # Select a station when clicked on the map ----
      observeEvent(input$map_marker_click, {
        stn <- input$map_marker_click
        req(stn)
        req(stn$group != "cur_point")

        updateSelectInput(
          session = main_session,
          inputId = "station",
          selected = stn$id
        )

        cur_zoom <- input$map_zoom
        req(cur_zoom)
        leafletProxy("map") %>%
          setView(
            lat = stn$lat,
            lng = stn$lng,
            zoom = max(cur_zoom, 10) # don't zoom out
          )
      })


      # Rendered UI elements ----

      ## totalStnsText ----
      output$totalStnsText <- renderText({
        paste("Showing", nrow(avail_pts()), "out of", nrow(all_pts), "total stations")
      })


      # Map setup ----

      basemaps <- tribble(
        ~label, ~provider,
        "ESRI Topo", providers$Esri.WorldTopoMap,
        "Satellite", providers$Esri.WorldImagery,
        "OpenStreetMap", providers$OpenStreetMap,
        "Grey Canvas", providers$CartoDB.Positron
      )

      addBasemaps <- function(map) {
        for (r in 1:nrow(basemaps)) {
          df <- slice(basemaps, r)
          map <- addProviderTiles(map, df$provider, group = df$label)
        }
        map
      }

      layers <- list(
        counties = "Counties/Regions",
        nkes = paste0("NKE Plans"),
        huc8 = paste0("HUC8 Subbasins"),
        huc10 = paste0("HUC10 Watersheds"),
        huc12 = paste0("HUC12 Subwatersheds"),
        baseline = paste0("Baseline stations (", colorize(stn_colors$baseline), ")"),
        nutrient = paste0("Nutrient stations (", colorize(stn_colors$nutrient), ")"),
        thermistor = paste0("Thermistor stations (", colorize(stn_colors$thermistor), ")"),
        pins = "Station clusters (groups and pins)"
      )

      hidden_layers <- c(layers$nkes, layers$huc8, layers$huc10, layers$huc12, layers$pins)


      # Render initial map ----

      output$map <- renderLeaflet({
        leaflet() %>%
          fitBounds(
            lat1 = 42.4,
            lat2 = 47.1,
            lng1 = -92.9,
            lng2 = -86.8
          ) %>%
          addBasemaps() %>%
          addMapPane("counties", 410) %>%
          addMapPane("cur_huc", 420) %>%
          addMapPane("huc8", 421) %>%
          addMapPane("huc10", 422) %>%
          addMapPane("huc12", 423) %>%
          addMapPane("nkes", 424) %>%
          addMapPane("baseline", 430) %>%
          addMapPane("thermistor", 431) %>%
          addMapPane("nutrient", 432) %>%
          addMapPane("cur_point_circle", 440) %>%
          addMapPane("pins", 450) %>%
          hideGroup(hidden_layers) %>%
          addLayersControl(
            baseGroups = basemaps$label,
            overlayGroups = unlist(layers, use.names = FALSE),
            options = layersControlOptions(collapsed = FALSE)
          ) %>%
          addFullscreenControl(pseudoFullscreen = TRUE) %>%
          addEasyButtonBar(
            easyButton(
              position = "topleft",
              icon = "fa-crosshairs",
              title = "Show my location on the map",
              onClick = JS("
                function(btn, map) {
                  map.locate({
                    setView: true,
                    enableHighAccuracy: false,
                    maxZoom: 12
                  }).on('locationfound', (event) => {
                    console.log(event)
                    Shiny.setInputValue('map-user_loc', event.latlng, {priority: 'event'})
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


      # Reactive map elements ----

      addStnPts <- function(stn_type, stn_types, stns, pt_size, color_by) {

        map <- leafletProxy("map")
        if (is.null(stn_types) | !(stn_type %in% stn_types) | nrow(stns) == 0) {
          map %>% clearGroup(layers[[stn_type]])
          return()
        }

        if (color_by == "stn_type") {
          stns <- mutate(stns, color = stn_colors[[stn_type]])
        } else {
          opts <- filter(stn_color_opts, value == color_by)
          domain <- unlist(opts$domain)
          pal <- colorNumeric(opts$pal, domain, reverse = opts$rev)
          stns <- stns %>%
            left_join(stn_attr_totals, join_by(station_id)) %>%
            rename(attr = all_of(color_by)) %>%
            mutate(attr = pmax(pmin(attr, domain[2]), domain[1])) %>%
            mutate(color = pal(attr)) %>%
            arrange(!is.na(attr), attr) %>%
            mutate(label = paste0(label, "<br>", opts$label, ": ", attr)) %>%
            mutate(label = lapply(label, shiny::HTML))
        }

        map %>%
          clearGroup(layers[[stn_type]]) %>%
          addCircleMarkers(
            data = stns,
            group = layers[[stn_type]],
            label = ~label,
            layerId = ~station_id,
            radius = pt_size,
            color = "black",
            weight = 0.5,
            fillColor = ~color,
            fillOpacity = 1,
            options = markerOptions(pane = stn_type, sticky = F)
          )
      }

      ## Baseline circle markers ----
      observe({
        addStnPts(
          stn_type = "baseline",
          stn_types = input$stn_types,
          stns = avail_baseline_pts(),
          pt_size = pt_size(),
          color_by = input$stn_color_by
        )
      })

      ## Nutrient circle markers ----
      observe({
        addStnPts(
          stn_type = "nutrient",
          stn_types = input$stn_types,
          stns = avail_nutrient_pts(),
          pt_size = pt_size(),
          color_by = input$stn_color_by
        )
      })

      ## Thermistor circle markers ----
      observe({
        addStnPts(
          stn_type = "thermistor",
          stn_types = input$stn_types,
          stns = avail_therm_pts(),
          pt_size = pt_size(),
          color_by = input$stn_color_by
        )
      })

      ## Map markers/clusters ----
      # update map pins/clusters on available stations change
      observe({
        map <- leafletProxy(ns("map"))
        map %>% clearGroup(layers$pins)

        pts <- avail_pts()
        if (nrow(pts) == 0) return()

        popups <- all_popups[names(all_popups) %in% pts$station_id] %>% setNames(NULL)
        map %>%
          addMarkers(
            data = pts,
            group = layers$pins,
            label = ~label,
            popup = popups,
            layerId = ~station_id,
            clusterOptions = markerClusterOptions()
          )
      })

      ## Show current station ----
      observe({
        map <- leafletProxy(ns("map"))
        map %>% clearGroup("cur_point")

        if (!any_stns()) return()
        stn <- cur_stn()
        popup <- all_popups[names(all_popups) == stn$station_id] %>% setNames(NULL)

        map %>%
          addCircleMarkers(
            data = stn,
            lat = ~latitude,
            lng = ~longitude,
            label = ~label,
            layerId = ~station_id,
            group = "cur_point",
            options = pathOptions(pane = "cur_point_circle"),
            radius = pt_size() + 1,
            weight = 3,
            color = stn_colors$current,
            opacity = 1,
            fillColor = "none"
          ) %>%
          addMarkers(
            data = stn,
            lat = ~latitude,
            lng = ~longitude,
            label = ~label,
            popup = popup,
            layerId = ~station_id,
            group = "cur_point"
          )
      }) %>% bindEvent(list(cur_stn(), pt_size()))

      ## Current station's watersheds ----
      ### HUC8 ----
      cur_huc8 <- reactive({ filter(huc8, Huc8Code == cur_stn()$huc8) })
      observe({
        leafletProxy(ns("map")) %>%
          removeShape("curHuc8") %>%
          addPolygons(
            data = cur_huc8(),
            group = layers$huc8,
            layerId = "curHuc8",
            weight = 4,
            fillOpacity = 0.1,
            options = pathOptions(pane = "cur_huc")
          )
      })

      ### HUC10 ----
      cur_huc10 <- reactive({ filter(huc10, Huc10Code == cur_stn()$huc10) })
      observe({
        leafletProxy(ns("map")) %>%
          removeShape("curHuc10") %>%
          addPolygons(
            data = cur_huc10(),
            group = layers$huc10,
            layerId = "curHuc10",
            weight = 3,
            fillOpacity = 0.1,
            options = pathOptions(pane = "cur_huc")
          )
      })

      ### HUC12 ----
      cur_huc12 <- reactive({ filter(huc12, Huc12Code == cur_stn()$huc12) })
      observe({
        leafletProxy(ns("map")) %>%
          removeShape("curHuc12") %>%
          addPolygons(
            data = cur_huc12(),
            group = layers$huc12,
            layerId = "curHuc12",
            weight = 2,
            fillOpacity = 0.1,
            options = pathOptions(pane = "cur_huc")
          )
      })


      # Delayed map actions ----

      ## Load watersheds ----
      observeEvent(TRUE, {
        map <- leafletProxy(ns("map"))
        color <- "blue"
        fill_color <- "lightblue"

        # Nine Key Elements
        delay(1000, {
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
        delay(1100, {
          map %>%
            addPolygons(
              data = huc8,
              group = layers$huc8,
              label = ~ lapply(Label, HTML),
              popup = ~ lapply(Label, HTML),
              weight = 1.5,
              color = color,
              opacity = .25,
              fillColor = fill_color,
              fillOpacity = 0.15,
              options = pathOptions(pane = "huc8")
            )
        })

        # HUC10
        delay(1200, {
          map %>%
            addPolygons(
              data = huc10,
              group = layers$huc10,
              label = ~ lapply(Label, HTML),
              popup = ~ lapply(Label, HTML),
              weight = 1,
              color = color,
              opacity = .25,
              fillColor = fill_color,
              fillOpacity = .1,
              options = pathOptions(pane = "huc10")
            )
        })


        # HUC12
        delay(1300, {
          map %>%
            addPolygons(
              data = huc12,
              group = layers$huc12,
              label = ~ lapply(Label, HTML),
              popup = ~ lapply(Label, HTML),
              weight = 0.5,
              color = color,
              opacity = .25,
              fillColor = fill_color,
              fillOpacity = 0.05,
              options = pathOptions(pane = "huc12")
            )
        })
      })

      ## Hide the legend ----
      observeEvent(TRUE, {
        delay(3000, {
          leafletProxy("map") %>%
            addLayersControl(
              baseGroups = basemaps$label,
              overlayGroups = unlist(layers, use.names = FALSE),
            )
        })
      })


      # Event reactives ----

      ## user_loc ----
      # shows a house icon and displays watershed info for the user's location
      observeEvent(input$user_loc, {
        if (user_loc_shown()) return()
        map <- leafletProxy("map")
        loc <- input$user_loc %>% lapply(\(x) round(x, 4))
        pt <- tibble(lat = loc$lat, lng = loc$lng) %>%
          st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = F)
        watershed <- suppressWarnings(st_intersection(huc12, pt))
        user_huc12 <- filter(huc12, Huc12Name == watershed$Huc12Name)
        user_huc10 <- filter(huc10, Huc10Name == watershed$Huc10Name)
        user_huc8 <- filter(huc8, Huc8Name == watershed$Huc8Name)
        label <- glue::glue("
          <b>My location ({pt$lat}, {pt$lng})</b><br>
          <i>Click for more information on your watershed</i>
        ")
        popup <- glue::glue("
          <b>My location ({pt$lat}, {pt$lng})</b><br>
          <i>Your watershed:</i><br>
          <div style='padding-left: 1em;'>
            Major basin: {watershed$MajorBasin}<br>
            HUC8 Subbasin: {watershed$Huc8Name}<br>
            HUC10 Watershed: {watershed$Huc10Name}<br>
            HUC12 Subwatershed: {watershed$Huc12Name}
          </div>
        ")
        popup <- paste0(popup, "<br>
          <a style='cursor:pointer;' onclick=\"Shiny.setInputValue('map-toggle_watersheds', false, {priority: 'event'})\">Toggle watersheds on map</a> |
          <a style='cursor:pointer;' onclick=\"Shiny.setInputValue('map-remove_user_loc', true, {priority: 'event'})\">Hide my location</a>
        ")
        icon <- makeAwesomeIcon(icon = "home", markerColor = "#c5050c", library = "ion")
        map %>%
          addAwesomeMarkers(
            data = pt,
            label = HTML(label),
            popup = HTML(popup),
            icon = icon,
            group = "user_loc"
          ) %>%
          addPolygons(
            data = user_huc8,
            group = layers$huc8,
            layerId = "user_watershed8",
            weight = 2,
            color = "#c5050c",
            fillOpacity = 0.1,
            options = pathOptions(pane = "cur_huc")
          ) %>%
          addPolygons(
            data = user_huc10,
            group = layers$huc10,
            layerId = "user_watershed10",
            weight = 2,
            color = "#c5050c",
            fillOpacity = 0.1,
            options = pathOptions(pane = "cur_huc")
          ) %>%
          addPolygons(
            data = user_huc12,
            group = layers$huc12,
            layerId = "user_watershed12",
            weight = 2,
            color = "#c5050c",
            fillOpacity = 0.1,
            options = pathOptions(pane = "cur_huc")
          )
        user_loc_shown(TRUE)
      })

      ## remove_user_loc ----
      observeEvent(input$remove_user_loc, {
        map <- leafletProxy("map")
        map %>%
          clearGroup("user_loc") %>%
          removeShape("user_watershed8") %>%
          removeShape("user_watershed10") %>%
          removeShape("user_watershed12")
        user_loc_shown(FALSE)
      })

      ## Zoom events ----
      zoomToSite <- function(z = input$map_zoom) {
        # cur_zoom <- input$map_zoom
        leafletProxy(ns("map")) %>%
          setView(
            lat = cur_stn()$latitude,
            lng = cur_stn()$longitude,
            zoom = z
          )
      }

      ### cur_stn ----
      # center map when station changes but only if out of view
      observeEvent(cur_stn(), {
        stn <- cur_stn()
        bounds <- input$map_bounds
        in_lat <- between(stn$latitude, bounds$south, bounds$north)
        in_long <- between(stn$longitude, bounds$west, bounds$east)
        in_view <- in_lat & in_long
        if (!in_view) zoomToSite()
      })

      ### zoom_in_btn ----
      observeEvent(input$zoom_in_btn, {
        zoomToSite(10)
      })

      ### zoom_all_btn ----
      observeEvent(input$zoom_all_btn, {
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

      ## show_watersheds ----
      # Toggle watershed visibility
      observeEvent(input$show_watersheds, {
        leafletProxy(ns("map")) %>%
          showGroup(layers$huc8) %>%
          showGroup(layers$huc10) %>%
          showGroup(layers$huc12)
        zoomToSite(9)
      })

      ## toggle_watersheds ----
      observeEvent(input$toggle_watersheds, {
        map <- leafletProxy(ns("map"))
        groups <- input$map_groups
        all_shown <- all(
          layers$huc8 %in% groups,
          layers$huc10 %in% groups,
          layers$huc12 %in% groups
        )
        if (all_shown) {
          map %>%
            hideGroup(layers$huc8) %>%
            hideGroup(layers$huc10) %>%
            hideGroup(layers$huc12)
        } else {
          map %>%
            showGroup(layers$huc8) %>%
            showGroup(layers$huc10) %>%
            showGroup(layers$huc12)
        }
      })

      # Return values ----
      # return the clicked point to the main session
      return(reactive(list(
        avail_stns = avail_stns()
      )))

      # end
    }
  )
}
