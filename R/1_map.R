## MAP ##

# UI ----

#' requires globals:
#' - station_types
#' - stn_colors
#' - data_years
#' - all_pts
#' - all_labels
#' - shapefiles: nkes, huc8, huc10, huc12

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
                "ANY selected year" = F,
                "ALL selected years" = T
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
          leafletOutput(ns("map"), width = "100%", height = "720px") %>% withSpinnerProxy(hide.ui = T)
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

      rv <- reactiveValues(
        user_loc_shown = FALSE,

        # holds the last selected station in case cur_stn() becomes blocked
        selected_stn = NULL,

        # tracks which map layers have been loaded
        rendered_layers = list()
      )

      observe({
        rv$selected_stn <- cur_stn()
      })

      ## avail_stns ----
      avail_stns <- reactive({
        types <- input$stn_types
        years <- input$stn_years
        match_type <- input$year_exact_match

        if (is.null(types) | is.null(years)) {
          return(head(all_stn_years, 0))
        }

        # expand list of years if last option is selected
        match_years <- years
        if (last(stn_year_choices) %in% match_years) {
          match_years <- union(match_years, last(match_years):last(data_years))
        }

        # handle any/all matching types
        if (match_type) {
          ids <- sapply(types, function(stn_type) {
            stn_coverages[[stn_type]] %>%
              filter(all(match_years %in% data_year_list)) %>%
              pull(station_id)
          })
        } else {
          ids <- sapply(types, function(stn_type) {
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
        stations = "Monitoring stations",
        counties = "Counties/Regions",
        nkes = "NKE Plans",
        huc8 = "HUC8 Subbasins",
        huc10 = "HUC10 Watersheds",
        huc12 = "HUC12 Subwatersheds",
        pins = "Station clusters"
      )

      hidden_layers <- c(layers$nkes, layers$huc8, layers$huc10, layers$huc12, layers$dnr_wsheds, layers$pins)


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
          addMapPane("stations", 430) %>%
          addMapPane("cur_point_circle", 440) %>%
          addMapPane("pins", 450) %>%
          hideGroup(hidden_layers) %>%
          addLayersControl(
            baseGroups = basemaps$label,
            overlayGroups = unlist(layers, use.names = F),
            options = layersControlOptions(collapsed = F)
          ) %>%
          addFullscreenControl(pseudoFullscreen = T) %>%
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
            label = ~ lapply(paste0("<b>", CountyName, " County</b><br>", DnrRegion), HTML),
            fillOpacity = 0.1,
            color = "grey",
            opacity = 0.5,
            fillColor = ~ colorFactor("Dark2", counties$DnrRegion)(DnrRegion),
            weight = 1,
            options = pathOptions(pane = "counties")
          )
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


      # On-demand layer loading ----
      # observers are destroyed once they fire once
      lapply(c("nkes", "huc8", "huc10", "huc12"), function(layer) {
        obs_name <- paste0(layer, "_observer")
        assign(
          envir = env_parent(),
          obs_name,
          observe({
            group <- layers[[layer]]
            if (group %in% input$map_groups) {
              leafletProxy("map") %>%
                addPolygons(
                  data = get(layer),
                  group = group,
                  label = ~ lapply(Label, HTML),
                  popup = ~ lapply(Label, HTML),
                  weight = 1,
                  color = "blue",
                  fillColor = "lightblue",
                  fillOpacity = 0.1,
                  options = pathOptions(pane = layer),
                  labelOptions = labelOptions(
                    style = list("width" = "300px", "white-space" = "normal")
                  )
                )
              get(obs_name)$destroy()
            }
          })
        )
      })


      # Reactive map elements ----

      ## Draw station circle markers ----

      observe({
        req(input$stn_color_by)

        stns <- avail_pts()
        stn_types <- input$stn_types
        color_by <- input$stn_color_by
        radius <- pt_size()

        map <- leafletProxy("map")
        map %>% clearGroup(layers$stations)

        req(nrow(stns) > 0)

        if (color_by == "stn_type") {
          stns <- stns %>%
            mutate(color = case_when(
              nutrient_stn & ("nutrient" %in% stn_types) ~ stn_colors$nutrient,
              therm_stn & ("thermistor" %in% stn_types) ~ stn_colors$thermistor,
              T ~ stn_colors$baseline
            )) %>%
            arrange(max_fw_date)
        } else {
          opts <- filter(stn_color_opts, value == color_by)
          domain <- unlist(opts$domain)
          pal <- colorNumeric(opts$pal, domain, reverse = opts$rev)
          stns <- stns %>%
            left_join(stn_attr_totals, join_by(station_id)) %>%
            rename(attr = all_of(color_by)) %>%
            mutate(attr_clamped = pmax(pmin(attr, domain[2]), domain[1])) %>%
            mutate(color = pal(attr_clamped)) %>%
            arrange(!is.na(attr), attr) %>%
            mutate(map_label = lapply(paste0(map_label, "<br>", opts$label, ": ", attr), shiny::HTML))
        }

        map %>%
          addCircleMarkers(
            data = stns,
            group = layers$stations,
            label = ~map_label,
            layerId = ~station_id,
            radius = radius,
            color = "black",
            weight = 0.5,
            fillColor = ~color,
            fillOpacity = 1,
            options = markerOptions(pane = "stations", sticky = F)
          )
      })

      ## Map markers/clusters ----
      # update map pins/clusters on available stations change
      observe({
        map <- leafletProxy(ns("map"))
        map %>% clearGroup(layers$pins)

        pts <- avail_pts()
        req(nrow(pts) > 0)

        popups <- all_popups[names(all_popups) %in% pts$station_id] %>% setNames(NULL)
        map %>%
          addMarkers(
            data = pts,
            group = layers$pins,
            label = ~map_label,
            popup = popups,
            layerId = ~station_id,
            clusterOptions = markerClusterOptions()
          )
      })

      ## Show current station ----
      observe({
        # don't remove/redraw the last selected stn if all the stations are hidden
        if (!any_stns()) return()

        map <- leafletProxy(ns("map"))
        map %>% clearGroup("cur_point")
        stn <- cur_stn()
        popup <- all_popups[names(all_popups) == stn$station_id] %>% setNames(NULL)

        map %>%
          addMarkers(
            data = stn,
            lat = ~latitude,
            lng = ~longitude,
            label = ~map_label,
            popup = popup,
            layerId = ~station_id,
            group = "cur_point"
          )

        # create hollow station icon if coloring by variable
        if (input$stn_color_by == "stn_type") {
          map %>%
            addCircleMarkers(
              data = stn,
              lat = ~latitude,
              lng = ~longitude,
              label = ~map_label,
              layerId = ~station_id,
              group = "cur_point",
              options = pathOptions(pane = "cur_point_circle"),
              radius = pt_size() + 1,
              weight = 1,
              color = "black",
              opacity = 1,
              fillColor = stn_colors$current,
              fillOpacity = 1
            )
        } else {
          map %>%
            addCircleMarkers(
              data = stn,
              lat = ~latitude,
              lng = ~longitude,
              label = ~map_label,
              layerId = ~station_id,
              group = "cur_point",
              options = pathOptions(pane = "cur_point_circle"),
              radius = pt_size() + 1,
              weight = 3,
              color = stn_colors$current,
              opacity = 1,
              fillColor = "none"
            )
        }
      })

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





      # Event reactives ----

      ## user_loc ----
      # shows a house icon and displays watershed info for the user's location
      observeEvent(input$user_loc, {
        if (rv$user_loc_shown) return()
        map <- leafletProxy("map")
        loc <- input$user_loc %>% lapply(\(x) round(x, 4))
        pt <- tibble(lat = loc$lat, lng = loc$lng) %>%
          st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = F)

        # TEST
        # message("==> huc12 crs: ", st_crs(huc12)$proj4string)
        # message("==> user loc crs: ", st_crs(pt)$proj4string)

        # TODO: st_transform used here to avoid error due to old PROJ version on server
        suppressWarnings({
          watershed <- st_intersection(huc12, st_transform(pt, st_crs(huc12)))
        })

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
        rv$user_loc_shown <- TRUE
      })

      ## remove_user_loc ----
      observeEvent(input$remove_user_loc, {
        map <- leafletProxy("map")
        map %>%
          clearGroup("user_loc") %>%
          removeShape("user_watershed8") %>%
          removeShape("user_watershed10") %>%
          removeShape("user_watershed12")
        rv$user_loc_shown <- FALSE
      })

      ## Zoom events ----
      zoomToSite <- function(z = input$map_zoom) {
        stn <- rv$selected_stn
        leafletProxy(ns("map")) %>%
          setView(
            lat = stn$latitude,
            lng = stn$longitude,
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
        zoomToSite(12)
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
