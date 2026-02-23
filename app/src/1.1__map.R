## MAP ##

mapUI <- function() {
  ns <- NS("map")

  tagList(
    sidebarLayout(
      # Main panel ----
      mainPanel = mainPanel(
        div(
          class = "map-container",
          id = "map",
          leafletOutput(ns("map"), width = "100%", height = "720px") |>
            with_spinner(hide.ui = TRUE)
        )
      ),

      position = "right",

      # Sidebar ----
      sidebarPanel = sidebarPanel(
        ## Station data types ----
        checkboxGroupInput(
          inputId = ns("stn_types"),
          label = "Station data types:",
          choices = stn_type_choices,
          selected = stn_type_choices
        ),

        div(
          "Stations with data from:",
          class = "control-label",
          style = "margin: .5rem 0;"
        ),

        ## Stations with data from ----
        fluidRow(
          column(
            width = 5,
            style = "white-space: nowrap;",
            checkboxGroupInput(
              inputId = ns("stn_years"),
              label = NULL,
              choices = stn_year_choices,
              selected = stn_year_choices[1:4]
            ),
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

        ## Color station markers by ----
        div(
          style = "margin-top: .5rem;",
          radioButtons(
            inputId = ns("stn_color_by"),
            label = "Color station markers by:",
            choices = map_color_choices
          ),
          div(
            style = "margin-left: 20px;",
            selectInput(
              inputId = ns("stn_color_measure"),
              label = NULL,
              choices = map_measure_choices
            )
          )
        ),

        ## Total stations ----
        uiOutput(ns("total_stns_text")),

        ## Zoom buttons ----
        div(
          style = "line-height: 3.5em;",
          align = "center",
          actionButton(
            ns("zoom_in_btn"),
            "Zoom to selected site",
            width = "100%"
          ),
          br(),
          actionButton(
            ns("zoom_all_btn"),
            "Zoom out to all sites",
            width = "100%"
          ),
        )
      )
    )
  )
}

#' @requires
#' - `stn_type_choices`
#' - `stn_colors`
#' - `stn_data_years`
#' - `all_pts`
#' - `all_labels`
#' - shapefiles: `nkes`, `huc8`, `huc10`, `huc12`
#' @param main_rv reactive values object from main server
#' @param main_session main server session
mapServer <- function(main_rv, main_session) {
  moduleServer(
    id = "map",
    function(input, output, session) {
      ns <- session$ns

      # Static vars ----

      proxy_map <- leafletProxy(ns("map"))

      ## stn_coverages ----
      stn_coverages <- list(
        "baseline" = baseline_coverage,
        "nutrient" = nutrient_coverage,
        "thermistor" = therm_coverage
      )

      # Reactive vals ----

      rv <- reactiveValues(
        # holds the last selected station in case cur_stn() becomes blocked
        selected_stn = NULL,
        # tracks which map layers have been loaded
        rendered_layers = list(),
        # holds lat/lng for user location
        user_loc_shown = FALSE
      )

      cur_stn <- reactive({
        main_rv$cur_stn
      })

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
          match_years <- union(
            match_years,
            last(match_years):last(stn_data_years)
          )
        }

        # handle any/all matching types
        if (match_type) {
          ids <- sapply(types, function(stn_type) {
            stn_coverages[[stn_type]] |>
              filter(all(match_years %in% data_year_list)) |>
              pull(station_id)
          })
        } else {
          ids <- sapply(types, function(stn_type) {
            stn_coverages[[stn_type]] |>
              filter(any(match_years %in% data_year_list)) |>
              pull(station_id)
          })
        }

        avail_ids <- sort(reduce(ids, union))
        filter(all_stn_years, station_id %in% avail_ids)
      })

      observe({
        main_rv$avail_stns <- avail_stns()
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
        case_when(
          is.null(z) ~ 4,
          z >= 14 ~ 8,
          z >= 10 ~ 6,
          TRUE ~ 4
        )
      }

      observe({
        z <- input$map_zoom
        if (is.null(z)) {
          return()
        }
        new_size <- getPtSize(z)
        if (pt_size() != new_size) pt_size(new_size)
      })

      # Event handlers ----
      # Select a station when clicked on the map ----
      observeEvent(input$map_marker_click, {
        stn <- req(input$map_marker_click)
        req(stn$group != "cur_point")

        updateSelectInput(
          session = main_session,
          inputId = "station",
          selected = stn$id
        )

        cur_zoom <- input$map_zoom
        req(cur_zoom)
        proxy_map |>
          setView(
            lat = stn$lat,
            lng = stn$lng,
            zoom = max(cur_zoom, 10) # don't zoom out
          )
      })

      # Rendered UI elements ----

      ## totalStnsText ----
      output$total_stns_text <- renderUI({
        div(
          style = "width: 100%; margin: 1em 0; padding: 5px; border: 2px solid grey; border-radius: 5px; text-align: center; font-weight: bold;",
          sprintf(
            "Showing %i out of %i total stations",
            nrow(avail_pts()),
            nrow(all_pts)
          )
        )
      })

      # Map setup ----

      basemaps <- tribble(
        ~label          , ~provider                   ,
        "ESRI Topo"     , providers$Esri.WorldTopoMap ,
        "Satellite"     , providers$Esri.WorldImagery ,
        "OpenStreetMap" , providers$OpenStreetMap     ,
        "Grey Canvas"   , providers$CartoDB.Positron
      )

      addBasemaps <- function(map) {
        for (r in seq_along(basemaps)) {
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

      hidden_layers <- c(
        layers$nkes,
        layers$huc8,
        layers$huc10,
        layers$huc12,
        layers$dnr_wsheds,
        layers$pins
      )

      # Render initial map ----

      output$map <- renderLeaflet({
        # map buttons
        btn1 <- easyButton(
          position = "topleft",
          icon = "fa-location",
          title = "Show my location on the map",
          onClick = JS(
            "(btn, map) => { Shiny.setInputValue('map-map_btn', 'user_loc', {priority: 'event'}); }"
          )
        )

        btn2 <- easyButton(
          position = "topleft",
          icon = "fa-location-pin",
          title = "Zoom to selected site",
          onClick = JS(
            "(btn, map) => { Shiny.setInputValue('map-map_btn', 'zoom_site', {priority: 'event'}); }"
          )
        )

        btn3 <- easyButton(
          position = "topleft",
          icon = "fa-globe",
          title = "Reset map view",
          onClick = JS(
            "(btn, map) => { Shiny.setInputValue('map-map_btn', 'zoom_extent', {priority: 'event'}); }"
          )
        )

        # render map
        leaflet() |>
          fitBounds(lat1 = 42.4, lat2 = 47.1, lng1 = -92.9, lng2 = -86.8) |>
          addBasemaps() |>
          addMapPane("counties", 410) |>
          addMapPane("cur_huc", 420) |>
          addMapPane("huc8", 421) |>
          addMapPane("huc10", 422) |>
          addMapPane("huc12", 423) |>
          addMapPane("nkes", 424) |>
          addMapPane("stations", 430) |>
          addMapPane("cur_point_circle", 440) |>
          addMapPane("pins", 450) |>
          hideGroup(hidden_layers) |>
          addLayersControl(
            baseGroups = basemaps$label,
            overlayGroups = unlist(layers, use.names = FALSE),
            options = layersControlOptions(collapsed = FALSE)
          ) |>
          addEasyButtonBar(btn1, btn2, btn3) |>
          suspendScroll(
            sleepTime = 0,
            wakeTime = 1000,
            hoverToWake = TRUE,
            sleepNote = FALSE,
            sleepOpacity = 1
          ) |>
          addPolygons(
            data = wi_counties,
            group = layers$counties,
            label = ~ lapply(
              paste0("<b>", CountyName, " County</b><br>", DnrRegion),
              HTML
            ),
            fillOpacity = 0.1,
            color = "grey",
            opacity = 0.5,
            fillColor = ~ colorFactor("Dark2", wi_counties$DnrRegion)(
              DnrRegion
            ),
            weight = 1,
            options = pathOptions(pane = "counties")
          ) |>
          # assign leaflet map object to global var 'map'
          htmlwidgets::onRender("() => { map = this; }")
      })

      ## Hide the legend ----
      observeEvent(TRUE, {
        delay(3000, {
          proxy_map |>
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
              proxy_map |>
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

      # select the 'measure' radio button if the dropdown changes
      observeEvent(
        input$stn_color_measure,
        {
          updateRadioButtons(
            inputId = "stn_color_by",
            selected = "measure"
          )
        },
        ignoreInit = TRUE
      )

      observe({
        pts <- avail_pts()

        # remove existing points before redraw
        proxy_map |> clearGroup(layers$stations)

        req(nrow(pts) > 0)

        stn_types <- req(input$stn_types)
        color_by <- req(input$stn_color_by)
        radius <- pt_size()

        if (color_by == "type") {
          # set station colors
          pts <- pts |>
            mutate(
              color = case_when(
                nutrient_stn &
                  ("nutrient" %in% stn_types) ~ stn_colors$nutrient,
                therm_stn &
                  ("thermistor" %in% stn_types) ~ stn_colors$thermistor,
                TRUE ~ stn_colors$baseline
              )
            ) |>
            arrange(max_fw_date)

          # no legend for station types
          proxy_map |> removeControl("legend")
        } else {
          # get value from drop-down if needed
          color_by <- ifelse(
            color_by == "measure",
            req(input$stn_color_measure),
            color_by
          )
          # prepare color palette
          opts <- data_opts |> filter(col == color_by)
          domain <- c(opts$palette_min, opts$palette_max)
          pal <- colorNumeric(
            opts$palette_name,
            domain,
            reverse = opts$palette_reverse
          )
          pal_rev <- colorNumeric(
            opts$palette_name,
            domain,
            reverse = !opts$palette_reverse
          )

          # select stations, add color
          pts <- pts |>
            left_join(map_color_data, join_by(station_id)) |>
            rename(attr = all_of(color_by)) |>
            mutate(attr_clamped = clamp(attr, domain[1], domain[2])) |>
            mutate(color = pal(attr_clamped)) |>
            arrange(!is.na(attr), attr) |>
            mutate(
              attr_label = if_else(
                is.na(attr),
                "No data",
                as.character(signif(attr, 4))
              ),
              map_label = paste0(
                map_label,
                "<br>",
                opts$label,
                ": ",
                attr_label
              ) |>
                lapply(HTML)
            )

          # add legend with reversed palette and labels
          proxy_map |>
            addLegend(
              layerId = "legend",
              position = "bottomright",
              pal = pal_rev,
              bins = 5,
              values = domain,
              labFormat = labelFormat(transform = function(x) sort(x, TRUE))
            )
        }

        # add stations to map
        proxy_map |>
          addCircleMarkers(
            data = pts,
            group = layers$stations,
            label = ~map_label,
            layerId = ~station_id,
            radius = radius,
            color = "black",
            weight = 0.5,
            fillColor = ~color,
            fillOpacity = 1,
            options = markerOptions(pane = "stations", sticky = FALSE)
          )
      })

      ## Map markers/clusters ----
      # update map pins/clusters on available stations change
      observe({
        proxy_map |> clearGroup(layers$pins)

        pts <- avail_pts()
        req(nrow(pts) > 0)

        popups <- all_popups[names(all_popups) %in% pts$station_id] |>
          setNames(NULL)
        proxy_map |>
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
        if (!any_stns()) {
          return()
        }

        proxy_map |> clearGroup("cur_point")
        stn <- req(rv$selected_stn)
        popup <- all_popups[names(all_popups) == stn$station_id] |>
          setNames(NULL)

        proxy_map |>
          addMarkers(
            data = stn,
            lat = ~latitude,
            lng = ~longitude,
            label = ~map_label,
            popup = popup,
            layerId = ~station_id,
            group = "cur_point"
          )

        if (input$stn_color_by == "type") {
          proxy_map |>
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
          # create hollow station icon if coloring by variable
          proxy_map |>
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
      observe({
        stn <- req(rv$selected_stn)
        shp <- huc8 |>
          filter(Huc8Code == stn$huc8)
        proxy_map |>
          removeShape("curHuc8") |>
          addPolygons(
            data = shp,
            group = layers$huc8,
            layerId = "curHuc8",
            weight = 4,
            fillOpacity = 0.1,
            options = pathOptions(pane = "cur_huc")
          )
      })

      ### HUC10 ----
      observe({
        stn <- req(rv$selected_stn)
        shp <- huc10 |>
          filter(Huc10Code == stn$huc10)
        proxy_map |>
          removeShape("curHuc10") |>
          addPolygons(
            data = shp,
            group = layers$huc10,
            layerId = "curHuc10",
            weight = 3,
            fillOpacity = 0.1,
            options = pathOptions(pane = "cur_huc")
          )
      })

      ### HUC12 ----
      observe({
        stn <- req(rv$selected_stn)
        shp <- huc12 |> filter(Huc12Code == stn$huc12)
        proxy_map |>
          removeShape("curHuc12") |>
          addPolygons(
            data = shp,
            group = layers$huc12,
            layerId = "curHuc12",
            weight = 2,
            fillOpacity = 0.1,
            options = pathOptions(pane = "cur_huc")
          )
      })

      # Event reactives ----

      ## Handle map button clicks ----
      observe({
        switch(
          req(input$map_btn),
          user_loc = getUserLoc(),
          zoom_site = zoomToSite(12),
          zoom_extent = zoomAllSites()
        )
      })

      ## Zoom events ----
      zoomToSite <- function(z = input$map_zoom) {
        stn <- rv$selected_stn
        proxy_map |>
          setView(
            lat = stn$latitude,
            lng = stn$longitude,
            zoom = z
          )
      }

      zoomAllSites <- function() {
        if (any_stns()) {
          fitBounds(
            proxy_map,
            lat1 = min(avail_pts()$latitude),
            lat2 = max(avail_pts()$latitude),
            lng1 = min(avail_pts()$longitude),
            lng2 = max(avail_pts()$longitude)
          )
        } else {
          fitBounds(
            proxy_map,
            lat1 = min(all_pts$latitude),
            lat2 = max(all_pts$latitude),
            lng1 = min(all_pts$longitude),
            lng2 = max(all_pts$longitude)
          )
        }
      }

      ### cur_stn ----
      # center map when station changes but only if out of view
      observeEvent(rv$selected_stn, {
        stn <- req(rv$selected_stn)
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
        zoomAllSites()
      })

      ## show_watersheds ----
      # Toggle watershed visibility
      observeEvent(input$show_watersheds, {
        proxy_map |>
          showGroup(layers$huc8) |>
          showGroup(layers$huc10) |>
          showGroup(layers$huc12)
        zoomToSite(9)
      })

      ## toggle_watersheds ----
      observeEvent(input$toggle_watersheds, {
        groups <- input$map_groups
        all_shown <- all(
          layers$huc8 %in% groups,
          layers$huc10 %in% groups,
          layers$huc12 %in% groups
        )
        if (all_shown) {
          proxy_map |>
            hideGroup(layers$huc8) |>
            hideGroup(layers$huc10) |>
            hideGroup(layers$huc12)
        } else {
          proxy_map |>
            showGroup(layers$huc8) |>
            showGroup(layers$huc10) |>
            showGroup(layers$huc12)
        }
      })

      ## Handle user location ----

      getUserLoc <- function() {
        runjs(
          "
          map.getMap().locate({ setView: false }).on('locationfound', (event) => {
            Shiny.setInputValue('map-user_loc', event.latlng, {priority: 'event'})
          })
        "
        )
      }

      # shows a house icon and displays watershed info for the user's location
      observeEvent(input$user_loc, {
        if (rv$user_loc_shown) {
          return()
        }

        loc <- input$user_loc |> lapply(\(x) round(x, 4))
        pt <- tibble(lat = loc$lat, lng = loc$lng) |>
          st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = FALSE)

        # TEST
        # message("==> huc12 crs: ", st_crs(huc12)$proj4string)
        # message("==> user loc crs: ", st_crs(pt)$proj4string)

        # TODO: st_transform used here to avoid error due to old PROJ version on server
        suppressWarnings({
          watershed <- st_intersection(huc12, st_transform(pt, st_crs(huc12)))
        })

        # map info for user loc
        label <- str_glue(
          "
          <b>My location ({pt$lat}, {pt$lng})</b><br>
          <i>Click for more information on your watershed</i>
        "
        )
        popup <- str_glue(
          "
          <b>My location ({pt$lat}, {pt$lng})</b><br>
          <i>Your watershed:</i><br>
          <div style='padding-left: 1em;'>
            Major basin: {watershed$MajorBasin}<br>
            HUC8 Subbasin: {watershed$Huc8Name}<br>
            HUC10 Watershed: {watershed$Huc10Name}<br>
            HUC12 Subwatershed: {watershed$Huc12Name}
          </div>
        "
        )
        popup <- paste0(
          popup,
          "<br>
          <a style='cursor:pointer;' onclick=\"Shiny.setInputValue('map-toggle_watersheds', false, {priority: 'event'})\">Toggle watersheds on map</a> |
          <a style='cursor:pointer;' onclick=\"Shiny.setInputValue('map-remove_user_loc', true, {priority: 'event'})\">Hide my location</a>
        "
        )
        icon <- makeAwesomeIcon(
          icon = "home",
          markerColor = "#c5050c",
          library = "ion"
        )

        # identify local watersheds
        user_huc12 <- filter(huc12, Huc12Name == watershed$Huc12Name)
        user_huc10 <- filter(huc10, Huc10Name == watershed$Huc10Name)
        user_huc8 <- filter(huc8, Huc8Name == watershed$Huc8Name)

        # add to map
        proxy_map |>
          addAwesomeMarkers(
            data = pt,
            label = HTML(label),
            popup = HTML(popup),
            icon = icon,
            group = "user_loc"
          ) |>
          addPolygons(
            data = user_huc8,
            group = layers$huc8,
            layerId = "user_watershed8",
            weight = 2,
            color = "#c5050c",
            fillOpacity = 0.1,
            options = pathOptions(pane = "cur_huc")
          ) |>
          addPolygons(
            data = user_huc10,
            group = layers$huc10,
            layerId = "user_watershed10",
            weight = 2,
            color = "#c5050c",
            fillOpacity = 0.1,
            options = pathOptions(pane = "cur_huc")
          ) |>
          addPolygons(
            data = user_huc12,
            group = layers$huc12,
            layerId = "user_watershed12",
            weight = 2,
            color = "#c5050c",
            fillOpacity = 0.1,
            options = pathOptions(pane = "cur_huc")
          ) |>
          setView(
            lat = loc$lat,
            lng = loc$lng,
            zoom = 12
          )

        rv$user_loc_shown <- TRUE
      })

      ## remove_user_loc ----
      observeEvent(input$remove_user_loc, {
        proxy_map |>
          clearGroup("user_loc") |>
          removeShape("user_watershed8") |>
          removeShape("user_watershed10") |>
          removeShape("user_watershed12")
        rv$user_loc_shown <- FALSE
      })
    }
  )
}
