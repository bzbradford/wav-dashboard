# server.R

server <- function(input, output, session) {

# Station select ----------------------------------------------------------

  ## Random station on load ----

  random_stn <- all_pts %>%
    filter(max_fw_year == max(data_years)) %>%
    pull(station_id) %>%
    sample(1)


  ## Available stations ----

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

    type_stns <- all_stn_years %>%
      filter(
        (baseline_stn & ("baseline" %in% input$stn_types)) |
          (therm_stn & ("thermistor" %in% input$stn_types)) |
          (nutrient_stn & ("nutrient" %in% input$stn_types))
      ) %>%
      pull(station_id) %>%
      unique()

    all_stn_years %>%
      filter((station_id %in% year_stns) & (station_id %in% type_stns))
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

    all_pts %>%
      filter(station_id == input$station)
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

  output$total_stns_text <- renderText({
    paste("Showing", nrow(avail_pts()), "out of", nrow(all_pts), "total stations")
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
          popup = setNames(all_popups[names(all_popups) %in% baseline$station_id], NULL),
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
          popup = setNames(all_popups[names(all_popups) %in% therm$station_id], NULL),
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
          popup = setNames(all_popups[names(all_popups) %in% nutrient$station_id], NULL),
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

  observeEvent(input$random_site, {
    stn_id <- stn_list()[sample(1:length(stn_list()), 1)]
    stn <- all_pts %>% filter(station_id == stn_id)
    leafletProxy("map") %>%
      setView(
        lat = stn$latitude,
        lng = stn$longitude,
        zoom = input$map_zoom
      )
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
            st_set_geometry(NULL) %>%
            select(station_id:longitude) %>%
            mutate(across(everything(), as.character)) %>%
            clean_names(case = "title") %>%
            pivot_longer(
              cols = everything(),
              names_to = "Property",
              values_to = "Value") %>%
            clean_names(case = "title")
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
    cur_baseline_data() %>%
      filter(year == input$baseline_year)
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
        style = paste(flex_row, "align-items: center;"),
        div(
          style = paste(flex_col, "flex: 0 0 auto; margin-right: 1em;"),
          p(em("Choose year:"))
        ),
        div(
          style = flex_col,
          radioGroupButtons(
            inputId = "baseline_year",
            label = NULL,
            choices = cur_baseline_years(),
            selected = last(cur_baseline_years())
          )
        )
      ),
      bsCollapse(
        bsCollapsePanel(
          title = "Baseline data table",
          value = "data",
          downloadButton("baseline_data_dl", label = "Download this data", class = "btn-default"),
          div(
            style = "overflow: auto;",
            dataTableOutput("baseline_data")
          )
        ),
        open = "data"
      )
    )
  })


  ## Data table ----

  output$baseline_data <- renderDataTable({

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
    cur_nutrient_data() %>%
      filter(year == input$nutrient_year)
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
        style = paste(flex_row, "align-items: center;"),
        div(
          style = paste(flex_col, "flex: 0 0 auto; margin-right: 1em;"),
          p(em("Choose year:"))
        ),
        div(
          style = flex_col,
          radioGroupButtons(
            inputId = "nutrient_year",
            label = NULL,
            choices = cur_nutrient_years(),
            selected = last(cur_nutrient_years())
          )
        )
      ),
      uiOutput("nutrient_plot_ui"),
      br(),
      uiOutput("nutrient_info"),
      br(),
      uiOutput("nutrient_data")
    )
  })


  ## Plot UI ----

  output$nutrient_plot_ui <- renderUI({
    list(
      plotlyOutput("nutrient_plot"),
      div(
        style = "margin: 0.5em 1em;",
        align = "center",
        p(em("The dashed line on this plot indicates the total phosphorus state exceedance level of 0.075 mg/L (ppm). If more than one month of data was collected, the median and 90% confidence interval for the true total phosphorus level are displayed as a horizontal band."))
      )
    )
  })


  ## Plot ----

  output$nutrient_plot <- renderPlotly({
    nutrient_year <- input$nutrient_year
    plot_title <- str_trunc(paste0("Station ", cur_stn()$station_id, ": ", cur_stn()$station_name), width = 80)
    df <- selected_nutrient_data() %>%
      mutate(exceedance = factor(
        ifelse(is.na(tp), "No data", ifelse(tp >= phoslimit, "High", "OK")),
        levels = c("OK", "High", "No data"))) %>%
      drop_na(tp)
    date_range <- as.Date(paste0(nutrient_year, c("-05-01", "-10-31")))
    outer_months <- as.Date(paste0(nutrient_year, c("-04-30", "-11-1")))
    data_dates <-  as.Date(paste(nutrient_year, 5:10, 15, sep = "-"))
    all_dates <-  c(outer_months, data_dates)
    yrange <- suppressWarnings(c(0, max(0.25, max(df$tp, na.rm = T))))

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
          y = ~upper,
          name = "Upper 90% CI",
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
          y = ~lower,
          name = "Lower 90% CI",
          xperiod = "M1",
          xperiodalignment = "middle",
          opacity = 0.5,
          line = list(color = ci_color, width = 0.5)
        )

      shapes <- list(
        rect(phos_estimate()$lower, phos_estimate()$upper, ci_color),
        hline(phoslimit)
      )
    } else {
      plt <- plot_ly()
      shapes <- hline(phoslimit)
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
        hovertemplate = "Measured TP: %{y:.3f}<extra></extra>"
      ) %>%
      layout(
        title = plot_title,
        showlegend = F,
        xaxis = list(
          title = "",
          type = "date",
          tickformat = "%B<br>%Y",
          dtick = "M1",
          ticklabelmode = "period",
          range = date_range),
        yaxis = list(
          title = "Total phosphorus (ppm)",
          zerolinecolor = "lightgrey",
          range = yrange),
        legend = list(
          traceorder = "reversed"
        ),
        hovermode = "x unified",
        margin = list(t = 50),
        shapes = shapes
      )

    plt
  })


  ## Info ----

  output$nutrient_info <- renderUI({
    fluidRow(
      column(12,
        h4("Exceedance criteria"),
        p("The shaded horizontal band on the plot represents the 90% confidence interval for the median total phosphorus (TP) at this site (if more than one month of data was collected). This means that, given the TP concentrations measured this year, there is about an 90% chance that the true median total phosphorus concentration falls somewhere between those lines. We know that TP in streams varies quite a bit, so individual samples could be higher or lower than the confidence interval."),
        p("A stream site is considered 'Criteria Exceeded' and the confidence interval band will be shaded", colorize("red"), "if: 1) the lower 90% confidence limit of the sample median exceeds the state TP criterion of", phoslimit, "mg/L or 2) there is corroborating WDNR biological data to support an adverse response in the fish or macroinvertebrate communities. If there is insufficient data for either of these requirements, more data will need to be collected in subsequent years before a decision can be made. A site is designated as 'Watch Waters' if the total phosphorus state criterion concentration falls within the confidence limit or additional data are required, and a site is considered to have 'Met Criteria' if the upper limit of the confidence interval does not exceed the criterion (shaded confidence interval band will be", HTML(paste0(colorize("teal"), ").")), "This year, many sites are assigned 'Watch Waters' because fewer than six samples were collected. Nevertheless, these total phosphorus measurements will still improve our understanding of stream health at this site."),
        br(),
        h4("Why Phosphorus?"),
        p("Phosphorus is an essential nutrient responsible for plant growth, but it is also the most visible, widespread water pollutant in lakes. Small increases in phosphorus levels can bring about substantial increases in aquatic plant and algae growth, which in turn can reduce the recreational use and biodiversity. When the excess plants die and are decomposed, oxygen levels in the water drop dramatically which can lead to fish kills. Additionally, one of the most common impairments in Wisconsin’s streams is excess sediment that covers stream bottoms. Since phosphorus moves attached to sediments, it is intimately connected with this source of pollution in our streams. Phosphorus originates naturally from rocks, but its major sources in streams and lakes today are usually associated with human activities: soil erosion, human and animal wastes, septic systems, and runoff from farmland or lawns. Phosphorus-containing contaminants from urban streets and parking lots such as food waste, detergents, and paper products are also potential sources of phosphorus pollution from the surrounding landscape. The impact that phosphorus can have in streams is less apparent than in lakes due to the overall movement of water, but in areas with low velocity, where sediment can settle and deposit along the bottom substrate, algae blooms can result."),
        br(),
        h4("Volunteer Monitoring Protocol"),
        p("To assess in-stream phosphorus levels, WAV volunteers collected water samples that were analyzed for total phosphorus (TP) at the State Lab of Hygiene during the growing season. Following Wisconsin Department of Natural Resources (WDNR) methods, four to six phosphorus water samples were collected at each monitoring site - one per month for up to each of the six months during the growing season. The monthly water samples were collected approximately 30 days apart and no samples were collected within 15 days of one another. Samples at several sites were collected every two weeks. The monthly values are an average of the biweekly sample results.")
      )
    )
  })


  ## Data table ----

  output$nutrient_data <- renderUI({
    list(
      bsCollapse(
        bsCollapsePanel(
          title = "View/download raw data:",
          p(downloadButton("nutrient_data_dl", "Download this data")),
          div(
            style = "overflow: auto;",
            renderDataTable({
              selected_nutrient_data() %>%
                clean_names(case = "big_camel")
            })
          )
        )
      )
    )
  })

  output$nutrient_data_dl <- downloadHandler(
    paste0("stn-", cur_stn()$station_id, "-nutrient-data-", input$nutrient_year, ".csv"),
    function(file) {write_csv(selected_nutrient_data(), file)}
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
    cur_therm_data() %>%
      filter(year == input$therm_year)
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
        style = paste(flex_row, "align-items: center;"),
        div(
          style = paste(flex_col, "flex: 0 0 auto; margin-right: 1em;"),
          p(em("Choose year:"))
        ),
        div(
          style = flex_col,
          radioGroupButtons(
            inputId = "therm_year",
            label = NULL,
            choices = cur_therm_years(),
            selected = last(cur_therm_years())
          )
        )
      ),
      uiOutput("therm_plot_ui"),
      br(),
      uiOutput("therm_info"),
      br(),
      uiOutput("therm_data")
    )
  })


  ## Plot UI

  output$therm_plot_ui <- renderUI({
    list(
      plotlyOutput("therm_plot"),
      div(
        style = "margin: 0.5em 1em; 0.5em 1em;",
        uiOutput("therm_annotation_text"),
        p(em("High or widely fluctuating temperatures may indicate that the logger became exposed to the air, either before/after deployment, or when stream levels dropped below the point where the logger was anchored.")),
        hr(),
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
        )
      )
    )
  })


  ## Plot ----

  output$therm_plot <- renderPlotly({
    req(input$therm_temp_units)
    req(input$therm_plot_annotations)

    units <- input$therm_temp_units
    annotation <- input$therm_plot_annotations

    df_daily <- therm_daily() %>%
      mutate(date_time = as.POSIXct(paste(date, "12:00:00")))
    df_hourly <- selected_therm_data()
    plot_title <- paste0("Station ", cur_stn()$station_id, ": ", cur_stn()$station_name) %>%
      str_trunc(width = 80) %>%
      str_to_title()

    req(nrow(df_daily) > 0)
    req(nrow(df_hourly) > 0)

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
        title = plot_title,
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
      )

    # add annotation color bands
    if (annotation != "None") {
      if (annotation == "btrout") {
        temps <- c(32, 52, 61, 72, 100) # F
        if (units == "C") temps <- f_to_c(temps)
        colors <- c("cornflowerblue", "green", "lightgreen", "darkorange")
      } else if (annotation == "wtemp") {
        temps <- c(0, 72, 77, 100) # F
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


  ## Info ----

  output$therm_info <- renderUI({
    list(
      p("Temperature is often referred to as a 'master variable' in aquatic ecosystems because temperature determines the speed of important processes, from basic chemical reactions to the growth and metabolism of fishes and other organisms. In addition, temperature determines the type of fish community that the stream can support. It is especially important to gather stream temperature information over many years in order to track how quickly our streams are warming due to climate change. The continuous data loggers you have deployed and maintained are a 21st-century approach to monitoring stream temperature, providing accurate, high-resolution temperature data as we monitor the health of streams across the state."),
      p("Tips for understanding and interacting with the temperature plot:"),
      tags$ul(
        tags$li("Hover over the chart to see hourly and daily temperature measurement details."),
        tags$li("Click on the legend to show / hide the time series of hourly or daily temperature."),
        tags$li("Drag the mouse over a time period to zoom in, and double click to return to the original view."),
        tags$li("While hovering over the plot, click the camera icon along the top to download a picture of the plot."),
        tags$li("Anomalous temperature readings at the very beginning and end of the data may reflect air temperatures before the logger was deployed into the stream. It's also possible that the logger because exposed to the air during deployment if water levels dropped.")
      )
    )
  })


  ## Data table ----

  output$therm_data <- renderUI({
    bsCollapse(
      bsCollapsePanel(
        title = "Daily average data",
        p(downloadButton("therm_daily_dl", "Download this data")),
        div(
          style = "overflow: auto;",
          renderDataTable({
            therm_daily() %>%
              clean_names(case = "big_camel")
          })
        )
      ),
      bsCollapsePanel(
        title = "Hourly logger data",
        p(downloadButton("therm_hourly_dl", "Download this data")),
        div(
          style = "overflow: auto;",
          renderDataTable({
            selected_therm_data() %>%
              clean_names(case = "big_camel")
          })
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









# Station lists -----------------------------------------------------------

  ## Layout ----

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
}
