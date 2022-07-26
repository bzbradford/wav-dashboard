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
  library(plotly)
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

  output$total_stns_text <- renderText({
    paste("Showing", nrow(avail_pts()), "out of", nrow(all_pts), "total stations")
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
        zoom = 11
      )
    updateSelectInput(
      inputId = "station",
      selected = stn_id
    )
  })




# Site info ---------------------------------------------------------------

  output$stn_info_ui <- renderUI({
    bsCollapse(
      bsCollapsePanel(
        title = "Station Information",
        value = "info",
        uiOutput("stn_info")
      ),
      open = "info"
    )
  })

  output$stn_info <- renderUI({
    validate(
      need(
        nrow(cur_stn()) > 0,
        "Select a station in the list or map above."
      )
    )

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
      ),
      hr(),
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
        },
        width = "100%"
      )
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




# Nutrient data -----------------------------------------------------------

  phoslimit <- 0.075 # mg/L, ppm

  cur_nutrient_data <- reactive({
    nutrient_data %>%
      filter(station_id == cur_stn()$station_id)
  })

  cur_nutrient_years <- reactive({
    unique(cur_nutrient_data()$year)
  })

  phos_estimate <- reactive({
    vals <- na.omit(cur_nutrient_data()$tp)
    log_vals <- log(vals)
    n <- length(vals)
    meanp <- mean(log_vals)
    se <- sd(log_vals) / sqrt(n)
    tval <- qt(p = 0.90, df = n - 1)

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
      uiOutput("nutrient_plot_ui"),
      uiOutput("nutrient_info"),
      uiOutput("nutrient_data_table")
    )
  })

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

  output$nutrient_plot <- renderPlotly({
    nutrient_year <- input$nutrient_year
    plot_title <- str_trunc(paste0("Station ", cur_stn()$station_id, ": ", cur_stn()$station_name), width = 80)
    df <- cur_nutrient_data() %>%
      mutate(exceedance = factor(
        ifelse(is.na(tp), "No data", ifelse(tp >= phoslimit, "High", "OK")),
        levels = c("OK", "High", "No data"))) %>%
      drop_na(tp)
    date_range <- as.Date(paste0(nutrient_year, c("-05-01", "-10-31")))
    outer_months <- as.Date(paste0(nutrient_year, c("-04-30", "-11-1")))
    data_dates <-  as.Date(paste(nutrient_year, 5:10, 15, sep = "-"))
    all_dates <-  c(outer_months, data_dates)
    yrange <- c(0, max(0.25, max(df$tp, na.rm = T)))

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

  output$nutrient_data <- renderUI({
    list(
      h4("Nutrient data access:"),
      bsCollapse(
        bsCollapsePanel(
          title = "Nutrient data table",
          renderDataTable({
            cur_nutrient_data() %>%
              filter(year == input$nutrient_year) %>%
              clean_names(case = "title")
          })
        )
      )
    )
  })

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





# Station lists -----------------------------------------------------------


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
