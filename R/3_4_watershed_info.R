# Watershed Information

watershedInfoUI <- function() {
  ns <- NS("watershed")
  link1 <- a("2021 USGS National Land Cover Database", href = "https://www.usgs.gov/centers/eros/science/national-land-cover-database", target = "_blank", .noWS = "outside")
  link2 <- a("Click here", href = "https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description", target = "_blank", .noWS = "outside")

  div(
    class = "data-tab",
    h3("Watersheds"),
    p(strong("What is a watershed?"), "NOAA defines a watershed as an area of land that channels rainfall, snowmelt, and runoff into a common body of water. The term \"watershed\" is often used interchangeably with \"drainage basin,\" which may make the concept easier to visualize. A watershed can encompass a small area of land that drains into a trickling creek. It can encompass multiple states in the Midwest, all draining into the Mississippi River. Or it can encompass multiple countries draining into the Atlantic Ocean. No matter where you are standing or sitting right now, you are in a watershed."),
    p(HTML("In the US, watersheds are divided into successively smaller areas called <em>hydrological units</em> and given a numerical designation called a <em>hydrological unit code</em> (HUC). These HUCs have a specific number of digits for each level of division. For example, Wisconsin is divided into 52 <em>sub-basins</em> (8 digit HUC), 372 <em>watersheds</em> (10 digit HUC), and 1,808 <em>sub-watersheds</em> (12 digit HUC). In Wisconsin the DNR has its own numbering system for watersheds (roughly equivalent to HUC10 scale); see the entry in the table below for links to the DNR's information pages for each watershed. Use the layers menu (upper right) in the map above or"), strong(a(href = "#map", onclick = "Shiny.setInputValue('map-show_watersheds', true, {priority: 'event'})", "click here")), "to enable these watershed boundaries on the map and explore them yourself."),
    uiOutput(ns("watershedInfoUI")) %>% withSpinnerProxy(proxy.height = 200),
    h4(strong("Landscape composition")),
    p("Landscape composition, defined here as the percent of a given watershed represented by one of several different types of developed, cultivated, or natural landcover classes, can have a significant impact on water quality. Water quality may be impaired in landscapes with high fractions of cultivated crops or developed land, while water quality may be improved where wetlands or forests dominate. Landcover data displayed below is derived from the ", link1, ". The watershed is automatically determined based on the current WAV station selected above. Use the buttons below to change the watershed scale from smaller (HUC12) to larger (HUC8). ", link2, " for more information and specific definitions of each land cover class."),
    div(class = "well flex-row year-btns",
      div(class = "year-btn-text", em("Landscape scale:")),
      radioGroupButtons(
        inputId = ns("scale"),
        label = NULL,
        choices = list(
          "Sub-watershed (HUC12)" = 12,
          "Watershed (HUC10)" = 10,
          "Sub-basin (HUC8)" = 8
        ),
        selected = 12
      )
    ),
    uiOutput(ns("landscapePlotsUI")) %>% withSpinnerProxy(),
  )
}


#' Requires global variable `landscape_data`
#' @param `cur_stn` a `reactive()` expression containing the 1-line data frame `cur_stn()`

watershedInfoServer <- function(cur_stn, has_focus) {
  moduleServer(
    id = "watershed",
    function(input, output, session) {
      ns <- session$ns

      # Reactive vars ----

      ## selected_data ----
      selected_data <- reactive({
        req(cur_stn())
        req(input$scale)

        col <- paste0("huc", input$scale)
        landscape_data %>%
          filter(huc == cur_stn()[[col]]) %>%
          arrange(class_name) %>%
          droplevels()
      })

      ## mean_data ----
      mean_data <- reactive({
        req(input$scale)

        mean_landscape %>%
          filter(huc_level == input$scale)
      })

      ## selected_name ----
      selected_name <- reactive({
        req(input$scale)
        req(cur_stn())

        stn <- cur_stn()
        case_match(
          as.numeric(input$scale),
          8 ~ paste(stn$sub_basin, "sub-basin"),
          10 ~ paste(stn$watershed, "watershed"),
          12 ~ paste(stn$sub_watershed, "sub-watershed")
        )
      })

      ## mean_name ----
      mean_name <- reactive({
        req(input$scale)

        case_match(
          as.numeric(input$scale),
          8 ~ "All Wisconsin sub-basins",
          10 ~ "All Wisconsin watersheds",
          12 ~ "All Wisconsin sub-watersheds"
        )
      })


      # Layout ----

      ## watershedInfoUI ----
      output$watershedInfoUI <- renderUI({
        wellPanel(
          h4("Location and watershed details for selected station:", style = "margin-top: 0px;"),
          div(
            style = "padding-left: 1em;",
            buildWatershedInfo(cur_stn())
          )
        )
      })

      ## landscapePlotsUI ----
      output$landscapePlotsUI <- renderUI({
        tagList(
          div(id = "landscape-plot-container",
            uiOutput(ns("pieChartUI")),
            uiOutput(ns("diffPlotUI")),
          ),
          uiOutput(ns("plotExportUI")),
        )
      })


      ## pieChartUI ----
      output$pieChartUI <- renderUI({
        req(input$scale)

        tagList(
          div(
            class = "flex-row",
            div(
              class = "pie-container well",
              h5(align = "center", strong(selected_name())),
              plotlyOutput(ns("curPlot"), height = "300px"),
              div(
                class = "plot-caption",
                "Drainage area:",
                fmt_area(selected_data()$total_area[1])
              )
            ),
            div(
              class = "pie-container well",
              h5(align = "center", strong(mean_name())),
              plotlyOutput(ns("allPlot"), height = "300px"),
              div(
                class = "plot-caption",
                "Average drainage:",
                fmt_area(watershed_sizes[[as.character(input$scale)]])
              )
            ),
          ),
        )
      })

      ## diffPlotUI ----
      output$diffPlotUI <- renderUI({
        req(input$scale)
        tagList(
          wellPanel(
            style = "margin-top: 10px;",
            h5(align = "center", strong("Difference in landscape composition")),
            div(align = "center", class = "note", "Compared to the statewide average shown above, the landscape composition of the", strong(selected_name()), "differs in each of the following respects:"),
            plotlyOutput(ns("diffPlot"), height = "500px")
          )
        )
      })

      ## plotExportUI ----
      output$plotExportUI <- renderUI({
        fname <- paste0("Landscape composition - ", selected_name(), ".png")
        p(class = "note", align = "center",
          a("Click here to download the landscape plots above as a PNG.",
            style = "cursor: pointer;",
            onclick = sprintf("
              html2canvas(
                document.querySelector('#landscape-plot-container'),
                {scale: 3}
              ).then(canvas => {
                saveAs(canvas.toDataURL(), '%s')
              })", fname)
          )
        )
      })


      # Plots ----

      ## curPlot ----
      output$curPlot <- renderPlotly({
         makeLandscapePieChart(selected_data())
      })

      ## allPlot ----
      output$allPlot <- renderPlotly({
         makeLandscapePieChart(mean_data())
      })

      ## diffPlot ----
      # shows a barplot of the difference between the current watershed and the state average
      output$diffPlot <- renderPlotly({
        makeLandscapeDiffPlot(mean_data(), selected_data())
      })


      # Return values ----
      return(reactive(list(huc = paste0("HUC", input$scale))))
    }
  )
}
