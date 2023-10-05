# Watershed Information


# UI ----

watershedInfoUI <- function() {
  ns <- NS("watershed")

  div(
    class = "data-tab",
    h3("Watersheds"),
    p(strong("What is a watershed?"), "NOAA defines a watershed as an area of land that channels rainfall, snowmelt, and runoff into a common body of water. The term \"watershed\" is often used interchangeably with \"drainage basin,\" which may make the concept easier to visualize. A watershed can encompass a small area of land that drains into a trickling creek. It can encompass multiple states in the Midwest, all draining into the Mississippi River. Or it can encompass multiple countries draining into the Atlantic Ocean. No matter where you are standing or sitting right now, you are in a watershed."),
    p("In the US, drainage basins are divided into successively smaller", strong("hydrological units"), "and given a numerical designation called a", em("hydrological unit code (HUC),"), "which for a given hydrological unit will contain the HUC code of the parent watershed plus two more digits to identify it within the parent watershed. Here we will focus on the major drainage basin (either the Mississippi or the Great Lakes), and their smaller subdivisions 'sub-basins' (HUC8), 'watersheds' (HUC10), and 'sub-watersheds' (HUC12). Wisconsin is divided into 52 sub-basins, 372 watersheds, and 1,808 sub-watersheds. Use the layers menu (upper right) in the map above or", strong(a(href = "#map", onclick = "Shiny.setInputValue('map-showWatersheds', 1, {priority: 'event'})", "click here")), "to enable these watershed boundaries on the map and explore them yourself."),
    uiOutput(ns("content"))
  )
}


# Server ----

#' Requires global variable `landscape_data`
#' @param `cur_stn` a `reactive()` expression containing the 1-line data frame `cur_stn()`

watershedInfoServer <- function(cur_stn) {
  moduleServer(
    id = "watershed",
    function(input, output, session) {
      ns <- session$ns


      ## Static vars ----

      mean_landscape <- landscape_data %>%
        group_by(huc_level, class_name, hex) %>%
        summarize(pct_area = mean(pct_area), .groups = "drop")

      watershed_sizes <- landscape_data %>%
        group_by(huc_level, huc) %>%
        summarize(area = mean(total_area), .groups = "drop_last") %>%
        summarize(area = mean(area)) %>%
        deframe()

      scale_choices <- list(
        "Sub-watershed (HUC12)" = 12,
        "Watershed (HUC10)" = 10,
        "Sub-basin (HUC8)" = 8
      )


      ## Reactive vars ----

      selected_data <- reactive({
        req(input$scale)
        col <- paste0("huc", input$scale)
        landscape_data %>%
          filter(huc == cur_stn()[[col]]) %>%
          arrange(class_name) %>%
          droplevels()
      })

      all_data <- reactive({
        req(input$scale)
        mean_landscape %>% filter(huc_level == input$scale)
      })

      selected_name <- reactive({
        req(input$scale)
        if (input$scale == 12) return(paste(cur_stn()$sub_watershed, "sub-watershed"))
        if (input$scale == 10) return(paste(cur_stn()$watershed, "watershed"))
        if (input$scale == 8) return(paste(cur_stn()$sub_basin, "sub-basin"))
      })

      all_name <- reactive({
        req(input$scale)
        if (input$scale == 12) return("All Wisconsin sub-watersheds")
        if (input$scale == 10) return("All Wisconsin watersheds")
        if (input$scale == 8) return("All Wisconsin sub-basins")
      })


      ## Layout ----

      output$content <- renderUI({
        stn <- cur_stn()
        maps_link <- sprintf("https://www.google.com/maps/search/?api=1&query=%s+%s", stn$latitude, stn$longitude)
        coords_html <- HTML(sprintf("%.6f, %.6f (<a href='%s' target='_blank'>View on Google Maps</a>)", stn$latitude, stn$longitude, maps_link))

        tagList(
          h4(strong("Station information and landscape context"), style = "margin-top: 1em;"),
          div(
            style = "padding-left: 1em;",
            strong("Selected station:"), stn$station_name, br(),
            strong("Coordinates:"), coords_html, br(),
            strong("Waterbody:"), sprintf("%s (WBIC: %s)", stn$waterbody, stn$wbic), br(),
            strong("Sub-watershed:"), sprintf("%s (HUC12: %s)", stn$sub_watershed, stn$huc12), br(),
            strong("Watershed:"), sprintf("%s (HUC10: %s)", stn$watershed, stn$huc10), br(),
            strong("Sub-basin:"), sprintf("%s (HUC8: %s)", stn$sub_basin, stn$huc8), br(),
            strong("Major basin: "), stn$major_basin, br(),
            strong("County name:"), stn$county_name, br(),
            strong("DNR region:"), stn$dnr_region
          ),
          uiOutput(ns("landscapes"))
        )
      })


      # Landscape composition plots ----

      output$landscapes <- renderUI({
        tagList(
          h4(strong("Landscape composition"), style = "margin-top: 1em;"),
          div(
            class = "well flex-row year-btns",
            div(class = "year-btn-text", em("Landscape scale:")),
            radioGroupButtons(
              inputId = ns("scale"),
              label = NULL,
              choices = scale_choices,
              selected = 12
            )
          ),
          div(
            class = "flex-row",
            div(class = "pie-container well", uiOutput(ns("selected_pie"))),
            div(class = "pie-container well", uiOutput(ns("all_pie")))
          )
        )
      })

      output$selected_pie <- renderUI({
        req(input$scale)
        area <- fmt_area(selected_data()$total_area[1])
        tagList(
          h5(align = "center", strong(selected_name())),
          plotlyOutput(ns("plot_selected"), height = "300px"),
          div(class = "plot-caption", "Drainage area:", area)
        )
      })

      output$all_pie <- renderUI({
        req(input$scale)
        area <- fmt_area(watershed_sizes[[as.character(input$scale)]])
        tagList(
          h5(align = "center", strong(all_name())),
          plotlyOutput(ns("plot_all"), height = "300px"),
          div(class = "plot-caption", "Average drainage:", area)
        )
      })


      # Pie chart helper ----

      make_plot <- function(df) {
        plot_ly(df) %>%
          add_trace(
            type = "pie",
            labels = ~class_name,
            values = ~pct_area,
            marker = list(
              colors = ~hex,
              line = list(color = "#fff", width = 0.5)
            ),
            textposition = "inside",
            texttemplate = "<b>%{label}</b><br>%{percent}",
            hovertemplate = "<b>%{label}</b><br>%{percent}<extra></extra>",
            sort = F
          ) %>%
          layout(
            showlegend = F,
            margin = list(l = 0, r = 0, t = 0, b = 0),
            paper_bgcolor = "rgba(0, 0, 0, 0)"
          ) %>%
          config(
            displayModeBar = F
          )
      }


      # Pie charts ----

      output$plot_selected <- renderPlotly({
        selected_data() %>% make_plot()
      })

      output$plot_all <- renderPlotly({
        all_data() %>% make_plot()
      })
    }
  )
}
