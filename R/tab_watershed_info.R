# Watershed Information


# UI ----

watershedInfoUI <- function() {
  ns <- NS("watershed")

  div(
    class = "data-tab",
    h3("Watersheds"),
    p("In the US, watersheds are divided and subdivided into successively smaller hydrological units and given a numerical designation called a hydrological unit code (HUC) that has a specific number of digits for each level. Here we will focus on the major drainage basin (either the Mississippi or the Great Lakes), and their smaller subdivisions 'sub-basins' (HUC8), 'watersheds' (HUC10), and 'sub-watersheds' (HUC12). A HUC8 watershed code number has 8 digits, HUC10 has 10 digits, and HUC12 has 12 digits, such that each smaller division simply adds two additional digits to its parent watershed number."),
    p("Wisconsin is divided into 52 sub-basins, 372 watersheds, and 1,808 sub-watersheds. The average sub-basin size is 3,476 sq km (2500 sq mi), the average watershed size is 812 sq km (314 sq mi), and the average sub-watershed size is 162 sq km (62 sq mi). Use the layers menu (upper right) in the map above or", strong(a(href = "#map", onclick = "Shiny.setInputValue('map-showWatersheds', 1, {priority: 'event'})", "click here")), "to enable these watershed boundaries on the map and explore them yourself."),
    uiOutput(ns("content"))
  )
}


# Server ----

#' Requires global variable `landscape_data`
#' @param `cur_stn` a `reactive()` expression containing the 1-line data frame `cur_stn()`

mean_landscape <- landscape_data %>%
  group_by(huc_level, class_name, hex) %>%
  summarize(pct_area = mean(pct_area), .groups = "drop")

watershed_sizes <- landscape_data %>%
  group_by(huc_level, huc) %>%
  summarize(area = mean(total_area), .groups = "drop_last") %>%
  summarize(area = mean(area)) %>%
  deframe()

watershedInfoServer <- function(cur_stn) {
  moduleServer(
    id = "watershed",
    function(input, output, session) {
      ns <- session$ns

      this_huc12_data <- reactive({
        landscape_data %>%
          filter(huc == cur_stn()$huc12) %>%
          arrange(class_name) %>%
          droplevels()
      })

      this_huc10_data <- reactive({
        landscape_data %>%
          filter(huc == cur_stn()$huc10) %>%
          arrange(class_name) %>%
          droplevels()
      })

      this_huc8_data <- reactive({
        landscape_data %>%
          filter(huc == cur_stn()$huc8) %>%
          arrange(class_name) %>%
          droplevels()
      })

      cur_watershed_sizes <- reactive({
        c(
          "8" = first(this_huc8_data()$total_area),
          "10" = first(this_huc10_data()$total_area),
          "12" = first(this_huc12_data()$total_area)
        )
      })


      ## Layout ----

      output$content <- renderUI({
        stn <- cur_stn()

        tagList(
          h4(strong("Station context")),
          div(
            style = "padding-left: 1em;",
            strong("Selected station:"), stn$station_name, br(),
            strong("HUC12 sub-watershed:"), stn$sub_watershed, br(),
            strong("HUC10 watershed:"), stn$watershed, br(),
            strong("HUC8 sub-basin:"), stn$sub_basin, br(),
            strong("Major basin: "), stn$major_basin
          ),
          uiOutput(ns("landscapes"))
        )
      })


      # Landscape composition plots ----

      output$landscapes <- renderUI({
        tagList(
          h4(strong("Landscape composition")),
          div(
            class = "flex-row",
            div(
              class = "pie-container well",
              div(
                class = "pie",
                h5(align = "center", strong(cur_stn()$sub_watershed, "sub-watershed")),
                plotlyOutput(ns("this_huc12"), height = "250px"),
                div(
                  class = "plot-caption",
                  "Drainage area:", fmt_area(cur_watershed_sizes()[["12"]]), br(),
                  "State average:", fmt_area(watershed_sizes[["12"]])
                )
              )
            ),
            div(
              class = "pie-container well",
              div(
                class = "pie",
                h5(align = "center", strong(cur_stn()$watershed, "watershed")),
                plotlyOutput(ns("this_huc10"), height = "250px"),
                div(
                  class = "plot-caption",
                  "Drainage area:", fmt_area(cur_watershed_sizes()[["10"]]), br(),
                  "State average:", fmt_area(watershed_sizes[["10"]])
                )
              )
            ),
            div(
              class = "pie-container well",
              div(
                class = "pie",
                h5(align = "center", strong(cur_stn()$sub_basin, "sub-basin")),
                plotlyOutput(ns("this_huc8"), height = "250px"),
                div(
                  class = "plot-caption",
                  "Drainage area:", fmt_area(cur_watershed_sizes()[["8"]]), br(),
                  "State average:", fmt_area(watershed_sizes[["8"]])
                )
              )
            ),
            div(
              class = "pie-container well",
              div(
                class = "pie",
                h5(align = "center", strong("All Wisconsin watersheds")),
                plotlyOutput(ns("all_huc10"), height = "250px")
              )
            )
          )
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

      output$this_huc12 <- renderPlotly({
        make_plot(this_huc12_data())
      })

      output$this_huc10 <- renderPlotly({
        make_plot(this_huc10_data())
      })

      output$this_huc8 <- renderPlotly({
        make_plot(this_huc8_data())
      })

      output$all_huc10 <- renderPlotly({
        mean_landscape %>%
          filter(huc_level == 10) %>%
          arrange(class_name) %>%
          droplevels() %>%
          make_plot()
      })

    }
  )
}
