# Watershed Information

watershedInfoUI <- function() {
  ns <- NS("watershed")
  link1 <- a("2021 USGS National Land Cover Database", href = "https://www.usgs.gov/centers/eros/science/national-land-cover-database", target = "_blank", .noWS = "outside")
  link2 <- a("Click here", href = "https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description", target = "_blank", .noWS = "outside")

  div(
    class = "data-tab",
    h3("Watersheds"),
    p(strong("What is a watershed?"), "NOAA defines a watershed as an area of land that channels rainfall, snowmelt, and runoff into a common body of water. The term \"watershed\" is often used interchangeably with \"drainage basin,\" which may make the concept easier to visualize. A watershed can encompass a small area of land that drains into a trickling creek. It can encompass multiple states in the Midwest, all draining into the Mississippi River. Or it can encompass multiple countries draining into the Atlantic Ocean. No matter where you are standing or sitting right now, you are in a watershed."),
    p(HTML("In the US, watersheds are divided into successively smaller areas called <em>hydrological units</em> and given a numerical designation called a <em>hydrological unit code</em> (HUC). These HUCs have a specific number of digits for each level of division. For example, Wisconsin is divided into 52 <em>sub-basins</em> (8 digit HUC), 372 <em>watersheds</em> (10 digit HUC), and 1,808 <em>sub-watersheds</em> (12 digit HUC). Use the layers menu (upper right) in the map above or"), strong(a(href = "#map", onclick = "Shiny.setInputValue('map-show_watersheds', true, {priority: 'event'})", "click here")), "to enable these watershed boundaries on the map and explore them yourself."),
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
        req(has_focus())
        req(input$scale)

        col <- paste0("huc", input$scale)
        landscape_data %>%
          filter(huc == cur_stn()[[col]]) %>%
          arrange(class_name) %>%
          droplevels()
      })

      ## all_data ----
      all_data <- reactive({
        req(input$scale)
        mean_landscape %>% filter(huc_level == input$scale)
      })

      ## landscape_diff ----
      landscape_diff <- reactive({
        all_data() %>%
          left_join(select(selected_data(), class_name, pct_area2 = pct_area), by = "class_name") %>%
          replace_na(list(pct_area2 = 0)) %>%
          mutate(diff = pct_area2 - pct_area) %>%
          mutate(label = scales::percent(diff, .1)) %>%
          mutate(label = if_else(substr(label, 1, 1) == "-", label, paste0("+", label))) %>%
          mutate(label_pos = -1 * sign(diff) * .00001) %>%
          mutate(hovertext = paste0(
            "Current watershed: ",
            scales::percent(pct_area2, .1),
            "<br>State average: ",
            scales::percent(pct_area, .1),
            "<br>Difference: ",
            label)
          ) %>%
          droplevels()
      })

      ## selected_name ----
      selected_name <- reactive({
        req(input$scale)
        if (input$scale == 12) return(paste(cur_stn()$sub_watershed, "sub-watershed"))
        if (input$scale == 10) return(paste(cur_stn()$watershed, "watershed"))
        if (input$scale == 8) return(paste(cur_stn()$sub_basin, "sub-basin"))
      })

      ## all_name ----
      all_name <- reactive({
        req(input$scale)
        if (input$scale == 12) return("All Wisconsin sub-watersheds")
        if (input$scale == 10) return("All Wisconsin watersheds")
        if (input$scale == 8) return("All Wisconsin sub-basins")
      })


      # Layout ----

      ## watershedInfoUI ----
      output$watershedInfoUI <- renderUI({
        wellPanel(
          h4("Watershed context for selected station:", style = "margin-top: 0px;"),
          div(
            style = "padding-left: 1em;",
            HTML(fmt_watershed_info(cur_stn()))
          )
        )
      })

      ## landscapePlotsUI ----
      output$landscapePlotsUI <- renderUI({
        tagList(

          div(id = "landscape-plot-container",
            div(class = "flex-row",
              div(class = "pie-container well", uiOutput(ns("curPlotUI"))),
              div(class = "pie-container well", uiOutput(ns("allPlotUI"))),
            ),
            uiOutput(ns("diffPlotUI")),
          ),
          uiOutput(ns("plotExportUI")),
        )
      })

      ## curPlotUI ----
      output$curPlotUI <- renderUI({
        req(input$scale)
        area <- fmt_area(selected_data()$total_area[1])
        tagList(
          h5(align = "center", strong(selected_name())),
          plotlyOutput(ns("curPlot"), height = "300px"),
          div(class = "plot-caption", "Drainage area:", area)
        )
      })

      ## allPlotUI ----
      output$allPlotUI <- renderUI({
        req(input$scale)
        area <- fmt_area(watershed_sizes[[as.character(input$scale)]])
        tagList(
          h5(align = "center", strong(all_name())),
          plotlyOutput(ns("allPlot"), height = "300px"),
          div(class = "plot-caption", "Average drainage:", area)
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
        p(
          style = "margin-left: 2em; margin-right: 2em; font-size: smaller;",
          align = "center",
          em(a("Click here to download the landscape plots above as a PNG.",
            style = "cursor: pointer;",
            onclick = paste0(
              "html2canvas(document.querySelector('",
              "#landscape-plot-container",
              "'), {scale: 3}).then(canvas => {saveAs(canvas.toDataURL(), '",
              "Landscape composition - ", selected_name(), ".png",
              "')})"
            )
          )
          )
        )
      })


      # Plots ----

      ## Pie chart helper ----
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

      ## curPlot ----
      output$curPlot <- renderPlotly({
        selected_data() %>% make_plot()
      })

      ## allPlot ----
      output$allPlot <- renderPlotly({
        all_data() %>% make_plot()
      })

      ## diffPlot ----
      # shows a barplot of the difference between the current watershed and the state average
      output$diffPlot <- renderPlotly({
        df <- landscape_diff()
        xrange <- with(df, c(min(diff) * 1.2, max(diff) * 1.2))
        df %>%
          plot_ly() %>%
          add_bars(
            y = ~class_name,
            x = ~label_pos,
            marker = list(
              opacity = 0
            ),
            text = ~class_name,
            textposition = "outside",
            texttemplate = "<b>%{text}</b>",
            hoverinfo = "none"
          ) %>%
          add_bars(
            y = ~class_name,
            x = ~diff,
            text = ~label,
            marker = list(
              opacity = 0
            ),
            textposition = "outside",
            texttemplate = "<b>%{text}</b>"
          ) %>%
          add_bars(
            y = ~class_name,
            x = ~diff,
            text = ~hovertext,
            marker = list(
              color = ~hex,
              line = list(color = "#000", width = 1)
            ),
            textposition = "none",
            hovertemplate = "<b>%{y}<br></b>%{text}<extra></extra>"
          ) %>%
          layout(
            barmode = "overlay",
            xaxis = list(
              title = "Difference from state average",
              tickformat = ",.0%",
              ticks = "outside",
              fixedrange = T,
              range = xrange,
              zerolinewidth = 1.5
            ),
            yaxis = list(
              visible = F,
              fixedrange = T
            ),
            showlegend = F,
            margin = list(l = 10, r = 10),
            plot_bgcolor = "rgba(0, 0, 0, 0)",
            paper_bgcolor = "rgba(0, 0, 0, 0)"
          ) %>%
          config(displayModeBar = F)
      })


      # Return values ----
      return(reactive(list(huc = paste0("HUC", input$scale))))
    }
  )
}
