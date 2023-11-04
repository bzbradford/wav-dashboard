## THERMISTOR TAB ##


# UI ----

thermistorDataUI <- function() {
  ns <- NS("thermistor")

  div(
    class = "data-tab",
    uiOutput(ns("content")) %>% withSpinnerProxy(),
  )
}


# Server ----

#' requires global data frame 'therm_data'
#' @param cur_stn a `reactive()` expression containing the current station

thermistorDataServer <- function(cur_stn) {
  moduleServer(
    id = "thermistor",
    function(input, output, session) {
      ns <- session$ns


      # Reactives ----

      ## cur_data ----
      cur_data <- reactive({
        req(cur_stn())

        filter(therm_data, station_id == cur_stn()$station_id)
      })

      ## data_ready ----
      data_ready <- reactive({
        nrow(cur_data()) > 0
      })

      ## cur_years ----
      cur_years <- reactive({
        sort(unique(cur_data()$year))
      })

      ## selected_data ----
      selected_data <- reactive({
        if (input$year == "All") {
          cur_data()
        } else {
          cur_data() %>% filter(year == input$year)
        }
      })

      ## selected_data_ready ----
      selected_data_ready <- reactive({
        nrow(selected_data()) > 0
      })

      ## logger_serials ----
      logger_serials <- reactive({
        req(input$year)
        req(selected_data_ready())

        loggers <- selected_data() %>%
          count(year, logger_sn)

        if (nrow(loggers) == 1) {
          loggers$logger_sn
        } else {
          loggers %>%
            mutate(label = paste(year, logger_sn, sep = ": ")) %>%
            pull(label) %>%
            paste(collapse = " | ")
        }
      })

      ## daily_data ----
      # create station daily totals
      daily_data <- reactive({
        req(selected_data_ready())
        req(input$units)

        units <- input$units
        temp_col <- paste0("temp_", tolower(units))

        selected_data() %>%
          group_by(date) %>%
          summarise(
            hours = n(),
            min = min(!!sym(temp_col)),
            max = max(!!sym(temp_col)),
            mean = round(mean(!!sym(temp_col)), 2),
            units = units,
            lat = latitude[1],
            long = longitude[1]
          ) %>%
          mutate(
            station_id = cur_stn()$station_id,
            station_name = cur_stn()$station_name,
            .before = lat
          )
      })

      ## summary_data ----
      summary_data <- reactive({
        req(selected_data_ready())
        req(input$year)
        req(input$units)

        temp_col <- ifelse(input$units == "F", "temp_f", "temp_c")
        monthly <- selected_data() %>%
          mutate(temp = .[[temp_col]]) %>%
          arrange(month) %>%
          mutate(name = fct_inorder(format(date, "%B"))) %>%
          summarize(
            days = n_distinct(date),
            min = round(min(temp, na.rm = T), 1),
            q10 = round(quantile(temp, .1, na.rm = T), 1),
            mean = round(mean(temp, na.rm = T), 1),
            q90 = round(quantile(temp, .9, na.rm = T), 1),
            max = round(max(temp, na.rm = T), 1),
            .by = c(month, name)
          ) %>%
          clean_names("title")

        total <- selected_data() %>%
          mutate(temp = .[[temp_col]]) %>%
          summarize(
            name = "Total",
            days = n_distinct(date),
            min = round(min(temp, na.rm = T), 1),
            q10 = round(quantile(temp, .1, na.rm = T), 1),
            mean = round(mean(temp, na.rm = T), 1),
            q90 = round(quantile(temp, .9, na.rm = T), 1),
            max = round(max(temp, na.rm = T), 1)
          ) %>%
          clean_names("title")

        bind_rows(monthly, total)
      })


      # Layout ----

      ## content ----
      output$content <- renderUI({
        if (!data_ready()) {
          return(div(class = "well", "This station has no thermistor data. Choose another station or view the baseline or nutrient data associated with this station."))
        }

        tagList(
          div(
            class = "well flex-row year-btns",
            div(class = "year-btn-text", em("Choose year:")),
            radioGroupButtons(
              inputId = ns("year"),
              label = NULL,
              choices = year_choices(cur_years()),
              selected = last(cur_years())
            )
          ),
          uiOutput(ns("plotOptionsUI")),
          div(
            id = "therm-plot-container",
            h3(cur_stn()$label, align = "center"),
            plotlyOutput(ns("plot")) %>% withSpinnerProxy(hide.ui = FALSE),
            uiOutput(ns("plotCaptionUI"))
          ),
          uiOutput(ns("plotExportUI")),
          h4("What does water temperature tell us?"),
          p("Temperature is often referred to as a 'master variable' in aquatic ecosystems because temperature determines the speed of important processes, from basic chemical reactions to the growth and metabolism of fishes and other organisms. In addition, temperature determines the type of fish community that the stream can support. It is especially important to gather stream temperature information over many years in order to track how quickly our streams are warming due to climate change. The continuous data loggers you have deployed and maintained are a 21st-century approach to monitoring stream temperature, providing accurate, high-resolution temperature data as we monitor the health of streams across the state."),
          p("Tips for understanding and interacting with the temperature plot:"),
          tags$ul(
            tags$li("Hover over the chart to see hourly and daily temperature measurement details."),
            tags$li("Click on the legend to show / hide the time series of hourly or daily temperature."),
            tags$li("Drag the mouse over a time period to zoom in, and double click to return to the original view."),
            tags$li("While hovering over the plot, click the camera icon along the top to download a picture of the plot."),
            tags$li("Anomalous temperature readings at the very beginning and end of the data may reflect air temperatures before the logger was deployed into the stream. It's also possible that the logger because exposed to the air during deployment if water levels dropped.")
          ),
          br(),
          bsCollapse(
            bsCollapsePanel(
              title = "View monthly water temperature summary",
              dataTableOutput(ns("stnSummaryDT")),
              uiOutput(ns("stnSummaryFootnoteUI"))
            )
          ),
          bsCollapse(
            bsCollapsePanel(
              title = "View or download stream temperature data",
              uiOutput(ns("viewDataUI"))
            )
          )
        )
      })

      # Plot ----

      ## plotOptionsUI ----
      output$plotOptionsUI <- renderUI({
        list(
          p(
            div(
              style = "float: left; margin-right: 1em;",
              strong("Temperature units:")
            ),
            radioButtons(
              inputId = ns("units"),
              label = NULL,
              inline = T,
              choices = list("Fahrenheit" = "F", "Celsius" = "C")
            ),
            div(
              style = "float: left; margin-right: 1em;",
              strong("Optional plot annotations:")
            ),
            radioButtons(
              inputId = ns("annotations"),
              label = NULL,
              inline = T,
              choices = list(
                "Brook trout temperature range" = "btrout",
                "Warm/cool/coldwater classification" = "wtemp",
                "None"
              )
            )
          ),
          hr()
        )
      })


      ## plot ----
      output$plot <- renderPlotly({
        req(selected_data_ready())
        req(input$units)
        req(input$annotations)

        units <- input$units
        annotation <- input$annotations

        df_daily <- daily_data() %>%
          mutate(date_time = as.POSIXct(paste(date, "12:00:00")))
        df_hourly <- selected_data()

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
            title = "Stream Temperature",
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
          ) %>%
          config(displayModeBar = F)

        # add annotation color bands
        if (annotation != "None") {
          if (annotation == "btrout") {
            temps <- c(32, 52, 61, 72, 100) # F
            if (units == "C") temps <- f_to_c(temps)
            colors <- c("cornflowerblue", "green", "lightgreen", "darkorange")
          } else if (annotation == "wtemp") {
            temps <- c(32, 72, 77, 100) # F
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

      ## plotCaptionUI ----
      output$plotCaptionUI <- renderUI({
        req(input$units)
        req(input$annotations)

        units <- input$units
        unit_text <- paste0("°", units)
        annotation <- input$annotations

        overlay_caption <- ""

        if (annotation == "btrout") {
          temps <- c(52, 61, 72)
          if (units == "C") temps <- f_to_c(temps)

          overlay_caption <- paste0(
            "Optimal brook trout temperatures are shown shaded ", colorize("dark green", "darkgreen"),
            " (", temps[1], "-", temps[2], unit_text, "), acceptable temperatures in ", colorize("light green", "darkseagreen"),
            " (", temps[2], "-", temps[3], unit_text, "), too hot in ", colorize("orange", "orange"),
            " and too cold in ", colorize("blue", "blue"), ".")

        } else if (annotation == "wtemp") {
          temps <- c(72, 77)
          if (units == "C") temps <- f_to_c(temps)

          overlay_caption <- paste0(
            "The DNR classifies streams as ", colorize("coldwater", "blue"), " when maximum summer temperatures are below ",
            temps[1], unit_text, ", as ", colorize("coolwater", "deepskyblue"), " streams when maximum temperatures are between ",
            temps[1], " and ", temps[2], unit_text, ", and as ", colorize("warmwater", "orange"),
            " streams when maximum temperatures are above ", temps[2], unit_text, ".")
        }

        p(
          style = "margin-left: 2em; margin-right: 2em; font-size: smaller;",
          em(HTML(paste(overlay_caption, "High or widely fluctuating temperatures may indicate that the logger became exposed to the air, either before/after deployment, or when stream levels dropped below the point where the logger was anchored.")))
        )
      })

      ## plotExportUI ----
      output$plotExportUI <- renderUI({
        req(input$year)

        p(
          style = "margin-left: 2em; margin-right: 2em; font-size: smaller;",
          align = "center",
          em(
            a("Click here to download this plot as a PNG.",
              style = "cursor: pointer;",
              onclick = paste0(
                "html2canvas(document.querySelector('",
                "#therm-plot-container",
                "'), {scale: 3}).then(canvas => {saveAs(canvas.toDataURL(), '",
                paste("thermistor-plot", cur_stn()$station_id, input$year, sep = "-"),
                ".png",
                "')})"
              )
            )
          )
        )
      })


      # Summary ----

      ## stnSummaryDT ----
      output$stnSummaryDT <- renderDT(
        summary_data(),
        rownames = F,
        options = list(
          dom = "t",
          columnDefs = list(list(targets = 0:7, className = "dt-center"))
        )
      )

      ## stnSummaryFootnoteUI ----
      output$stnSummaryFootnoteUI <- renderUI({
        req(input$units)

        div(
          style = "margin-top: 0.5em",
          em(paste0("Temperatures shown in units of °", input$units, "."), "Q10 and Q90 reflect the 10th percentile and 90th percentile temperatures, respectively.")
        )
      })


      # View thermistor data table ----

      ## viewDataUI ----
      output$viewDataUI <- renderUI({
        req(selected_data_ready())

        min_date <- min(selected_data()$date)
        max_date <- max(selected_data()$date)

        tagList(
          p(
            strong("Station ID:"), cur_stn()$station_id, br(),
            strong("Station Name:"), cur_stn()$station_name, br(),
            strong("Waterbody:"), cur_stn()$waterbody, br(),
            strong("Date range:"),
            paste0(
              format(min_date, "%B %d"), " - ",
              format(max_date, "%B %d, %Y"),
              " (", max_date - min_date, " days)"
            ), br(),
            strong("Logger SN:"), logger_serials()
          ),
          tabsetPanel(
            tabPanel(
              title = "Daily temperature data",
              class = "data-tab",
              p(downloadButton(ns("downloadDaily"), "Download this data")),
              renderDataTable({
                clean_names(daily_data(), case = "big_camel")
              })
            ),
            tabPanel(
              title = "Hourly temperature data",
              class = "data-tab",
              p(downloadButton(ns("downloadHourly"), "Download this data")),
              renderDataTable({
                clean_names(selected_data(), case = "big_camel")
              })
            )
          )
        )
      })

      ## downloadDaily ----
      output$downloadDaily <- downloadHandler(
        paste0("stn-", cur_stn()$station_id, "-therm-daily-data-", input$year, ".csv"),
        function(file) {write_csv(daily_data(), file)}
      )

      ## downloadDaily ----
      output$downloadHourly <- downloadHandler(
        paste0("stn-", cur_stn()$station_id, "-therm-hourly-data-", input$year, ".csv"),
        function(file) {write_csv(selected_data(), file)}
      )


      # Return values ----
      return(reactive(list(year = input$year)))
    }
  )
}
