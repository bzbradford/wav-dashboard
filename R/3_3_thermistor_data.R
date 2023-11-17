## THERMISTOR TAB ##

thermistorDataUI <- function() {
  ns <- NS("thermistor")

  div(
    class = "data-tab",
    uiOutput(ns("content")) %>% withSpinnerProxy(),
  )
}


#' requires global data frame 'therm_data'
#' @param cur_stn a `reactive()` expression containing the current station

thermistorDataServer <- function(cur_stn, has_focus) {
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
        req(input$year)

        if (input$year == "All") return(cur_data())
        cur_data() %>% filter(year == input$year)
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
        req(cur_stn())
        req(input$units)

        createDailyThermData(selected_data(), input$units, cur_stn())
      })

      ## summary_data ----
      summary_data <- reactive({
        req(selected_data_ready())
        req(input$units)

        createThermSummary(selected_data(), input$units)
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
          uiOutput(ns("moreInfoUI")),
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

      output$moreInfoUI <- renderUI({
        includeMarkdown("md/thermistor_info.md")
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
                "None" = "none"
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

        makeThermistorPlot(selected_data(), daily_data(), input$units, input$annotations)
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
