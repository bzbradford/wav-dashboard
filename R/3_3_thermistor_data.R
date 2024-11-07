### Thermistor Data Tab ###


# Functions ---------------------------------------------------------------

createDailyThermData <- function(df, units, stn) {
  temp_col <- ifelse(tolower(units) == "f", "temp_f", "temp_c")

  df %>%
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
      station_id = stn$station_id,
      station_name = stn$station_name,
      .before = lat
    )
}

createThermSummary <- function(df, units) {
  temp_col <- ifelse(tolower(units) == "f", "temp_f", "temp_c")

  daily <- df %>%
    mutate(temp = .[[temp_col]]) %>%
    drop_na(temp) %>%
    arrange(date) %>%
    summarize(temp = mean(temp), .by = c(date, month))

  monthly <- daily %>%
    mutate(name = fct_inorder(format(date, "%B"))) %>%
    summarize(
      days = n_distinct(date),
      min = round(min(temp), 1),
      mean = round(mean(temp), 1),
      max = round(max(temp), 1),
      .by = c(month, name)
    ) %>%
    clean_names("title")

  total <- daily %>%
    summarize(
      name = "Total",
      days = n_distinct(date),
      min = round(min(temp), 1),
      mean = round(mean(temp), 1),
      max = round(max(temp), 1)
    ) %>%
    clean_names("title")

  bind_rows(monthly, total)
}



# Static UI ---------------------------------------------------------------

thermistorDataUI <- function() {
  ns <- NS("thermistor")

  div(
    class = "data-tab",
    uiOutput(ns("content")) %>% withSpinnerProxy(),
  )
}



# Server ------------------------------------------------------------------

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

        req(cur_stn())
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
            uiOutput(ns("plotCaptionUI")),
            uiOutput(ns("naturalCommunityUI"))
          ),
          uiOutput(ns("plotExportUI")),
          uiOutput(ns("moreInfoUI")),
          br(),
          bsCollapse(
            bsCollapsePanel(
              title = "View or download monthly, daily, and hourly temperature data",
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
                "Warm/cool/coldwater classification" = "wtemp",
                "Brook trout temperature range" = "btrout",
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

        df_hourly <- selected_data()
        df_daily <- daily_data()

        # insert rows to break plotly lines across years
        # ribbon can't handle NA values to min/max are pinched to = mean
        if (input$year == "All") {
          df_daily <- df_daily %>%
            mutate(days_to_next = as.numeric(lead(date) - date)) %>%
            mutate(days_since_last = as.numeric(date - lag(date)))
          gap_starts <- df_daily %>%
            filter(days_to_next > 7) %>%
            mutate(date = date + 1, min = mean, max = mean, mean = NA)
          gap_ends <- df_daily %>%
            filter(days_since_last > 7) %>%
            mutate(date = date - 1, min = mean, max = mean, mean = NA)
          df_daily <- df_daily %>%
            bind_rows(gap_starts, gap_ends) %>%
            arrange(date)
          df_hourly <- df_hourly %>%
            bind_rows(gap_starts, gap_ends) %>%
            arrange(date)
        }

        makeThermistorPlot(df_hourly, df_daily, input$units, input$annotations)
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

          overlay_caption <- HTML(paste0(
            "Optimal brook trout temperatures are shown shaded ", colorize("dark green", "darkgreen"),
            " (", temps[1], "-", temps[2], unit_text, "), acceptable temperatures in ", colorize("light green", "darkseagreen"),
            " (", temps[2], "-", temps[3], unit_text, "), too hot in ", colorize("orange", "orange"),
            " and too cold in ", colorize("blue", "blue"), "."))

        } else if (annotation == "wtemp") {
          temps <- c(69.3, 72.5, 76.3)
          if (units == "C") temps <- f_to_c(temps)

          overlay_caption <- HTML(paste0(
            "The DNR classifies streams into four 'Natural Community' types based on their maximum daily average temperature: ",
            colorize("coldwater", "blue"), " when below 69.3°F (20.7°C); ",
            colorize("cool-cold", "cornflowerblue"), " when between 69.3 and 72.5°F (20.7 and 22.5°C); ",
            colorize("cool-warm", "lightsteelblue"), " when between 72.5 and 76.3°F (22.5 and 24.6°C); and ",
            colorize("warmwater", "darkorange"), " when above 76.3°F (24.6°C)."
          ))
        }

        p(
          class = "plot-caption",
          overlay_caption,
          "High or widely fluctuating temperatures may indicate that the logger became exposed to the air."
        )
      })

      ## naturalCommunityUI ----
      output$naturalCommunityUI <- renderUI({
        req(nrow(daily_data()) > 0)

        max_temp <- max(daily_data()$mean)
        if (input$units == "C") max_temp <- c_to_f(max_temp)
        temp_class <- case_when(
          max_temp < 69.3 ~ "coldwater",
          max_temp < 72.5 ~ "cool-cold",
          max_temp < 76.3 ~ "cool-warm",
          T ~ "warmwater"
        )
        p(
          class = "plot-caption", style = "font-weight: bold;",
          paste0("Based on the maximum daily average water temperature of ", round(max_temp, 1), "°F (", round(f_to_c(max_temp), 1), "°C), this is likely to be a ", temp_class, " stream.")
        )
      })

      ## plotExportUI ----
      output$plotExportUI <- renderUI({
        req(input$year)
        filename <- sprintf("WAV thermistor data - Stn %s - %s.png", cur_stn()$station_id, input$year)
        buildPlotDlBtn("#therm-plot-container", filename)
      })


      # View summary and raw data tables ----

      ## viewDataUI ----
      output$viewDataUI <- renderUI({
        req(selected_data_ready())
        req(input$units)

        dates <- unique(selected_data()$date)
        date_span <- as.numeric(max(dates) - min(dates)) + 1
        monthly_dt <- summary_data() %>%
          mutate(across(c(Min, Mean, Max), ~sprintf("%.1f %s", .x, input$units))) %>%
          renderDataTable(
            rownames = F,
            extensions = "Buttons",
            options = list(
              dom = "Bt",
              buttons = c("copy"),
              columnDefs = list(
                list(targets = 0:5, className = "dt-center")
              )
            )
          )
        daily_dt <- daily_data() %>%
          clean_names("big_camel") %>%
          renderDataTable()
        hourly_dt <- selected_data() %>%
          clean_names("big_camel") %>%
          renderDataTable()

        tagList(
          p(
            strong("Station ID:"), cur_stn()$station_id, br(),
            strong("Station Name:"), cur_stn()$station_name, br(),
            strong("Waterbody:"), cur_stn()$waterbody, br(),
            strong("Date range:"),
            paste(
              format(
                min(dates),
                ifelse(year(min(dates)) == year(max(dates)), "%B %d", "%B %d, %Y")
              ), "-",
              format(max(dates), "%B %d, %Y")
            ),
            br(),
            strong("Coverage:"),
            sprintf(
              "%s calendar days, %s with data (%.0f%%)",
              date_span,
              length(dates),
              length(dates) / date_span * 100
            ),
            br(),
            strong("Logger SN:"), logger_serials()
          ),
          tabsetPanel(
            tabPanel(
              title = "Monthly temperature data",
              class = "data-tab",
              p(paste0("To limit the influence of hourly temperature fluctuations, daily average temperatures are used to generate these monthly summaries. Temperatures are shown in units of °", input$units, ", option to change units is above the plot.")),
              monthly_dt,
            ),
            tabPanel(
              title = "Daily temperature data",
              class = "data-tab",
              p(downloadButton(ns("downloadDaily"), "Download this data")),
              daily_dt
            ),
            tabPanel(
              title = "Hourly temperature data",
              class = "data-tab",
              p("The DateTime associated with each hourly observation below is in UTC time, but the Hour column reflects the local time at the logger (timezone: America/Chicago)."),
              p(downloadButton(ns("downloadHourly"), "Download this data")),
              hourly_dt
            )
          )
        )
      })

      ## downloadDaily ----
      output$downloadDaily <- downloadHandler(
        sprintf("WAV Stn %s Daily Temperature Data (%s).csv", cur_stn()$station_id, input$year),
        function(file) { write_csv(daily_data(), file, na = "") }
      )

      ## downloadDaily ----
      output$downloadHourly <- downloadHandler(
        sprintf("WAV Stn %s Hourly Temperature Data (%s).csv", cur_stn()$station_id, input$year),
        function(file) { write_csv(selected_data(), file, na = "")}
      )


      # Return values ----
      return(reactive(list(year = input$year)))
    }
  )
}
