##  THERMISTOR TAB  ##

thermistorDataUI <- function() {
  ns <- NS("thermistor")

  div(
    class = "data-tab",
    uiOutput(ns("ui")) %>% with_spinner(),
  )
}

#' @requires `therm_data`
#' @param main_rv reactive values object from main server session
thermistorDataServer <- function(main_rv) {
  moduleServer(
    id = "thermistor",
    function(input, output, session) {
      ns <- session$ns


      # Reactives ---------------------------------------------------------------

      rv <- reactiveValues(
        ready = FALSE
      )

      ## rv$ready handler ----
      observe({
        ready <- nrow(stn_data()) > 0
        if (rv$ready != ready) rv$ready <- ready
      })

      ## cur_stn ----
      cur_stn <- reactive({
        req(main_rv$cur_stn)
      })

      ## stn_data ----
      stn_data <- reactive({
        therm_data %>%
          filter(station_id == cur_stn()$station_id)
      })

      ## selected_data ----
      selected_data <- reactive({
        df <- stn_data()
        yr <- req(input$year)
        df <- if (yr == "All") df else filter(df, year == yr)
        req(nrow(df) > 0)
        df
      })

      ## logger_serials ----
      logger_serials <- reactive({
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
        build_therm_daily(
          df = selected_data(),
          units = req(input$units),
          stn = cur_stn()
        )
      })

      ## summary_data ----
      summary_data <- reactive({
        build_therm_summary(
          df = selected_data(),
          units = req(input$units)
        )
      })



      # Interface ---------------------------------------------------------------

      ## ui ----
      output$ui <- renderUI({
        if (rv$ready) {
          uiOutput(ns("main_ui"))
        } else {
          div(class = "well", "This station has no thermistor data. Choose another station or view the baseline or nutrient data associated with this station.")
        }
      })

      ## main_ui ----
      output$main_ui <- renderUI({
        tagList(
          uiOutput(ns("year_select_ui")),
          uiOutput(ns("plot_opts_ui")),
          div(
            id = "therm-plot-container",
            h3(textOutput(ns("stn_title")), align = "center"),
            plotlyOutput(ns("plot")) %>% with_spinner(hide.ui = FALSE),
            uiOutput(ns("plot_caption_ui")),
            uiOutput(ns("natural_community_ui"))
          ),
          div(
            style = "display: flex; flex-direction: row-reverse;",
            uiOutput(ns("plot_dl_btn"))
          ),
          div(
            style = "margin-top: 1em; margin-bottom: 2em;",
            includeMarkdown("md/thermistor_info.md")
          ),
          h3("Station data summary and downloads"),
          uiOutput(ns("stn_data_ui"))
        )
      })

      output$stn_title <- renderText({
        cur_stn()$label
      })

      ## Year selector ----
      output$year_select_ui <- renderUI({
        yrs <- sort(unique(stn_data()$year))
        div(
          class = "well flex-row year-btns",
          div(class = "year-btn-text", em("Choose year:")),
          radioGroupButtons(
            inputId = ns("year"),
            label = NULL,
            choices = year_choices(yrs),
            selected = first_truthy(
              intersect(isolate(input$year), yrs),
              last(yrs)
            )
          )
        )
      })


      # Plot -------------------------------------------------------------------

      ## plot_opts_ui----
      output$plot_opts_ui <- renderUI({
        tagList(
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
            )
          ),
          p(
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
        df_hourly <- selected_data()
        df_daily <- daily_data()
        units <- req(input$units)
        annot <- req(input$annotations)

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

        plotly_thermistor(df_hourly, df_daily, units, annot)
      })

      ## plot_caption_ui ----
      output$plot_caption_ui <- renderUI({
        units <- req(input$units)
        annotation <- req(input$annotations)
        unit_text <- paste0("°", units)
        caption <- ""

        if (annotation == "btrout") {
          temps <- c(52, 61, 72)
          if (units == "C") temps <- f_to_c(temps)

          caption <- HTML(paste0(
            "Optimal brook trout temperatures are shown shaded ", colorize("dark green", "darkgreen"),
            " (", temps[1], "-", temps[2], unit_text, "), acceptable temperatures in ", colorize("light green", "darkseagreen"),
            " (", temps[2], "-", temps[3], unit_text, "), too hot in ", colorize("orange", "orange"),
            " and too cold in ", colorize("blue", "blue"), "."
          ))
        } else if (annotation == "wtemp") {
          temps <- c(69.3, 72.5, 76.3)
          if (units == "C") temps <- f_to_c(temps)

          caption <- HTML(paste0(
            "The DNR classifies streams into four 'Natural Community' types based on their maximum daily average temperature: ",
            colorize("coldwater", "blue"), " when below 69.3°F (20.7°C); ",
            colorize("cool-cold", "cornflowerblue"), " when between 69.3 and 72.5°F (20.7 and 22.5°C); ",
            colorize("cool-warm", "lightsteelblue"), " when between 72.5 and 76.3°F (22.5 and 24.6°C); and ",
            colorize("warmwater", "darkorange"), " when above 76.3°F (24.6°C)."
          ))
        }

        p(
          class = "plot-caption",
          caption,
          "High or widely fluctuating temperatures may indicate that the logger became exposed to the air."
        )
      })

      ## natural_community_ui ----
      output$natural_community_ui <- renderUI({
        df <- daily_data()
        max_temp <- max(df$mean)
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

      ## plot_dl_btn ----
      output$plot_dl_btn <- renderUI({
        yr <- req(input$year)
        stn <- cur_stn()

        build_plot_download_btn(
          id = "#therm-plot-container",
          filename = sprintf("WAV thermistor data - Stn %s - %s.png", stn$station_id, yr)
        )
      })


      # View summary and raw data tables ----

      ## stn_data_ui ----
      output$stn_data_ui <- renderUI({
        stn <- cur_stn()
        data <- selected_data()
        units <- req(input$units)
        dates <- unique(data$date)
        date_span <- as.numeric(max(dates) - min(dates)) + 1

        monthly_dt <- summary_data() %>%
          mutate(across(c(Min, Mean, Max), ~ sprintf("%.1f %s", .x, input$units))) %>%
          renderDataTable(
            rownames = FALSE,
            extensions = "Buttons",
            options = list(
              dom = "Bt",
              buttons = c("copy"),
              columnDefs = list(
                list(targets = 0:5, className = "dt-center")
              ),
              scrollX = TRUE
            )
          )

        daily_dt <- daily_data() %>%
          clean_names("big_camel") %>%
          renderDataTable(
            options = list(
              scrollX = TRUE
            )
          )

        hourly_dt <- selected_data() %>%
          clean_names("big_camel") %>%
          renderDataTable(
            options = list(
              scrollX = TRUE
            )
          )

        tagList(
          p(
            strong("Station ID:"), stn$station_id, br(),
            strong("Station Name:"), stn$station_name, br(),
            strong("Waterbody:"), stn$waterbody, br(),
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
              p(downloadButton(ns("dl_daily"), "Download this data")),
              daily_dt
            ),
            tabPanel(
              title = "Hourly temperature data",
              class = "data-tab",
              p("The DateTime associated with each hourly observation below is in UTC time, but the Hour column reflects the local time at the logger (timezone: America/Chicago)."),
              p(downloadButton(ns("dl_hourly"), "Download this data")),
              hourly_dt
            )
          )
        )
      })

      ## dl_daily ----
      output$dl_daily <- downloadHandler(
        sprintf("WAV Stn %s Daily Temperature Data (%s).csv", cur_stn()$station_id, input$year),
        function(file) {
          write_csv(daily_data(), file, na = "")
        }
      )

      ## dl_hourly ----
      output$dl_hourly <- downloadHandler(
        sprintf("WAV Stn %s Hourly Temperature Data (%s).csv", cur_stn()$station_id, input$year),
        function(file) {
          write_csv(selected_data(), file, na = "")
        }
      )

    }
  )
}
