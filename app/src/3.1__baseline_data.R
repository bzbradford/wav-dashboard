## BASELINE TAB ##

baselineDataUI <- function() {
  ns <- NS("baseline")

  div(
    class = "data-tab",
    uiOutput(ns("main_ui_wrapper")) %>% with_spinner(),
  )
}

#' @requires `baseline_data`
#' @param main_rv reactive values object from main server session
baselineDataServer <- function(main_rv) {
  moduleServer(
    id = "baseline",
    function(input, output, session) {
      ns <- session$ns


      # Reactive values --------------------------------------------------------

      rv <- reactiveValues(

        ## rv$ready ----
        ready = FALSE

      )

      ## cur_stn ----
      cur_stn <- reactive({
        req(main_rv$cur_stn)
      })

      observe({
        stn <- cur_stn()
        df <- baseline_data %>%
          filter(station_id == stn$station_id)
        ready <- nrow(df) > 0
        if (!identical(rv$ready, ready)) rv$ready <- ready
      })

      ## stn_data ----
      stn_data <- reactive({
        stn <- cur_stn()
        df <- baseline_data %>%
          filter(station_id == stn$station_id)
        req(nrow(df) > 0)
        df
      })

      ## stn_years ----
      stn_years <- reactive({
        data <- stn_data()
        sort(unique(data$year))
      })

      ## selected_data ----
      selected_data <- reactive({
        data <- stn_data()
        plot_type <- req(input$plot_type)
        df <- if (plot_type == "annual") {
          data %>% filter(year == req(input$year))
        } else {
          data
        }
        req(nrow(df) > 0)
        df
      })

      ## parameter_choices ----
      parameter_choices <- reactive({
        stn_data() %>%
          get_baseline_param_choices()
      })

      plot_type_choices <- reactive({
        choices <- list(
          "Annual" = "annual",
          "Long-term" = "trend",
          "Macroinvertebrate" = "macro",
          "Heatmap" = "ribbon"
        )
        if (!("biotic_index_score" %in% parameter_choices()))
          choices[["Macroinvertebrate"]] <- NULL
        choices
      })



      # Main UI ----------------------------------------------------------------

      ## main_ui_wrapper ----
      output$main_ui_wrapper <- renderUI({
        if (rv$ready) {
          uiOutput(ns("main_ui"))
        } else {
          div(class = "well", "This station has no baseline data. Choose another station or view the thermistor or nutrient data associated with this station.")
        }
      })

      ## main_ui ----
      output$main_ui <- renderUI({
        tagList(
          div(
            class = "well flex-row",
            style = "margin-bottom: 1rem;",
            uiOutput(ns("plot_type_ui")),
            conditionalPanel(
              "input.plot_type == 'annual'", ns = ns,
              uiOutput(ns("plot_year_ui"))
            ),
            conditionalPanel(
              "input.plot_type == 'trend'", ns = ns,
              uiOutput(ns("trend_type_ui"))
            ),
            conditionalPanel(
              "input.plot_type == 'trend'", ns = ns,
              uiOutput(ns("trend_param_ui"))
            ),
            conditionalPanel(
              "input.plot_type == 'macro'", ns = ns,
              uiOutput(ns("macro_type_ui"))
            ),
          ),
          div(
            id = "baseline-plot-container",
            uiOutput(ns("stn_title_ui")),
            conditionalPanel(
              "input.plot_type == 'annual'", ns = ns,
              plotlyOutput(ns("annual_plot"))
            ),
            conditionalPanel(
              "input.plot_type == 'trend'", ns = ns,
              plotlyOutput(ns("trend_plot"))
            ),
            conditionalPanel(
              "input.plot_type == 'macro'", ns = ns,
              plotlyOutput(ns("macro_plot"), height = "500px")
            ),
            conditionalPanel(
              "input.plot_type == 'ribbon'", ns = ns,
              uiOutput(ns("ribbon_plot_ui"))
            ),
            uiOutput(ns("plot_caption_ui")),
            uiOutput(ns("plot_export_ui"))
          ),
          uiOutput(ns("stn_summary_ui")),
          uiOutput(ns("info_ui")),
          uiOutput(ns("stn_data_ui"))
        )
      })

      ## stn_data_ui ----
      output$stn_data_ui <- renderUI({
        dataViewUI(ns("data_view"))
      })

      ## dataViewServer ----
      dataViewServer(
        id = "data_view",
        dataset_name = "Baseline",
        master_data = baseline_data,
        cur_stn = cur_stn,
        stn_data = stn_data,
        selected_data = selected_data
      )



      # Plot options -----------------------------------------------------------

      ## plot_type_ui ----
      output$plot_type_ui <- renderUI({
        choices <- plot_type_choices()
        selected <- first_truthy(
          intersect(isolate(input$plot_type), choices),
          first(choices)
        )

        radioGroupButtons(
          inputId = ns("plot_type"),
          label = "Plot type",
          size = "sm",
          choices = choices,
          selected = selected
        )
      })

      ## plot_year_ui ----
      output$plot_year_ui <- renderUI({
        yrs <- stn_years()
        selected <- first_truthy(
          intersect(isolate(input$year), yrs),
          last(yrs)
        )

        radioGroupButtons(
          inputId = ns("year"),
          label = "Year",
          size = "sm",
          choices = yrs,
          selected = selected
        )
      })

      ## trend_type_ui ----
      output$trend_type_ui <- renderUI({
        choices <- list(
          "None" = "scatter",
          "Month" = "month",
          "Year" = "year"
        )
        selected <- isolate(input$trend_type) %||% first(choices)

        radioGroupButtons(
          inputId = ns("trend_plot_type"),
          label = "Group by",
          size = "sm",
          choices = choices,
          selected = selected
        )
      })

      ## trend_param_ui ----
      output$trend_param_ui <- renderUI({
        choices <- parameter_choices()
        selected <- isolate(input$trend_value) %||% first(choices)

        radioGroupButtons(
          inputId = ns("trend_plot_param"),
          label = "Parameter",
          size = "sm",
          choices = choices,
          selected = selected
        )
      })

      ## macro_type_ui ----
      output$macro_type_ui <- renderUI({
        choices <- list(
          "Year" = "annual",
          "Date" = "all"
        )
        selected <- isolate(input$macro_type) %||% first(choices)

        radioGroupButtons(
          inputId = ns("macro_plot_type"),
          label = "Show by",
          size = "sm",
          choices = choices,
          selected = selected
        )
      })


      # Plot UI ----------------------------------------------------------------

      ## stn_title_ui ----
      output$stn_title_ui <- renderUI({
        stn <- cur_stn()
        title <- str_to_title(stn$label)
        h3(title, align = "center")
      })

      ## info_ui ----
      output$info_ui <- renderUI({
        includeMarkdown("md/baseline_info.md")
      })

      ## annual_plot ----
      output$annual_plot <- renderPlotly({
        req(identical(input$plot_type, "annual"))
        df <- selected_data()
        req(n_distinct(df$year) == 1)

        plotly_baseline(df)
      })

      ## trend_plot ----
      output$trend_plot <- renderPlotly({
        req(identical(input$plot_type, "trend"))
        df <- stn_data()
        value_col <- req(input$trend_plot_param)
        type <- req(input$trend_plot_type)

        plotly_baseline_trend(df, value_col, type)
      })

      ## macro_plot ----
      output$macro_plot <- renderPlotly({
        req(identical(input$plot_type, "macro"))
        stn <- cur_stn()
        type <- req(input$macro_plot_type)

        plotly_macros(stn$station_id, type)
      })

      ## trend_plot_caption_ui
      output$plot_caption_ui <- renderUI({
        caption <- switch(req(input$plot_type),
          "annual" = "A selection of available baseline parameters are shown above. Click on an item in the legend below the plot to hide/show individual parameters.",
          "trend" = paste(switch(
            req(input$trend_plot_type),
            "scatter" = "All observations for the selected parameter are shown above.",
            "month" = "Measurements from each month across all years are summarized using boxplots, which illustrate the median value (solid central bar), mean value (dashed central bar), Q1-Q3 interquartile range (main box) and full value range (whiskers). Individual observations are overlaid as points.",
            "year" = "Measurements from each year are summarized using boxplots, which illustrate the median value, mean value, interquartile range (main box), and full value range (whiskers). Individual observations are overlaid as points."
          ), "Interpretive ranges are illustrated to contextualize the observations."),
          "macro" = HTML(paste0(
            "The presence or absence of aquatic macroinvertebrates can reflect a stream’s general condition, as some species are more sensitive to water quality than others. ",
            colorize("Group 1 species (blue)", "blue"),
            " are the most sensitive. ",
            colorize("Group 2 (green)", "green"),
            " are somewhat sensitive, ",
            colorize("Group 3 (orange)", "orange"),
            " are somewhat tolerant, followed by ",
            colorize("Group 4 (red)", "red"),
            ", the most tolerant. Suspected invasive species observations are shown in ",
            colorize("purple"),
            "."
          )),
          "ribbon" = "This figure shows which parameters have been measured for this station, with tickmarks showing Jan 1 of each year, and each column represents one month of observations."
        )

        div(class = "plot-caption", caption)
      })

      ## ribbon_plot_ui ----
      output$ribbon_plot_ui <- renderUI({
        params <- parameter_choices()

        plotlyOutput(ns("ribbon_plot"), height = 50 + 15 * length(params))
      })

      ## ribbon_plot ----
      output$ribbon_plot <- renderPlotly({
        df <- stn_data()

        plotly_baseline_ribbon(df)
      })

      ## plot_export_ui ----
      output$plot_export_ui <- renderUI({
        filename <- paste(
          "WAV Stn", cur_stn()$station_id, req(input$plot_type), "plot -",
          switch(req(input$plot_type),
            "annual" = req(input$year),
            "trend" = paste(req(input$trend_type), req(input$trend_param)),
            "macro" = req(input$macro_type)
          )
        )

        div(
          style = "display: flex; flex-direction: row-reverse;",
          build_plot_download_btn(
            id = "#baseline-plot-container",
            filename = paste0(str_to_title(filename), ".png")
          )
        )
      })


      # Summary table ----------------------------------------------------------

      ## stn_summary_data() ----
      stn_summary_data <- reactive({
        df <- selected_data()
        date_fmt <- ifelse(length(unique(df$year)) > 1, "%b %e, %Y", "%b %e")
        data_opts %>%
          filter(source == "baseline") %>%
          select(col, name, units) %>%
          rowwise() %>%
          reframe(pick(everything()), get_min_max(df, col)) %>%
          mutate(across(c(min, mean, max), ~ if_else(is.na(units), as.character(.x), paste(.x, units)))) %>%
          mutate(across(c(date_of_min, date_of_max), ~ format(.x, date_fmt))) %>%
          select(-c(col, units)) %>%
          clean_names("title")
      })

      # identify min and max values and returns the dates for each
      get_min_max <- function(df, var) {
        v <- df[[var]]
        if (length(v) == 0) {
          return(tibble())
        }
        tibble(
          observations = length(na.omit(v)),
          min = df[which.min(v), ][[var]],
          mean = round(mean(df[[var]], na.rm = TRUE), 1),
          max = df[which.max(v), ][[var]],
          date_of_min = df[which.min(v), ]$date,
          date_of_max = df[which.max(v), ]$date
        )
      }

      ## stn_summary_ui ----
      output$stn_summary_ui <- renderUI({
        req(stn_summary_data())

        accordion(
          accordion_panel(
            title = "Station data summary",
            style = "background-color: rgba(0, 0, 0, 0.025);",
            div(
              style = "overflow: auto;",
              tableOutput(ns("stn_summary_tbl"))
            )
          )
        )
      })

      ## stn_summary_tbl ----
      output$stn_summary_tbl <- renderTable(
        stn_summary_data(),
        width = "100%",
        spacing = "xs",
        align = "lcccccc"
      )


      # Station data -----------------------------------------------------------




#       ## stn_data_ui ----
#       output$stn_data_ui <- renderUI({
#         type <- req(input$plot_type)
#         stn <- cur_stn()
#
#         accordion(
#           accordion_panel(
#             title = "View/download baseline data",
#             class = "btn-primary",
#             p(
#               strong("Station ID:"), stn$station_id, br(),
#               strong("Station Name:"), stn$station_name, br(),
#               strong("Waterbody:"), stn$waterbody
#             ),
#             p(
#               if (type == "annual") {
#                 yr <- req(input$year)
#                 downloadButton(ns("dl_cur_yr"), sprintf("Download station data (%s)", yr))
#               },
#               downloadButton(ns("dl_cur_stn"), "Download station data (all years)"),
#               downloadButton(ns("dl_all_baseline"), "Download entire baseline dataset")
#             ),
#             uiOutput(ns("dt_transpose")),
#             dataTableOutput(ns("stn_dt")),
#           ),
#           open = FALSE
#         )
#       })
#
#       # transpose switch for table
#       output$dt_transpose <- renderUI({
#         data <- selected_data()
#         stn_observations <- length(unique(data$date))
#         materialSwitch(
#           inputId = ns("transpose"),
#           label = "Transpose table",
#           inline = TRUE,
#           value = stn_observations <= 10
#         )
#       })
#
#       stn_data_for_dt <- reactive({
#         df <- selected_data() %>%
#           arrange(date) %>%
#           distinct(date, .keep_all = T) %>%
#           clean_names(case = "title")
#
#         if (isTRUE(input$transpose)) {
#           date_fmt <- ifelse(n_distinct(year(df$Date)) > 1, "%b %d, %Y", "%b %d")
#           df <- df %>%
#             mutate(label = format(Date, date_fmt)) %>%
#             mutate(across(everything(), as.character)) %>%
#             pivot_longer(cols = -label, names_to = "Parameter") %>%
#             pivot_wider(names_from = label) %>%
#             mutate(Parameter = gsub("D o", "DO", Parameter)) %>%
#             mutate(Parameter = gsub("P h", "pH", Parameter))
#         }
#
#         df
#       })
#
#       ## stn_dt ----
#       output$stn_dt <- renderDataTable(
#         stn_data_for_dt(),
#         selection = "none",
#         rownames = F,
#         options = list(
#           paging = F,
#           scrollX = T,
#           scrollCollapse = T
#         ),
#         server = F
#       )
#
#       ## dl_cur_yr ----
#       output$dl_cur_yr <- downloadHandler(
#         sprintf("WAV Stn %s Baseline Data (%s).csv", cur_stn()$station_id, input$year),
#         function(file) {
#           write_csv(selected_data(), file, na = "")
#         }
#       )
#
#       ## dl_cur_stn ----
#       output$dl_cur_stn <- downloadHandler(
#         sprintf("WAV Stn %s Baseline Data.csv", cur_stn()$station_id),
#         function(file) {
#           write_csv(stn_data(), file, na = "")
#         }
#       )
#
#       ## dl_all_baseline ----
#       output$dl_all_baseline <- downloadHandler(
#         "WAV Baseline Data.csv",
#         function(file) {
#           write_csv(baseline_data, file, na = "")
#         }
#       )

    }
  )
}
