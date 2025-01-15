### Baseline Data Tab ###


# Static UI --------------------------------------------------------------------

baselineDataUI <- function() {
  ns <- NS("baseline")

  div(
    class = "data-tab",
    uiOutput(ns("uiWrapper")) %>% withSpinnerProxy(),
  )
}


# Server -----------------------------------------------------------------------

#' requires global data frame 'baseline_data'
#' @param cur_stn a `reactive()` single line data frame

baselineDataServer <- function(cur_stn, has_focus) {
  moduleServer(
    id = "baseline",
    function(input, output, session) {
      ns <- session$ns


      # REACTIVE VALUES ----

      rv <- reactiveValues(
        ready = FALSE
      )

      ## cur_data ----
      cur_data <- reactive({
        req(cur_stn())
        baseline_data %>% filter(station_id == cur_stn()$station_id)
      })

      observe({
        ready <- nrow(cur_data()) > 0
        if (rv$ready != ready) rv$ready <- ready
      })

      ## cur_years ----
      cur_years <- reactive({
        req(rv$ready)
        sort(unique(cur_data()$year))
      })

      ## selected_data ----
      selected_data <- reactive({
        switch(
          req(input$plot_type),
          "annual" = cur_data() %>% filter(year == req(input$year)),
          "trend" = cur_data()
        )
      })

      ## selected_data_ready ----
      selected_data_ready <- reactive({
        nrow(selected_data()) > 0
      })

      ## trend_value_choices ----
      trend_value_choices <- reactive({
        req(req(input$plot_type) == "trend")
        req(selected_data_ready())

        opts <- OPTS$baseline_plot_opts

        df <- selected_data() %>%
          pivot_longer(all_of(opts$col), names_to = "col") %>%
          drop_na(value) %>%
          summarize(n = n(), .by = col) %>%
          left_join(opts, join_by(col))

        setNames(df$col, df$name)
      })


      # MAIN UI ----

      ## uiWrapper ----
      output$uiWrapper <- renderUI({
        if (rv$ready) {
          uiOutput(ns("mainUI"))
        } else {
          div(class = "well", "This station has no baseline data. Choose another station or view the thermistor or nutrient data associated with this station.")
        }
      })

      ## mainUI ----
      output$mainUI <- renderUI({
        tagList(
          div(
            class = "well flex-row",
            uiOutput(ns("plotTypeUI")),
            uiOutput(ns("plotYearUI")),
            uiOutput(ns("trendTypeUI")),
            uiOutput(ns("trendValueUI"))
          ),
          uiOutput(ns("plotUI")),
          uiOutput(ns("stnSummaryUI")),
          uiOutput(ns("infoUI")),
          br(),
          bsCollapse(
            bsCollapsePanel(
              title = "View/download baseline data",
              uiOutput(ns("viewDataUI"))
            )
          )
        )
      })


      ## PLOT OPTIONS ----

      ## plotTypeUI ----
      output$plotTypeUI <- renderUI({
        choices <- OPTS$baseline_plot_type_choices

        radioGroupButtons(
          inputId = ns("plot_type"),
          label = "Plot type",
          size = "sm",
          choices = choices,
          selected = input$plot_type %||% first(choices)
        )
      })

      ## plotYearUI ----
      output$plotYearUI <- renderUI({
        type <- req(input$plot_type)
        req(type == "annual")

        radioGroupButtons(
          inputId = ns("year"),
          label = "Year",
          size = "sm",
          choices = cur_years(),
          selected = first_truthy(
            intersect(isolate(input$year), cur_years()),
            last(cur_years())
          )
        )
      })

      ## trendTypeUI ----
      output$trendTypeUI <- renderUI({
        type <- req(input$plot_type)
        req(type == "trend")

        choices <- OPTS$baseline_trend_type_choices

        radioGroupButtons(
          inputId = ns("trend_type"),
          label = "Group by",
          size = "sm",
          choices = choices,
          selected = input$trend_type %||% first(choices)
        )
      })

      ## trendValueUI ----
      output$trendValueUI <- renderUI({
        req(req(input$plot_type) == "trend")
        req(selected_data_ready())

        choices <- trend_value_choices()

        radioGroupButtons(
          inputId = ns("trend_value"),
          label = "Measurement",
          size = "sm",
          choices = choices,
          selected = input$trend_value %||% first(choices)
        )
      })


      ## PLOT UI ----

      ## stnTitleUI ----
      output$stnTitleUI <- renderUI({
        h3(str_to_title(cur_stn()$label), align = "center")
      })

      ## infoUI ----
      output$infoUI <- renderUI({
        includeMarkdown("md/baseline_info.md")
      })

      ## plotUI ----
      output$plotUI <- renderUI({
        switch(
          req(input$plot_type),
          "annual" = {
            tagList(
              div(
                id = "baseline-plot-container",
                uiOutput(ns("stnTitleUI")),
                plotlyOutput(ns("annualPlot")),
              ),
              uiOutput(ns("plotCaptionUI"))
            )
          },
          "trend" = {
            n_vars <- length(trend_value_choices())
            tagList(
              div(
                id = "baseline-plot-container",
                uiOutput(ns("stnTitleUI")),
                plotlyOutput(ns("trendPlot"))
              ),
              uiOutput(ns("plotCaptionUI")),
              h4("Observation heatmap"),
              plotlyOutput(ns("ribbonPlot"), height = 20 + 15 * n_vars),
            )
          }
        )
      })

      ## annualPlot ----
      output$annualPlot <- renderPlotly({
        req(selected_data_ready())
        df <- selected_data()
        req(length(unique(df$year)) == 1)
        makeBaselinePlot(df)
      })

      ## trendPlot ----
      output$trendPlot <- renderPlotly({
        req(rv$ready)
        type <- req(input$trend_type)
        value_col <- req(input$trend_value)
        df <- cur_data()
        switch(
          type,
          "scatter" = makeBaselineScatterplot(df, value_col),
          "box_month" = makeBaselineBoxplot(df, value_col, "month"),
          "box_year" = makeBaselineBoxplot(df, value_col, "year")
        )
      })

      ## ribbonPlot ----
      output$ribbonPlot <- renderPlotly({
        req(rv$ready)
        df <- cur_data()
        makeBaselineRibbonPlot(df)
      })

      ## plotCaptionUI ----
      output$plotCaptionUI <- renderUI({
        caption <- switch(
          req(input$plot_type),
          "annual" = "A selection of available baseline parameters are shown above. Click on an item in the legend below the plot to hide/show individual parameters.",
          "trend" = OPTS$baseline_trend_captions[[req(input$trend_type)]]
        )

        tagList(
          div(class = "plot-caption", caption),
          uiOutput(ns("plotExportUI"))
        )
      })

      ## plotExportUI ----
      output$plotExportUI <- renderUI({
        req(input$year)
        filename <- sprintf("WAV baseline data - Stn %s - %s.png", cur_stn()$station_id, input$year)
        buildPlotDlBtn("#baseline-plot-container", filename)
      })


      # SUMMARY TABLE ----

      ## stnSummaryUI ----
      output$stnSummaryUI <- renderUI({
        req(stnSummaryData())

        div(
          class = "well", style = "margin-top: 25px; overflow: auto",
          h4("Station data summary", style = "margin-top: 0px; border-bottom: 2px solid #d0d7d9;"),
          tableOutput(ns("stnSummaryTable"))
        )
      })

      ## stnSummaryTable ----
      output$stnSummaryTable <- renderTable(
        stnSummaryData(),
        width = "100%",
        spacing = "xs",
        align = "lccccc"
      )

      ## stnSummaryData ----
      makeMinMax <- function(df, var) {
        v <- df[[var]]
        if (length(v) == 0) return(tibble())
        tibble(
          observations = length(na.omit(v)),
          min = df[which.min(v), ][[var]],
          max = df[which.max(v), ][[var]],
          date_of_min = df[which.min(v), ]$date,
          date_of_max = df[which.max(v), ]$date
        )
      }

      stnSummaryData <- reactive({
        req(selected_data_ready())
        df <- selected_data()
        date_fmt <- ifelse(length(unique(df$year)) > 1, "%b %e, %Y", "%b %e")
        OPTS$baseline_summary_vars %>%
          reframe(pick(everything()), makeMinMax(df, var)) %>%
          mutate(across(c(min, max), ~paste(.x, units))) %>%
          mutate(across(c(date_of_min, date_of_max), ~format(.x, date_fmt))) %>%
          select(-c(var, units)) %>%
          clean_names("title")
      })


      # VIEW DATA ----

      ## viewDataUI ----
      output$viewDataUI <- renderUI({
        type <- req(input$plot_type)

        tagList(
          p(
            strong("Station ID:"), cur_stn()$station_id, br(),
            strong("Station Name:"), cur_stn()$station_name, br(),
            strong("Waterbody:"), cur_stn()$waterbody
          ),
          p(
            if (type == "annual")
              downloadButton(ns("downloadStnYear"), sprintf("Download station data (%s)", input$year)),
            downloadButton(ns("downloadStn"), "Download station data (all years)"),
            downloadButton(ns("downloadBaseline"), "Download entire baseline dataset")
          ),
          dataTableOutput(ns("dataTable"))
        )
      })

      ## dataTable ----
      output$dataTable <- renderDataTable({
        req(selected_data_ready())

        df <- selected_data()
        date_fmt <- ifelse(length(unique(df$year)) > 1, "%b %d, %Y", "%b %d")
        df <- df %>%
          arrange(date) %>%
          distinct(date, .keep_all = T) %>%
          clean_names(case = "title") %>%
          mutate(label = format(Date, date_fmt)) %>%
          mutate(across(everything(), as.character)) %>%
          pivot_longer(cols = -label, names_to = "Parameter") %>%
          pivot_wider(names_from = label) %>%
          mutate(Parameter = gsub("D o", "DO", Parameter)) %>%
          mutate(Parameter = gsub("P h", "pH", Parameter))

        datatable(
          df,
          selection = "none",
          rownames = F,
          options = list(
            paging = F,
            scrollX = T,
            scrollCollapse = T
          )
        )
      }, server = F)

      ## downloadStnYear ----
      output$downloadStnYear <- downloadHandler(
        sprintf("WAV Stn %s Baseline Data (%s).csv", cur_stn()$station_id, input$year),
        function(file) { write_csv(selected_data(), file, na = "") }
      )

      ## downloadStn ----
      output$downloadStn <- downloadHandler(
        sprintf("WAV Stn %s Baseline Data.csv", cur_stn()$station_id),
        function(file) { write_csv(cur_data(), file, na = "") }
      )

      ## downloadBaseline ----
      output$downloadBaseline <- downloadHandler(
        "WAV Baseline Data.csv",
        function(file) { write_csv(baseline_data, file, na = "") }
      )


      # Return values ----
      # return(reactive(list(year = input$year)))
    }
  )
}
