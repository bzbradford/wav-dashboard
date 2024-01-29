## BASELINE TAB ##

baselineDataUI <- function() {
  ns <- NS("baseline")

  div(
    class = "data-tab",
    uiOutput(ns("mainUI")) %>% withSpinnerProxy(),
  )
}

#' requires global data frame 'baseline_data'
#' @param cur_stn a `reactive()` single line data frame

baselineDataServer <- function(cur_stn, has_focus) {
  moduleServer(
    id = "baseline",
    function(input, output, session) {
      ns <- session$ns

      # Reactive vals ----

      ## cur_data ----
      cur_data <- reactive({
        req(cur_stn())

        baseline_data %>%
          filter(station_id == cur_stn()$station_id)
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


      # Rendered UI ----

      ## mainUI ----
      output$mainUI <- renderUI({
        if (!data_ready()) {
          return(div(class = "well", "This station has no baseline data. Choose another station or view the thermistor or nutrient data associated with this station."))
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
          div(
            id = "baseline-plot-container",
            h3(cur_stn()$label, align = "center"),
            plotlyOutput(ns("plot")) %>% withSpinnerProxy(hide.ui = F),
          ),
          div(class = "plot-caption", "Click on any of the plot legend items to show or hide it in the plot. Click and drag left-to-right to select a date range (double click to reset)."),
          uiOutput(ns("plotExportUI")),
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

      ## infoUI ----
      output$infoUI <- renderUI({
        includeMarkdown("md/baseline_info.md")
      })


      # Plot ----

      ## plot ----
      output$plot <- renderPlotly({
        req(selected_data_ready())
        makeBaselinePlot(selected_data())
      })

      ## plotExportUI ----
      output$plotExportUI <- renderUI({
        req(input$year)
        filename <- sprintf("WAV baseline data - Stn %s - %s.png", cur_stn()$station_id, input$year)
        buildPlotDlBtn("#baseline-plot-container", filename)
      })


      # Station summary box ----

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

      ## baseline_summary_vars ----
      baseline_summary_vars <- tribble(
        ~var, ~parameter, ~units,
        "d_o", "Dissolved oxygen", "mg/L",
        "water_temp", "Water temperature", "°C",
        "air_temp", "Air temperature", "°C",
        "transparency", "Transparency", "cm",
        "streamflow", "Stream flow", "cfs",
        "average_stream_depth", "Stream depth", "ft",
      ) %>% rowwise()

      ## stnSummaryData ----
      stnSummaryData <- reactive({
        req(input$year)
        req(selected_data_ready())

        df <- selected_data() %>%
          distinct(date, .keep_all = T)
        date_fmt <- ifelse(input$year == "All", "%b %e, %Y", "%b %e")
        baseline_summary_vars %>%
          reframe(pick(everything()), make_min_max(df, var)) %>%
          mutate(across(c(min, max), ~paste(.x, units))) %>%
          mutate(across(c(date_of_min, date_of_max), ~format(.x, date_fmt))) %>%
          select(-c(var, units)) %>%
          clean_names("title")
      })



      # View Baseline Data ----

      ## viewDataUI ----
      output$viewDataUI <- renderUI({
        req(input$year)

        btn_year <- downloadButton(ns("downloadYear"), paste("Download", input$year, "data"))
        btn_all <- downloadButton(ns("downloadAll"), "Download all years of baseline data for this site")
        dl_btns <- { if (input$year == "All") list(btn_all) else list(btn_year, btn_all) }

        tagList(
          p(dl_btns),
          div(
            style = "overflow: auto;",
            dataTableOutput(ns("dataTable"))
          )
        )
      })

      ## dataTable ----
      output$dataTable <- renderDataTable({
        req(selected_data_ready())

        date_fmt <- ifelse(input$year == "All", "%b %d, %Y", "%b %d")

        df <- selected_data() %>%
          arrange(date) %>%
          distinct(date, .keep_all = T) %>%
          clean_names(case = "title") %>%
          mutate(label = format(Date, date_fmt)) %>%
          mutate(across(everything(), as.character)) %>%
          pivot_longer(cols = -label, names_to = "Parameter") %>%
          pivot_wider(names_from = label) %>%
          mutate(Parameter = gsub("D o", "D.O.", Parameter)) %>%
          mutate(Parameter = gsub("P h", "pH", Parameter))

        datatable(df, selection = "none", options = list(paging = F))
      },
        server = F)

      ## downloadYear ----
      output$downloadYear <- downloadHandler(
        paste0("stn-", cur_stn()$station_id, "-baseline-data-", input$year, ".csv"),
        function(file) { write_csv(selected_data(), file) }
      )

      ## downloadAll ----
      output$downloadAll <- downloadHandler(
        paste0("stn-", cur_stn()$station_id, "-baseline-data.csv"),
        function(file) { write_csv(cur_data(), file) }
      )

      # Return values ----
      return(reactive(list(year = input$year)))
    }
  )
}
