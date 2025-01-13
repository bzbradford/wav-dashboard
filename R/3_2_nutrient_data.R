## NUTRIENT TAB ##

nutrientDataUI <- function() {
  ns <- NS("nutrient")

  div(
    class = "data-tab",
    uiOutput(ns("mainUI")) %>% withSpinnerProxy(),
  )
}


#' requires global data frame 'nutrient_data'
#' @param cur_stn a `reactive()` expression containing the current station

nutrientDataServer <- function(cur_stn, has_focus) {
  moduleServer(
    id = "nutrient",
    function(input, output, session) {
      ns <- session$ns

      # Reactives ----

      ## cur_data ----
      cur_data <- reactive({
        req(cur_stn())

        filter(nutrient_data, station_id == cur_stn()$station_id)
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
        nrow(cur_data()) > 0
      })

      ## phos_estimate ----
      phos_estimate <- reactive({
        req(selected_data_ready())

        getPhosEstimate(selected_data()$tp)
      })


      # Rendered UIs ----

      ## mainUI ----
      output$mainUI <- renderUI({
        if (!data_ready()) {
          return(div(class = "well", "This station has no nutrient data. Choose another station or view the baseline or thermistor data associated with this station."))
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
              selected = first_truthy(
                intersect(isolate(input$year), cur_years()),
                last(cur_years())
              )
            )
          ),
          div(
            id = "nutrient-plot-container",
            h3(cur_stn()$label, align = "center"),
            plotlyOutput(ns("plot")) %>% withSpinnerProxy(hide.ui = F),
            uiOutput(ns("plotCaptionUI"))
          ),
          uiOutput(ns("plotExportUI")), br(),
          uiOutput(ns("moreInfoUI")), br(),
          bsCollapse(
            bsCollapsePanel(
              title = "View or download nutrient data",
              uiOutput(ns("viewDataUI"))
            )
          )
        )
      })

      ## moreInfoUI ----
      output$moreInfoUI <- renderUI({
        includeMarkdown("md/nutrient_info.md")
      })

      ## plotCaptionUI ----
      output$plotCaptionUI <- renderUI({
        tagList(
          div(
            class = "plot-caption",
            "The dashed line on this plot indicates the total phosphorus state exceedance level of 0.075 mg/L (ppm). If more than one month of data was collected, the median and 80% confidence interval for the true total phosphorus level are displayed as a horizontal band. A zero value indicates the submitted sample was below the limit of detection."
          ),
          div(
            class = "plot-caption",
            strong(getPhosExceedanceText(phos_estimate()))
          )
        )
      })


      # Plot ----

      ## plot ----
      output$plot <- renderPlotly({
        req(selected_data_ready())

        makeNutrientPlot(selected_data(), phoslimit, phos_estimate())
      })

      ## plotExportUI ----
      output$plotExportUI <- renderUI({
        req(input$year)
        filename <- sprintf("WAV Nutrient Data - Stn %s - %s.png", cur_stn()$station_id, input$year)
        buildPlotDlBtn("#nutrient-plot-container", filename)
      })


      # View/download nutrient data ----

      ## viewDataUI ----
      output$viewDataUI <- renderUI({
        btn_year <-  downloadButton(ns("downloadYear"), paste("Download", input$year, "data"))
        btn_all <- downloadButton(ns("downloadAll"), paste("Download all years of nutrient data for this site"))
        dl_btns <- if (input$year == "All") list(btn_all) else list(btn_year, btn_all)

        tagList(
          p(
            strong("Station ID:"), cur_stn()$station_id, br(),
            strong("Station Name:"), cur_stn()$station_name, br(),
            strong("Waterbody:"), cur_stn()$waterbody
          ),
          p(
            if (input$year != "All")
              downloadButton(ns("downloadStnYear"), sprintf("Download station data (%s)", input$year)),
            downloadButton(ns("downloadStn"), "Download station data (all years)"),
            downloadButton(ns("downloadBaseline"), "Download entire nutrient dataset")
          ),
          div(style = "overflow: auto;", dataTableOutput(ns("dataTable"))),
          p(em("Total phosphorus is shown in units of mg/L (ppm)."))
        )
      })

      ## dataTable ----
      output$dataTable <- renderDataTable({
        req(selected_data_ready())

        selected_data() %>%
          drop_na(tp) %>%
          clean_names(case = "big_camel")
      })

      ## downloadStnYear ----
      output$downloadStnYear <- downloadHandler(
        sprintf("WAV Stn %s Nutrient Data (%s).csv", cur_stn()$station_id, input$year),
        function(file) { write_csv(selected_data(), file, na = "") }
      )

      ## downloadStn ----
      output$downloadStn <- downloadHandler(
        sprintf("WAV Stn %s Nutrient Data.csv", cur_stn()$station_id),
        function(file) { write_csv(cur_data(), file, na = "") }
      )

      ## downloadBaseline ----
      output$downloadBaseline <- downloadHandler(
        "WAV Nutrient Data.csv",
        function(file) { write_csv(nutrient_data, file, na = "") }
      )


      # Return values ----
      return(reactive(list(year = input$year)))

    }
  )
}
