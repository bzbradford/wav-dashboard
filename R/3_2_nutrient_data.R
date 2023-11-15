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

      ## phos_exceedance_text ----
      phos_exceedance_text <- reactive({
        est <- phos_estimate()
        getPhosExceedanceText(
          median = est$median,
          lower = est$lower,
          upper = est$upper
        )
      })


      # Rendered UIs ----

      ## mainUI ----
      output$mainUI <- renderUI({
        if (!data_ready()) {
          return(div(class = "well", "This station has no nutrient data. Choose another station or view the baseline or thermistor data associated with this station."))
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
              title = "View or download nutrient data data",
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
            "The dashed line on this plot indicates the total phosphorus state exceedance level of 0.075 mg/L (ppm). If more than one month of data was collected, the median and 90% confidence interval for the true total phosphorus level are displayed as a horizontal band."
          ),
          div(class = "plot-caption", strong(phos_exceedance_text()))
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
        fname <- paste0("nutrient-plot-", cur_stn()$station_id, "-", input$nutrient_year, ".png")
        p(
          class = "note",
          align = "center",
          em(
            a("Click here to download this plot as a PNG.",
              style = "cursor: pointer;",
              onclick = sprintf("
                html2canvas(
                  document.querySelector('#nutrient-plot-container'),
                  {scale: 3}
                ).then(canvas => {
                  saveAs(canvas.toDataURL(), '%s')
                })
              ", fname)
            )
          )
        )
      })


      # View/download nutrient data ----

      ## viewDataUI ----
      output$viewDataUI <- renderUI({
        btn_year <- downloadButton(ns("downloadYear"), paste("Download", input$year, "data"))
        btn_all <- downloadButton(ns("downloadAll"), paste("Download all years of nutrient data for this site"))

        if (input$year == "All") {
          dl_btns <- list(btn_all)
        } else {
          dl_btns <- list(btn_year, btn_all)
        }

        tagList(
          p(dl_btns),
          div(
            style = "overflow: auto;",
            dataTableOutput(ns("dataTable"))
          ),
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

      ## downloadYear ----
      output$downloadYear <- downloadHandler(
        paste0("stn-", cur_stn()$station_id, "-nutrient-data-", input$year, ".csv"),
        function(file) {write_csv(selected_data(), file)}
      )

      ## downloadAll ----
      output$downloadAll <- downloadHandler(
        paste0("stn-", cur_stn()$station_id, "-nutrient-data.csv"),
        function(file) {write_csv(cur_data(), file)}
      )


      # Return values ----
      return(reactive(list(year = input$year)))
    }
  )
}
