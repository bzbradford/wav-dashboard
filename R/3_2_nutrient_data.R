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
        req(has_focus())

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

        get_phos_estimate(selected_data()$tp)
      })

      ## phos_exceedance_text ----
      phos_exceedance_text <- reactive({
        params = phos_estimate()
        get_phos_exceedance(
          median = params$median,
          lower = params$lower,
          upper = params$upper
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
            plotlyOutput(ns("plot")) %>% withSpinnerProxy(hide.ui = FALSE),
            uiOutput(ns("plotCaptionUI"))
          ),
          uiOutput(ns("plotExportUI")),
          br(),
          uiOutput(ns("moreInfoUI")),
          br(),
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
          div(
            class = "plot-caption",
            strong(phos_exceedance_text())
          )
        )
      })


      # Plot ----

      ## plot ----
      output$plot <- renderPlotly({
        req(selected_data_ready())

        df <- selected_data() %>%
          mutate(exceedance = factor(
            ifelse(is.na(tp), "No data", ifelse(tp >= phoslimit, "TP High", "TP OK")),
            levels = c("TP OK", "TP High", "No data"))) %>%
          drop_na(tp) %>%
          mutate(phoslimit = phoslimit)

        suppressWarnings({
          min_year <- min(df$year)
          max_year <- max(df$year)
        })
        date_range <- c(ISOdate(min_year, 5, 1), ISOdate(max_year, 10, 31))
        outer_months <- c(ISOdate(min_year, 4, 30), ISOdate(max_year, 11, 1))
        data_dates <- unique(df$date)
        all_dates <-  c(outer_months, data_dates)
        yrange <- suppressWarnings(c(0, max(phoslimit * 1.2, max(df$tp, na.rm = T) * 1.2)))

        phos_params <- tibble(
          date = all_dates,
          lower = phos_estimate()$lower,
          upper = phos_estimate()$upper,
          median = phos_estimate()$median
        )

        # no confidence invervals if only one month of data
        if (phos_estimate()$n > 1) {
          ci_color <- ifelse(phos_estimate()$upper >= phoslimit, "red", "teal")

          plt <- plot_ly(phos_params) %>%
            add_lines(
              x = ~date,
              y = ~phoslimit,
              name = "TP limit",
              xperiod = "M1",
              xperiodalignment = "middle",
              opacity = 0.75,
              line = list(color = "black", dash = "dash", width = 1.5)
            ) %>%
            add_lines(
              x = ~date,
              y = ~lower,
              name = "Lower 90% CI",
              xperiod = "M1",
              xperiodalignment = "middle",
              opacity = 0.5,
              line = list(color = ci_color, width = 0.5)
            ) %>%
            add_lines(
              x = ~date,
              y = ~median,
              name = "Median",
              xperiod = "M1",
              xperiodalignment = "middle",
              opacity = 0.5,
              line = list(color = "darkblue", width = 2)
            ) %>%
            add_lines(
              x = ~date,
              y = ~upper,
              name = "Upper 90% CI",
              xperiod = "M1",
              xperiodalignment = "middle",
              opacity = 0.5,
              line = list(color = ci_color, width = 0.5)
            )

          shapes <- list(
            rect(phos_estimate()$lower, phos_estimate()$upper, ci_color)
          )
        } else {
          plt <- plot_ly()
          shapes <- list()
        }

        plt <- plt %>%
          add_trace(
            data = df,
            x = ~date,
            y = ~tp,
            type = "bar",
            text = ~tp,
            textposition = "auto",
            color = ~exceedance,
            colors = "Set2",
            width = 0.5 * 1000 * 60 * 60 * 24 * 30,
            xperiod = "M1",
            xperiodalignment = "middle",
            marker = list(
              line = list(color = "rgb(8,48,107)", width = 1)
            ),
            textfont = list(color = "black"),
            hovertemplate = "Measured TP: %{y:.3f} ppm<extra></extra>"
          ) %>%
          layout(
            title = "Total Phosphorus",
            xaxis = list(
              title = "",
              type = "date",
              tickformat = "%B<br>%Y",
              dtick = "M1",
              ticklabelmode = "period",
              range = date_range),
            yaxis = list(
              title = "Total phosphorus",
              ticksuffix = " ppm",
              zerolinecolor = "lightgrey",
              range = yrange),
            legend = list(
              traceorder = "reversed",
              orientation = "h",
              x = 0.25, y = 1
            ),
            hovermode = "x unified",
            margin = list(t = 50),
            shapes = shapes
          ) %>%
          config(displayModeBar = F)

        plt
      })





      ## plotExportUI ----
      output$plotExportUI <- renderUI({
        p(
          style = "margin-left: 2em; margin-right: 2em; font-size: smaller;",
          align = "center",
          em(
            a("Click here to download this plot as a PNG.",
              style = "cursor: pointer;",
              onclick = paste0(
                "html2canvas(document.querySelector('",
                "#nutrient-plot-container",
                "'), {scale: 3}).then(canvas => {saveAs(canvas.toDataURL(), '",
                paste("nutrient-plot", cur_stn()$station_id, input$nutrient_year, sep = "-"),
                ".png",
                "')})"
              )
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
