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
            plotlyOutput(ns("plot")) %>% withSpinnerProxy(hide.ui = FALSE),
          ),
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

        df <- selected_data() %>%
          distinct(date, .keep_all = T)

        yranges <- list(
          d_o = c(0, find_max(df$d_o, 12)),
          temp = c(0, find_max(c(df$water_temp, df$ambient_air_temp), 30)),
          trans = c(0, 125),
          cfs = c(0, find_max(df$streamflow_cfs, 10))
        )

        do_data <- df %>%
          filter(!is.na(d_o)) %>%
          mutate(label = case_when(
            is.na(d_o_percent_saturation) ~ paste0(d_o, " mg/L"),
            T ~ paste0(d_o, " mg/L<br>", d_o_percent_saturation, "% sat"))) %>%
          rowwise() %>%
          mutate(do_color = do_color(d_o))
        temp_data <- filter(df, !(is.na(water_temp) & is.na(ambient_air_temp)))
        trans_data <- filter(df, !is.na(transparency_average))
        flow_data <- filter(df, !is.na(streamflow_cfs))

        # settings for longer date periods
        years <- as.numeric(max(df$date) - min(df$date)) / 365
        date_tick <- "M1"
        marker_opacity <- 1
        if (years > 3) {
          date_tick <- "M3"
          marker_opacity <- 0
        }
        if (years > 6) date_tick <- "M6"

        df %>%
          plot_ly() %>%
          layout(
            title = "Baseline Measurements",
            hovermode = "x unified",
            margin = list(t = 50, r = 50),
            legend = list(orientation = "h"),
            xaxis = list(
              title = "",
              type = "date",
              fixedrange = F, # allow user to zoom the axis?
              dtick = date_tick,
              ticklabelmode = "period",
              hoverformat = "%b %d, %Y",
              domain = c(.1, .9))
          ) %>%
          add_trace(
            data = do_data,
            name = "D.O.",
            x = ~date,
            y = ~d_o,
            text = ~label,
            marker = list(
              color = ~do_color,
              line = list(color = "black", width = 0.5)
            ),
            type = "bar",
            width = 1000 * 60 * 60 * 24 * 15,
            hovertemplate = "%{y}"
          ) %>%
          add_trace(
            data = temp_data,
            name = "Water temp",
            x = ~date,
            y = ~water_temp,
            type = "scatter",
            mode = "lines+markers",
            yaxis = "y2",
            marker = list(
              color = "lightblue",
              size = 10,
              line = list(color = "white", width = 1),
              opacity = marker_opacity
            ),
            line = list(
              color = "lightblue",
              width = 3
            )
          ) %>%
          add_trace(
            data = temp_data,
            name = "Air temp",
            x = ~date,
            y = ~ambient_air_temp,
            type = "scatter",
            mode = "lines+markers",
            yaxis = "y2",
            marker = list(
              color = "orange",
              size = 10,
              line = list(color = "white", width = 1),
              opacity = marker_opacity
            ),
            line = list(color = "orange", width = 3)
          ) %>%
          add_trace(
            data = trans_data,
            name = "Transparency",
            x = ~date,
            y = ~transparency_average,
            type = "scatter",
            mode = "lines+markers",
            yaxis = "y3",
            marker = list(
              color = "brown",
              size = 10,
              symbol = "square",
              line = list(color = "white", width = 1),
              opacity = marker_opacity
            ),
            line = list(color = "brown", width = 3)
          ) %>%
          add_trace(
            data = flow_data,
            name = "Stream flow",
            x = ~date,
            y = ~streamflow_cfs,
            type = "scatter",
            mode = "lines+markers",
            yaxis = "y4",
            marker = list(
              color = "#48a67b",
              size = 10,
              symbol = "triangle-right",
              line = list(color = "white", width = 1),
              opacity = marker_opacity
            ),
            line = list(color = "#48a67b", width = 3)
          ) %>%
          layout(
            yaxis = list(
              title = "Dissolved oxygen",
              ticksuffix = " mg/L",
              range = yranges$d_o,
              fixedrange = T
            ),
            yaxis2 = list(
              title = "Temperature",
              overlaying = "y",
              side = "left",
              ticksuffix = "&deg;C",
              position = 0,
              showgrid = F,
              zeroline = F,
              range = yranges$temp,
              fixedrange = T
            ),
            yaxis3 = list(
              title = "Transparency",
              overlaying = "y",
              side = "right",
              ticksuffix = " cm",
              showgrid = F,
              zeroline = F,
              range = yranges$trans,
              fixedrange = T
            ),
            yaxis4 = list(
              title = "Stream flow",
              overlaying = "y",
              side = "right",
              ticksuffix = " cfs",
              position = 1,
              showgrid = F,
              zeroline = F,
              range = yranges$cfs,
              fixedrange = T
            )
          ) %>%
          config(displayModeBar = F)
      })

      ## plotExportUI ----
      output$plotExportUI <- renderUI({
        fname <- paste0("baseline-plot-", cur_stn()$station_id, "-", input$year, ".png")
        onclick <- sprintf("
          html2canvas(
            document.querySelector('#baseline-plot-container'),
            {scale: 3}
          ).then(
            canvas => {saveAs(canvas.toDataURL(), '%s')}
          )", fname)
        p(
          class = "plot-export", align = "center",
          em("Click on any of the plot legend items to show or hide it in the plot. Click and drag left-to-right to select a date range (double click to reset).", a("Click here to download this plot as a PNG.", style = "cursor: pointer;", onclick = onclick))
        )
      })


      # Station summary box ----

      ## stnSummaryUI ----
      output$stnSummaryUI <- renderUI({
        req(stnSummaryData())

        div(
          class = "well",
          style = "overflow: auto",
          h4("Station data summary", style = "border-bottom: 2px solid #d0d7d9;"),
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
        "ambient_air_temp", "Air temperature", "°C",
        "transparency_average", "Transparency", "cm",
        "streamflow_cfs", "Stream flow", "cfs",
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
          )
        )
      })

      ## dataTable ----
      output$dataTable <- renderDataTable({
        req(selected_data_ready())

        date_fmt <- ifelse(input$year == "All", "%b %d, %Y", "%b %d")

        df <- selected_data() %>%
          arrange(date) %>%
          distinct(date, .keep_all = TRUE) %>%
          clean_names(case = "title") %>%
          mutate(label = format(Date, date_fmt)) %>%
          mutate(across(everything(), as.character)) %>%
          pivot_longer(cols = -label, names_to = "Parameter") %>%
          pivot_wider(names_from = label) %>%
          mutate(Parameter = gsub("D o", "D.O.", Parameter)) %>%
          mutate(Parameter = gsub("P h", "pH", Parameter))

        datatable(df, selection = "none", options = list(paging = FALSE))
      },
        server = F)

      ## downloadYear ----
      output$downloadYear <- downloadHandler(
        paste0("stn-", cur_stn()$station_id, "-baseline-data-", input$year, ".csv"),
        function(file) {write_csv(selected_data(), file)}
      )

      ## downloadAll ----
      output$downloadAll <- downloadHandler(
        paste0("stn-", cur_stn()$station_id, "-baseline-data.csv"),
        function(file) {write_csv(cur_data(), file)}
      )

      # Return values ----
      return(reactive(list(year = input$year)))
    }
  )
}
