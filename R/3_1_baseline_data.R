## BASELINE TAB ##

# Helpers ----

do_color <- function(do) {
  i <- min(max(round(do), 1), 11)
  brewer.pal(11, "RdBu")[i]
}

find_max <- function(vals, min_val) {
  vals <- na.omit(vals)
  if (length(vals) == 0) return(min_val)
  ceiling(max(min_val, max(vals)) * 1.1)
}

make_min_max <- function(df, var) {
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


# UI ----

baselineDataUI <- function() {
  ns <- NS("baseline")

  div(
    class = "data-tab",
    uiOutput(ns("content")) %>% withSpinnerProxy(),
  )
}


# Server ----

#' requires global data frame 'baseline_data'
#' @param cur_stn a `reactive()` single line data frame

baselineDataServer <- function(cur_stn) {
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


      # Layout ----

      ## content | Primary UI----
      output$content <- renderUI({
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
          uiOutput(ns("stnSummaryUI")) %>% withSpinnerProxy(hide.ui = FALSE, proxy.height = 200),
          p(strong("Dissolved Oxygen:"), "The amount of dissolved oxygen (D.O.) in a stream is critical for aquatic life, particularly larger animals like fish. 5 mg/L is considered the minimum level for fish, while 7 mg/L is the minimum required by trout in the spawning season. Colder waters can support higher concentrations of dissolved oxygen than warmer waters. The percent saturation refers to the equilibrium amount of oxygen that can dissolve into the water from the atmosphere. Higher than 100% D.O. saturation means oxygen is being actively added to the water, either by aquatic life or by air-water mixing. Lower than 100% D.O. saturation means the dissolved oxygen has been depleted below equilibrium level by plant or algal respiration or decomposition."),
          p(strong("Temperature:"), "The chart shows both the recorded air temperature and water temperature. Cold streams generally provide better habitat because they can contain higher levels of dissolved oxygen, and higher water temperatures may indicate shallow, pooled, or stagnant water. Learn more about water temperature on the", strong("Thermistor"), "data tab."),
          p(strong("Transparency:"), "These measurements reflect the turbidity of the stream water. Lower transparency means the water is cloudier/murkier and could indicate a recent storm event kicking up silt and mud in the stream. Lower transparency isn't necessarily bad but can be associated with warm waters with low dissolved oxygen and lots of suspended algae."),
          p(strong("Stream flow:"), "Stream flow measurements give you an idea of the general size of the stream. Periods of higher than normal stream flow would suggest recent rains in the watershed. Most streams have a consistent and predictable stream flow based on the size of the watershed that they drain, but stream flow will 'pulse' after rain and storm events, returning to baseflow after several days. For more stream flow data check out the", a("USGS Water Dashboard", href = "https://dashboard.waterdata.usgs.gov/app/nwd/?aoi=state-wi", target = "_blank", .noWS = "after"), "."),
          br(),
          bsCollapse(
            bsCollapsePanel(
              title = "View/download baseline data",
              uiOutput(ns("viewDataUI"))
            )
          )
        )
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
        p(
          style = "margin-left: 2em; margin-right: 2em; font-size: smaller;",
          align = "center",
          em("Click on any of the plot legend items to show or hide it in the plot. Click and drag left-to-right to select a date range (double click to reset).",
            a("Click here to download this plot as a PNG.",
              style = "cursor: pointer;",
              onclick = paste0(
                "html2canvas(document.querySelector('",
                "#baseline-plot-container",
                "'), {scale: 3}).then(canvas => {saveAs(canvas.toDataURL(), '",
                paste("baseline-plot", cur_stn()$station_id, input$year, sep = "-"),
                ".png",
                "')})")
            )
          )
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
        "streamflow_cfs", "Stream flow", "cfs"
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
