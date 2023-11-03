## NUTRIENT TAB ##

# Helpers ----

phoslimit <- 0.075 # mg/L, ppm

get_phos_estimate <- function(vals) {
  vals <- na.omit(vals)
  log_vals <- log(vals)
  n <- length(vals)
  meanp <- mean(log_vals)
  se <- sd(log_vals) / sqrt(n)
  suppressWarnings({
    tval <- qt(p = 0.90, df = n - 1)
  })

  params <- list(
    mean = meanp,
    median = median(log_vals),
    lower = meanp - tval * se,
    upper = meanp + tval * se
  )

  params <- lapply(params, exp)
  params <- lapply(params, round, 3)
  params["n"] <- n
  params
}

get_phos_exceedance <- function(median, lower, upper, limit = phoslimit) {
  fail_msg <- "Unable to determine phosphorus exceedance type based on the data shown above."

  if (anyNA(c(median, lower, upper))) return(fail_msg)

  if (lower >= limit) {
    "Total phosphorus clearly exceeds the DNR's criteria (lower confidence interval > phosphorus limit.)"
  } else if (lower <= limit & median >= limit) {
    "Total phosphorus may exceed the DNR's criteria (median greater than phosphorus limit, but lower confidence interval below limit)."
  } else if (upper >= limit & median <= limit) {
    "Total phosphorus may meet the DNR's criteria (median below phosphorus limit, but upper confidence interval above limit)."
  } else if (upper <= limit) {
    "Total phosphorus clearly meets the DNR's criteria (upper confidence interval below limit)."
  } else {
    fail_msg
  }
}


# UI ----

nutrientDataUI <- function() {
  ns <- NS("nutrient")

  div(
    class = "data-tab",
    uiOutput(ns("content"))
  )
}


# Server ----

#' requires global data frame 'nutrient_data'
#' @param cur_stn a `reactive()` expression containing the current station

nutrientDataServer <- function(cur_stn) {
  moduleServer(
    id = "nutrient",
    function(input, output, session) {
      ns <- session$ns


      ## Reactives ----

      cur_data <- reactive({
        req(cur_stn())

        filter(nutrient_data, station_id == cur_stn()$station_id)
      })

      data_ready <- reactive({
        nrow(cur_data()) > 0
      })

      cur_years <- reactive({
        sort(unique(cur_data()$year))
      })

      selected_data <- reactive({
        if (input$year == "All") {
          cur_data()
        } else {
          cur_data() %>% filter(year == input$year)
        }
      })

      selected_data_ready <- reactive({
        nrow(cur_data()) > 0
      })

      phos_estimate <- reactive({
        req(selected_data_ready())

        get_phos_estimate(selected_data()$tp)
      })

      phos_exceedance_text <- reactive({
        params = phos_estimate()
        get_phos_exceedance(
          median = params$median,
          lower = params$lower,
          upper = params$upper
        )
      })


      ## Layout ----

      output$content <- renderUI({
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
            plotlyOutput(ns("plot")),
            uiOutput(ns("plotCaptionUI"))
          ),
          uiOutput(ns("plotExportUI")),
          br(),
          p("The shaded horizontal band on the plot represents the 90% confidence interval for the median total phosphorus (TP) at this site (if more than one month of data was collected). This means that, given the TP concentrations measured this year, there is about an 90% chance that the true median total phosphorus concentration falls somewhere between those lines. We know that TP in streams varies quite a bit, so individual samples could be higher or lower than the confidence interval."),
          p(HTML("<strong>Exceedance criteria.</strong> A stream site <b><i>clearly exceeds</i></b> the phosphorus limit and the confidence interval band will be shaded <b><span style='color: red'>red</span></b> if the lower 90% confidence limit of the sample median exceeds the state total phosphorus limit of 0.075 mg/L. A stream site <b><i>may exceed</i></b> the phosphorus limit if the median is higher than the phosphorus limit, but the lower confidence interval is below the limit. A stream site <b><i>may meet</i></b> the phosphorus criteria if the median is lower than the phosphorus limit, but the upper confidence interval remains above the limit. When the entire confidence interval is below the phosphorus limit, the site <b><i>clearly meets</i></b> the phosphorus limit, and the shaded confidence interval band in the plot above will be colored <b><span style='color: teal'>teal</span></b>.")),
          p(strong("Why phosphorus?"), "Phosphorus is an essential nutrient responsible for plant growth, but it is also the most visible, widespread water pollutant in lakes. Small increases in phosphorus levels can bring about substantial increases in aquatic plant and algae growth, which in turn can reduce the recreational use and biodiversity. When the excess plants die and are decomposed, oxygen levels in the water drop dramatically which can lead to fish kills. Additionally, one of the most common impairments in Wisconsin’s streams is excess sediment that covers stream bottoms. Since phosphorus moves attached to sediments, it is intimately connected with this source of pollution in our streams. Phosphorus originates naturally from rocks, but its major sources in streams and lakes today are usually associated with human activities: soil erosion, human and animal wastes, septic systems, and runoff from farmland or lawns. Phosphorus-containing contaminants from urban streets and parking lots such as food waste, detergents, and paper products are also potential sources of phosphorus pollution from the surrounding landscape. The impact that phosphorus can have in streams is less apparent than in lakes due to the overall movement of water, but in areas with low velocity, where sediment can settle and deposit along the bottom substrate, algae blooms can result."),
          p(strong("Volunteer monitoring protocol."), "To assess in-stream phosphorus levels, WAV volunteers collected water samples that were analyzed for total phosphorus (TP) at the State Lab of Hygiene during the growing season. Following Wisconsin Department of Natural Resources (WDNR) methods, six phosphorus water samples were collected at each monitoring site - one per month for six months during the growing season. The monthly water samples were collected approximately 30 days apart and no samples were collected within 15 days of one another."),
          br(),
          bsCollapse(
            bsCollapsePanel(
              title = "View or download nutrient data data",
              uiOutput(ns("viewDataUI"))
            )
          )
        )
      })


      ## Plot ----

      output$plot <- renderPlotly({
        req(selected_data_ready())

        df <- selected_data() %>%
          mutate(exceedance = factor(
            ifelse(is.na(tp), "No data", ifelse(tp >= phoslimit, "TP High", "TP OK")),
            levels = c("TP OK", "TP High", "No data"))) %>%
          drop_na(tp) %>%
          mutate(phoslimit = phoslimit)

        min_year <- min(df$year)
        max_year <- max(df$year)
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


      ## Plot caption UI ----

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


      ## Plot export UI ----

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


      ## View data UI ----

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

      output$dataTable <- renderDataTable({
        req(selected_data_ready())

        selected_data() %>%
          drop_na(tp) %>%
          clean_names(case = "big_camel")
      })

      output$downloadYear <- downloadHandler(
        paste0("stn-", cur_stn()$station_id, "-nutrient-data-", input$year, ".csv"),
        function(file) {write_csv(selected_data(), file)}
      )

      output$downloadAll <- downloadHandler(
        paste0("stn-", cur_stn()$station_id, "-nutrient-data.csv"),
        function(file) {write_csv(cur_data(), file)}
      )
    }
  )
}
