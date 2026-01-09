## NUTRIENT TAB ##

nutrientDataUI <- function() {
  ns <- NS("nutrient")

  div(
    class = "data-tab",
    uiOutput(ns("main_ui_wrapper")) |> with_spinner(),
  )
}

#' @requires `nutrient_data`
#' @param main_rv reactive values object from main server session
nutrientDataServer <- function(main_rv) {
  moduleServer(
    id = "nutrient",
    function(input, output, session) {
      ns <- session$ns


      # Reactive values ----

      ## rv$ready ----
      rv <- reactiveValues(
        # controls rendering of the interface
        ready = FALSE,

        # nutrient data for this station, if it exists
        stn_data = NULL,
      )

      ## cur_stn ----
      cur_stn <- reactive({
        req(main_rv$cur_stn)
      })

      ## set rv$stn_data ----
      observe({
        stn <- cur_stn()
        data <- nutrient_data |>
          filter(station_id == stn$station_id)
        if (nrow(data) == 0) {
          if (rv$ready) rv$ready <- FALSE
          rv$stn_data <- NULL
        } else {
          if (!rv$ready) rv$ready <- TRUE
          rv$stn_data <- data
        }
      })

      stn_years <- reactive({
        df <- req(rv$stn_data)
        sort(unique(df$year))
      })

      ## selected_data ----
      selected_data <- reactive({
        df <- req(rv$stn_data)
        yr <- req(input$year)
        df <- if (yr == "All") df else filter(df, year == yr)
        req(nrow(df) > 0)
        df
      })

      ## phos_estimate ----
      phos_estimate <- reactive({
        df <- selected_data()
        get_phos_estimate(df$tp)
      })


      # Main UI ----

      ## ui ----
      output$main_ui_wrapper <- renderUI({
        if (rv$ready) {
          uiOutput(ns("main_ui"))
        } else {
          div(class = "well", "This station has no nutrient data. Choose another station or view the baseline or thermistor data associated with this station.")
        }
      })

      ## main_ui ----
      output$main_ui <- renderUI({
        tagList(
          uiOutput(ns("year_select_ui")),
          div(
            id = "nutrient-plot-container",
            h3(textOutput(ns("stn_title_text")), align = "center"),
            plotlyOutput(ns("plot")),
            div(
              class = "plot-caption",
              "The dashed line on this plot indicates the total phosphorus state exceedance level of 0.075 mg/L (ppm). If more than one month of data was collected, the median and 80% confidence interval for the true total phosphorus level are displayed as a horizontal band. A zero value indicates the submitted sample was below the limit of detection.",
              br(),
              strong(uiOutput(ns("phos_text")))
            ),
          ),
          div(
            style = "display: flex; flex-direction: row-reverse;",
            uiOutput(ns("plot_dl_btn"))
          ),
          div(
            style = "margin-top: 1em; margin-bottom: 2em;",
            includeMarkdown("md/nutrient_info.md")
          ),
          accordion(
            accordion_panel(
              title = "View/download nutrient data",
              class = "btn-primary",
              uiOutput(ns("stn_data_ui"))
            ),
            open = FALSE
          )
        )
      })

      ## year_select_ui ----
      output$year_select_ui <- renderUI({
        yrs <- stn_years()
        div(
          class = "well flex-row year-btns",
          style = "margin-bottom: 1rem;",
          div(class = "control-label", "Choose year:"),
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

      ## stn_title_text ----
      output$stn_title_text <- renderText({
        cur_stn()$label
      })

      ## plot_caption_ui ----
      output$phos_text <- renderText({
        phos_estimate() |>
          get_phos_exceedance_text()
      })

      ## plot ----
      output$plot <- renderPlotly({
        plotly_nutrient(
          df = selected_data(),
          phoslimit = phoslimit,
          phos_estimate = phos_estimate()
        )
      })

      ## plot_dl_btn ----
      output$plot_dl_btn <- renderUI({
        yr <- req(input$year)
        stn <- cur_stn()
        build_plot_download_btn(
          id = "#nutrient-plot-container",
          filename = sprintf("WAV Nutrient Data - Stn %s - %s.png", stn$station_id, yr)
        )
      })

      # Data downloads ----

      ## stn_data_ui ----
      output$stn_data_ui <- renderUI({
        stn <- cur_stn()
        yrs <- stn_years()
        yr_choices <- if (length(yrs) > 1) c(yrs, "All years") else yrs
        tagList(
          p(
            strong("Station ID:"), stn$station_id, br(),
            strong("Station Name:"), stn$station_name, br(),
            strong("Waterbody:"), stn$waterbody
          ),
          wellPanel(
            div(
              class = "flex-row align-center",
              div(
                class = "flex-row align-center",
                div(class = "control-label", "Show data for:"),
                radioGroupButtons(
                  inputId = ns("dt_year"),
                  label = NULL,
                  size = "sm",
                  choices = yr_choices,
                  selected = last(yr_choices)
                )
              ),
              div(
                class = "flex-row align-center",
                div(class = "control-label", "Show observations in:"),
                radioGroupButtons(
                  inputId = ns("dt_transpose"),
                  label = NULL,
                  size = "sm",
                  choices = c("Columns", "Rows")
                )
              )
            )
          ),
          p(
            downloadButton(ns("dl_cur_data"), "Download this data"),
            downloadButton(ns("dl_all_data"), "Download entire phosphorus dataset")
          ),
          dataTableOutput(ns("dt"))
        )
      })

      ## stn_dt_data ----
      stn_dt_data <- reactive({
        df <- req(rv$stn_data)
        transpose <- req(input$dt_transpose) == "Columns"
        yr <- req(input$dt_year)

        if (yr != "All years") df <- filter(df, year == yr)

        format_for_dt(df, transpose)
      })

      ## dt ----
      output$dt <- renderDataTable({
        stn_dt_data() |>
          datatable(
            selection = "none",
            rownames = FALSE,
            extensions = "FixedColumns",
            options = list(
              paging = FALSE,
              scrollX = TRUE,
              scrollCollapse = TRUE,
              fixedColumns = list(leftColumns = 1)
            ),
            callback = JS("addTopScroll(table);")
          )
      }, server = FALSE)

      ## dl_cur_yr ----
      output$dl_cur_data <- downloadHandler(
        sprintf("WAV Stn %s Phosphorus Data (%s).csv", cur_stn()$station_id, req(input$dt_year)),
        function(file) {
          write_csv(stn_dt_data(), file, na = "")
        }
      )

      ## dl_all_baseline ----
      output$dl_all_data <- downloadHandler(
        "WAV Phosphorus Data.csv",
        function(file) {
          write_csv(nutrient_data, file, na = "")
        }
      )


    }
  )
}
