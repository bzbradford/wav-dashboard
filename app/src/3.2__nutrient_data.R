## NUTRIENT TAB ##

nutrientDataUI <- function() {
  ns <- NS("nutrient")

  div(
    class = "data-tab",
    uiOutput(ns("ui")) %>% with_spinner(),
  )
}

#' @requires `nutrient_data`
#' @param main_rv reactive values object from main server session
nutrientDataServer <- function(main_rv) {
  moduleServer(
    id = "nutrient",
    function(input, output, session) {
      ns <- session$ns


      # Reactives --------------------------------------------------------------

      ## rv$ready ----
      rv <- reactiveValues(
        ready = FALSE
      )

      ## rv$ready handler ----
      observe({
        ready <- nrow(stn_data()) > 0
        if (rv$ready != ready) rv$ready <- ready
      })

      ## cur_stn ----
      cur_stn <- reactive({
        req(main_rv$cur_stn)
      })

      ## stn_data ----
      stn_data <- reactive({
        nutrient_data %>%
          filter(station_id == cur_stn()$station_id)
      })

      ## selected_data ----
      selected_data <- reactive({
        df <- stn_data()
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


      # UI ---------------------------------------------------------------------

      ## ui ----
      output$ui <- renderUI({
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
            uiOutput(ns("plot_caption_ui"))
          ),
          uiOutput(ns("plot_export_ui")),
          div(
            style = "margin-top: 1em; margin-bottom: 2em;",
            includeMarkdown("md/nutrient_info.md")
          ),
          dataViewUI(ns("data_view"))
        )
      })

      ## dataViewServer ----
      dataViewServer(
        id = "data_view",
        dataset_name = "Nutrient",
        master_data = nutrient_data,
        cur_stn = cur_stn,
        stn_data = stn_data,
        selected_data = selected_data
      )

      ## year_select_ui ----
      output$year_select_ui <- renderUI({
        yrs <- sort(unique(stn_data()$year))
        div(
          class = "well flex-row year-btns",
          style = "margin-bottom: 1rem;",
          div(class = "year-btn-text", em("Choose year:")),
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
      output$plot_caption_ui <- renderUI({
        phostext <- phos_estimate() %>%
          get_phos_exceedance_text()

        tagList(
          div(
            class = "plot-caption",
            "The dashed line on this plot indicates the total phosphorus state exceedance level of 0.075 mg/L (ppm). If more than one month of data was collected, the median and 80% confidence interval for the true total phosphorus level are displayed as a horizontal band. A zero value indicates the submitted sample was below the limit of detection.",
            br(),
            strong(phostext)
          ),
        )
      })

      ## plot ----
      output$plot <- renderPlotly({
        plotly_nutrient(
          df = selected_data(),
          phoslimit = phoslimit,
          phos_estimate = phos_estimate()
        )
      })

      ## plot_export_ui ----
      output$plot_export_ui <- renderUI({
        yr <- req(input$year)
        stn <- cur_stn()
        p(
          align = "center",
          build_plot_download_btn(
            id = "#nutrient-plot-container",
            filename = sprintf("WAV Nutrient Data - Stn %s - %s.png", stn$station_id, yr)
          )
        )
      })

    }
  )
}
