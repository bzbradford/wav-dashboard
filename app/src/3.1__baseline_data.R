## BASELINE TAB ##

baselineDataUI <- function() {
  ns <- NS("baseline")

  div(
    class = "data-tab",
    uiOutput(ns("main_ui_wrapper")) %>% with_spinner(),
  )
}

#' @requires `baseline_data`
#' @param main_rv reactive values object from main server session
baselineDataServer <- function(main_rv) {
  moduleServer(
    id = "baseline",
    function(input, output, session) {
      ns <- session$ns


      # Reactive values ----

      ## rv ----
      rv <- reactiveValues(
        # controls rendering of the interface
        ready = FALSE,

        # baseline data for this station, if it exists
        stn_data = NULL,

        # min/mean/max by param for the summary table
        stn_summary_data = NULL
      )

      ## cur_stn ----
      cur_stn <- reactive({
        req(main_rv$cur_stn)
      })

      ## set rv$stn_data ----
      observe({
        stn <- cur_stn()
        data <- baseline_data %>%
          filter(station_id == stn$station_id)
        if (nrow(data) == 0) {
          if (rv$ready) rv$ready <- FALSE
          rv$stn_data <- NULL
        } else {
          if (!rv$ready) rv$ready <- TRUE
          rv$stn_data <- data
        }
      })

      ## annual_stn_data ----
      # for when the annual plot type is selected. Used for plot and summary tbl
      annual_stn_data <- reactive({
        yr <- req(input$plot_year)
        df <- req(rv$stn_data) %>%
          filter(year == yr)
        req(nrow(df) > 0)
        df
      })

      ## stn_years ----
      stn_years <- reactive({
        df <- req(rv$stn_data)
        sort(unique(df$year))
      })

      ## set rv$stn_summary_data ----
      observe({
        df <- if (req(input$plot_type) == "annual") {
          annual_stn_data()
        } else {
          req(rv$stn_data)
        }

        df <- build_baseline_summary(df)

        if (!identical(rv$stn_summary_data, df)) {
          rv$stn_summary_data <- df
        }
      })

      ## parameter_choices ----
      parameter_choices <- reactive({
        df <- req(rv$stn_data)
        get_baseline_param_choices(df)
      })

      ## plot_type_choices ----
      plot_type_choices <- reactive({
        c(
          list("Annual" = "annual"),
          list("Long-term" = "trend"),
          if ("biotic_index_score" %in% parameter_choices())
            list("Macroinvertebrate" = "macro"),
          list("Heatmap" = "ribbon")
        )
      })



      # Main UI ----------------------------------------------------------------

      ## main_ui_wrapper ----
      output$main_ui_wrapper <- renderUI({
        if (rv$ready) {
          uiOutput(ns("main_ui"))
        } else {
          div(class = "well", "This station has no baseline data. Choose another station or view the nutrient or thermistor data associated with this station.")
        }
      })

      ## main_ui ----
      output$main_ui <- renderUI({
        tagList(
          div(
            class = "well flex-row",
            style = "margin-bottom: 1rem;",
            uiOutput(ns("plot_type_ui")),
            conditionalPanel(
              "input.plot_type == 'annual'", ns = ns,
              uiOutput(ns("plot_year_ui"))
            ),
            conditionalPanel(
              "input.plot_type == 'trend'", ns = ns,
              uiOutput(ns("trend_type_ui"))
            ),
            conditionalPanel(
              "input.plot_type == 'trend'", ns = ns,
              uiOutput(ns("trend_param_ui"))
            ),
            conditionalPanel(
              "input.plot_type == 'macro'", ns = ns,
              uiOutput(ns("macro_type_ui"))
            ),
          ),
          div(
            id = "baseline-plot-container",
            uiOutput(ns("stn_title_ui")),
            conditionalPanel(
              "input.plot_type == 'annual'", ns = ns,
              plotlyOutput(ns("annual_plot")),
              div(class = "plot-caption", "A selection of available baseline parameters are shown above. Click on an item in the legend below the plot to hide/show individual parameters.")
            ),
            conditionalPanel(
              "input.plot_type == 'trend'", ns = ns,
              plotlyOutput(ns("trend_plot")),
              div(class = "plot-caption", textOutput(ns("trend_plot_caption")))
            ),
            conditionalPanel(
              "input.plot_type == 'macro'", ns = ns,
              plotlyOutput(ns("macro_plot"), height = "500px"),
              div(class = "plot-caption", HTML(paste0(
                "The aquatic macroinvertebrate community can reflect a stream’s general condition, as some species are more sensitive to water quality than others. ",
                colorize("Group 1 (blue)", "blue"),
                " are the most sensitive. ",
                colorize("Group 2 (green)", "green"),
                " are somewhat sensitive, ",
                colorize("Group 3 (orange)", "orange"),
                " are somewhat tolerant, followed by ",
                colorize("Group 4 (red)", "red"),
                ", the most tolerant. Suspected invasive species are shown in ",
                colorize("purple"),
                "."
              )))
            ),
            conditionalPanel(
              "input.plot_type == 'ribbon'", ns = ns,
              uiOutput(ns("ribbon_plot_ui")),
              div(class = "plot-caption", "This figure shows which parameters have been measured for this station, with tickmarks showing Jan 1 of each year, and each column represents one month of observations.")
            ),
            div(class = "plot-caption", htmlOutput(ns("plot_caption_text"))),
          ),
          div(
            style = "display: flex; flex-direction: row-reverse;",
            uiOutput(ns("plot_dl_btn"))
          ),
          accordion(
            accordion_panel(
              title = "Selected data summary",
              style = "background-color: rgba(0, 0, 0, 0.025);",
              div(
                style = "overflow: auto;",
                tableOutput(ns("stn_summary_tbl"))
              )
            )
          ),
          includeMarkdown("md/baseline_info.md"),
          accordion(
            accordion_panel(
              title = "View/download baseline data",
              class = "btn-primary",
              uiOutput(ns("stn_data_ui"))
            ),
            open = FALSE
          )
        )
      })

      ## trend_plot_caption ----
      output$trend_plot_caption <- renderText({
        paste(switch(
          req(input$trend_type),
          "scatter" = "All observations for the selected parameter are shown above.",
          "month" = "Measurements from each month across all years are summarized using boxplots, which illustrate the median value (solid central bar), mean value (dashed central bar), Q1-Q3 interquartile range (main box) and full value range (whiskers). Individual observations are overlaid as points.",
          "year" = "Measurements from each year are summarized using boxplots, which illustrate the median value, mean value, interquartile range (main box), and full value range (whiskers). Individual observations are overlaid as points."
        ), "Interpretive ranges are illustrated to contextualize the observations.")
      })


      # Plot options ----

      ## plot_type_ui ----
      output$plot_type_ui <- renderUI({
        id <- "plot_type"
        choices <- plot_type_choices()
        selected <- first_truthy(
          intersect(isolate(input[[id]]), choices),
          first(choices)
        )

        radioGroupButtons(
          inputId = ns(id),
          label = "Plot type",
          size = "sm",
          choices = choices,
          selected = selected
        )
      })

      ## plot_year_ui ----
      output$plot_year_ui <- renderUI({
        id <- "plot_year"
        # choices <- req(rv$stn_years)
        choices <- stn_years()
        selected <- first_truthy(
          intersect(isolate(input[[id]]), choices),
          last(choices)
        )

        radioGroupButtons(
          inputId = ns(id),
          label = "Year",
          size = "sm",
          choices = choices,
          selected = selected
        )
      })

      ## trend_type_ui ----
      output$trend_type_ui <- renderUI({
        id <- "trend_type"
        choices <- list(
          "None" = "scatter",
          "Month" = "month",
          "Year" = "year"
        )

        radioGroupButtons(
          inputId = ns(id),
          label = "Group by",
          size = "sm",
          choices = choices,
          selected = isolate(input[[id]]) %||% first(choices)
        )
      })

      ## trend_param_ui ----
      output$trend_param_ui <- renderUI({
        id <- "trend_param"
        choices <- parameter_choices()

        radioGroupButtons(
          inputId = ns(id),
          label = "Parameter",
          size = "sm",
          choices = choices,
          selected = isolate(input[[id]]) %||% first(choices)
        )
      })

      ## macro_type_ui ----
      output$macro_type_ui <- renderUI({
        id <- "macro_type"
        choices <- list(
          "Year" = "annual",
          "Date" = "all"
        )

        radioGroupButtons(
          inputId = ns(id),
          label = "Show by",
          size = "sm",
          choices = choices,
          selected = isolate(input[[id]]) %||% first(choices)
        )
      })


      # Station title ----

      ## stn_title_ui ----
      output$stn_title_ui <- renderUI({
        stn <- cur_stn()
        title <- str_to_title(stn$label)
        h3(title, align = "center")
      })


      # Plots ----

      ## annual_plot ----
      output$annual_plot <- renderPlotly({
        df <- annual_stn_data()
        plotly_baseline(df)
      })

      ## trend_plot ----
      output$trend_plot <- renderPlotly({
        value_col <- req(input$trend_param)
        type <- req(input$trend_type)
        df <- req(rv$stn_data)

        plotly_baseline_trend(df, value_col, type)
      })

      ## macro_plot ----
      output$macro_plot <- renderPlotly({
        stn <- cur_stn()
        type <- req(input$macro_type)

        plotly_macros(stn$station_id, type)
      })

      ## ribbon_plot_ui ----
      # height of the ribbon plot changes based on number of params
      output$ribbon_plot_ui <- renderUI({
        params <- parameter_choices()

        plotlyOutput(ns("ribbon_plot"), height = 50 + 20 * length(params))
      })

      ## ribbon_plot ----
      output$ribbon_plot <- renderPlotly({
        df <- req(rv$stn_data)

        plotly_baseline_ribbon(df)
      })


      # Plot download button ----

      ## plot_dl_btn ----
      # TODO: input$trend_param should use pretty names
      output$plot_dl_btn <- renderUI({
        filename <- paste(
          paste("WAV Stn", cur_stn()$station_id, req(input$plot_type), "plot"),
          switch(req(input$plot_type),
            "annual" = req(input$plot_year),
            "trend" = local({
              params <- parameter_choices()
              paste(req(input$trend_type), names(params[params == req(input$trend_param)]), sep = " - ")
            }),
            "macro" = req(input$macro_type)
          ),
          sep = " - "
        )

        build_plot_download_btn(
          id = "#baseline-plot-container",
          filename = paste0(str_to_title(filename), ".png")
        )
      })


      # Station data summary ----

      ## stn_summary_tbl ----
      output$stn_summary_tbl <- renderTable(
        req(rv$stn_summary_data),
        width = "100%",
        spacing = "xs",
        align = "lcccccc"
      )


      # Data downloads ----

      ## stn_data_ui ----
      output$stn_data_ui <- renderUI({
        stn <- cur_stn()
        # yrs <- req(rv$stn_years)
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
              ),
              # div(
              #   class = "flex-row align-center",
              #   materialSwitch(
              #     inputId = ns("dt_hide"),
              #     label = strong("Hide empty?"),
              #     inline = TRUE,
              #     value = TRUE
              #   )
              # )
            )
          ),
          p(
            downloadButton(ns("dl_cur_data"), "Download this data"),
            downloadButton(ns("dl_all_baseline"), "Download all baseline data"),
          ),
          dataTableOutput(ns("dt"))
        )
      })

      ## stn_dl_data ----
      stn_dl_data <- reactive({
        df <- req(rv$stn_data)
        yr <- req(input$dt_year)
        if (yr != "All years") df <- filter(df, year == yr)
        df
      })

      ## stn_dt_data ----
      stn_dt_data <- reactive({
        transpose <- req(input$dt_transpose) == "Columns"
        stn_dl_data() %>%
          merge_unit_cols() %>%
          format_for_dt(transpose)
      })

      ## dt ----
      output$dt <- renderDataTable({
        stn_dt_data() %>%
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
        filename = function() {
          stn <- cur_stn()
          yr <- req(input$dt_year)
          paste0(
            "WAV Stn ", stn$station_id, " Baseline Data",
            ifelse(yr == "All years", "", str_glue(" ({yr})")),
            ".csv"
          )
        },
        content = function(file) {
          transpose <- req(input$dt_transpose) == "Columns"
          stn_dl_data() %>%
            format_for_dt(transpose) %>%
            write_csv(file, na = "")
        }
      )

      ## dl_all_baseline ----
      output$dl_all_baseline <- downloadHandler(
        filename = "WAV Baseline Data.csv",
        content = function(file) {
          write_csv(baseline_data, file, na = "")
        }
      )
    }
  )
}
