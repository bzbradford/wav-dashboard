## REPORT TAB ##

stnReportUI <- function() {
  ns <- NS("report")

  tagList(
    div(
      class = "data-tab",
      uiOutput(ns("mainUI")) %>% withSpinnerProxy()
    )
  )
}


stnReportServer <- function(cur_stn, has_focus) {
  moduleServer(
    id = "report",
    function(input, output, session) {
      ns <- session$ns

      # Reactive values ----

      ## stn_data ----
      stn_data <- reactive({
        req(cur_stn())
        req(has_focus())

        list(
          baseline = baseline_data %>%
            filter(station_id == cur_stn()$station_id),
          nutrient = nutrient_data %>%
            filter(station_id == cur_stn()$station_id),
          thermistor = therm_data %>%
            filter(station_id == cur_stn()$station_id)
        )
      })

      ## avail_years ----
      avail_years <- reactive({
        years <- stn_data() %>%
          lapply(function(df) unique(df$year)) %>%
          unlist() %>%
          unique() %>%
          sort()
      })
      observe(print(avail_years()))

      # selected_year <- reactive({
      #   req(input$download_year)
      #   input$download_year
      # })
      #
      # observe(print(selected_year()))

      selected_data <- reactive({
        req(input$year)
        sapply(stn_data(), function(df) {
          filter(df, year == input$year)
        })
      })
      observe(print(selected_data()))

      observeEvent(input$year, {
        message(input$year)
        # runjs(sprintf("document.querySelector('#report-btn-%s').disabled = false", input$year))
        click("download")
      })


      ## mainUI ----
      output$mainUI <- renderUI({
        tagList(
          h4("Downloads for", cur_stn()$station_name, align = "center"),
          div(
            align = "center",
            class = "report-tbl",
            tableOutput(ns("avail_reports_tbl")),
          ),
          div(
            style = "visibility: hidden; height: 0px;",
            downloadButton(ns("download"))
          )

        )
      })

      ## avail_reports ----
      avail_reports <- reactive({
        df <- stn_data()
        obs <- list(
          baseline = df$baseline %>%
            count(year, name = "Baseline data<br>fieldwork events"),
          nutrient = df$nutrient %>%
            count(year, name = "Nutrient monitoring<br>monthly observations"),
          thermistor = df$thermistor %>%
            count(year, date) %>%
            count(year, name = "Temperature logger<br>days deployed")
        )
        tibble(year = avail_years()) %>%
          left_join(obs$baseline, join_by(year)) %>%
          left_join(obs$nutrient, join_by(year)) %>%
          left_join(obs$thermistor, join_by(year)) %>%
          mutate(year = as.character(year)) %>%
          rename(Year = year) %>%
          mutate(`Download<br>report` = lapply(Year, function(yr) {
            tags$button(
              "Download PDF",
              id = paste0("report-btn-", yr),
              class = "btn btn-default btn-sm",
              onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'}); this.disabled = true;", ns("year"), yr)
            ) %>% as.character()
          }))
      })

      output$avail_reports_tbl <- renderTable(
        avail_reports(),
        striped = T,
        hover = T,
        align = "c",
        na = "-",
        sanitize.text.function = identity
      )

      # output$avail_reports_tbl <- renderDataTable(
      #   avail_reports(),
      #   escape = FALSE,
      #   options = list(
      #     preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      #     drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
      #   )
      # )

      # glue("{yr}-{stn_id}-{stn_name}.pdf")
      output$download <- downloadHandler(
        filename = function() {
          fname <- str_trim(paste0(
            "WAV ", input$year, " Report for ",
            cur_stn()$station_id, " ",
            str_trunc(fs::path_sanitize(cur_stn()$station_name), 25)
          ))
          fname <- paste0(fname, ".csv")
          print(fname)
          fname
          # "foo.csv"
        },
        content = function(file) {
          # yr <- input$download_year
          # disable(paste0("report-btn-", yr))
          # stn <- cur_stn()
          # stn_id <- stn$station_id
          # stn_name <- str_trunc(fs::path_sanitize(stn$station_name), 25, ellipsis = "")
          # report_data <- sapply(stn_data(), function(df) { filter(df, year == yr) })
          # message(yr)
          # print(report_data)
          # fpath <- file.path(tempdir(), fname)
          # if (file.exists(fpath)) return(file.copy(fpath, file))
          #
          # tempfile <- tempfile()
          # template <- "./Rmd/station_report.Rmd"
          # # rmarkdown::render(
          # #   input = template,
          # #   output_file = tempfile,
          # #   params = list(
          # #     year = yr,
          # #     stn = stn,
          # #     data = report_data
          # #   )
          # # )
          # message("foo")
          #
          # # file.remove(tempfile)
          # # file.copy(tempfile, fpath)
          # # file.copy(fpath, file)
          # enable(paste0("report-btn-", yr))
          # write.csv(data.frame(x = runif(5), y = rnorm(5)), file)
          write_csv(selected_data()$baseline, file)
          runjs(sprintf("document.querySelector('#report-btn-%s').disabled = false", input$year))
        }
      )


      # end
    }
  )
}
