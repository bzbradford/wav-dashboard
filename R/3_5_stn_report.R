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
        stn_id <- cur_stn()$station_id

        list(
          baseline = filter(baseline_data, station_id == stn_id),
          nutrient = filter(nutrient_data, station_id == stn_id),
          thermistor = filter(therm_data, station_id == stn_id)
        )
      })

      ## avail_years ----
      avail_years <- reactive({
        req(cur_stn())
        as.numeric(unlist(filter(all_coverage, station_id == cur_stn()$station_id)$data_year_list))
      })

      ## report ----
      report <- reactiveValues()

      ## reacts to button clicks to download a year's report ----
      observeEvent(input$year, {
        message(input$year)

        report$filename <- paste0(
          "WAV ", input$year, " Report for ",
          cur_stn()$station_id, " ",
          str_trunc(fs::path_sanitize(cur_stn()$station_name), 30),
          ".pdf"
        )
        report$stn <- cur_stn()
        report$data <- sapply(stn_data(), function(df) {
          filter(df, year == input$year)
        })

        # trigger the downloadbutton once data is set up
        click("download")
      })


      ## mainUI ----
      output$mainUI <- renderUI({
        tagList(
          h4("Downloads for", cur_stn()$label, align = "center"),
          div(
            align = "center",
            class = "report-tbl",
            style = "overflow: auto;",
            tableOutput(ns("avail_reports_tbl")) %>% withSpinnerProxy(),
          ),
          div(
            style = "visibility: hidden; height: 0px;",
            downloadButton(ns("download")),
          ),
          div(
            id = "report-msg-container",
            class = "notice notice-error",
            style = "margin-top: 1em; display: none;",
            div(
              HTML("&#10006;"),
              style = "float: right; cursor: pointer;",
              onclick = "this.parentElement.style.display = 'none';"
            ),
            div(id = "report-msg")
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
              onclick = sprintf("
                Shiny.setInputValue('%s', %s, {priority: 'event'});
                this.disabled = true;
                this.innerHTML = 'Please wait...';
                document.querySelector('#report-msg-container').style.display = 'none';
              ", ns("year"), yr)
            ) %>% as.character()
          }))
      })

      ## avail_reports_tbl ----
      output$avail_reports_tbl <- renderTable(
        avail_reports(),
        striped = T,
        hover = T,
        width = "100%",
        align = "c",
        na = "-",
        sanitize.text.function = identity
      )

      ## download ----
      output$download <- downloadHandler(
        filename = function() { report$filename },
        content = function(file) {
          final_out <- file.path(tempdir(), report$filename)
          use_existing <- FALSE
          if (file.exists(final_out) & use_existing) {
            message('file existed')
            file.copy(final_out, file)
          } else {
            tryCatch({
              template <- file.path("md", "station_report.Rmd")
              temp_dir <- tempdir()
              temp_in <- file.path(temp_dir, "report.Rmd")
              file.copy(template, temp_in, overwrite = TRUE)
              rmarkdown::render(
                input = temp_in,
                output_file = final_out,
                params = list(
                  year = input$year,
                  stn = report$stn,
                  data = report$data
                )
              )
              file.copy(final_out, file)
            }, error = function(cond) {
              runjs(sprintf("
                document.querySelector('#report-msg').innerHTML = 'Failed to create report: %s';
                document.querySelector('#report-msg-container').style.display = null;
              ", cond$message))
            })
          }
          runjs(sprintf("
            let btn = document.querySelector('#report-btn-%s');
            btn.disabled = false;
            btn.innerHTML = 'Download PDF';
          ", input$year))
        }
      )


      # end
    }
  )
}
