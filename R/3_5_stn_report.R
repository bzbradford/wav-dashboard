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

      temp_dir <- tempdir()

      # Reactive values ----

      ## report ----
      report <- reactiveValues()

      ## stn_data ----
      stn_data <- reactive({
        req(cur_stn())
        stn_id <- cur_stn()$station_id

        list(
          baseline = filter(baseline_data, station_id == stn_id),
          nutrient = filter(nutrient_data, station_id == stn_id),
          thermistor = filter(therm_data, station_id == stn_id)
        )
      })

      ## avail_reports ----
      avail_reports <- reactive({
        df <- stn_data()
        baseline_obs <- df$baseline %>%
          summarize(
            `Baseline<br>temperature` = sum(!is.na(air_temp) | !is.na(water_temp)),
            `Baseline<br>DO` = sum(!is.na(d_o) | !is.na(d_o_percent_saturation)),
            `Baseline<br>pH` = sum(!is.na(ph)),
            `Baseline<br>conductivity` = sum(!is.na(specific_cond)),
            `Baseline<br>transparency` = sum(!is.na(transparency)),
            `Baseline<br>streamflow` = sum(!is.na(streamflow)),
            .by = year
          )
        nutrient_obs <- df$nutrient %>%
          count(year, name = "Total<br>phosphorus")
        therm_obs <- df$thermistor %>%
          count(year, date) %>%
          count(year, name = "Continuous<br>temp. days")
        years <- sort(unique(c(baseline_obs$year, nutrient_obs$year, therm_obs$year)))

        tibble(year = years) %>%
          left_join(baseline_obs, join_by(year)) %>%
          left_join(nutrient_obs, join_by(year)) %>%
          left_join(therm_obs, join_by(year)) %>%
          rename(Year = year) %>%
          mutate(Year = as.character(Year)) %>%
          mutate(across(where(is.numeric), ~ifelse(.x == 0, NA, .x))) %>%
          mutate(`Download<br>report` = lapply(Year, function(yr) {
            tags$button(
              HTML("<i class='fas fa-download'></i>"),
              "Download PDF",
              id = paste0("report-btn-", yr),
              class = "btn btn-default btn-sm",
              onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'});", ns("year"), yr)
            ) %>% as.character()
          }))
      })


      # UI components ----

      ## mainUI ----
      output$mainUI <- renderUI({
        tagList(
          h4("Station", cur_stn()$label, align = "center"),
          tags$span(em("Station data summary:")),
          div(
            align = "center",
            class = "report-tbl",
            style = "overflow: auto;",
            dataTableOutput(ns("avail_reports_tbl")) %>% withSpinnerProxy(),
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
          ),
          br(),
          div(class = "note", "These station reports are a new feature currently in development. We encourage any feedback! Please email WAV staff at", a("wav@extension.wisc.edu", href = "mailto:wav@extension.wisc.edu"), "with any questions or comments."),
        )
      })

      ## avail_reports_tbl ----
      # output$avail_reports_tbl <- renderTable(
      #   avail_reports(),
      #   striped = T,
      #   hover = T,
      #   width = "100%",
      #   align = "c",
      #   na = "-",
      #   sanitize.text.function = identity
      # )

      output$avail_reports_tbl <- renderDataTable(
        avail_reports(),
        selection = "none", rownames = F, filter = "none",
        extensions = "FixedColumns",
        options = list(
          paging = F, searching = F, info = F, sort = F,
          fixedColumns = list(leftColumns = 1, rightColumns = 1),
          columnDefs = list(
            list(targets = "_all", className = "dt-center", defaultContent = "-")
          )
        ),
        escape = F
      )


      # Handle downloads ----

      ## reacts to button clicks to download a year's report ----
      observeEvent(input$year, {
        yr <- input$year
        stn <- cur_stn()
        stndata <- stn_data()

        safe_name <- str_squish(substr(gsub("[^A-Za-z0-9 ]", "", stn$station_name), 1, 30))
        report$filename <- sprintf("WAV %s Report for %s %s.pdf", yr, stn$station_id, safe_name)
        report$stn <- stn
        report$data <- sapply(stndata, function(df) filter(df, year == yr))

        # trigger the hidden downloadbutton once data is set up
        click("download")
      })

      ## download ----
      output$download <- downloadHandler(
        filename = function() { report$filename },
        content = createReport
      )

      createReport <- function(file) {
        yr <- input$year
        fname <- report$filename
        final_out <- file.path(temp_dir, fname)
        runjs("document.querySelector('#report-msg-container').style.display = 'none';")
        runjs("document.querySelectorAll('[id^=report-btn-]').forEach((btn) => {btn.disabled = true;})")
        use_existing <- F
        if (file.exists(final_out) & use_existing) {
          file.copy(final_out, file)
        } else {
          tryCatch({
            runjs(sprintf("document.querySelector('#report-btn-%s').innerHTML = 'Please wait...';", yr))
            temp_rmd <- file.path(temp_dir, "report.Rmd")
            temp_hdr <- file.path(temp_dir, "header.png")
            file.copy("md/station_report.Rmd", temp_rmd, overwrite = T)
            file.copy("md/report-header.png", temp_hdr)
            rmarkdown::render(
              input = temp_rmd,
              output_file = final_out,
              params = list(
                year = yr,
                stn = report$stn,
                data = report$data
              )
            )
            file.copy(final_out, file)
            runjs(sprintf("document.querySelector('#report-btn-%s').innerHTML = 'Downloaded!';", yr))
          }, error = function(cond) {
            runjs(sprintf("document.querySelector('#report-btn-%s').innerHTML = 'Error';", yr))
            runjs(sprintf("document.querySelector('#report-msg').innerHTML = 'Failed to create the %s report for %s. Please email WAV staff with this information and we will get it fixed.';", yr, report$stn$label))
            runjs("document.querySelector('#report-msg-container').style.display = null;")
          })
        }
        runjs("document.querySelectorAll('[id^=report-btn-]').forEach((btn) => {btn.disabled = false;});")
      }

      # end
    }
  )
}
