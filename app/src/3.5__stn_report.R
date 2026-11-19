##  REPORT TAB  ##

stnReportUI <- function() {
  ns <- NS("report")

  tagList(
    div(
      class = "data-tab",
      uiOutput(ns("main_ui"))
    )
  )
}

#' @param main_rv reactive values object from main server session
stnReportServer <- function(main_rv) {
  moduleServer(
    id = "report",
    function(input, output, session) {
      ns <- session$ns

      temp_dir <- tempdir()

      # Reactive values --------------------------------------------------------

      cur_stn <- reactive({
        req(main_rv$cur_stn)
      })

      ## report rv ----
      # holds data for rendering a report
      report <- reactiveValues(
        filename = NULL,
        stn = NULL,
        data = NULL
      )

      ## stn_data ----
      stn_data <- reactive({
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
        baseline_obs <- df$baseline |>
          summarize(
            `Baseline<br>temperature` = sum(
              !is.na(air_temp) | !is.na(water_temp)
            ),
            `Baseline<br>DO` = sum(!is.na(d_o) | !is.na(d_o_saturation)),
            `Baseline<br>pH` = sum(!is.na(ph)),
            `Baseline<br>conductivity` = sum(!is.na(specific_cond)),
            `Baseline<br>transparency` = sum(!is.na(transparency)),
            `Baseline<br>streamflow` = sum(!is.na(streamflow)),
            .by = year
          )
        nutrient_obs <- df$nutrient |>
          count(year, name = "Total<br>phosphorus")
        therm_obs <- df$thermistor |>
          count(year, date) |>
          count(year, name = "Continuous<br>temp. days")
        years <- sort(unique(c(
          baseline_obs$year,
          nutrient_obs$year,
          therm_obs$year
        )))

        tibble(year = years) |>
          left_join(baseline_obs, join_by(year)) |>
          left_join(nutrient_obs, join_by(year)) |>
          left_join(therm_obs, join_by(year)) |>
          rename(Year = year) |>
          mutate(Year = as.character(Year)) |>
          mutate(across(where(is.numeric), ~ ifelse(.x == 0, NA, .x))) |>
          mutate(
            `Download<br>report` = lapply(Year, function(yr) {
              tags$button(
                HTML("<i class='fas fa-download' aria-hidden='true'></i>"),
                "Download PDF",
                type = "button",
                id = paste0("report-btn-", yr),
                class = "btn btn-default btn-sm",
                `aria-label` = paste("Download", yr, "PDF report"),
                onclick = sprintf(
                  "Shiny.setInputValue('%s', %s, {priority: 'event'});",
                  ns("year"),
                  yr
                )
              ) |>
                as.character()
            })
          )
      })

      # UI components ----

      ## mainUI ----
      output$main_ui <- renderUI({
        tagList(
          h2("Station", cur_stn()$label, class = "h4", align = "center"),
          p(
            id = "report-tbl-caption",
            em("Station data summary: number of observations of each type by year, with a button to download each year's PDF report.")
          ),
          div(
            align = "center",
            class = "report-tbl",
            style = "overflow: auto;",
            dataTableOutput(ns("avail_reports_tbl")),
          ),
          # hidden download link triggered by the per-year buttons; removed
          # from the accessibility tree and tab order
          div(
            style = "visibility: hidden; height: 0px;",
            `aria-hidden` = "true",
            downloadButton(ns("download"), tabindex = "-1"),
          ),
          # announces report progress to screen readers (WCAG 4.1.3)
          div(id = "report-status", class = "visually-hidden", role = "status", `aria-live` = "polite"),
          div(
            id = "report-msg-container",
            class = "notice notice-error",
            role = "alert",
            style = "margin-top: 1em; display: none;",
            tags$button(
              type = "button",
              class = "notice-close",
              `aria-label` = "Dismiss error message",
              HTML("&#x2715;"),
              onclick = "this.parentElement.style.display = 'none';"
            ),
            div(id = "report-msg")
          ),
          br(),
          div(
            class = "note",
            "If you have any issues with these generated reports please email WAV staff at",
            a("wav@extension.wisc.edu", href = "mailto:wav@extension.wisc.edu"),
            "with any questions or comments."
          ),
        )
      })

      ## avail_reports_tbl ----
      output$avail_reports_tbl <- renderDataTable(
        avail_reports(),
        selection = "none",
        rownames = FALSE,
        filter = "none",
        extensions = "FixedColumns",
        callback = JS(
          "table.table().node().setAttribute('aria-describedby', 'report-tbl-caption');"
        ),
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          sort = FALSE,
          fixedColumns = list(leftColumns = 1, rightColumns = 1),
          columnDefs = list(
            list(
              targets = "_all",
              className = "dt-center",
              defaultContent = "-"
            )
          )
        ),
        escape = FALSE
      )

      # Handle downloads ----

      ## reacts to button clicks to download a year's report ----
      observeEvent(input$year, {
        yr <- input$year
        stn <- cur_stn()
        stndata <- stn_data()

        safe_name <- str_squish(substr(
          gsub("[^A-Za-z0-9 ]", "", stn$station_name),
          1,
          30
        ))
        report$filename <- sprintf(
          "WAV %s Report for %s %s.pdf",
          yr,
          stn$station_id,
          safe_name
        )
        report$stn <- stn
        report$data <- sapply(stndata, function(df) filter(df, year == yr))

        # trigger the hidden downloadbutton once data is set up
        click("download")
      })

      ## download ----
      output$download <- downloadHandler(
        filename = function() {
          report$filename
        },
        content = createReport
      )

      createReport <- function(file) {
        yr <- input$year
        temp_out <- file.path(temp_dir, paste0(hash(report$filename), ".pdf"))
        runjs(
          "document.querySelector('#report-msg-container').style.display = 'none';"
        )
        runjs(
          "document.querySelectorAll('[id^=report-btn-]').forEach((btn) => {btn.disabled = true;})"
        )
        status <- function(msg) {
          runjs(sprintf(
            "document.querySelector('#report-status').textContent = %s;",
            jsonlite::toJSON(msg, auto_unbox = TRUE)
          ))
        }
        tryCatch(
          {
            runjs(sprintf(
              "document.querySelector('#report-btn-%s').innerHTML = 'Please wait...';",
              yr
            ))
            status(sprintf("Creating the %s PDF report. This may take up to a minute.", yr))
            temp_rmd <- file.path(temp_dir, "report.Rmd")
            file.copy("report/station_report.Rmd", temp_rmd, overwrite = TRUE)
            file.copy("report/header.png", temp_dir, overwrite = TRUE)
            lapply(
              list.files("report", "*.ttf", full.names = TRUE),
              function(f) {
                file.copy(f, temp_dir)
              }
            )
            # tries an accessible tagged PDF, falls back to untagged
            render_station_report(
              rmd = temp_rmd,
              output_file = temp_out,
              params = list(
                year = yr,
                stn = report$stn,
                data = report$data
              )
            )
            file.copy(temp_out, file)
            runjs(sprintf(
              "document.querySelector('#report-btn-%s').innerHTML = 'Downloaded!';",
              yr
            ))
            status(sprintf("The %s PDF report has been downloaded.", yr))
          },
          error = function(e) {
            runjs(sprintf(
              "document.querySelector('#report-btn-%s').innerHTML = 'Error';",
              yr
            ))
            # BUG FIX: message was interpolated into a single-quoted JS string
            # and assigned to innerHTML, so station names or error messages
            # containing quotes broke the script (and allowed HTML injection)
            msg <- sprintf(
              "Failed to create the %s report for %s. Please email WAV staff and we will get it fixed. Error: %s",
              yr,
              report$stn$label,
              conditionMessage(e)
            )
            runjs(sprintf(
              "document.querySelector('#report-msg').textContent = %s;",
              jsonlite::toJSON(msg, auto_unbox = TRUE)
            ))
            runjs(
              "document.querySelector('#report-msg-container').style.display = null;"
            )
          }
        )
        runjs(
          "document.querySelectorAll('[id^=report-btn-]').forEach((btn) => {btn.disabled = false;});"
        )
      }
    } # end server
  )
}
