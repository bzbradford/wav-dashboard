
# id must NOT be wrapped in ns() from the calling server
dataViewUI <- function(id, title = "View/download data") {
  ns <- NS(id)

  accordion(
    accordion_panel(
      title = title,
      class = "btn-primary",
      uiOutput(ns("station_info_ui")),
      uiOutput(ns("downloads_ui")),
      materialSwitch(
        inputId = ns("transpose"),
        label = "Transpose table",
        inline = TRUE,
        value = TRUE
      ),
      dataTableOutput(ns("stn_dt"))
    ),
    open = FALSE
  )
}

#' @param id same id as passed to dataViewUI but wrapped in ns() from the calling server
#' @param dataset_name e.g. 'Baseline'
#' @param master_data full dataset eg `baseline_data`
#' @param cur_stn reactive from calling server
#' @param stn_data reactive from calling server
#' @param selected_data reactive from calling server

dataViewServer <- function(id, dataset_name, master_data, cur_stn, stn_data, selected_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Station info ----
    output$station_info_ui <- renderUI({
      stn <- cur_stn()
      p(
        strong("Station ID:"), stn$station_id, br(),
        strong("Station Name:"), stn$station_name, br(),
        strong("Waterbody:"), stn$waterbody
      )
    })

    # Download buttons ----
    output$downloads_ui <- renderUI({
      df <- selected_data()
      yrs <- unique(df$year)

      tagList(
        p(
          # Conditional "Current Year" button
          if (length(yrs) == 1) {
            downloadButton(ns("dl_cur_yr"), sprintf("Download station data (%s)", yrs))
          },
          downloadButton(ns("dl_cur_stn"), "Download station data (all years)"),
          downloadButton(ns("dl_all"), sprintf("Download entire %s dataset", tolower(dataset_name)))
        )
      )
    })

    # Data for DT ----
    stn_data_for_dt <- reactive({
      req(!is.null(input$transpose))

      df <- selected_data() %>%
        arrange(date) %>%
        distinct(date, .keep_all = TRUE) %>%
        clean_names(case = "title")

      if (isTRUE(input$transpose)) {
        date_fmt <- ifelse(n_distinct(year(df$Date)) > 1, "%b %d, %Y", "%b %d")
        df <- df %>%
          mutate(label = format(Date, date_fmt)) %>%
          mutate(across(everything(), as.character)) %>%
          pivot_longer(cols = -label, names_to = "Parameter") %>%
          pivot_wider(names_from = label) %>%
          mutate(across(Parameter, ~str_replace_all(.x, c("D o" = "DO", "P h" = "pH"))))
      }

      df
    })

    # Data table ----
    output$stn_dt <- renderDataTable(
      stn_data_for_dt(),
      selection = "none",
      rownames = FALSE,
      options = list(
        paging = FALSE,
        scrollX = TRUE,
        scrollCollapse = TRUE
      ),
      server = FALSE
    )

    # Download handlers ----
    output$dl_cur_yr <- downloadHandler(
      filename = function() {
        yr <- unique(selected_data()$year)
        sprintf("WAV Stn %s %s Data (%s).csv", cur_stn()$station_id, dataset_name, yr)
      },
      content = function(file) {
        write_csv(selected_data(), file, na = "")
      }
    )

    output$dl_cur_stn <- downloadHandler(
      filename = function() {
        sprintf("WAV Stn %s %s Data.csv", cur_stn()$station_id, dataset_name)
      },
      content = function(file) {
        write_csv(stn_data(), file, na = "")
      }
    )

    output$dl_all <- downloadHandler(
      filename = function() {
        sprintf("WAV %s Data.csv", dataset_name)
      },
      content = function(file) {
        write_csv(master_data, file, na = "")
      }
    )

  })
}
