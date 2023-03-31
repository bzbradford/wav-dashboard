## STATION LIST TAB ##

# Server ----

#' requires global data frame 'all_stns'

stationListServer <- function(id = "station-list") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      ## Layout ----

      output$content <- renderUI({
        bsCollapse(
          bsCollapsePanel(
            title = "Baseline monitoring stations",
            p(downloadButton(ns("baselineDownload"), "Download this list")),
            dataTableOutput(ns("baselineTable"))
          ),
          bsCollapsePanel(
            title = "Nutrient monitoring locations",
            p(downloadButton(ns("nutrientDownload"), "Download this list")),
            dataTableOutput(ns("nutrientTable"))
          ),
          bsCollapsePanel(
            title = "Thermistor station locations",
            p(downloadButton(ns("thermistorDownload"), "Download this list")),
            dataTableOutput(ns("thermistorTable"))
          ),
          bsCollapsePanel(
            title = "Complete station list",
            p(downloadButton(ns("allDownload"), "Download this list")),
            dataTableOutput(ns("allTable"))
          )
        )
      })


      ## Tables ----

      output$baselineTable <- renderDataTable({
        all_stns %>%
          filter(baseline_stn) %>%
          clean_names(case = "big_camel")
      })

      output$nutrientTable <- renderDataTable({
        all_stns %>%
          filter(nutrient_stn) %>%
          clean_names(case = "big_camel")
      })

      output$thermistorTable <- renderDataTable({
        all_stns %>%
          filter(therm_stn) %>%
          clean_names(case = "big_camel")
      })

      output$allTable <- renderDataTable({
        all_stns %>%
          clean_names(case = "big_camel")
      })


      ## Download handlers ----

      output$baselineDownload <- downloadHandler(
        "wav-baseline-stations.csv",
        function(file) {
          all_stns %>%
            filter(baseline_stn) %>%
            write_csv(file)
        }
      )

      output$nutrientDownload <- downloadHandler(
        "wav-nutrient-stations.csv",
        function(file) {
          all_stns %>%
            filter(nutrient_stn) %>%
            write_csv(file)
        }
      )

      output$thermistorDownload <- downloadHandler(
        "wav-thermistor-stations.csv",
        function(file) {
          all_stns %>%
            filter(therm_stn) %>%
            write_csv(file)
        }
      )

      output$allDownload <- downloadHandler(
        "wav-station-list.csv",
        function(file) {
          all_stns %>%
            write_csv(file)
        }
      )
    }
  )
}
