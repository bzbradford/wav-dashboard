## STATION LISTS TAB ##

# UI ----

stationListUI <- function() {
  ns <- NS("station-list")

  createStnPanel <- function(title, btn_id, tbl_id) {
    bsCollapsePanel(
      title = title,
      style = "primary",
      p(downloadButton(ns(btn_id), "Download this list")),
      dataTableOutput(ns(tbl_id))
    )
  }

  tagList(
    p(em("Open the panels below to view or download a list of WAV stations, information, and locations shown in this dashboard.")),
    bsCollapse(
      createStnPanel("Baseline monitoring stations", "baselineDownload", "baselineTable"),
      createStnPanel("Nutrient monitoring stations", "nutrientDownload", "nutrientTable"),
      createStnPanel("Thermistor station stations", "thermistorDownload", "thermistorTable"),
      createStnPanel("Complete station list", "allDownload", "allTable")
    )
  )
}


# Server ----

#' requires global data frame 'all_stns'

stationListServer <- function() {
  moduleServer(
    id = "station-list",
    function(input, output, session) {
      ns <- session$ns

      ## Defs ----

      baseline_stns <- filter(all_stns, baseline_stn)
      nutrient_stns <- filter(all_stns, nutrient_stn)
      therm_stns <- filter(all_stns, therm_stn)


      ## Tables ----

      renderStnTbl <- function(df) {
        renderDataTable({
          df %>% clean_names(case = "big_camel")
        }, selection = "none")
      }

      output$baselineTable <- renderStnTbl(baseline_stns)
      output$nutrientTable <- renderStnTbl(nutrient_stns)
      output$thermistorTable <- renderStnTbl(therm_stns)
      output$allTable <- renderStnTbl(all_stns)


      ## Download handlers ----

      createStnDl <- function(fname, df) {
        downloadHandler(fname, function(file) { write_csv(df, file) } )
      }

      output$baselineDownload <- createStnDl("wav-baseline-stations.csv", baseline_stns)
      output$nutrientDownload <- createStnDl("wav-nutrient-stations.csv", nutrient_stns)
      output$thermistorDownload <- createStnDl("wav-thermistor-stations.csv", therm_stns)
      output$allDownload <- downloadHandler("wav-station-list.csv", all_stns)

    }
  )
}
