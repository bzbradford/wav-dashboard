## STATION LISTS TAB ##

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
      createStnPanel("Stations currently shown on map", "dl_cur_stns", "cur_stns"),
      createStnPanel("All baseline monitoring stations", "dl_baseline_stns", "baseline_stns"),
      createStnPanel("All nutrient monitoring stations", "dl_nutrient_stns", "nutrient_stns"),
      createStnPanel("All thermistor stations", "dl_therm_stns", "therm_stns"),
      createStnPanel("All WAV stations", "dl_all_stns", "all_stns")
    )
  )
}


#' requires global data frame 'all_stns'
#' @param avail_stns list of stations currently shown on the map

stationListServer <- function(stn_list) {
  moduleServer(
    id = "station-list",
    function(input, output, session) {
      ns <- session$ns

      # Station lists ----

      fmtStns <- function(df) {
        df %>%
          select(-c(label, baseline_stn, therm_stn, nutrient_stn, map_label)) %>%
          clean_names(case = "big_camel")
      }

      cur_stns <- reactive({
        all_stns %>%
          filter(station_id %in% stn_list()) %>%
          fmtStns()
      })

      baseline_stns <- all_stns %>%
        filter(baseline_stn) %>%
        fmtStns()

      nutrient_stns <- all_stns %>%
        filter(nutrient_stn) %>%
        fmtStns()

      therm_stns <- all_stns %>%
        filter(therm_stn) %>%
        fmtStns()

      all_stns_fmt <- fmtStns(all_stns)


      # Output tables ----

      output$cur_stns <- renderDataTable(cur_stns(), selection = "none")
      output$baseline_stns <- renderDataTable(baseline_stns, selection = "none")
      output$nutrient_stns <- renderDataTable(nutrient_stns, selection = "none")
      output$therm_stns <- renderDataTable(therm_stns, selection = "none")
      output$all_stns <- renderDataTable(all_stns_fmt, selection = "none")


      # Download handlers ----

      createStnDl <- function(fname, df) {
        downloadHandler(fname, function(file) { write_csv(df, file) } )
      }

      output$dl_cur_stns <- createStnDl("WAV Selected Station List.csv", cur_stns())
      output$dl_baseline_stns <- createStnDl("WAV Baseline Stations.csv", baseline_stns)
      output$dl_nutrient_stns <- createStnDl("WAV Nutrient Stations.csv", nutrient_stns)
      output$dl_therm_stns <- createStnDl("WAV Thermistor Stations.csv", therm_stns)
      output$dl_all_stns <- createStnDl("WAV Station List.csv", all_stns_fmt)

    }
  )
}
