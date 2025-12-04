## STATION INFO TAB ##

stationInfoUI <- function() {
  ns <- NS("station-info")

  tagList(
    div(
      class = "flex-row",
      div(
        class = "flex-col",
        h4("Station Information"),
        tableOutput(ns("details_tbl"))
      ),
      div(
        class = "flex-col",
        h4("Station Data Coverage"),
        tableOutput(ns("coverage_tbl"))
      )
    )
  )
}

#' @param main_rv reactive values object from main server session
stationInfoServer <- function(main_rv) {
  moduleServer(
    id = "station-info",
    function(input, output, session) {

      cur_stn <- reactive({
        req(main_rv$cur_stn)
      })

      # Station details table
      output$details_tbl <- renderTable({
        cur_stn() %>%
          select(station_id:geometry) %>%
          st_set_geometry(NULL) %>%
          mutate(across(everything(), as.character)) %>%
          clean_names(case = "title", abbreviations = c("ID", "DNR", "WBIC", "HUC")) %>%
          pivot_longer(
            cols = everything(),
            names_to = "Property",
            values_to = "Value"
          ) %>%
          na.omit()
      })

      # Station data coverage table
      output$coverage_tbl <- renderTable({
        all_stn_data %>%
          filter(station_id == cur_stn()$station_id) %>%
          select(-station_id) %>%
          arrange(desc(year)) %>%
          clean_names(case = "title")
      })
    }
  )
}
