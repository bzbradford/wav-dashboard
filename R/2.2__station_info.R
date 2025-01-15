## STATION INFO TAB ##

stationInfoUI <- function() {
  ns <- NS("station-info")

  tagList(
    div(
      class = "flex-row",
      div(
        class = "flex-col",
        uiOutput(ns("details"))
      ),
      div(
        class = "flex-col",
        uiOutput(ns("coverage"))
      )
    )
  )
}


#' @param cur_stn a `reactive()` expression containing the currently selected station

stationInfoServer <- function(cur_stn) {
  moduleServer(
    id = "station-info",
    function(input, output, session) {

      # Station details table
      output$details <- renderUI({
        df <- cur_stn() %>%
          select(station_id:geometry) %>%
          st_set_geometry(NULL) %>%
          mutate(across(everything(), as.character)) %>%
          clean_names(case = "title", abbreviations = c("ID", "DNR", "WBIC", "HUC")) %>%
          pivot_longer(
            cols = everything(),
            names_to = "Property",
            values_to = "Value") %>%
          na.omit()

        tagList(
          h4("Station Information"),
          renderTable(df)
        )
      })

      # Station data coverage table
      output$coverage <- renderUI({
        df <- all_stn_data %>%
          filter(station_id == cur_stn()$station_id) %>%
          select(-station_id) %>%
          arrange(desc(year)) %>%
          clean_names(case = "title")

        tagList(
          h4("Station Data Coverage"),
          renderTable(df, align = "c")
        )
      })
    }
  )
}
