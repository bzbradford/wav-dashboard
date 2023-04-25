
# UI ----

watershedInfoUI <- function() {
  ns <- NS("watershed")

  div(
    class = "data-tab",
    uiOutput(ns("content"))
  )
}


# Server ----

watershedInfoServer <- function(cur_stn) {
  moduleServer(
    id = "watershed",
    function(input, output, session) {
      ns <- session$ns


      ## Layout ----

      output$content <- renderUI({

        tagList(
          p(em("In the US, watersheds are divided and subdivided into successively smaller hydrological units, called regions, sub-regions, basins, sub-basins, watersheds, and sub-watersheds. Wisconsin actually straddles two regions, the Upper Mississippi, and the Great Lakes Region, and the divide between these two major drainages is heavily influenced by past glacial activity.")),
          hr(),
          h4("Watershed information for Station ", cur_stn()$station_id, ": ", cur_stn()$station_name),
          p(":", cur_stn()$huc8),
          p("HUC 10:", cur_stn()$huc10),
          p("HUC 12:", cur_stn()$huc12)
        )
      })
    }
  )
}
