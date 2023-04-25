
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

      cur_ws <- reactiveVal()

      # observeEvent(cur_stn(), {
      #   req(cur_stn())
      #   print(cur_stn())
      # })


      ## Layout ----

      output$content <- renderUI({
        # if (is.null(cur_stn())) return(div(class = "well", "Select a station to view information about the watersheds in which it's located."))

        tagList(
          p("HUC 8:", cur_ws()$huc8),
          p("HUC 10:", cur_ws()$huc10),
          p("HUC 12:", cur_ws()$huc12)
        )
      })
    }
  )
}
