## RECENT STATIONS TAB ##

recentStationsUI <- function() {
  ns <- NS("recent-stations")

  tagList(
    p(em("The most recent 5 stations you have selected are shown below. Click 'Select' to re-select any station.")),
    dataTableOutput(ns("table")),
    p(actionButton(ns("clearList"), "Clear list"), align = "right", style = "margin-top: 1em;"),
  )
}

#' requires globals:
#' - all_stns
#' - station_types
#' - data_years
#'
#' @param cur_stn a `reactive()` expression containing the current station
#' @param stn_list a `reactive()` expression containing the currently available stations

recentStationsServer <- function(cur_stn, stn_list) {
  moduleServer(
    id = "recent-stations",
    function(input, output, session) {
      ns <- session$ns

      # Reactive values ----

      ## recent_stns ----
      recent_stns <- reactiveVal(c())


      # Event reactives ----

      ## cur_stn() ----
      # add to list when station changes
      observeEvent(cur_stn(), {
        cur_id <- cur_stn()$station_id

        if (!(cur_id %in% recent_stns())) {
          new_list <- c(cur_id, recent_stns())
          if (length(new_list) > 5) new_list <- new_list[1:5]
          recent_stns(new_list)
        }
      })

      ## input$clearList ----
      # clear recents on button press
      observeEvent(input$clearList, {
        recent_stns(cur_stn()$station_id)
      })


      # Rendered UIs ----

      ## table ----
      output$table <- renderDataTable({
        ids <- recent_stns()
        cur_id <- cur_stn()$station_id

        tibble(station_id = ids) %>%
          left_join(all_stns, by = "station_id") %>%
          select(id = station_id, name = station_name, baseline = baseline_stn, nutrient = nutrient_stn, thermistor = therm_stn) %>%
          mutate(across(where(is_logical), ~ ifelse(.x, "\u2705", "\u274c"))) %>%
          mutate(action = lapply(ids, function(id) {
            paste0("<a style='cursor: pointer;' id=", id, " onclick=\"Shiny.setInputValue('recent_stn', this.id, {priority: 'event'}); Shiny.setInputValue('station', this.id);\">Select</a>")
          }), .before = 1) %>%
          mutate(current = ifelse(id == cur_id, "\u27a4", ""), .before = everything()) %>%
          clean_names("title")
      },
      server = F,
      rownames = T,
      selection = "none",
      options = list(
        paging = F,
        bFilter = F,
        bSort = F,
        bInfo = F,
        columnDefs = list(list(targets = c(0:1, 3:5), className = "dt-center")))
      )

      # end
    }
  )
}
