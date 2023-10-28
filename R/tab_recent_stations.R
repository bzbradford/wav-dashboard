## RECENT STATIONS TAB ##


# UI ----

recentStationsUI <- function() {
  ns <- NS("recent-stations")

  tagList(
    p(em("The most recent 5 stations you have selected are shown below. Click 'Select' to re-select any station.")),
    uiOutput("recent-stations-content")
  )
}


# Server ----

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


      ## Reactives ----

      recent_stns <- reactiveVal(c())


      ## Layout ----

      output$content <- renderUI({
        list(
          dataTableOutput(ns("table")),
          p(actionButton(ns("clearList"), "Clear list"), align = "right", style = "margin-top: 1em;")
        )
      })


      ## Table ----

      output$table <- renderDataTable({
        ids <- recent_stns()
        cur_id <- cur_stn()$station_id

        tibble(station_id = ids) %>%
          left_join(all_stns, by = "station_id") %>%
          select(id = station_id, name = station_name, baseline = baseline_stn, nutrient = nutrient_stn, thermistor = therm_stn) %>%
          mutate(across(where(is_logical), ~ ifelse(.x, "\u2705", "\u274c"))) %>%
          mutate(action = lapply(ids, function(id) {
            paste0("<a style='cursor: pointer;' id=", id, " onclick=\"Shiny.setInputValue('recent_stn', this.id, {priority: 'event'}); Shiny.setInputValue('station', this.id);\">Select</a>")
          }), .before = everything()) %>%
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


      ## Event handlers ----

      # add to list when station changes
      observeEvent(cur_stn(), {
        cur_id <- cur_stn()$station_id

        if (!(cur_id %in% recent_stns())) {
          new_list <- c(cur_id, recent_stns())
          if (length(new_list) > 5) new_list <- new_list[1:5]
          recent_stns(new_list)
        }
      })

      # update current station on button press
      # observeEvent(input$newId, {
      #   id <- input$newId
      #   message(id)
      #   print(stn_list())
      #   print(length(stn_list()))
      #   browser()
      #
      #   # station list empty, need to re-enable checkboxes
      #   if (length(stn_list()) == 0) {
      #     message('foo')
      #     updateCheckboxGroupInput(
      #       session = session,
      #       inputId = "map-stn_types",
      #       choices = station_types,
      #       selected = station_types
      #     )
      #     updateCheckboxGroupInput(
      #       session = session,
      #       inputId = "stn_years",
      #       choices = data_years,
      #       selected = data_years
      #     )
      #     updateRadioButtons(
      #       session = session,
      #       inputId = "year_exact_match",
      #       selected = TRUE
      #     )
      #   }
      #
      #   if (id %in% stn_list()) {
      #     updateSelectInput(
      #       inputId = "station",
      #       selected = id
      #     )
      #   }
      # })

      # clear recents on button press
      observeEvent(input$clearList, {
        recent_stns(cur_stn()$station_id)
      })
    }
  )
}