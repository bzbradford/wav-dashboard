## RECENT STATIONS TAB ##

recentStationsUI <- function() {
  ns <- NS("recent-stations")

  tagList(
    p(em(
      "The most recent 5 stations you have selected are shown below. Click 'Select' to re-select any station."
    )),
    dataTableOutput(ns("table"), fill = F),
    p(
      actionButton(ns("clear_list"), "Clear list"),
      align = "right",
      style = "margin-top: 1em;"
    ),
  )
}

#' @requires `all_stns`
#' @param main_rv reactive values object from main server session
recentStationsServer <- function(main_rv) {
  moduleServer(
    id = "recent-stations",
    function(input, output, session) {
      ns <- session$ns


      # Reactives --------------------------------------------------------------

      rv <- reactiveValues(
        recent_stns = NULL
      )

      cur_stn <- reactive({
        req(main_rv$cur_stn)
      })



      # Observers --------------------------------------------------------------

      # add new stations to list
      observeEvent(main_rv$cur_stn, {
        cur_id <- cur_stn()$station_id

        if (!(cur_id %in% rv$recent_stns)) {
          new_list <- c(cur_id, rv$recent_stns)
          if (length(new_list) > 5) {
            new_list <- new_list[1:5]
          }
          rv$recent_stns <- new_list
        }
      })

      # clear recents on button press
      observeEvent(input$clear_list, {
        rv$recent_stns <- cur_stn()$station_id
      })


      # Recents table ----------------------------------------------------------

      btn_for_stn <- function(id) {
        sprintf(
          "<a class='btn btn-default btn-sm' style='cursor: pointer; text-decoration: none;' id=%s onclick=\"Shiny.setInputValue('recent_stn', this.id, {priority: 'event'}); Shiny.setInputValue('station', this.id);\">Select</a>",
          id
        )
      }

      # formatted data for rendering
      recent_stns_table <- reactive({
        ids <- rv$recent_stns
        stn <- cur_stn()

        tibble(station_id = ids) |>
          left_join(all_stns, join_by(station_id)) |>
          mutate(
            arrow = if_else(station_id == stn$station_id, "\u27a4", ""),
            action = btn_for_stn(station_id),
            across(c(baseline_stn, nutrient_stn, therm_stn), ~if_else(.x, "\u2705", "\u274c")),
          ) |>
          select(
            ` ` = arrow,
            Action = action,
            ID = station_id,
            Name = station_name,
            Baseline = baseline_stn,
            Nutrient = nutrient_stn,
            Thermistor = therm_stn
          )
      })

      # render table
      output$table <- renderDataTable(
        recent_stns_table(),
        server = FALSE,
        rownames = TRUE,
        selection = "none",
        escape = FALSE,
        options = list(
          autoWidth = TRUE,
          responsive = TRUE,
          paging = FALSE,
          bFilter = FALSE,
          bSort = FALSE,
          bInfo = FALSE,
          columnDefs = list(
            list(targets = 0:7, className = "align-middle"),
            list(targets = c(1, 3), className = "dt-right"),
            list(targets = c(0, 2, 5, 6, 7), className = "dt-center")
          )
        )
      )
    }
  )
}
