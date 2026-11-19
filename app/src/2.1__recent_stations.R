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

      # icon is decorative; screen readers get the text instead (emoji
      # check/cross marks are announced inconsistently)
      sr_icon <- function(icon, text) {
        sprintf(
          "<span aria-hidden='true'>%s</span><span class='visually-hidden'>%s</span>",
          icon,
          text
        )
      }

      # formatted data for rendering
      recent_stns_table <- reactive({
        ids <- rv$recent_stns
        stn <- cur_stn()

        tibble(station_id = ids) |>
          left_join(all_stns, join_by(station_id)) |>
          mutate(
            arrow = if_else(
              station_id == stn$station_id,
              sr_icon("\u27a4", "Current station"),
              ""
            ),
            action = map2_chr(station_id, station_name, build_stn_select_btn),
            across(
              c(baseline_stn, nutrient_stn, therm_stn),
              ~ if_else(.x, sr_icon("\u2705", "Yes"), sr_icon("\u274c", "No"))
            ),
          ) |>
          select(
            Current = arrow,
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
        # row numbers removed: they rendered as an unlabeled, empty column header
        rownames = FALSE,
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
            list(targets = 0:6, className = "align-middle"),
            list(targets = 2, className = "dt-right"),
            list(targets = c(0, 1, 4, 5, 6), className = "dt-center")
          )
        )
      )
    }
  )
}
