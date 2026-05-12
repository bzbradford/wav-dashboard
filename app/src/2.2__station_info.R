## STATION INFO TAB ##

stationInfoUI <- function() {
  ns <- NS("station-info")

  tagList(
    div(
      class = "flex-row",
      div(
        class = "flex-col",
        h4("Station Information"),
        tableOutput(ns("details_tbl")),
      ),
      div(
        class = "flex-col",
        h4("Station Data Coverage"),
        tableOutput(ns("coverage_tbl")),
        h4("View on SWIMS", style = "margin-top: 1rem;"),
        p(
          "The",
          a(
            "Surface Water Integrated Monitoring System (SWIMS)",
            href = "https://dnr.wisconsin.gov/topic/SurfaceWater/SWIMS",
            target = "_blank"
          ),
          "is a DNR system that holds chemistry (water, sediment, fish tissue) data, physical data, biological (macroinvertebrate, aquatic invasives) data and more."
        ),
        uiOutput(ns("swims_link"))
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
        cur_stn() |>
          select(station_id:geometry) |>
          st_set_geometry(NULL) |>
          mutate(across(everything(), as.character)) |>
          clean_names(
            case = "title",
            abbreviations = c("ID", "DNR", "WBIC", "HUC")
          ) |>
          pivot_longer(
            cols = everything(),
            names_to = "Property",
            values_to = "Value"
          ) |>
          na.omit()
      })

      output$swims_link <- renderUI({
        stn <- cur_stn()
        seq_no <- stn$station_seq_no
        id <- stn$station_id
        span(
          sprintf(
            "On SWIMS, this station is linked to by its sequence number (%s), not its ID (%s).",
            seq_no,
            id
          ),
          HTML(swims_stn_link(seq_no, "View this station on SWIMS."))
        )
      })

      # Station data coverage table
      output$coverage_tbl <- renderTable({
        all_stn_data |>
          filter(station_id == cur_stn()$station_id) |>
          select(-station_id) |>
          arrange(desc(year)) |>
          clean_names(case = "title")
      })
    }
  )
}
