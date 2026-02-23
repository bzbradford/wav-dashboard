# server.R ----

## screenshot => download pdf ----
# observeEvent(stns_avail(), {
#   if (stns_avail()) enable("screenshot") else disable("screenshot")
# })

#' use html2canvas to screenshot the main page content
#' have to remove the map div for now because leaflet is using svg instead of canvas
#' map polygons render in the incorrect location with html2canvas
#' once cloned the radio buttons are modified because they didn't appear correctly
#' after rendering, the screenshot button is re-enabled
# buildScreenshotFilename <- function() {
#   stn_id <- cur_stn()$station_id
#   tab_name <- input$data_tabs
#   suffix <- case_match(tab_name,
#     tab_names$baseline ~ baselineReturn()$year,
#     tab_names$nutrient ~ nutrientReturn()$year,
#     tab_names$thermistor ~ thermistorReturn()$year,
#     tab_names$watershed ~ watershedReturn()$huc,
#     .default = ""
#   )
#   fname <- paste0("WAV Dashboard - Station ", cur_stn()$station_id, " - ", input$data_tabs)
#   if (!is.null(suffix)) fname <- paste(fname, suffix)
#   fname
# }

# observeEvent(input$screenshot, {
#   fname <- buildScreenshotFilename()
#   runjs(sprintf("
#     html2canvas(
#       document.querySelector('#main-content'),
#       {
#         scale: 1,
#         crossOrigin: 'anonymous',
#         useCORS: true,
#         imageTimeout: 5000,
#         onclone: (cloneDoc) => {
#           cloneDoc.querySelector('#map-content').style.display = 'none';
#           const style = cloneDoc.createElement('style');
#           style.innerHTML = 'input[type=\"radio\"] { appearance: none !important; };'
#           cloneDoc.body.appendChild(style);
#         }
#       }
#     ).then(canvas => {
#       saveAs(canvas.toDataURL(), '%s.png')
#     });
#   ", fname))
#   enable("screenshot")
#   runjs("document.querySelector('#screenshot-msg').style.display = 'none';")
# })

# global.R ----------------------------------------------------------------

#' each option should have a value list and color list.
#' the value list will be expanded with the plot limits on either side

# BaselineOpts <- function(
#   name = character(),
#   units = character(),
#   plot_opts = list(), # create with PlotlyOpts()
#   map_opts = list() # create with ColorNumericOpts()
# ) {
#   stopifnot(is.character(name), length(name) == 1)
#   stopifnot(is.character(units), length(units) %in% c(0, 1))
#   stopifnot(is.list(plot_opts), length(plot_opts) %in% c(0, 3))
#   stopifnot(is.list(map_opts), length(map_opts) %in% c(0, 3))
#
#   lst(name, unit, plot_opts, map_opts)
# }
#
# PlotOpts <- function(
#   range = numeric(),
#   color = character(),
#   annot = list() # create with PlotlyAnnotOpts()
# ) {
#   stopifnot(is.numeric(range), length(range) == 2)
#   stopifnot(is.character(color), length(color) == 1)
#   stopifnot(is.list(annot), length(annot) %in% c(0, 3))
#   # if (length(annot) == 3) stopifnot(all(between(annot$values, range[1], range[2])))
#
#   lst(range, color, annot)
# }

# data structure for calling colorNumeric for leaflet
# ColorNumericOpts <- function(
#   domain = numeric(),
#   palette = character(),
#   reverse = logical()
# ) {
#   stopifnot(is.numeric(domain), length(domain) == 2)
#   pals <- c(rownames(RColorBrewer::brewer.pal.info), "viridis")
#   stopifnot(is.character(palette), palette %in% pals)
#   stopifnot(is.logical(reverse), length(reverse) == 1)
#
#   lst(domain, palette, reverse)
# }

# Baseline data options
# baseline_opts <- lst(
#
#   water_temp = BaselineOpts(
#     name = "Water temperature",
#     units = "°C",
#     plot_opts = PlotOpts(
#       range = c(5, 25),
#       color = "steelblue",
#       annot = PlotlyAnnotOpts(
#         values = c(20.7, 22.5, 24.6),
#         labels = c(
#           "Cold/Cool-cold transition",
#           "Cool-cold/Cool-warm transition",
#           "Cool-warm/Warm transition"
#         ),
#         colors = c("blue", "cornflowerblue", "lightsteelblue", "darkorange")
#       )
#     ),
#     map_opts = ColorNumericOpts(
#       domain = c(5, 25),
#       palette = "RdYlBu",
#       reverse = TRUE
#     )
#   ),
#
#   air_temp = BaselineOpts(
#     name = "Air temperature",
#     units = "°C",
#     plot_opts = PlotOpts(
#       range = c(0, 35),
#       color = "orange",
#       annot = PlotlyAnnotOpts(
#         values = c(0, 10, 20, 30),
#         labels = c(
#           "Freezing weather",
#           "Cold weather",
#           "Moderate weather",
#           "Warm weather"
#         ),
#         colors = c(
#           "blue",
#           "steelblue",
#           "cornflowerblue",
#           "lightsteelblue",
#           "darkorange"
#         )
#       )
#     )
#   ),
#
#   d_o = BaselineOpts(
#     name = "Dissolved oxygent",
#     units = "mg/L",
#     plot_opts = PlotOpts(
#       range = c(2, 14),
#       color = "navy",
#       annot = PlotlyAnnotOpts(
#         values = c(1, 3, 5, 6, 7),
#         labels = c(
#           "Aquatic life minimum\n(1 mg/L) ",
#           "Limited forage fish\n(>3 mg/L) ",
#           "Warmwater fish\n(>5 mg/L) ",
#           "Coldwater fish\n(>6 mg/L) ",
#           "Coldwater spawning\n(>7 mg/L) "
#         ),
#         colors = c(
#           "red",
#           "orange",
#           "gold",
#           "lightblue",
#           "steelblue",
#           "cornflowerblue"
#         )
#       )
#     ),
#     map_opts = ColorNumericOpts(
#       domain = c(3, 12),
#       palette = "RdYlBu",
#       reverse = FALSE
#     )
#   ),
#
#   transparency = BaselineOpts(
#     name = "Transparency",
#     units = "cm",
#     plot_opts = PlotOpts(
#       range = c(0, 120),
#       color = "brown",
#       annot = PlotlyAnnotOpts(
#         values = c(55, 90, 120),
#         labels = c(
#           "Low transparency",
#           "Moderate transparency",
#           "High transparency"
#         ),
#         colors = c("khaki", "lightgreen", "lightblue", "lightblue")
#       )
#     ),
#     map_opts = ColorNumericOpts(
#       domain = c(0, 120),
#       palette = "BrBG",
#       reverse = FALSE
#     )
#   ),
#
#   ph = BaselineOpts(
#     name = "pH",
#     units = "",
#     plot_opts = PlotOpts(
#       range = c(5, 10),
#       color = "orchid",
#       annot = PlotlyAnnotOpts(
#         values = c(6, 7.5, 9),
#         labels = c(
#           "Minimum water quality\nstandard (pH 6.0) ",
#           "Optimal for fish\n(pH 7.5) ",
#           "Maximum water quality\nstandard (pH 9.0) "
#         ),
#         colors = c("orange", "lightgreen", "lightgreen", "purple")
#       )
#     ),
#     map_opts = ColorNumericOpts(
#       domain = c(5, 10),
#       palette = "Spectral",
#       reverse = FALSE
#     )
#   ),
#
#   specific_cond = BaselineOpts(
#     name = "Conductivity",
#     units = "µS/cm",
#     plot_opts = PlotOpts(
#       range = c(100, 1000),
#       color = "pink",
#       annot = PlotlyAnnotOpts(
#         values = c(150, 800, 1500, Inf),
#         labels = c(
#           "Low conductivity (<150 µs/cm)",
#           "Normal conductivity (150-800 µs/cm)",
#           "High conductivity (800-1500 µs/cm)",
#           "Very high conductivity (>1500 µs/cm)"
#         ),
#         colors = c("steelblue", "lightblue", "pink", "orchid", "purple")
#       )
#     ),
#     map_opts = ColorNumericOpts(
#       domain = c(0, 2000),
#       palette = "RdYlBu",
#       reverse = TRUE
#     )
#   ),
#
#   streamflow = BaselineOpts(
#     name = "Streamflow",
#     units = "cfs",
#     plot_opts = PlotOpts(
#       range = c(0, 10),
#       color = "#48a67b",
#       annot = PlotlyAnnotOpts(
#         values = c(.03, 3, 150),
#         labels = c(
#           "Headwater stream (0.03-3 cfs)\nEphemeral stream (< 0.03 cfs)",
#           "Mainstem stream (3-150 cfs)\nHeadwater stream (0.03-3 cfs)",
#           "Large river (> 150 cfs)\nMainstem stream (3-150 cfs)"
#         ),
#         colors = c("#915119", "#e3c283", "#73cdc1", "#09968e")
#       )
#     ),
#     map_opts = ColorNumericOpts(
#       domain = c(0, 50),
#       palette = "RdBu",
#       reverse = TRUE
#     )
#   )
#
# )

# # old plot options
# baseline_plot_opts = tribble(
#   ~col            , ~name               , ~unit    , ~range_min , ~range_max , ~color      ,
#   "water_temp"    , "Water temperature" , "°C"     ,          5 ,         25 , "steelblue" ,
#   "air_temp"      , "Air temperature"   , "°C"     ,          5 ,         35 , "orange"    ,
#   "d_o"           , "Dissolved oxygen"  , "mg/L"   ,          2 ,         14 , "navy"      ,
#   "transparency"  , "Transparency"      , "cm"     ,          0 ,        120 , "brown"     ,
#   "streamflow"    , "Streamflow"        , "cfs"    ,          0 ,         10 , "#48a67b"   ,
#   "ph"            , "pH"                , "pH"     ,          6 ,          9 , "orchid"    ,
#   "specific_cond" , "Conductivity"      , "µS/cm"  ,        100 ,       1000 , "pink"      ,
# ) %>%
#   mutate(label = sprintf("%s (%s)", name, unit))
#
# # old map palette options
# stn_color_opts <- tribble(
#   ~label                          , ~value                       , ~domain    , ~rev , ~pal       ,
#   "Years of data"                 , "n_years"                    , c(0, 10)   , F    , "viridis"  ,
#   "Fieldwork events"              , "n_fieldwork"                , c(0, 100)  , F    , "viridis"  ,
#   "Water temp (°C)"              , "water_temp"                 , c(5, 25)   , T    , "RdYlBu"   ,
#   "Dissolved oxygen (mg/L)"       , "d_o"                        , c(3, 12)   , F    , "RdYlBu"   ,
#   "pH"                            , "ph"                         , c(5, 10)   , F    , "Spectral" ,
#   "Specific conductance (µS/cm)" , "specific_cond"              , c(0, 2000) , T    , "RdYlBu"   ,
#   "Transparency (cm)"             , "transparency"               , c(0, 120)  , F    , "BrBG"     ,
#   "Streamflow (cfs)"              , "streamflow"                 , c(0, 50)   , T    , "RdBu"     ,
#   "Biotic index total animals"    , "biotic_index_total_animals" , c(0, 15)   , F    , "RdYlBu"   ,
#   "Biotic index score"            , "biotic_index_score"         , c(1, 4)    , F    , "RdYlBu"   ,
#   "Total phosphorus (mg/L)"       , "tp"                         , c(0, .25)  , T    , "Spectral" ,
# )
#
# stn_color_choices <- append(
#   list("Station type" = "stn_type"),
#   deframe(stn_color_opts[, 1:2])
# )
#
# baseline_summary_vars = tribble(
#   ~var                   , ~parameter          , ~units ,
#   "d_o"                  , "Dissolved oxygen"  , "mg/L" ,
#   "water_temp"           , "Water temperature" , "°C"  ,
#   "air_temp"             , "Air temperature"   , "°C"  ,
#   "transparency"         , "Transparency"      , "cm"   ,
#   "streamflow"           , "Stream flow"       , "cfs"  ,
#   "average_stream_depth" , "Stream depth"      , "ft"   ,
# ) %>%
#   rowwise()

