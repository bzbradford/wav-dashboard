# PDF Reports (pending) ----

# output$baseline_report <- downloadHandler(
#   filename = paste0(
#     input$baseline_year,
#     " Baseline Report for Station ",
#     as.character(cur_stn()$station_id),
#     ".pdf"),
#   content = function(file) {
#     temp_dir <- tempdir()
#     temp_file <- tempfile(tmpdir = temp_dir, fileext = ".Rmd")
#     file.copy("baseline-report.Rmd", temp_file)
#     print(temp_file)
#     rmarkdown::render(
#       temp_file,
#       output_file = file,
#       output_options = list(self_contained = TRUE),
#       envir = new.env(parent = globalenv()),
#       params = list(
#         id = cur_stn()$station_id,
#         name = cur_stn()$station_name,
#         year = input$baseline_year,
#         plot = baseline_plot()
#         )
#     )
#   }
# )
