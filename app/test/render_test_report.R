# Render a station PDF report outside of Shiny, for testing / accessibility checks.
#
# Usage (from the app directory):
#   Rscript test/render_test_report.R [station_id] [year] [output_dir]
#
# With no station given, picks a station/year that has baseline, nutrient and
# thermistor data so that every section of the report is exercised.

suppressMessages(source("global.R"))

args <- commandArgs(TRUE)
out_dir <- if (length(args) >= 3) args[3] else tempdir()

if (length(args) >= 2 && nzchar(args[1])) {
  stn_id <- args[1]
  yr <- as.integer(args[2])
} else {
  full <- baseline_data |>
    distinct(station_id, year) |>
    semi_join(distinct(nutrient_data, station_id, year), join_by(station_id, year)) |>
    semi_join(distinct(therm_data, station_id, year), join_by(station_id, year)) |>
    arrange(desc(year)) |>
    slice(1)
  stn_id <- full$station_id
  yr <- full$year
}

stn <- filter(all_pts, station_id == stn_id)
stopifnot(nrow(stn) == 1)

data <- list(
  baseline = filter(baseline_data, station_id == stn_id, year == yr),
  nutrient = filter(nutrient_data, station_id == stn_id, year == yr),
  thermistor = filter(therm_data, station_id == stn_id, year == yr)
)

# mirror createReport() in src/3.5__stn_report.R
work_dir <- file.path(tempdir(), "report-test")
dir.create(work_dir, showWarnings = FALSE)
file.copy("report/station_report.Rmd", file.path(work_dir, "report.Rmd"), overwrite = TRUE)
file.copy("report/header.png", work_dir, overwrite = TRUE)
file.copy(list.files("report", "\\.ttf$", full.names = TRUE), work_dir, overwrite = TRUE)

# LaTeX must compile next to header.png/fonts, so render in work_dir and copy
pdf_name <- sprintf("test_report_%s_%s.pdf", stn_id, yr)
out_file <- file.path(work_dir, pdf_name)

tictoc::tic("render")
tagged <- render_station_report(
  rmd = file.path(work_dir, "report.Rmd"),
  output_file = out_file,
  params = list(year = yr, stn = stn, data = data)
)
tictoc::toc()
message("Tagged PDF: ", tagged)
file.copy(out_file, file.path(out_dir, pdf_name), overwrite = TRUE)
message("Wrote ", normalizePath(file.path(out_dir, pdf_name)))
