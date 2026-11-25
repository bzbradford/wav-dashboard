library(tidyverse)

# Utility ----------------------------------------------------------------------

data_dir <- function(f) {
  file.path("../data", f)
}

# update dir when new data
load_xl <- function(fname, clean = TRUE) {
  f <- file.path("swims", fname)
  f.mtime <- file.mtime(f)
  df <- readxl::read_excel(f, na = c("", "NA"), guess_max = 1e6) |>
    as_tibble()
  if (clean) {
    df <- clean_names(df)
  }
  glimpse(df)
  message(
    "Loaded data from '",
    f,
    "' which was last modified ",
    f.mtime,
    " (",
    format(now() - f.mtime),
    " ago)"
  )
  df
}

# a left join but places all joined columns after the join column
left_join_at <- function(x, y, after = NULL, ...) {
  df <- left_join(x, y, ...)
  if (!is.null(after)) {
    nm <- setdiff(names(y), names(x))
    df <- relocate(df, all_of(nm), .after = after[1])
  }
  df
}

#' coalesce values within columns, grouping by a common variable like the fieldwork
#' sequence number. Used when merging data from different sources that may have partial
#' or overlapping data. Will retain the first row in each column by group that has a
#' valid value
# merge_by <- function(df, by) {
#   data.table::setDT(df) |>
#     _[, lapply(.SD, \(x) x[!is.na(x)][1]), by = by] |>
#     as_tibble()
# }

merge_by <- function(df, by) {
  data.table::setDT(df) |>
    _[,
      lapply(.SD, \(x) {
        real <- x[!is.na(x)]
        if (length(real)) {
          return(real[1])
        }
        if (is.double(x)) {
          nan <- x[is.nan(x)]
          if (length(nan)) {
            return(NaN)
          }
          return(NA_real_)
        }
        x[1] # returns typed NA for character, logical, etc.
      }),
      by = by
    ] |>
    as_tibble()
}

# restrict a value to a given range
clamp <- function(x, lower = x, upper = x, na.rm = F) {
  pmax(lower, pmin(upper, x, na.rm = na.rm), na.rm = na.rm)
}

# replaces out of range values with NA
trim <- function(x, low = NULL, high = NULL, msg = FALSE) {
  if (!is.null(low)) {
    x[x < low] <- NA
  }
  if (!is.null(high)) {
    x[x > high] <- NA
  }
  x
}

# convert temperature
f_to_c <- function(x, digits = 1) {
  round((x - 32) * (5 / 9), digits)
}

# convert to numeric, anything that failed to convert is set to NaN
to_numeric <- function(x) {
  original_na <- is.na(x) | trimws(x) == ""
  result <- suppressWarnings(as.numeric(x))
  result[!original_na & (is.na(result) | is.infinite(result))] <- NaN
  result
}

# see which values failed to convert
try_convert_numeric <- function(df, cols) {
  converted <- df |>
    select(fsn, date, group_desc, all_of(cols)) |>
    pivot_longer(
      all_of(cols),
      values_transform = as.character,
      names_to = "parameter",
      values_to = "entered"
    ) |>
    filter(is.nan(to_numeric(entered))) |>
    arrange(desc(date))
  print(converted)
  invisible(converted)
}

# format numbers for messages without floating point noise (88.4, not 88.400000000000006)
fmt_num <- function(x, digits = 6) {
  out <- trimws(formatC(signif(x, digits), digits = digits, format = "fg"))
  out[is.na(x)] <- NA_character_
  out
}

# describe a numeric range in words, eg "4 to 11", "0 or more"
range_text <- function(lo, hi, units = "") {
  txt <- case_when(
    is.finite(lo) & is.finite(hi) ~ paste(fmt_num(lo), "to", fmt_num(hi)),
    is.finite(lo) ~ paste(fmt_num(lo), "or more"),
    is.finite(hi) ~ paste(fmt_num(hi), "or less"),
    .default = "any value"
  )
  str_squish(paste(txt, units))
}

# preserves NaNs
nan_coalesce <- function(...) {
  args <- list(...)
  result <- coalesce(!!!args)
  all_nan <- Reduce(`&`, lapply(args, is.nan))
  ifelse(all_nan, NaN, result)
}


# Shapefiles -------------------------------------------------------------------

quickmap <- function(shape) {
  require(leaflet)
  message(
    "Shape has ",
    nrow(shape),
    " objects and ",
    format(mapview::npts(shape), big.mark = ","),
    " vertices"
  )
  leaflet() |>
    addTiles() |>
    addPolygons(
      data = shape,
      color = "black",
      weight = 2,
      opacity = .5,
      fillColor = "grey",
      fillOpacity = .1
    )
}

# Stations ---------------------------------------------------------------------

validate_stns <- function(stns, return_valid = TRUE) {
  cat("Input stations:", nrow(stns), "\n")
  invalid <- list(
    missing_id = list(
      label = "Stations missing id",
      data = stns %>% filter(is.na(station_id))
    ),
    non_numeric_id = list(
      label = "Stations with letters in id",
      data = stns %>% filter(grepl("[a-zA-Z]+", station_id))
    ),
    missing_ll = list(
      label = "Stations missing latitude/longitude",
      data = stns %>% filter(is.na(latitude) | is.na(longitude))
    ),
    zero_ll = list(
      label = "Stations with zero latitude/longitude",
      data = stns %>% filter(latitude == 0 | longitude == 0)
    ),
    pos_ll = list(
      label = "Stations with positive longitude",
      data = stns %>% filter(longitude > 0)
    ),
    missing_wbic = list(
      label = "Stations missing WBIC",
      data = stns %>% filter(is.na(wbic))
    ),
    multiple_wbic = list(
      label = "Stations with multiple WBIC",
      data = stns %>% filter(grepl(",", wbic, fixed = T))
    )
  )

  # iterate each element of invalid and print the number of rows and the label
  for (inv in invalid) {
    if (nrow(inv$data) > 0) {
      cat(paste0("\n", inv$label, ": ", nrow(inv$data), "\n"))
      print(inv$data)
    }
  }

  # generate valid station list
  suppressWarnings({
    valid <- stns %>%
      filter(latitude != 0, longitude != 0) %>%
      mutate(longitude = -1 * abs(longitude)) %>%
      mutate(station_id = as.integer(station_id)) %>%
      mutate(wbic = as.integer(wbic)) %>%
      drop_na(station_id, latitude, longitude)
  })

  cat("\nValid stations:", nrow(valid), "\n")

  if (return_valid) valid else invalid
}

# Baseline ---------------------------------------------------------------------

# how many fieldwork sequence numbers are common between datasets
compare_fieldwork <- function(..., col = "fsn") {
  dfs <- list(...)
  nms <- as.character(match.call())[-1]
  nms <- nms[nms != col] # remove col arg name if passed by name
  all_vals <- unique(unlist(lapply(dfs, \(df) df[[col]])))

  message("Total unique: ", length(all_vals))

  for (i in seq_along(dfs)) {
    message("In ", nms[i], ": ", length(unique(dfs[[i]][[col]])))
  }

  for (i in seq_along(dfs)) {
    others <- unlist(lapply(dfs[-i], \(df) df[[col]]))
    only_in <- sum(!(dfs[[i]][[col]] %in% others))
    message("Only in ", nms[i], ": ", only_in)
  }

  shared <- Reduce(
    \(a, b) intersect(a, b[[col]]),
    dfs[-1],
    init = dfs[[1]][[col]]
  )
  message("Shared across all: ", length(shared))
}

#' Validate measures against a rules table (see `validation_rules` in data_prep.R).
#'
#' Returns one row per fsn x measure with the original `value`, the value to use
#' going forward (`new_value`), a `status` (NA when fine, otherwise "not_a_number",
#' "removed", "converted", "capped" or "flagged") and a short `msg` for the
#' dashboard's data_validation column.
#'
#' Order of operations: NaN (non-numeric entry) -> removed; otherwise try the
#' rule's automatic `fix`; then values outside hard_min/hard_max are removed and
#' values outside min/max (or zero when !zero_ok) are kept but flagged.
validate_measures <- function(df, rules) {
  df |>
    select(fsn, transparency_tube_length, all_of(rules$measure)) |>
    pivot_longer(all_of(rules$measure), names_to = "measure") |>
    left_join(rules, by = "measure") |>
    mutate(
      # transparency can't exceed the tube; only trust standard tube lengths
      tube_length = if_else(
        transparency_tube_length %in% c(60, 100, 120),
        transparency_tube_length,
        120
      ),
      max = if_else(fix %in% "cap_tube", tube_length, max),
      is_nan = is.nan(value),
      fixed = case_when(
        is_nan ~ NA,
        fix %in% "f_to_c" & value > hard_max ~ f_to_c(value),
        fix %in% "ms_to_us" & value > 0 & value < min ~ value * 1000,
        fix %in% "cap_tube" & value > max ~ max,
        .default = value
      ),
      was_fixed = !is.na(value) & !is_nan & fixed != value,
      impossible = !is.na(fixed) &
        (fixed < hard_min |
          fixed > hard_max |
          (fix %in% "ms_to_us" & fixed == 0)),
      unusual = !is.na(fixed) &
        (fixed < min | fixed > max | (fixed == 0 & !zero_ok)),
      new_value = if_else(impossible, NA, fixed),
      status = case_when(
        is_nan ~ "not_a_number",
        is.na(value) ~ NA,
        impossible ~ "removed",
        was_fixed & fix %in% "cap_tube" ~ "capped",
        was_fixed ~ "converted",
        unusual ~ "flagged",
        .default = NA
      ),
      msg = case_when(
        status == "not_a_number" ~ str_glue("{measure} was not a number"),
        status == "removed" & was_fixed ~
          str_glue(
            "{measure} value {fmt_num(value)} not possible even after conversion, removed"
          ),
        status == "removed" ~
          str_glue("{measure} value {fmt_num(value)} not possible, removed"),
        status == "capped" ~
          str_glue(
            "{measure} value {fmt_num(value)} capped to tube length {fmt_num(new_value)}"
          ),
        status == "converted" & unusual ~
          str_glue(
            "{measure} converted from {fmt_num(value)} to {fmt_num(new_value)}, still suspect"
          ),
        status == "converted" ~
          str_glue(
            "{measure} converted from {fmt_num(value)} to {fmt_num(new_value)}"
          ),
        status == "flagged" ~
          str_glue("{measure} value {fmt_num(value)} suspect"),
        .default = NA
      ),
      msg = as.character(msg)
    ) |>
    select(
      fsn,
      measure,
      value,
      new_value,
      status,
      msg,
      min,
      max,
      zero_ok,
      hard_min,
      hard_max,
      fix,
      tube_length
    )
}

# validate a baseline data column
validate <- function(x, low = NULL, high = NULL) {
  nm <- deparse(substitute(x))
  len <- length(x)
  nas <- sum(is.na(x))
  vals <- len - nas
  message("Values: ", len)
  message("NA: ", nas, sprintf(" (%.1f%%)", 100 * nas / len))
  message("Numeric: ", vals, sprintf(" (%.1f%%)", 100 * vals / len))
  message(
    "Quantiles:\n  ",
    quantile(x, c(0, .01, .05, .5, .95, .99, .995, .999, 1), TRUE) %>%
      paste(names(.), ., sep = ": ", collapse = "\n  ")
  )
  if (!is.null(low)) {
    message("x < ", low, ": ", sum(x < low, na.rm = T))
  }
  if (!is.null(high)) {
    message("x > ", high, ": ", sum(x > high, na.rm = T))
  }
  x <- trim(x, low, high)

  # show a histogram
  hist(x, main = paste("Histogram of", nm))
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  md <- median(x, na.rm = TRUE)
  for (i in 1:3) {
    abline(v = m + i * s, col = "red", lwd = 1 / i)
    abline(v = m - i * s, col = "red", lwd = 1 / i)
  }
  abline(v = m, col = "blue", lwd = 2)
  abline(v = md, col = "blue", lwd = 1.5, lty = 2)
  invisible(x)
}
