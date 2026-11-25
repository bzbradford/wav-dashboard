#' Volunteer data quality emails
#'
#' Turns data-validation/validation_issues.csv (written by data_prep.R) into one
#' email per monitoring group, listing each fieldwork event that needs fixing
#' with links to the station and fieldwork pages in SWIMS.
#'
#' Run from the "data prep" folder (open Data prep.Rproj). Needs only the issues
#' CSV and the SWIMS contacts export, so it can run on a different computer.
#'
#' Output goes to data-validation/emails_<YEAR>/:
#'   preview.html    every email on one page, to review, or copy/paste into Outlook
#'   summary.csv     one row per email: recipients, subject, counts
#'   no_contact.csv  groups with problems but no usable email address
#'   eml/*.eml       (MODE "eml") drafts that open in desktop Outlook ready to send
#'
#' MODE "outlook" creates drafts directly in an Outlook mailbox through
#' Microsoft365R (sign in when the browser opens). Anything drafted with "eml"
#' or "outlook" is recorded in data-validation/drafted_log.csv and skipped next
#' time, so re-running after a new SWIMS pull only drafts new problems.

library(tidyverse)
library(readxl)
library(janitor)


# Settings ---------------------------------------------------------------------

YEAR <- 2026
MODE <- "preview" # "preview", "eml" or "outlook"

# draft into a shared mailbox instead of your own (needs access to it), eg
# SHARED_MAILBOX <- "wav@extension.wisc.edu"
SHARED_MAILBOX <- NULL

# only email about fieldwork that appears on the dashboard
DASHBOARD_ONLY <- FALSE
SKIP_ALREADY_DRAFTED <- TRUE

ISSUES_FILE <- "data-validation/validation_issues.csv"
CONTACTS_FILE <- "swims/wav_all_ip_groups.xlsx"
LOG_FILE <- "data-validation/drafted_log.csv"
OUT_DIR <- file.path("data-validation", paste0("emails_", YEAR))

TUTORIAL_URL <- NA # link to the SWIMS Quick Tutorial Video, if there is one
DASHBOARD_URL <- "https://connect.doit.wisc.edu/wav-dashboard/"

SIGNATURE <- c(
  "Katy Bradford",
  "Water Action Volunteers Program Manager",
  "UW-Madison Division of Extension | Natural Resources Institute",
  "<a href=\"https://wav.extension.wisc.edu/\">https://wav.extension.wisc.edu/</a>"
)
CONTACT_EMAIL <- "wav@extension.wisc.edu"
CONTACT_PHONE <- "608-331-0173"
FOOTER <- "The University of Wisconsin–Madison Division of Extension provides equal opportunities in employment and programming in compliance with state and federal law."


# Load -------------------------------------------------------------------------

issues <- read_csv(
  ISSUES_FILE,
  col_types = cols(
    .default = col_character(),
    year = col_integer(),
    date = col_date(),
    fsn = col_double(),
    station_id = col_double(),
    station_seq_no = col_double(),
    group_seq_no = col_double(),
    in_dashboard = col_logical()
  )
) |>
  filter(year == YEAR)

if (DASHBOARD_ONLY) {
  issues <- filter(issues, in_dashboard)
}

drafted_log <- if (file.exists(LOG_FILE)) {
  read_csv(LOG_FILE, col_types = cols(.default = col_character()))
} else {
  tibble(
    issue_id = character(),
    group_seq_no = character(),
    drafted = character(),
    mode = character()
  )
}

if (SKIP_ALREADY_DRAFTED) {
  n_before <- nrow(issues)
  issues <- filter(issues, !(issue_id %in% drafted_log$issue_id))
  message("Skipping ", n_before - nrow(issues), " issues already drafted")
}

# one row per person per group, preferring active members with a real address
valid_email <- function(x) {
  x <- str_to_lower(str_trim(x))
  if_else(str_detect(x, "^[^@\\s]+@[^@\\s]+\\.[a-z]+$"), x, NA)
}

fix_case <- function(x) if_else(x == str_to_upper(x), str_to_title(x), x)

contacts <- read_excel(CONTACTS_FILE) |>
  clean_names() |>
  mutate(group_seq_no = as.numeric(group_seq_no)) |>
  filter(group_seq_no %in% issues$group_seq_no) |>
  mutate(
    email = coalesce(valid_email(primary_email), valid_email(secondary_email)),
    first_name = fix_case(str_squish(first_name)),
    active = interested_party_status_code %in% "ACTIVE"
  ) |>
  drop_na(email) |>
  filter(active | !any(active), .by = group_seq_no) |>
  distinct(group_seq_no, email, .keep_all = TRUE)

recipients <- contacts |>
  summarize(
    to = list(email),
    first_names = list(unique(na.omit(first_name))),
    .by = group_seq_no
  )


# Build emails -----------------------------------------------------------------

esc <- function(x) htmltools::htmlEscape(x)
link <- function(url, text) {
  if_else(
    is.na(url),
    esc(text),
    sprintf("<a href=\"%s\">%s</a>", url, esc(text))
  )
}
nice_date <- function(d) {
  paste0(month.name[month(d)], " ", day(d), ", ", year(d))
}

greeting <- function(names) {
  n <- length(names)
  if (n == 0 || n > 4) {
    return("Dear WAV volunteers,")
  }
  if (n == 1) {
    return(paste0("Dear ", names, ","))
  }
  paste0("Dear ", paste(names[-n], collapse = ", "), " and ", names[n], ",")
}

# the list of problems, grouped by station then fieldwork date
issues_html <- function(df) {
  df <- arrange(df, station_id, date, parameter)
  stations <- split(df, factor(df$station_id, levels = unique(df$station_id)))
  map_chr(stations, function(s) {
    fws <- split(s, factor(s$fsn, levels = unique(s$fsn)))
    fw_items <- map_chr(fws, function(f) {
      problems <- sprintf(
        "<li><b>%s:</b> %s</li>",
        esc(f$parameter),
        esc(f$problem)
      )
      sprintf(
        "<li>%s<ul>%s</ul></li>",
        link(f$fieldwork_url[1], nice_date(f$date[1])),
        paste(problems, collapse = "")
      )
    })
    sprintf(
      "<p style=\"margin:12px 0 4px 0\"><b>Station %s</b></p><ul style=\"margin-top:0\">%s</ul>",
      link(s$station_url[1], paste0(s$station_id[1], ": ", s$station_name[1])),
      paste(fw_items, collapse = "")
    )
  }) |>
    paste(collapse = "\n")
}

email_html <- function(df, names) {
  n_issues <- nrow(df)
  n_fw <- n_distinct(df$fsn)
  found <- if (n_issues == 1) {
    "a small data entry problem"
  } else if (n_fw == 1) {
    "a few data entry problems on one fieldwork date"
  } else {
    sprintf("%d data entry problems across %d fieldwork dates", n_issues, n_fw)
  }
  tutorial <- if (!is.na(TUTORIAL_URL)) {
    sprintf(
      "<p>If this is your first time editing your data, you can watch our %s to learn how to submit and edit your data.</p>",
      link(TUTORIAL_URL, "SWIMS Quick Tutorial Video (9 min)")
    )
  } else {
    ""
  }
  paste0(
    "<div style=\"font-family:Calibri,Arial,sans-serif;font-size:11pt\">",
    "<p>",
    esc(greeting(names)),
    "</p>",
    str_glue(
      "<p>Each year, WAV staff conduct data quality checks of all baseline stream monitoring data submitted by volunteers to the DNR's SWIMS database. We found {found} in your {YEAR} data that we'd like you to check on and edit in SWIMS. Details are below.</p>"
    ),
    issues_html(df),
    "<p>When you have time, please log in to your SWIMS account and fix these values. Each date above links to that fieldwork event in SWIMS, or you can go to the “View Data” tab at the top of the SWIMS page and click the edit icon for each event listed above. You can also view your fieldwork on our interactive <a href=\"DASHBOARD_URL\">data dashboard</a>, but any changes you make in SWIMS won't be reflected in the dashboard until our monthly update.</p>",
    tutorial,
    str_glue(
      "<p>If you need assistance or have questions, you can email me at <a href=\"mailto:{CONTACT_EMAIL}?subject=SWIMS data validation\">{CONTACT_EMAIL}</a> or call me at {CONTACT_PHONE}.</p>"
    ),
    "<p>Thank you for monitoring your stream site this year!</p>",
    "<p>Best,<br>",
    paste(SIGNATURE, collapse = "<br>"),
    "</p>",
    "<p style=\"font-size:9pt;color:#666\">",
    esc(FOOTER),
    "</p>",
    "</div>"
  )
}

emails <- issues |>
  nest(.by = c(group_seq_no, group_desc), .key = "issues") |>
  left_join(recipients, by = "group_seq_no") |>
  mutate(
    n_issues = map_int(issues, nrow),
    n_fieldwork = map_int(issues, \(x) n_distinct(x$fsn)),
    has_contact = map_lgl(to, \(x) length(x) > 0),
    subject = sprintf(
      "WAV data check: please review %s in your %d stream monitoring data",
      if_else(n_issues == 1, "1 item", paste(n_issues, "items")),
      YEAR
    ),
    html = map2_chr(issues, first_names, \(df, nm) {
      email_html(df, nm %||% character())
    })
  ) |>
  arrange(group_desc)

ready <- filter(emails, has_contact)
no_contact <- filter(emails, !has_contact)

message(
  nrow(ready),
  " emails ready (",
  sum(ready$n_issues),
  " issues), ",
  nrow(no_contact),
  " groups with no usable email address"
)


# Write review files -----------------------------------------------------------

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

emails |>
  transmute(
    group_seq_no,
    group_desc,
    to = map_chr(to, \(x) paste(x, collapse = "; ")),
    subject,
    n_fieldwork,
    n_issues,
    has_contact
  ) |>
  write_excel_csv(file.path(OUT_DIR, "summary.csv"), na = "")

no_contact |>
  select(group_seq_no, group_desc, issues) |>
  unnest(issues, names_sep = "_") |>
  select(
    group_seq_no,
    group_desc,
    date = issues_date,
    station_id = issues_station_id,
    parameter = issues_parameter,
    problem = issues_problem,
    fieldwork_url = issues_fieldwork_url
  ) |>
  write_excel_csv(file.path(OUT_DIR, "no_contact.csv"), na = "")

preview_sections <- emails |>
  mutate(
    section = sprintf(
      paste0(
        "<section style=\"border:1px solid #ccc;border-radius:6px;padding:12px 16px;margin:24px 0\">",
        "<p style=\"font-family:Arial,sans-serif;font-size:10pt;color:#444;background:#f4f4f4;padding:8px\">",
        "<b>Group:</b> %s<br><b>To:</b> %s<br><b>Subject:</b> %s</p>%s</section>"
      ),
      esc(group_desc),
      if_else(
        has_contact,
        esc(map_chr(to, \(x) paste(x, collapse = "; "))),
        "<span style=\"color:#b00\">NO EMAIL ADDRESS - see no_contact.csv</span>"
      ),
      esc(subject),
      html
    )
  ) |>
  pull(section)

writeLines(
  c(
    "<!doctype html><html><head><meta charset=\"utf-8\">",
    sprintf(
      "<title>WAV data check emails %d</title></head><body style=\"max-width:900px;margin:auto;padding:16px\">",
      YEAR
    ),
    sprintf(
      "<h1 style=\"font-family:Arial,sans-serif\">WAV data check emails, %d</h1><p style=\"font-family:Arial,sans-serif\">%d emails, %d issues. Generated %s.</p>",
      YEAR,
      nrow(emails),
      sum(emails$n_issues),
      format(Sys.time(), "%Y-%m-%d %H:%M")
    ),
    preview_sections,
    "</body></html>"
  ),
  file.path(OUT_DIR, "preview.html"),
  useBytes = TRUE
)
message("Preview written to ", file.path(OUT_DIR, "preview.html"))


# Drafts -----------------------------------------------------------------------

log_drafted <- function(email_row, mode) {
  entry <- tibble(
    issue_id = email_row$issues[[1]]$issue_id,
    group_seq_no = as.character(email_row$group_seq_no),
    drafted = format(Sys.time(), "%Y-%m-%d %H:%M"),
    mode = mode
  )
  write_csv(entry, LOG_FILE, append = file.exists(LOG_FILE))
}

b64 <- function(x) gsub("\\s", "", jsonlite::base64_enc(charToRaw(enc2utf8(x))))

# .eml with "X-Unsent: 1" opens as an unsent draft in desktop Outlook
write_eml <- function(path, to, subject, html) {
  body <- b64(paste0(
    "<html><head><meta charset=\"utf-8\"></head><body>",
    html,
    "</body></html>"
  ))
  body_lines <- substring(
    body,
    seq(1, nchar(body), 76),
    pmin(seq(76, nchar(body) + 75, 76), nchar(body))
  )
  lines <- c(
    paste0("To: ", paste(to, collapse = ", ")),
    paste0("Subject: =?UTF-8?B?", b64(subject), "?="),
    "X-Unsent: 1",
    "MIME-Version: 1.0",
    "Content-Type: text/html; charset=\"utf-8\"",
    "Content-Transfer-Encoding: base64",
    "",
    body_lines
  )
  writeBin(charToRaw(paste0(paste(lines, collapse = "\r\n"), "\r\n")), path)
}

if (MODE == "eml") {
  eml_dir <- file.path(OUT_DIR, "eml")
  dir.create(eml_dir, showWarnings = FALSE)
  for (i in seq_len(nrow(ready))) {
    e <- ready[i, ]
    fname <- paste0(
      make_clean_names(str_trunc(e$group_desc, 40, ellipsis = "")),
      "_",
      e$group_seq_no,
      ".eml"
    )
    write_eml(file.path(eml_dir, fname), e$to[[1]], e$subject, e$html)
    log_drafted(e, "eml")
  }
  message(nrow(ready), " .eml drafts written to ", eml_dir)
}

if (MODE == "outlook") {
  outlook <- Microsoft365R::get_business_outlook(
    shared_mbox_email = SHARED_MAILBOX
  )
  for (i in seq_len(nrow(ready))) {
    e <- ready[i, ]
    outlook$create_email(
      body = e$html,
      content_type = "html",
      subject = e$subject,
      to = e$to[[1]]
    )
    log_drafted(e, "outlook")
    message("Drafted ", i, "/", nrow(ready), ": ", e$group_desc)
  }
  message(
    "Done. Drafts are in the Drafts folder",
    if (!is.null(SHARED_MAILBOX)) paste(" of", SHARED_MAILBOX)
  )
}
