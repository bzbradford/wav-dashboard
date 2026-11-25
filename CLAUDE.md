# WAV Dashboard

R Shiny dashboard for Water Action Volunteers (WAV) stream monitoring data, run by UW-Madison Division of Extension. Volunteers submit data to the Wisconsin DNR's SWIMS database; this repo pulls SWIMS exports, validates and reshapes them, and serves them in the app (deployed on Posit Connect at connect.doit.wisc.edu/wav-dashboard).

## Layout

- `data prep/`: SWIMS exports → clean CSVs. Main script `data_prep.R`, helpers in `functions.R`. Raw SWIMS query exports go in `data prep/swims/*.xlsx`.
- `data prep/data-validation/`: volunteer data-quality follow-up (issue catalog, email drafts).
- `data/`: outputs of the prep scripts that the app reads (`baseline_data.csv`, `stn_list.csv`, `tp_data.csv`, `macro_*.csv`, `therm_*`, `shp/*.fgb`).
- `app/`: the Shiny app. `setup.R` loads `../data`, does the slow preprocessing and saves `.RData`; `global.R`, `server.R`, `ui.R`, modules in `src/`. Uses renv. Release notes go in `app/CHANGELOG.md`.
- `landcover/`, `thermistors/`: separate R projects that also write into `data/`.
- `docs/accessibility-audit.md`: WCAG 2.1 AA audit of the app.

Each subfolder is its own RStudio/Positron project, so scripts use paths relative to that folder (eg `data_prep.R` writes `../data/...` via `data_dir()`).

## Pipeline

1. Drop new SWIMS exports into `data prep/swims/`, set `MAX_DATE` at the top of `data_prep.R` (and in `app/setup.R`), then source `data_prep.R`.
2. `data_prep.R` writes straight into `../data/`. Those files are gitignored, so overwriting them can't be undone. Don't run the whole script just to test something. Instead, load `data prep/.RData` (a saved workspace from the last full run) and run only the section you changed, with `data_dir()` pointed at a scratch folder.
3. Run `app/setup.R` to rebuild the app's `.RData`, then run or deploy the app.

The app's table view shows every column of `baseline_data.csv` (and `setup.R` adds units for a fixed list of columns). Treat that file's schema (column names, order and types) as a contract with the app. When it has to change, update `app/setup.R` to match.

## Data validation (`data_prep.R`, "Data validation" section)

- Rules are in the `validation_rules` tribble and applied by `validate_measures()` in `functions.R`. `min`/`max` is the expected range (outside it the value is kept but flagged). `hard_min`/`hard_max` is the physically possible range (outside it the value is removed). `zero_ok` says whether exactly 0 is acceptable. `fix` is an automatic correction applied first and then re-checked: `f_to_c`, `ms_to_us`, or `cap_tube` (transparency capped at the tube length).
- It runs in two passes. Measured values are cleaned first. Then `calc_do_*` (DO saturation consistency, from the APHA equation) is computed from the cleaned temperature and DO, and checked.
- Each fieldwork gets a `data_validation` message string, which the app shows in its table.
- The "Validation report" section writes `data-validation/validation_issues.csv` (one row per volunteer-fixable problem, in plain language, with SWIMS links) and `data-validation/dropped_fieldwork.csv` (for staff). Derived fields are merged so one mistake appears once: streamflow comes from entered or corrected flow, and DO checks become a single "do_consistency" issue. "NaN" or "Infinity" in a field SWIMS calculates means inputs are missing, so it's reported as `flow_calculation` with the missing inputs named.
- `data-validation/volunteer_emails.R` turns the issues CSV into one email per monitoring group (`group_seq_no`), addressed to the members' emails from `swims/wav_all_ip_groups.xlsx`. Modes: `preview` (HTML page plus summary CSVs), `eml` (`.eml` files with `X-Unsent: 1` that open as drafts in desktop Outlook), and `outlook` (Microsoft365R drafts, optionally in a shared mailbox). Drafted issue IDs go in `drafted_log.csv` so they aren't drafted again.
- The emails are sent by Katy Bradford (WAV program manager). Her work laptop is locked down: user-level R and Positron installs work, but whether Microsoft365R can sign in there (tenant app consent) hasn't been confirmed.

## Environment and conventions

- Windows. Use R 4.6.2: `C:/Users/quack/AppData/Local/Programs/R/R-4.6.2/bin/Rscript.exe`. The `Rscript` on PATH is an old R 3.0.1 and won't load the tidyverse.
- Code style: tidyverse with the native pipe `|>`. Formatted with Air, which column-aligns `tribble()` calls. Interactive checks go in `if (FALSE) { ... }` blocks.
- SWIMS links: `swims_fw_url()`/`swims_stn_url()` (plus `*_link()` HTML versions) are in `data_prep.R`. The app has its own copy of the `*_link()` functions in `app/global.R`.
- `.gitignore` ignores every file with an extension except `*.R`, `*.Rproj`, `docs/*.md` and this file, so data and CSVs aren't tracked. A few files from before that rule, like `app/md/*.md`, are still tracked. A new non-R file has to be force-added or given its own `!` rule. `submitter_list.csv` and the `data-validation/` outputs contain volunteer names and emails: don't commit them or send them to outside services.
- `fsn` = SWIMS fieldwork_seq_no, one fieldwork event (station visit). The app renames it to `fieldwork_seq_no`.
