# Accessibility Audit & Remediation Log

**Project:** WAV Stream Monitoring Data Dashboard (R Shiny) and station PDF reports
**Branch:** `accessibility-audit`
**Started:** 2026-11-19
**Standard:** WCAG 2.1 Level AA

## Why WCAG 2.1 AA

- The [UW–Madison Digital Accessibility policy](https://digitalaccessibility.wisc.edu/ada-rule-extension/) and the [UW System policy](https://www.wisconsin.edu/uw-policies/uw-system-administrative-policies/accessibility-of-web-content-and-mobile-applications/accessibility-of-web-content-and-mobile-applications-guidance/) both require WCAG 2.1 Level AA. UW–Madison Extension follows the campus policy.
- On 2026-04-20, DOJ moved the ADA Title II digital accessibility compliance date from 2026-04-24 to **2027-04-26**. UW–Madison's formal enforcement starts on that date, but the Office of Compliance already acts on complaints. Both downloadable PDFs and the web app are in scope.

> TODO (owner): confirm with Extension Communications whether Extension has its own accessibility checklist on top of campus policy (for example, rules for brand colors or PDF documents). Neither turned up in the public guidance, so this audit uses campus policy plus WCAG 2.1 AA.

---

## Method

| Check | Tool / approach |
|---|---|
| Automated rules | **axe-core 4.10** run in headless Chrome (via `chromote`) against the running app, across 11 UI states (initial load plus every tab in both tab sets), with WCAG 2.0/2.1/2.2 A and AA and best-practice tags |
| Keyboard | Scripted Tab-order walk; focus visibility checked with computed `outline`; activation of the custom controls; focus kept after state changes |
| Screen reader semantics | Accessible names, roles, live regions, and heading outline, inspected in the DOM |
| Color | Contrast ratios computed for every theme and caption color (WCAG relative luminance) |
| Code review | Manual review of every UI module, the plot helpers, and the report Rmd |
| PDF | `pdfinfo` (tagging and metadata), visual review of the rendered pages, and inspection of the generated LaTeX |

Automated tools catch roughly 30–40% of WCAG issues, so most of the findings below come from manual review.

The audit scripts aren't committed. They used dev-only packages (`chromote`, `pdftools`) kept outside the renv library. The report render harness *is* committed: `app/test/render_test_report.R`.

---

## Results summary

| | Before | After |
|---|---|---|
| axe-core violations (distinct rules) | **9** (color-contrast ×12 nodes, html-has-lang, label ×2 critical, page-has-heading-one, region ×7, heading-order, empty-table-header, aria-allowed-role, presentation-role-conflict) | **0** across all 11 states |
| Keyboard-operable "Select station" buttons | 0 (they were `<a onclick>` with no `href`, so not focusable) | All |
| Plot download buttons keyboard-operable | No | Yes |
| Charts with a text alternative | 0 | All plotly charts (`role="img"` plus a description that points to the data table) |
| PDF title / language / author metadata | None | Yes |
| PDF figure alt text | None | All figures, in tagged mode |
| PDF structure tagging | Untagged | **Tagged PDF 2.0 / PDF/UA-2, verified locally** (9 Figures with Alt, 8 Tables with 69 TH header cells, headings, lists, links, `/Lang en-US`). Automatic untagged fallback where the server's LaTeX can't tag (see [PDF reports](#pdf-reports)) |

---

## Web app findings & fixes

Status: ✅ fixed · ⚠️ partially fixed / mitigated · ⏳ open

### Perceivable

| # | WCAG | Finding | Fix | Status |
|---|---|---|---|---|
| W1 | 1.4.3 | Flatly theme link color `#18bc9c` was 2.4:1 on white and 2.1:1 on the gray wells. | `bs_theme(link-color = "#11745a")`, a darker WAV teal (5.7:1 on white, 5.0:1 on wells). | ✅ |
| W2 | 1.4.3 | Default/secondary buttons used white on `#95a5a6` (2.6:1): Zoom, Select, Clear list, Download plot. | `bs_theme(secondary = "#5d6d6e")` (5.4:1). | ✅ |
| W3 | 1.4.3, 1.4.1 | `colorize()` drew caption words in the plot colors: orange 2.0:1, deepskyblue 2.1:1, lightsteelblue 1.8:1, gold 1.4:1. | `colorize()` now shows a color swatch followed by the word in normal dark text. Meaning still comes through the color *name*. | ✅ |
| W4 | 1.4.3 | Footer text `grey` was 3.9:1; the error notice `red` on pink was 3.4:1. | Footer is now `#595959` (7:1); notices use `#b00020` / `#1e6b1e`. | ✅ |
| W5 | 1.4.1 | Map station types were shown by marker color only. The only key was colored words in the text above the map. | Added a Leaflet legend ("Station type") in station-type mode. Station type is also listed in the station details. | ✅ |
| W6 | 1.1.1 | Plotly charts are SVG with no text alternative. | New `plotly_a11y()` helper sets `role="img"` and a data-driven `aria-label` (station, year, ranges, and the phosphorus interpretation), and points to the equivalent table. Landscape pie charts list every class and percentage. | ✅ |
| W7 | 1.1.1 | WAV logo `<img>` had no `alt`. | Added `alt="Water Action Volunteers logo. Visit the WAV website."` | ✅ |
| W8 | 1.1.1 | Leaflet pin markers were focusable `<img alt="">`. | Added `alt` ("Station 123", "Selected station 123"). | ✅ |
| W9 | 1.3.1 | No `<h1>`, and heading levels skipped (h2 → h4). | The page title is the single `h1`. Tab headings are h2/h3/h4, with visual size kept via Bootstrap `.h3`/`.h4` classes. Markdown headings were promoted one level. | ✅ |
| W10 | 1.3.1 | Several inputs were labelled by a separate `<div>`/`<strong>` that wasn't connected to the input: year buttons, "Show data for", "Show observations in", temperature units, annotations, landscape scale, "Stations with data from", station selector, measure dropdown. | All now use the input's real `label` (visually hidden where the design has no visible label). An `.inline-label` CSS class keeps the side-by-side layout. | ✅ |
| W11 | 1.3.1 | No landmarks; content was outside regions. | Added `<main>`, labelled `<nav>` in the header, labelled `<section>`s for station selection and data, and labelled regions for the map and map options. Replaced `sidebarLayout()`, which emits an invalid `<form role="complementary">`. | ✅ |
| W12 | 1.3.1 | The recent-stations table had empty header cells (arrow and row-number columns). Emoji ✅/❌ are read inconsistently. | Columns are now labelled ("Current"), row names are removed, and icons are `aria-hidden` with "Yes"/"No"/"Current station" as visually hidden text. | ✅ |
| W13 | 3.1.1 | `<html>` had no `lang`. | `fluidPage(lang = "en")`. | ✅ |

### Operable

| # | WCAG | Finding | Fix | Status |
|---|---|---|---|---|
| W14 | 2.1.1 | "Select" station buttons (Recent and Station Lists tables) were `<a onclick>` with no `href`, so keyboard users couldn't reach them. | New `build_stn_select_btn()` renders a `<button>` with a unique name ("Select station 33223: Yellow River - 17th St"). | ✅ |
| W15 | 2.1.1 | "Download plot" was also `<a onclick>` with no `href`. | Now a `<button>`. | ✅ |
| W16 | 2.1.1 | The notice and report-error close controls were `<div onclick>`. The "Toggle watersheds" and "Hide my location" links in the map popup had no `href`. | Changed to `<button>`s with `aria-label`s. | ✅ |
| W17 | 2.4.1 | No way to bypass the header. | Added a "Skip to main content" link as the first tab stop. | ✅ |
| W18 | 2.4.7 | The accordion focus ring was `rgba(0,0,0,.1)` (invisible), and the white header links had no visible focus. | Global two-tone `:focus-visible` outline (3px dark plus a white halo), inverted on the red header. Verified on every tab stop. | ✅ |
| W19 | 2.4.3 | The bookmark button was re-rendered by `renderUI` on every press, so keyboard focus jumped to the top of the page. | The button is static now. Server code toggles its class, `aria-pressed`, and title. | ✅ |
| W20 | 2.4.4 | Link text "click here" / "Click here". | Changed to "show watershed boundaries on the map" and "NLCD land cover class legend and descriptions". | ✅ |
| W21 | 2.1.1 | **The map itself requires a pointer**: circle markers are SVG paths and aren't focusable. | Mitigated, as the WCAG equivalent-alternative approach allows: every station can be selected with the station selector (searchable), the Station Lists tab, the prev/next/random buttons, and the Recent tab. A screen-reader description on the map region says so. Leaflet's own controls (zoom, layers, pan with arrow keys) are keyboard operable, and the easy-buttons and layers toggle now have accessible names. | ⚠️ |
| W22 | 2.5.3 / 4.1.2 | Icon-only buttons (prev/next/random/bookmark, the map easy-buttons, the layers toggle) were named only by `title` or not at all. | Added `aria-label`s, plus `aria-hidden` on the icons. | ✅ |
| W23 | 2.3.3 (AAA, best practice) | Accordion and other animations ignored reduced-motion settings. | Added a `prefers-reduced-motion` CSS rule that disables transitions and smooth scrolling. (The first version also shortened `animation-duration`, which made infinite loading spinners spin extremely fast. See R1.) | ✅ |

### Understandable / Robust

| # | WCAG | Finding | Fix | Status |
|---|---|---|---|---|
| W24 | 4.1.3 | Station changes, filter counts, and report progress were visual only. | A polite live region (`#sr-announcer`) announces "Selected station …". The "Showing N of M stations" count is a live region. Report generation announces "Creating…" and "Downloaded". The URL notice uses `role="status"`/`"alert"`. | ✅ |
| W25 | 3.2.5 (AAA) / G201 | External links open new tabs without warning. | `scripts.js` appends visually hidden "(opens in new tab)" text and `rel="noopener"` to every `target=_blank` link, including links in dynamically rendered and markdown content. | ✅ |
| W26 | 4.1.2 | The bookmark toggle state was conveyed only by a gold background. | Added `aria-pressed`. | ✅ |
| W27 | 3.3.2 | The thermistor help text said to use the plotly camera icon, but the modebar is disabled. | Text now points to the "Download plot" button. Also fixed the typo "because exposed". | ✅ |

### Known limitations / open items (web)

| # | Item | Notes |
|---|---|---|
| L1 | ⏳ Plotly legend toggles and hover tooltips are mouse-only. | They're supplementary: the same data is in the summary and data tables. The `role="img"` description covers the chart. Revisit if plotly adds keyboard legend support. |
| L2 | ⏳ DataTables: wide tables scroll horizontally (1.4.10 Reflow at 320px). | Allowed for data tables under the 1.4.10 exception for content that needs two-dimensional layout. The FixedColumns extension keeps the first column visible. |
| L3 | ⏳ Map circle-marker colors against the basemap (1.4.11 non-text contrast). | Markers have a black outline. Color isn't the only cue (legend plus station details), but some basemaps reduce contrast. Consider a thicker outline. |
| L4 | ⏳ Manual screen-reader pass. | Semantics were verified in the DOM. A full pass with NVDA + Chrome and VoiceOver + Safari is still recommended before sign-off. |
| L5 | ⏳ 200% zoom / text-spacing pass. | No fixed-height text containers were found in code review. Still needs a visual check. |
| L6 | ⏳ Google Analytics (`gtag`). | Not an accessibility issue, but check that the privacy notice requirements in campus policy are met. |

---

## PDF reports

### Findings (before)

`pdfinfo` on a sample report (station 413814, 2025):

```
Tagged: no      Metadata Stream: no     PDF version: 1.7    (no Title / Author / Lang)
```

| # | Requirement | Finding | Fix | Status |
|---|---|---|---|---|
| P1 | PDF/UA structure, WCAG 1.3.1 | Untagged PDF: no headings, lists, tables, or reading order for assistive technology. | `render_station_report()` (in `src/reports.R`) first renders a **tagged PDF/UA-2** with LuaLaTeX and LaTeX's tagging code (pandoc's `pdfstandard` → `\DocumentMetadata{pdfstandard=ua-2, tagging=on, lang=en-US}`), and table header rows are tagged as `TH`. **If that fails for any reason, it falls back to the previous untagged XeLaTeX render**, so a report is always produced. The result is cached per R process, so an unsupported server only pays for the failed attempt once. | ✅ local · ⏳ production (see P1 notes) |
| P2 | 1.1.1 | Figures (header logo, location map, 8 plot types) had no alt text. | New `report_fig()` helper includes each figure with `\includegraphics[alt={…}]`. Alt text is generated from the data (date ranges, min–max values, phosphorus median and CI) and points to the summary table that follows. | ✅ (tagged mode) |
| P3 | 2.4.2 / PDF/UA | No document title; viewers showed the file name. | YAML sets `title-meta`, `author-meta`, `subject`, and `lang: en-US`. | ✅ verified with `pdfinfo` |
| P4 | 1.4.1 | The air vs water temperature plot used color only (orange vs lightsteelblue, which also had low contrast). | Shapes are now circle vs triangle, and water is drawn in `steelblue`. | ✅ |
| P5 | 3.1.1 | No document language. | `lang: en-US`. | ✅ (in tagged mode via `\DocumentMetadata`; untagged mode via babel/hyperref) |

### P1 notes: tagged PDFs need a recent LaTeX

- LaTeX tagging requires **LuaLaTeX** (or pdfLaTeX). **XeLaTeX isn't supported**, so the tagged path switches engines. The fonts (`times*.ttf` via fontspec) work with both.
- It needs a LaTeX release from 2025 or later with the `pdfmanagement`, `tagpdf`, and `latex-lab` packages at **matching versions**.
- **Local status: ✅ verified 2026-11-19.** After the owner updated MiKTeX, `render_test_report.R` printed `Tagged PDF: TRUE`. `pdfinfo` shows `Tagged: yes`, `PDF version: 2.0`, and an XMP metadata stream. The structure tree (decompressed) contains Document, section/subsection headings, 8 `Table`s (69 `TH`, 193 `TD`), 9 `Figure`s each with `/Alt` text, `itemize`/`item` lists, `Link`s, and `/Lang (en-US)`, with page headers and footers as `Artifact`s. Render time is about 30 s with LuaLaTeX (vs about 19 s with XeLaTeX). As a side effect, LuaLaTeX also stops the short table dates from wrapping.
- *History:* MiKTeX 25.4 had a 2025-11 kernel but no `pdfmanagement`. I installed it with the owner's approval. The tagged build then failed with `\__tag_AssignStructureRole … Undefined control sequence` because the existing `tagpdf`/`latex-lab` are older than the new `pdfmanagement`. The generated `.tex` was checked and is correct (`\DocumentMetadata` present, 9 `alt=` figures). **Next step for the owner:** update MiKTeX (MiKTeX Console → Updates, or `miktex packages update`), then run `Rscript test/render_test_report.R` and confirm `Tagged PDF: TRUE` and `pdfinfo` → `Tagged: yes`.
- **Production (connect.doit.wisc.edu):** the server's TeX installation is managed by DoIT. Ask DoIT which TeX Live version is installed and whether it can be TeX Live 2025+ (with `latex-lab`, `pdfmanagement-testphase`/`pdfmanagement`, and `tagpdf`). Until then the fallback keeps reports working, untagged.
- **Cosmetic follow-up:** under LuaLaTeX the header logo PNG shows a faint gray background box (probably how its alpha channel is handled). Flattening `header.png` onto white would fix it.
- **Kill switch:** set the environment variable **`WAV_PDF_TAGGING=false`** in Posit Connect (content → Vars) to skip the tagged attempt and render untagged directly. It also accepts `0`, `no`, or `off`. Unset or `true` means try tagged first. Don't put it in `app/.Renviron`, because that file is deployed.
- **Server request (DoIT), sent 2026-11-19.** Specifics:
  - **TeX Live 2025 or newer** (LaTeX kernel ≥ 2025-06-01; TL 2026 preferred), kept current with `tlmgr update --self --all`.
  - **LuaLaTeX** (`luahbtex` + `luaotfload`). Tagging does not support XeLaTeX.
  - Tagging packages at **matching versions**: `latex-lab`, `tagpdf`, `pdfmanagement-testphase`, `l3kernel`, `l3backend`.
  - `babel` + **`babel-english`**. The current server is missing this; its absence caused the production error *"Package babel Error: You haven't specified a language option"*.
  - Packages the report loads: amsmath amssymb array bookmark booktabs calc enumitem etoolbox fancyhdr fontspec footnotehyper geometry graphicx hyperref iftex lastpage lmodern longtable microtype parskip selnolig titlesec unicode-math upquote xcolor xurl.
  - The simplest way to get all of the above: TeX Live **scheme-full**, or at least `collection-latexrecommended`, `collection-luatex`, `collection-fontsrecommended`, and `collection-langenglish`.
  - On a tagged-render failure the app now logs a `LaTeX setup:` line (lualatex version, LaTeX format date, and whether `tagpdf.sty`, `pdfmanagement-init.sty`, `latex-lab-testphase-table.sty`, and `american.ldf` are found) to confirm the server state.
- **Validate** tagged output with [veraPDF](https://verapdf.org/) (PDF/UA-2 profile; needs Java) or PAC (PDF Accessibility Checker, Windows), and spot-check with a screen reader. Neither was available in this session.
- **Fallback cost:** when tagging is unsupported, the first report in each R process takes about 30 s longer (one failed LuaLaTeX attempt). Set `report_tagging_ok <- FALSE` in `global.R` to skip the attempt entirely if needed.
- **Alternative if server LaTeX can't be upgraded:** offer an accessible HTML version of the report (`rmarkdown::html_document`), which is far easier to make WCAG-conformant, next to the PDF.

---

## Bugs found during the audit (fixed)

| # | Where | Bug | Impact |
|---|---|---|---|
| B1 | `global.R` `get_phos_exceedance_text()` | The function ended on `if (n < 6) {…}`, so it **returned NULL whenever ≥ 6 samples** were collected. | The phosphorus interpretation was missing from the Nutrient tab *and* from PDF reports for every properly sampled station. |
| B2 | `src/reports.R` `build_report_baseline_table()` | `\n` was inserted into column names, which broke the markdown pipe table. | **The "Baseline stream monitoring measurements" table printed as raw `\|...\|` text** in every PDF report. |
| B3 | `src/reports.R` `build_report_fieldwork_comments()` | `gsub(..., .)` used the magrittr `.` inside a native `\|>` pipe. | **The "Additional fieldwork details" section printed `Error: object '.' not found`** in every report. |
| B4 | same | `na.omit(com1, com2)`: the second argument is ignored. | `additional_comments` never appeared. |
| B5 | `server.R` startup | A `?stn=` bookmark for a station without data in the 4 default map years was filtered out, so no station loaded. | **722 of 1,539 stations (47%) couldn't be opened from a shared or bookmarked link.** The year filter now widens automatically. |
| B6 | `server.R` | The bad-URL notice used the undefined variable `query`. | Visiting with an invalid `?stn=` errored instead of showing the notice. |
| B7 | `global.R` `set_page_title()` | The station name was injected into single-quoted JS. | 29 stations with `'` in their names (e.g. "US 300' of CTH 95") silently failed to update the page title. |
| B8 | `src/3.5__stn_report.R` | Report error messages were injected into single-quoted JS and set with `innerHTML`. | Error messages with quotes broke, and there was HTML injection from error text. Now uses JSON-encoding + `textContent`. |
| B9 | `src/2.3__station_list.R` | KML/GeoJSON downloads used `StationId`/`Longitude` columns that no longer exist (`clean_names()` is commented out). | **The KML and GeoJSON station-list downloads errored.** |
| B10 | nutrient and thermistor tabs | `downloadHandler(filename = sprintf(...))` was a value, not a function: a promise forced once and then reused. | After the first download, CSV filenames kept the first station's ID and year. |
| B11 | `src/1.1__map.R` | `between()` was called on `input$map_bounds` before the map rendered (NULL → `logical(0)` → `if` error). | Possible error on a fast station change at startup. Now `req()`'d. |
| B12 | `src/plotly.R` `macro_add_missing_years()` | Hard-coded `2015:2025`. | Years after 2025 with no sample weren't shown as gaps. |
| B13 | `src/plotly.R` nutrient plot | The legend said "Lower/Upper **90%** CI"; text everywhere else says 80%. | Inconsistent labelling. The label now says 80%. |
| B16 | `global.R` `get_phos_estimate()` | **The "80%" phosphorus CI was actually a 60% CI.** A two-sided interval used `qt(0.80, df)`; an 80% interval needs `qt(0.90, df)`. Fixed by owner decision (2026-11-19). | Intervals were too narrow, so stations were labelled "clearly exceeds/meets" too often. With the fix, **233 of 2,075 nutrient station-years (11%) change interpretation**: 145 from "clearly exceed" and 88 from "clearly meet" move to the "may exceed / may meet" categories. Example (n = 6): 0.0359–0.0504 becomes 0.0324–0.0558. |
| B17 | `src/3.4__watershed_info.R` | Used `case_match()`, deprecated in dplyr 1.2. | Replaced with `recode_values()` (same `from ~ to` syntax). Verified identical output. |
| B14 | `src/reports.R` `build_report_summary()` | "Report downloaded on …" was only added when baseline data existed. | Missing date on nutrient-only and thermistor-only reports. |
| B15 | `src/reports.R` | `build_link()` ignored its `title` argument (always used `href`). | Minor. |

## Questions for the owner (not changed)

| # | Question |
|---|---|
| ~~Q1~~ | ~~Phosphorus CI is 60%, not 80%.~~ **Resolved:** switched to `qt(0.90)` (see B16). Consider a CHANGELOG entry, since published interpretations change for 11% of station-years. |
| Q2 | Are the new link teal (`#11745a`) and button gray (`#5d6d6e`) acceptable under Extension brand guidance? Both pass AA. |

## Structural improvements (suggested, not done)

- **Map circle-marker edges look soft.** This was confirmed as *pre-existing*: `main` and this branch render pixel-identical markers. The cause is the 0.5 px black outline (`weight = 0.5`) on radius 4–8 SVG circles, which anti-aliases across partial pixels. Options: `weight = 1` with a softer outline color (e.g. `#333`), or slightly larger radii.
- **Server-side selectize** for the 1,539-station `selectInput` (Shiny warns about it at runtime). It would speed up load and the selectize a11y plugin.
- **Report table widths:** pandoc's relative column widths wrap short dates ("May / 25"). Consider `kableExtra`/explicit column specs.
- **`test/test_station_report.R`** uses old function names (`buildReportSummary`, `makeReportPlots`…) and no longer runs. `test/render_test_report.R` replaces its rendering use.
- Add the axe-core check to a repeatable script or CI job (for example `shinytest2` + `axe-core`) so regressions are caught.

---

## Work log

**2026-11-19**
- Created branch `accessibility-audit`.
- Reviewed all UI modules, plot helpers, and the report Rmd.
- Baseline axe-core scan: 9 rule violations. Rendered a baseline PDF: untagged, no metadata, and 3 broken sections found (B1–B3).
- Implemented web fixes W1–W27, then re-ran axe: 0 violations in all 11 states. Scripted keyboard checks confirmed:
  - The skip link is the first tab stop.
  - Every stop shows a 3px focus outline.
  - "Select" buttons are reachable buttons with unique names and select the station.
  - The bookmark toggle keeps focus and updates `aria-pressed`.
  - Selections are announced through the live region.
  - All radio groups have accessible names.
- Fixed bugs B1–B15 and verified B1–B5, B7, and B9 directly. Before/after PDFs were compared visually.
- PDF:
  - Added metadata, alt text, table header tagging, and the tagged-render-with-fallback path.
  - Installed MiKTeX `pdfmanagement` (owner approved). The local tagged build is blocked by older `tagpdf`/`latex-lab`. Updating MiKTeX packages beyond the approved one was not done; the owner needs to update.
  - Verified the untagged fallback output and the generated tagged `.tex`.

**2026-11-19 (round 2, after owner review)**
- **R1 (regression, fixed):** loading spinners "spun wildly". The cause was my `prefers-reduced-motion` rule setting `animation-duration: 0.01ms`. On a machine with OS animations reduced, infinite spinner animations then ran at extreme speed. The rule now only disables transitions and smooth scrolling.
- **Replaced shinycssloaders with native Shiny busy indicators:** `useBusyIndicators(spinners = TRUE, pulse = TRUE)` and `busyIndicatorOptions(spinner_color = "#30a67d")`. Removed `with_spinner()` and all its uses. Ran `renv::snapshot()` to drop `shinycssloaders` from `app/renv.lock`; this also recorded R 4.6.2, the local R version. Verified with emulated reduced motion: the indicators use 0.25 s fade-ins with normal timing.
- **R2 (regression, fixed):** the Macroinvertebrate plot errored with "NA/NaN argument". My B12 fix computed `max(c(df$year, ...))`, which included the `NA` year of the "All time" column. Added `na.rm = TRUE`. Verified both macro modes for 10 stations, and all 4 baseline plot types in the browser for 2 stations.
- **Phosphorus CI switched to a true two-sided 80% interval (B16).**
- **`case_match()` → `recode_values()` (B17).**
- **Bold text:** no WCAG requirement calls for heavier text. Two things got bolder in round 1. The page title became an `h1`, which the CSS gives weight 700 (it was an `h2` at 600); that is now set back to 600. And several input labels that used to be plain or italic text ("Choose year:", "Landscape scale:") are now real `<label>`s, which use the theme's `.control-label` weight of 600. That's needed for the labels to be programmatic, but the weight is just styling and can be reduced if preferred.
- **Map marker anti-aliasing:** pre-existing, not caused by this work (see Structural improvements).
- **Two more contrast issues** found with a station that has every data type:
  - Red/teal words in `md/nutrient_info.md` now use swatches.
  - Flatly's teal on secondary `nav-tabs` (the thermistor data tables) now uses the accessible link color.
  - Re-scanned stations 683299 and 413814 across 15 states each (all tabs plus all 4 baseline plot types): **0 violations, no Shiny output errors.**
- Root `.gitignore` ignores `*.*`; added `!docs/*.md` so this log can be committed.

**2026-11-19 (round 3)**
- Owner updated MiKTeX, and tagged PDF rendering now works locally. Independently re-rendered station 413814 / 2025 and verified the structure tree (see P1 notes).
- Committed on `accessibility-audit` and merged to `main` (owner request). **Before deploying:** ask DoIT about TeX Live on connect.doit.wisc.edu. Until it supports tagging, production reports use the untagged fallback.

**2026-11-19 (round 4: production failure)**
- **R3 (regression, fixed):** report downloads failed on Connect. Both attempts errored with `Package babel Error: You haven't specified a language option`.
  - Cause: I had put `lang: en-US` in the Rmd YAML, which makes pandoc load `babel` in *both* the tagged and the untagged render. The server's TeX lacks `babel-english`, so the "safety net" had a new dependency. MiKTeX's auto-install hid this locally.
  - Fix: `lang` now goes only into the tagged-mode metadata file. The untagged render is back to the original package set. The PDF language for untagged renders is set with `\AddToHook{package/hyperref/after}{\hypersetup{pdflang=en-US}}`, guarded by `\ifdefined\AddToHook` so very old LaTeX ignores it. (An `\AtBeginDocument` version ran too late for hyperref to write `/Lang`.)
  - Verified: the untagged `.tex` contains no babel and no `\DocumentMetadata`, and the PDF has `/Lang(en-US)` plus title metadata. The tagged render still works (`Tagged: yes`, PDF 2.0, `/Lang (en-US)`).
- Added the `WAV_PDF_TAGGING` env var kill switch and the `LaTeX setup:` diagnostic log line.
- Hardened `report_fig()` to use `pdf()` when R lacks cairo support.
- **Lesson:** anything the fallback depends on must be tested on the target server's TeX (or a minimal TinyTeX), not only on a machine that auto-installs packages.
