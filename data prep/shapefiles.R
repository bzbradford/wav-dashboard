#' Shapefile data prep
#' This takes some time and only needs to be run if shapes are updated

library(tidyverse)
library(janitor)
library(sf)
library(rmapshaper) # ms_simplify
library(leaflet)


# Counties ----

counties <- read_sf("shp/wi-county-bounds.geojson") |>
  clean_names("big_camel") |>
  st_make_valid() |>
  select(
    CountyName,
    DnrRegion = DnrRegionName,
    geometry
  )

counties.simp <- ms_simplify(counties, 0.25)

if (FALSE) {
  quickmap(counties)
  quickmap(counties.simp)
}

## NKEs ----

nkes <- read_sf("shp/wi-nke-plans-2022.geojson") |>
  clean_names("big_camel") |>
  st_make_valid() |>
  drop_na(PlanId)

nkes.simp <- ms_simplify(nkes, 0.25)

nke_data <- nkes |>
  select(
    nke_plan_name = PlanName,
    nke_plan_purpose = PurposeDe,
    nke_plan_objective = Objective,
    nke_start = StartDate,
    nke_end = EndDate
  ) |>
  mutate(across(
    where(is_character),
    ~ str_to_sentence(str_trim(gsub("[\r\n]", "", .x)))
  ))

if (FALSE) {
  quickmap(nkes)
  quickmap(nkes.simp)
}


# Watersheds ----
# transform to 3071 (WTM) for faster joining

# huc6 basins
huc6.wtm <- read_sf("shp/wi-huc06-basins.geojson") |>
  clean_names(case = "big_camel") |>
  st_make_valid() |>
  st_transform(3071)

# load huc8 subbasins and join huc6 info
huc8.wtm <- read_sf("shp/wi-huc08-subbasins.geojson") |>
  clean_names(case = "big_camel") |>
  st_make_valid() |>
  st_transform(3071) |>
  select(-ShapeLeng) |>
  st_join(huc6.wtm, largest = TRUE) |>
  select(
    Huc8Code,
    Huc8Name,
    MajorBasin,
    Area = ShapeArea,
    geometry
  )

# load huc10 watersheds and join huc8 info
huc10.wtm <- read_sf("shp/wi-huc10-watersheds.geojson") |>
  clean_names(case = "big_camel") |>
  st_make_valid() |>
  st_transform(3071) |>
  st_join(select(huc8.wtm, -Area), largest = TRUE) |>
  select(
    Huc10Code,
    Huc10Name,
    Huc8Code,
    Huc8Name,
    MajorBasin,
    Area = ShapeArea,
    geometry
  )

# load huc12 watersheds and join huc10 info
huc12.wtm <- read_sf("shp/wi-huc12-subwatersheds.geojson") |>
  clean_names(case = "big_camel") |>
  st_make_valid() |>
  st_transform(3071) |>
  st_join(select(huc10.wtm, -Area), largest = TRUE) |>
  select(
    Huc12Code,
    Huc12Name,
    Huc10Code,
    Huc10Name,
    Huc8Code,
    Huc8Name,
    MajorBasin,
    Area = ShapeArea,
    geometry
  )

# convert to WGS
huc6 <- st_transform(huc6.wtm, 4326)
huc8 <- st_transform(huc8.wtm, 4326)
huc10 <- st_transform(huc10.wtm, 4326)
huc12 <- st_transform(huc12.wtm, 4326)

# DNR watersheds (approx HUC10)
dnr_watersheds <- read_sf("shp/wi-dnr-watersheds.geojson") |>
  clean_names(case = "big_camel") |>
  st_make_valid() |>
  select(
    DnrWatershedCode = WshedCode,
    DnrWatershedName = WshedName,
    SizeAcres = WatershedSizeAcresAmt,
    SizeSqMiles = WatershedSizeSqMilesAmt,
    TotalLakeAcres = TotalLakeAcresAmt,
    TotalWetlandAcres = TotalWetlandAcresAmt,
    geometry
  )

# simplify
huc8.simp <- ms_simplify(huc8, 0.5)
huc10.simp <- ms_simplify(huc10, 0.5)
huc12.simp <- ms_simplify(huc12, 0.5)
dnr_watersheds.simp <- ms_simplify(dnr_watersheds, 0.15)

# inspect
if (FALSE) {
  quickmap(huc6)
  quickmap(huc8)
  quickmap(huc8.simp)
  quickmap(huc10)
  quickmap(huc10.simp)
  quickmap(huc12)
  quickmap(huc12.simp)
  quickmap(dnr_watersheds)
  quickmap(dnr_watersheds.simp)
}


# Major waterbodies ----
# Top 1000 waterbodies in the state by area, for use on the pdf reports

waterbodies <- read_sf("shp/wi-major-lakes.geojson")

if (FALSE) {
  quickmap(waterbodies)
}


# Flowlines ----

flowlines <- read_sf("shp/wi-hydro-nhd-flowlines.gpkg")
# head(flowlines)
# str(flowlines)
# sort(unique(flowlines$visibilityfilter))
flow2d <- flowlines |>
  rename(geometry = geom) |>
  st_zm() |>
  st_make_valid() |>
  st_transform(4326) |>
  arrange(desc(visibilityfilter)) |>
  mutate(level = consecutive_id(visibilityfilter))

flowlines.simp <- flow2d |>
  select(level, geometry) |>
  filter(level <= 5) |>
  ms_simplify(0.25)

rm(flowlines)
rm(flow2d)

if (FALSE) {
  flowlines.simp |>
    ggplot() +
    geom_sf(aes(color = factor(level))) +
    scale_color_brewer(palette = "Spectral", direction = -1)
}


# Export ----

local({
  save.image("shapefiles.RData")
  shapes <- list(
    counties = counties.simp,
    nkes = nkes.simp,
    huc8 = huc8.simp,
    huc10 = huc10.simp,
    huc12 = huc12.simp,
    dnr_watersheds = dnr_watersheds.simp,
    waterbodies = waterbodies,
    flowlines = flowlines.simp
  )
  for (shape in names(shapes)) {
    fname <- paste0(shape, ".rds")
    fpath <- sprintf("../data/shp/%s", fname)
    saveRDS(shapes[[shape]], fpath)
    message("Save shape => ", fpath)
  }
})
