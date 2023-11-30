makeReportMap <- function(stn) {
  suppressWarnings({
    bbox <- with(stn, c(
      xmin = longitude - .3,
      xmax = longitude + .3,
      ymin = latitude - .2,
      ymax = latitude + .2
    ))

    crop_counties <- st_crop(counties, bbox)
    crop_wsheds <- st_crop(huc10, bbox)
    # crop_wsheds <- st_crop(dnr_watersheds, bbox)
    crop_wshed_labels <- crop_wsheds %>%
      st_centroid(of_largest_polygon = T) %>%
      mutate(label = str_wrap(paste(Huc10Name, "Watershed"), 25))
    crop_water <- st_crop(waterbodies, bbox)

    plt1 <- ggplot() +
      geom_sf(data = counties) +
      geom_sf_text(
        data = counties,
        aes(label = CountyName),
        size = 1.5,
        alpha = .5,
        angle = 15,
        hjust = .5) +
      geom_sf(
        data = st_as_sfc(st_bbox(bbox, crs = 4326)),
        fill = NA,
        color = "#c5050c") +
      geom_sf(data = stn, fill = "red", size = 4, shape = 24) +
      theme_void() +
      theme(legend.position = "none")

    plt2 <- ggplot() +
      geom_sf(data = crop_counties, color = NA) +
      geom_sf(data = crop_water, color = "steelblue", fill = "lightsteelblue", size = 2) +
      geom_sf(data = crop_counties, fill = NA, linetype = "dashed") +
      geom_sf(data = crop_wsheds, alpha = .25) +
      geom_sf_text(
        data = crop_wshed_labels,
        aes(label = label),
        alpha = .5,
        size = 2,
        check_overlap = T) +
      geom_sf(data = stn, fill = "red", size = 4, shape = 24) +
      theme_void()

    gridExtra::grid.arrange(plt1, plt2, nrow = 1)
  })
}
