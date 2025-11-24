
# Helpers  ---------------------------------------------------------------------

# used for x axis in plots
set_report_date_range <- function(dates, pad_right = FALSE) {
  yr <- format(dates[1], "%Y")
  default_range <- as.Date(paste0(yr, c("-05-1", "-10-1")))
  lims <- c(
    min(dates - 10, default_range[1]),
    max(dates + 10, default_range[2])
  )
  if (pad_right) lims[2] <- lims[2] + 30
  lims
}

set_axis_limits <- function(vals, lower, upper) {
  lims <- c(
    min(vals, lower, na.rm = TRUE),
    max(vals, upper, na.rm = TRUE)
  )
  lims + abs(lims) * c(-.1, .1)
}

# adds a filled rectangle shape from min to max date axis and ymin to ymax y axis
add_rect_date <- function(ymin, ymax, color) {
  gg <- annotate("rect",
    xmin = as.Date(-Inf), xmax = as.Date(Inf),
    ymin = ymin, ymax = ymax, fill = alpha(color, .05)
  )
  if (!is.infinite(ymax)) {
    gg <- c(gg, geom_hline(yintercept = ymax, color = alpha(color, .25)))
  }
  gg
}

# adds a filled rectangle shape from min to max datetime axis and ymin to ymax y axis
add_rect_dttm <- function(ymin, ymax, color) {
  gg <- annotate("rect",
    xmin = as.POSIXct(-Inf), xmax = as.POSIXct(Inf),
    ymin = ymin, ymax = ymax, fill = alpha(color, .05)
  )
  if (!is.infinite(ymax)) {
    gg <- c(gg, geom_hline(yintercept = ymax, color = alpha(color, .2)))
  }
  gg
}


# Map --------------------------------------------------------------------------

#' @param stn single row from `all_pts` sf data
buildReportMap <- function(stn) {
  suppressWarnings({

    bbox <- with(
      stn,
      c(
        xmin = longitude - .3,
        xmax = longitude + .3,
        ymin = latitude - .2,
        ymax = latitude + .2
      )
    )

    county_lines <- wi_counties %>%
      st_cast("MULTILINESTRING")
    crop_counties <- county_lines %>%
      st_crop(bbox)
    crop_state <- st_crop(wi_state, bbox)
    crop_wsheds <- st_crop(huc10, bbox)
    crop_wshed_labels <- crop_wsheds %>%
      st_centroid(of_largest_polygon = T) %>%
      mutate(label = str_wrap(paste(Huc10Name, "Watershed"), 20))
    crop_water <- st_crop(waterbodies, bbox)
    crop_flow <- st_crop(flowlines, bbox)

    big_plt <- ggplot() +
      geom_sf(data = wi_state, fill = "grey90", color = "black", lwd = .25) +
      geom_sf(data = county_lines, color = "grey", lwd = .1) +
      geom_sf(data = wi_state, fill = NA, color = "black", lwd = .25) +
      geom_sf_text(
        data = wi_counties,
        aes(label = CountyName),
        size = 1.5,
        alpha = .5,
        angle = 15,
        hjust = .5
      ) +
      geom_sf(
        data = st_as_sfc(st_bbox(bbox, crs = 4326)),
        fill = NA,
        color = "#c5050c"
      ) +
      geom_sf(data = stn, fill = "red", size = 3, shape = 24) +
      coord_sf(expand = F) +
      theme_void() +
      theme(
        legend.position = "none",
        plot.margin = margin_auto(5)
      )

    inset_plt <- ggplot() +
      geom_sf(
        data = crop_state,
        fill = "grey90",
        color = "grey",
        lwd = 1
      ) +
      geom_sf(
        data = crop_flow,
        aes(linewidth = .25 + .1 * (5 - level)),
        color = "lightsteelblue",
        lineend = "round",
        linejoin = "round"
      ) +
      geom_sf(
        data = crop_water,
        fill = "lightsteelblue",
        # color = "steelblue",
        color = NA,
        lwd = .1
      ) +
      geom_sf(
        data = crop_wsheds,
        fill = NA,
        color = "grey35",
        lwd = .1,
        alpha = .5
      ) +
      geom_sf(
        data = crop_counties,
        fill = NA,
        color = "grey50",
        lwd = .05,
        alpha = .5
      ) +
      geom_sf_text(
        data = crop_wshed_labels,
        aes(label = label),
        size = 1.5,
        alpha = .5,
        check_overlap = T
      ) +
      geom_sf(data = stn, fill = "red", size = 3, shape = 24) +
      coord_sf(expand = F) +
      scale_linewidth_identity() +
      theme_void() +
      theme(
        legend.position = "none",
        panel.border = element_rect(color = "black", linewidth = .5),
        plot.margin = margin_auto(0)
      )

    gridExtra::grid.arrange(big_plt, inset_plt, nrow = 1)
  })

  # message(as.numeric(now() - t))
}


# all_pts %>%
#   filter(station_id == sample(station_id, 1)) %>%
#   buildReportMap()
# ggsave("test.png", h = 4, w = 7)




# Plots -------------------------------------------------------------------

buildReportPlot <- function(df, type) {
  try({
    # Shared ----
    common_theme <- theme_classic() +
      theme(
        panel.grid.major.x = element_line(linewidth = .25),
        legend.position = "none",
        axis.line = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = .5)
      )

    # coldwater < 69.3 F
    # cool-cold 69.3 - 72.5 (middle 70.9)
    # cool-warm 72.5 - 76.3 (middle 74.4)
    # warmwater > 76.3
    temp_labels <- tibble(
      y = c(68, 70.9, 74.4, 78),
      label = c(
        "Coldwater stream (< 69.3°F)",
        "Cool-cold stream (69.3 - 72.5°F)",
        "Cool-warm stream (72.5 - 76.3°F)",
        "Warmwater stream (> 76.3°F)"
      )
    )

    # Temperature ----

    if (type == "temp") {
      df <- df %>%
        select(date, Air = air_temp, Water = water_temp, ) %>%
        filter(!is.na(Air) | !is.na(Water)) %>%
        pivot_longer(
          c(Air, Water),
          names_to = "measure",
          values_to = "temp"
        ) %>%
        mutate(temp = c_to_f(temp)) %>%
        mutate(label = paste0(temp, "°F")) %>%
        mutate(measure = paste(measure, "temperature"))
      if (nrow(df) == 0) {
        return("No data")
      }
      n_dates <- n_distinct(df$date)
      x_lims <- set_report_date_range(df$date, pad_right = T)
      y_lims <- set_axis_limits(df$temp, 50, 80)
      air <- df %>%
        filter(measure == "Air temperature") %>%
        drop_na(temp)
      water <- df %>%
        filter(measure == "Water temperature") %>%
        drop_na(temp)

      plt <- df %>%
        ggplot(aes(x = date, y = temp)) +
        add_rect_date(-Inf, 69.3, "blue") +
        add_rect_date(69.3, 72.5, "cornflowerblue") +
        add_rect_date(72.5, 76.3, "lightsteelblue") +
        add_rect_date(76.3, Inf, "darkorange") +
        geom_text(
          data = temp_labels,
          aes(y = y, label = label),
          x = as.Date(Inf),
          hjust = 1.05,
          size = 2.5,
          alpha = .8
        ) +
        {
          if (nrow(air) > 1) {
            geom_line(data = air, aes(color = measure), linewidth = 1.5)
          }
        } +
        {
          if (nrow(water) > 1) {
            geom_line(data = water, aes(color = measure), linewidth = 1.5)
          }
        } +
        geom_point(aes(fill = measure), size = 4, shape = 21) +
        ggrepel::geom_text_repel(
          aes(label = label),
          size = 3,
          box.padding = .5,
          min.segment.length = 0
        ) +
        scale_x_date(
          breaks = "months",
          date_labels = "%b\n%Y"
        ) +
        scale_y_continuous(
          breaks = scales::breaks_pretty(6),
          labels = ~ sprintf("%s°F\n(%s°C)", .x, round(f_to_c(.x), 1)),
          expand = expansion()
        ) +
        coord_cartesian(xlim = x_lims, ylim = y_lims) +
        scale_color_manual(values = c("orange", "lightsteelblue")) +
        scale_fill_manual(values = c("orange", "lightsteelblue")) +
        labs(
          x = NULL,
          y = "Temperature",
          fill = "Measurement",
          color = "Measurement"
        ) +
        common_theme +
        theme(legend.position = "top")

      return(plt)
    }

    # Thermistor ----

    if (type == "thermistor") {
      n_days <- as.numeric(max(df$date) - min(df$date))
      date_breaks <- ifelse(n_days > 150, "months", "weeks")
      date_format <- ifelse(n_days > 150, "%b", "%b %d")
      daily_min <- df %>%
        slice_min(order_by = temp_f, by = date) %>%
        select(date_time, min = temp_f)
      daily_max <- df %>%
        slice_max(order_by = temp_f, by = date) %>%
        select(date_time, max = temp_f)
      daily_range <-
        bind_rows(daily_min, daily_max) %>%
        arrange(date_time) %>%
        mutate(across(c(min, max), ~ zoo::na.approx(.x, na.rm = F))) %>%
        drop_na()
      daily_means <- df %>%
        summarize(mean = mean(temp_f), .by = date) %>%
        mutate(date_time = as.POSIXct(paste(date, "12:00:00")))

      plt <- daily_range %>%
        ggplot(aes(x = date_time)) +
        add_rect_dttm(-Inf, 69.3, "blue") +
        add_rect_dttm(69.3, 72.5, "cornflowerblue") +
        add_rect_dttm(72.5, 76.3, "lightsteelblue") +
        add_rect_dttm(76.3, Inf, "darkorange") +
        geom_text(
          data = temp_labels,
          aes(y = y, label = label),
          x = as.POSIXct(Inf),
          hjust = 1.05,
          size = 2.5,
          alpha = .8
        ) +
        geom_ribbon(
          aes(ymin = min, ymax = max),
          color = NA,
          fill = alpha("lightblue", .1)
        ) +
        geom_line(
          data = df,
          aes(y = temp_f),
          color = alpha("#1f77b4", .5),
          linewidth = .25
        ) +
        geom_ribbon(
          aes(ymin = min, ymax = max),
          color = alpha("#2590da", .25),
          fill = NA
        ) +
        geom_line(
          data = daily_means,
          aes(y = mean),
          color = "orange",
          linewidth = 1
        ) +
        scale_x_datetime(
          breaks = date_breaks,
          date_labels = date_format
        ) +
        scale_y_continuous(
          breaks = scales::pretty_breaks(),
          labels = ~ sprintf("%s°F\n(%s°C)", .x, round(f_to_c(.x), 1))
        ) +
        labs(x = NULL, y = "Water temperature") +
        common_theme +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))

      return(plt)
    }

    # Dissolved oxygen ----

    if (type == "d_o") {
      df <- df %>%
        select(date, d_o, do_sat = d_o_percent_saturation) %>%
        drop_na(d_o) %>%
        mutate(do_color = map_chr(d_o, do_color))
      n_dates <- n_distinct(df$date)
      df <- df %>%
        mutate(
          sat_label = if_else(
            is.na(do_sat),
            "",
            paste0("\n(", do_sat, ifelse(n_dates < 8, "% sat)", "%)"))
          ),
          label = paste0(d_o, ifelse(n_dates < 8, " mg/L", ""), sat_label)
        )
      x_lims <- set_report_date_range(df$date, pad_right = T)
      y_lims <- set_axis_limits(df$d_o, 0, 8)
      col_width <- ifelse(n_dates > 6, 10, 15)
      do_labels <- tibble(
        y = c(1, 3, 5, 6, 7),
        label = c(
          "Aquatic life minimum\n(1 mg/L) ",
          "Limited forage fish\n(>3 mg/L) ",
          "Warmwater fish\n(>5 mg/L) ",
          "Coldwater fish\n(>6 mg/L) ",
          "Coldwater spawning\n(>7 mg/L) "
        )
      )

      plt <- df %>%
        ggplot(aes(x = date, y = d_o)) +
        add_rect_date(-Inf, 1, "red") +
        add_rect_date(1, 3, "orange") +
        add_rect_date(3, 5, "gold") +
        add_rect_date(5, 6, "lightblue") +
        add_rect_date(6, 7, "steelblue") +
        add_rect_date(7, Inf, "blue") +
        geom_text(
          data = do_labels,
          aes(y = y, label = label),
          x = as.Date(Inf),
          hjust = 1.1,
          size = 2.5,
          lineheight = 1,
          alpha = .8
        ) +
        geom_col(
          aes(fill = do_color),
          position = "identity",
          color = "black",
          width = col_width
        ) +
        ggrepel::geom_text_repel(
          aes(label = label),
          size = 3,
          nudge_y = .25,
          min.segment.length = 0
        ) +
        scale_x_date(
          breaks = "months",
          date_labels = "%b\n%Y"
        ) +
        scale_y_continuous(
          breaks = scales::breaks_pretty(6),
          expand = expansion()
        ) +
        coord_cartesian(xlim = x_lims, ylim = y_lims) +
        scale_fill_identity() +
        labs(x = NULL, y = "Dissolved oxygen (mg/L)") +
        common_theme

      return(plt)
    }

    # pH ----

    if (type == "ph") {
      df <- df %>%
        select(date, ph) %>%
        drop_na(ph) %>%
        mutate(ph_diff = ph - 7)
      if (nrow(df) == 0) {
        return("No data")
      }
      x_lims <- set_report_date_range(df$date, pad_right = T)
      y_lims <- set_axis_limits(df$ph, 6, 9)
      ph_labels <- tibble(
        y = c(6, 7.5, 9),
        label = c(
          "Minimum water quality\nstandard (pH 6.0) ",
          "Optimal for fish\n(pH 7.5) ",
          "Maximum water quality\nstandard (pH 9.0) "
        )
      )

      plt <- df %>%
        ggplot(aes(x = date, y = ph)) +
        add_rect_date(-Inf, 6, "orange") +
        add_rect_date(6, 9, "chartreuse") +
        add_rect_date(9, Inf, "purple") +
        geom_text(
          data = ph_labels,
          aes(y = y, label = label),
          x = as.Date(Inf),
          hjust = 1.1,
          size = 2.5,
          alpha = .8
        ) +
        geom_hline(yintercept = 7.5, linetype = "dashed") +
        geom_point(
          aes(fill = ph_diff),
          shape = 23,
          color = "black",
          size = 10
        ) +
        ggrepel::geom_text_repel(
          aes(label = round(ph, 1)),
          size = 3,
          nudge_y = .35,
          min.segment.length = 0
        ) +
        scale_x_date(
          breaks = "months",
          date_labels = "%b\n%Y"
        ) +
        scale_y_continuous(
          breaks = scales::breaks_pretty(6),
          expand = expansion()
        ) +
        coord_cartesian(xlim = x_lims, ylim = y_lims) +
        scale_fill_gradient2(
          low = "#ffd43a",
          mid = "#00b82b",
          high = "#0099f7",
          limits = set_axis_limits(df$ph_diff, -2, 3)
        ) +
        labs(x = NULL, y = "pH") +
        common_theme

      return(plt)
    }

    # Conductivity ----

    if (type == "cond") {
      df <- df %>%
        select(date, cond = specific_cond) %>%
        drop_na(cond) %>%
        mutate(label = round(cond, 1))
      if (nrow(df) == 0) {
        return("No data")
      }
      n_dates <- n_distinct(df$date)
      df <- df %>%
        mutate(
          label = paste0(round(cond, 1), if_else(n_dates < 8, " uS/cm", ""))
        )
      x_lims <- set_report_date_range(df$date, pad_right = T)
      y_lims <- set_axis_limits(df$cond, 400, 700)
      cond_labels <- tibble(
        y = c(800, 1500, 2000),
        label = c(
          "High conductivity\n(> 800 uS/cm)\n\n",
          "Potentially toxic chloride\nlevels (1500-2000 uS/cm)\n\n",
          "Likely toxic chloride\nlevel (> 2000 uS/cm)\n\n"
        )
      ) %>%
        filter(y < y_lims[2])

      plt <- df %>%
        ggplot(aes(x = date, y = cond)) +
        add_rect_date(-Inf, 800, "turquoise") +
        add_rect_date(800, 1500, "orange") +
        add_rect_date(1500, 2000, "tomato") +
        add_rect_date(2000, Inf, "red") +
        geom_text(
          data = cond_labels,
          aes(y = y, label = label),
          x = as.Date(Inf),
          hjust = 1.1,
          size = 2.5,
          alpha = .8
        ) +
        {
          if (n_dates > 1) geom_line(color = "violet", linewidth = 2)
        } +
        geom_point(
          color = "black",
          fill = "violet",
          shape = 21,
          size = 4
        ) +
        ggrepel::geom_text_repel(
          aes(label = label),
          size = 3,
          nudge_y = max(df$cond) / 20,
          min.segment.length = 0
        ) +
        scale_x_date(
          breaks = "months",
          date_labels = "%b\n%Y"
        ) +
        scale_y_continuous(
          breaks = scales::breaks_pretty(6),
          expand = expansion()
        ) +
        coord_cartesian(xlim = x_lims, ylim = y_lims) +
        labs(x = NULL, y = "Specific conductance (uS/cm)") +
        common_theme +
        theme(legend.position = "none")

      return(plt)
    }

    # Transparency ----

    if (type == "trans") {
      df <- df %>%
        select(date, trans = transparency, tube = transparency_tube_length) %>%
        drop_na(trans) %>%
        mutate(trans = round(trans, 0))
      if (nrow(df) == 0) {
        return("No data")
      }
      n_dates <- n_distinct(df$date)
      df <- df %>%
        mutate(
          label = paste0(
            trans,
            if_else(trans == tube, "+", ""),
            if_else(n_dates < 8, " cm", "")
          )
        )
      x_lims <- set_report_date_range(df$date)
      y_lims <- set_axis_limits(df$trans, 0, 120)
      col_width <- ifelse(n_dates > 6, 10, 15)

      plt <- df %>%
        ggplot(aes(x = date, y = trans)) +
        geom_col(
          aes(y = tube, color = "grey50"),
          position = "identity",
          fill = "grey95",
          alpha = .25,
          width = col_width
        ) +
        geom_col(
          aes(fill = trans),
          position = "identity",
          color = "black",
          width = col_width
        ) +
        ggrepel::geom_text_repel(
          aes(label = label),
          size = 3,
          nudge_y = 1,
          min.segment.length = 0
        ) +
        scale_x_date(
          breaks = "months",
          date_labels = "%b\n%Y"
        ) +
        scale_y_continuous(
          breaks = seq(0, 120, 20),
          expand = expansion()
        ) +
        coord_cartesian(xlim = x_lims, ylim = y_lims) +
        scale_color_identity(guide = guide_legend(label = F)) +
        scale_fill_distiller(
          palette = "BuGn",
          limits = c(0, 120),
          breaks = c(0, 120),
          guide = guide_colorbar(
            label = F,
            frame.colour = "black",
            ticks = F
          )
        ) +
        labs(
          x = NULL,
          y = "Transparency (cm)",
          color = "Tube\nlength",
          fill = "Water\nclarity"
        ) +
        common_theme +
        theme(legend.position = "right", legend.title = element_text(size = 10))

      return(plt)
    }

    # Streamflow ----

    if (type == "flow") {
      df <- df %>%
        select(date, flow = streamflow) %>%
        drop_na(flow)
      if (nrow(df) == 0) {
        return("No data")
      }
      n_dates <- n_distinct(df$date)
      df <- df %>%
        mutate(
          label = paste0(round(flow, 1), if_else(n_dates < 8, " cfs", ""))
        )
      x_lims <- set_report_date_range(df$date, pad_right = T)
      y_lims <- set_axis_limits(df$flow, 0, 1)
      flow_labels <- tibble(
        y = c(.03, 3, 150),
        max = c(3, 150, Inf),
        label = c(
          "Headwater stream (0.03-3 cfs)\nEphemeral stream (< 0.03 cfs)  ",
          "Mainstem stream (3-150 cfs)\nHeadwater stream (0.03-3 cfs)  ",
          "Large river (> 150 cfs)\nMainstem stream (3-150 cfs)  "
        )
      ) %>%
        filter(y < y_lims[2], max > y_lims[2])

      plt <- df %>%
        ggplot(aes(x = date, y = flow)) +
        add_rect_date(-Inf, .03, "#915119") +
        add_rect_date(.03, 3, "#e3c283") +
        add_rect_date(3, 150, "#73cdc1") +
        add_rect_date(150, Inf, "#09968e") +
        geom_text(
          data = flow_labels,
          aes(y = y, label = label),
          x = as.Date(Inf),
          hjust = 1.05,
          size = 2.5,
          lineheight = 1.1,
          alpha = .8
        ) +
        {
          if (n_dates > 1) geom_line(color = "cadetblue", linewidth = 2)
        } +
        geom_point(
          color = "black",
          fill = "cadetblue",
          shape = 21,
          size = 4
        ) +
        ggrepel::geom_text_repel(
          aes(label = label),
          size = 3,
          nudge_y = max(df$flow) / 20,
          min.segment.length = 0
        ) +
        scale_x_date(
          breaks = "months",
          date_labels = "%b\n%Y"
        ) +
        scale_y_continuous(
          breaks = scales::breaks_pretty(6),
          expand = expansion(c(0, .1))
        ) +
        coord_cartesian(xlim = x_lims, ylim = y_lims) +
        labs(x = NULL, y = "Streamflow (cfs)") +
        common_theme +
        theme(legend.position = "none")

      return(plt)
    }

    # Total phosphorus ----

    if (type == "nutrient") {
      dates <- df$date
      yr <- lubridate::year(dates[1])
      eoy_date <- as.Date(paste0(yr, "-12-1"))
      df <- df %>%
        select(date, tp) %>%
        drop_na(tp) %>%
        mutate(exceeds = tp > phoslimit) %>%
        mutate(label = ifelse(tp == 0, "< LOD", signif(tp, 3)))
      x_lims <- c(dates[1] - 15, eoy_date + 15)
      y_lims <- set_axis_limits(df$tp, 0, .1)
      est <- getPhosEstimate(df$tp)
      est_labels <- tribble(
        ~value     , ~label           ,
        est$lower  , "Lower 80% CI"   ,
        est$median , "Median value"   ,
        est$upper  , "Upper 80% CI"   ,
        est$limit  , "State criteria" ,
      ) %>%
        mutate(date = eoy_date)
      ci <- est$n > 1 # conf int if more than 1 observation

      plt <- df %>%
        ggplot(aes(x = date, y = tp)) +
        {
          # draw confidence interval
          if (ci) {
            c(
              geom_rect(
                data = tibble(),
                inherit.aes = F,
                aes(fill = est$lower > phoslimit),
                xmin = -Inf,
                xmax = Inf,
                ymin = est$lower,
                ymax = est$upper,
                alpha = .2
              ),
              geom_hline(yintercept = est$lower),
              geom_hline(yintercept = est$upper),
              geom_hline(yintercept = est$median, linetype = "dashed")
            )
          }
        } +
        geom_hline(yintercept = phoslimit, linewidth = 1, color = "red") +
        geom_col(aes(fill = exceeds), color = "black", width = 15) +
        {
          # label confidence interval
          if (ci) {
            ggrepel::geom_text_repel(
              data = est_labels,
              aes(y = value, label = label),
              size = 3,
              nudge_x = 1,
              box.padding = .5,
              min.segment.length = 0
            )
          }
        } +
        ggrepel::geom_text_repel(
          aes(label = label),
          size = 3,
          nudge_y = .005,
          min.segment.length = 0
        ) +
        scale_x_date(
          breaks = dates,
          date_labels = "%b %d\n%Y"
        ) +
        scale_y_continuous(
          breaks = scales::breaks_pretty(6),
          expand = expansion()
        ) +
        coord_cartesian(xlim = x_lims, ylim = y_lims) +
        scale_fill_manual(
          breaks = c(T, F),
          values = c("#ffb568", "#40b0a6"),
          labels = c("Yes", "No")
        ) +
        labs(
          x = NULL,
          y = "Total phosphorus (mg/L)",
          fill = "Exceeds 0.075 mg/L criteria?"
        ) +
        common_theme +
        theme(legend.position = "bottom")

      return(plt)
    }
  })
}
