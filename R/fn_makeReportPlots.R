makeReportPlots <- function(df, type) {
  try({

    # Shared ----
    common_theme <- theme_classic() +
      theme(
        panel.grid.major.x = element_line(linewidth = .25),
        legend.position = "none",
        axis.line = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = .5)
      )


    # Temperature ----
    # assumes temperatures are passed in as C

    if (type == "temp") {

      df <- df %>%
        select(date, Air = air_temp, Water = water_temp,) %>%
        filter(!is.na(Air) | !is.na(Water)) %>%
        pivot_longer(c(Air, Water), names_to = "measure", values_to = "temp") %>%
        mutate(label = paste0(temp, "°C")) %>%
        mutate(measure = paste(measure, "temperature"))
      n_dates <- n_distinct(df$date)
      x_lims <- setReportDateRange(df$date, pad_right = T)
      y_lims <- setAxisLimits(df$temp, 10, 25)
      temp_labels <- tibble(
        x = as.Date(Inf),
        y = c(20.7, 24.6),
        label = c(
          "Cold-cool transition\n(20.7°C / 69.3°F)",
          "Cool-warm transition\n(24.6°C / 76.3°F)"))

      plt <- df %>%
        ggplot(aes(x = date, y = temp)) +
        addRectDate(-Inf, 20.7, "blue") +
        addRectDate(20.7, 24.6, "cornflowerblue") +
        addRectDate(24.6, Inf, "darkorange") +
        ggrepel::geom_text_repel(
          data = temp_labels,
          aes(x, y, label = label),
          size = 2.5) +
        { if (n_dates > 1) geom_line(aes(color = measure), linewidth = 1.5) } +
        geom_point(aes(fill = measure), size = 4, shape = 21) +
        ggrepel::geom_text_repel(
          aes(label = label),
          size = 3,
          box.padding = unit(.5, "lines"),
          min.segment.length = unit(0, "lines")) +
        scale_x_date(
          breaks = "months",
          date_labels = "%b\n%Y") +
        scale_y_continuous(
          breaks = scales::breaks_pretty(6),
          labels = ~sprintf("%s°C\n(%s°F)", .x, round(c_to_f(.x), 1)),
          expand = expansion()) +
        coord_cartesian(xlim = x_lims, ylim = y_lims) +
        scale_color_manual(values = c("orange", "lightsteelblue")) +
        scale_fill_manual(values = c("orange", "lightsteelblue")) +
        labs(x = NULL, y = "Temperature", fill = "Measurement", color = "Measurement") +
        common_theme +
        theme(legend.position = "top")

      return(plt)
    }


    # Dissolved oxygen ----

    if (type == "do") {

      df <- df %>%
        select(date, d_o, do_sat = d_o_percent_saturation) %>%
        drop_na(d_o) %>%
        mutate(do_color = map_chr(d_o, do_color)) %>%
        mutate(sat_label = if_else(is.na(do_sat), "", paste0("\n(", do_sat, "% sat)"))) %>%
        mutate(label = paste0(d_o, " mg/L", sat_label))
      n_dates <- n_distinct(df$date)
      x_lims <- setReportDateRange(df$date, pad_right = T)
      y_lims <- setAxisLimits(df$d_o, 0, 10)
      col_width <- ifelse(n_dates > 8, 10, 15)
      do_labels <- tibble(
        x = as.Date(Inf),
        y = c(1, 3, 5, 7),
        label = c(
          "Aquatic life\nminimum (1 mg/L)",
          "Forage fish\nminimum (3 mg/L)",
          "Minimum for\nmost fish (5 mg/L)",
          "Trout spawning\n(7 mg/L)"))

      plt <- df %>%
        ggplot(aes(x = date, y = d_o)) +
        addRectDate(-Inf, 1, "red") +
        addRectDate(1, 3, "orange") +
        addRectDate(3, 5, "gold") +
        addRectDate(5, 7, "lightblue") +
        addRectDate(7, Inf, "blue") +
        ggrepel::geom_text_repel(
          data = do_labels,
          aes(x, y, label = label),
          size = 2.5) +
        geom_col(
          aes(fill = do_color),
          position = "identity",
          color = "black",
          width = col_width) +
        ggrepel::geom_text_repel(
          aes(label = label),
          size = 3.5,
          nudge_y = .25,
          min.segment.length = unit(0, "lines")) +
        scale_x_date(
          breaks = "months",
          date_labels = "%b\n%Y") +
        scale_y_continuous(
          breaks = scales::breaks_pretty(6),
          expand = expansion()) +
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
      x_lims <- setReportDateRange(df$date, pad_right = T)
      y_lims <- setAxisLimits(df$ph, 6, 9)
      ph_labels <- tibble(
        x = as.Date(Inf),
        y = c(6, 7.5, 9),
        label = c(
          "Minimum water quality\nstandard (pH 6.0)",
          "Optimal for fish\n(pH 7.5)",
          "Maximum water quality\nstandard (pH 9.0)"))

      plt <- df %>%
        ggplot(aes(x = date, y = ph)) +
        addRectDate(-Inf, 6, "orange") +
        addRectDate(6, 9, "chartreuse") +
        addRectDate(9, Inf, "purple") +
        ggrepel::geom_text_repel(
          data = ph_labels,
          aes(x, y, label = label),
          size = 2.5) +
        geom_hline(yintercept = 7.5, linetype = "dashed") +
        geom_point(
          aes(fill = ph_diff),
          shape = 23,
          color = "black",
          size = 10) +
        ggrepel::geom_text_repel(
          aes(label = round(ph, 1)),
          nudge_y = .35,
          size = 3.5,
          min.segment.length = unit(0, "lines")) +
        scale_x_date(
          breaks = "months",
          date_labels = "%b\n%Y") +
        scale_y_continuous(
          breaks = scales::breaks_pretty(6),
          expand = expansion()) +
        coord_cartesian(xlim = x_lims, ylim = y_lims) +
        scale_fill_gradient2(
          low = "#ffd43a",
          mid = "#00b82b",
          high = "#0099f7",
          limits = setAxisLimits(df$ph_diff, -2, 3)) +
        labs(x = NULL, y = "pH") +
        common_theme

      return(plt)
    }


    # Conductivity ----

    if (type == "cond") {

      df <- df %>%
        select(date, cond = specific_cond) %>%
        drop_na(cond) %>%
        mutate(label = paste(round(cond, 1), "μS/cm"))
      n_dates <- n_distinct(df$date)
      x_lims <- setReportDateRange(df$date, pad_right = T)
      y_lims <- setAxisLimits(df$cond, 400, 700)
      cond_labels <- tibble(
        x = as.Date(Inf),
        y = c(800, 1500, 2000),
        label = c(
          "High conductivity\n(> 800 μS/cm)\n\n",
          "Potentially toxic chloride\nlevels (1500-2000 μS/cm)\n\n",
          "Likely toxic chloride\nlevel (> 2000 μS/cm)\n\n")) %>%
        filter(y < y_lims[2])

      plt <- df %>%
        ggplot(aes(x = date, y = cond)) +
        addRectDate(-Inf, 800, "turquoise") +
        addRectDate(800, 1500, "orange") +
        addRectDate(1500, 2000, "tomato") +
        addRectDate(2000, Inf, "red") +
        ggrepel::geom_text_repel(
          data = cond_labels,
          aes(x, y, label = label),
          size = 2.5) +
        { if (n_dates > 1) geom_line(color = "violet", linewidth = 2) } +
        geom_point(
          color = "black",
          fill = "violet",
          shape = 21,
          size = 4) +
        ggrepel::geom_text_repel(
          aes(label = label),
          size = 3.5,
          nudge_y = max(df$cond) / 20,
          min.segment.length = unit(0, "lines")) +
        scale_x_date(
          breaks = "months",
          date_labels = "%b\n%Y") +
        scale_y_continuous(
          breaks = scales::breaks_pretty(6),
          expand = expansion()) +
        coord_cartesian(xlim = x_lims, ylim = y_lims) +
        labs(x = NULL, y = "Specific conductance (μS/cm)") +
        common_theme +
        theme(legend.position = "none")

      return(plt)
    }


    # Transparency ----

    if (type == "trans") {

      df <- df %>%
        select(date, trans = transparency, tube = transparency_tube_length) %>%
        drop_na(trans) %>%
        mutate(trans = round(trans, 0)) %>%
        mutate(label = case_when(
          trans == tube ~ paste0(trans, "+ cm"),
          .default = paste0(trans, " cm")))
      n_dates <- n_distinct(df$date)
      x_lims <- setReportDateRange(df$date)
      y_lims <- setAxisLimits(df$trans, 0, 120)
      col_width <- ifelse(n_dates > 8, 10, 15)

      plt <- df %>%
        ggplot(aes(x = date, y = trans)) +
        geom_col(
          aes(y = tube, color = "grey50"),
          position = "identity",
          fill = "grey95",
          width = col_width) +
        geom_col(
          aes(fill = trans),
          position = "identity",
          color = "black",
          width = col_width) +
        ggrepel::geom_text_repel(
          aes(label = label),
          size = 3,
          nudge_y = 1,
          min.segment.length = unit(0, "lines")) +
        scale_x_date(
          breaks = "months",
          date_labels = "%b\n%Y") +
        scale_y_continuous(
          breaks = seq(0, 120, 20),
          expand = expansion()) +
        coord_cartesian(xlim = x_lims, ylim = y_lims) +
        scale_color_identity(guide = guide_legend(label = F)) +
        scale_fill_distiller(
          palette = "BuGn",
          limits = c(0, 120),
          breaks = c(0, 120),
          guide = guide_colorbar(
            label = F,
            frame.colour = "black",
            ticks = F)) +
        labs(x = NULL, y = "Transparency (cm)", color = "Tube\nlength", fill = "Water\nclarity") +
        common_theme +
        theme(legend.position = "right", legend.title = element_text(size = 10))

      return(plt)
    }


    # Streamflow ----

    if (type == "flow") {

      df <- df %>%
        select(date, flow = streamflow) %>%
        drop_na(flow) %>%
        mutate(label = paste(round(flow, 1), "cfs"))
      n_dates <- n_distinct(df$date)
      x_lims <- setReportDateRange(df$date, pad_right = T)
      y_lims <- setAxisLimits(df$flow, 0, 1)
      flow_labels <- tibble(
        x = as.Date(Inf),
        y = c(.03, 3, 150),
        max = c(3, 150, Inf),
        label = c(
          "Headwater stream (0.03-3 cfs)\n\nEphemeral stream (< 0.03 cfs)",
          "Mainstem stream (3-150 cfs)\n\nHeadwater stream (0.03-3 cfs)",
          "Large river (> 150 cfs)\n\nMainstem stream (3-150 cfs)")) %>%
        filter(y < y_lims[2], max > y_lims[2])

      plt <- df %>%
        ggplot(aes(x = date, y = flow)) +
        addRectDate(-Inf, .03, "#915119") +
        addRectDate(.03, 3, "#e3c283") +
        addRectDate(3, 150, "#73cdc1") +
        addRectDate(150, Inf, "#09968e") +
        ggrepel::geom_text_repel(
          data = flow_labels,
          aes(x, y, label = label),
          size = 2.5) +
        { if (n_dates > 1) geom_line(color = "cadetblue", linewidth = 2) } +
        geom_point(
          color = "black",
          fill = "cadetblue",
          shape = 21,
          size = 4) +
        ggrepel::geom_text_repel(
          aes(label = label),
          size = 3.5,
          nudge_y = max(df$flow) / 20,
          min.segment.length = unit(0, "lines")) +
        scale_x_date(
          breaks = "months",
          date_labels = "%b\n%Y") +
        scale_y_continuous(
          breaks = scales::breaks_pretty(6),
          expand = expansion(c(0, .1))) +
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
        # filter(tp > 0) %>% # true zeros shouldn't exist in this data
        mutate(exceeds = tp > phoslimit) %>%
        mutate(label = ifelse(tp == 0, "< LOD", signif(tp, 3)))
      x_lims <- c(dates[1] - 15, eoy_date + 15)
      y_lims <- setAxisLimits(df$tp, 0, .1)
      est <- getPhosEstimate(df$tp)
      est_labels <- tribble(
        ~value, ~label,
        est$lower, "Lower 90% CI",
        est$median, "Median value",
        est$upper, "Upper 90% CI",
        est$limit, "State limit",
      ) %>% mutate(date = eoy_date)
      ci <- est$n > 1 # conf int if more than 1 observation

      plt <- df %>%
        ggplot(aes(x = date, y = tp)) +
        {
          # draw confidence interval
          if (ci) c(
            geom_rect(
              data = tibble(), inherit.aes = F,
              aes(fill = est$lower > phoslimit),
              xmin = -Inf, xmax = Inf,
              ymin = est$lower, ymax = est$upper,
              alpha = .2),
            geom_hline(yintercept = est$lower),
            geom_hline(yintercept = est$upper),
            geom_hline(yintercept = est$median, linetype = "dashed")
          )
        } +
        geom_hline(yintercept = phoslimit, linewidth = 1, color = "red") +
        geom_col(aes(fill = exceeds), color = "black", width = 15) +
        {
          # label confidence interval
          if (ci) ggrepel::geom_text_repel(
            data = est_labels,
            aes(y = value, label = label),
            size = 3,
            nudge_x = 1,
            box.padding = unit(.5, "lines"),
            min.segment.length = unit(0, "lines")
          )
        } +
        ggrepel::geom_text_repel(
          aes(label = label),
          size = 3.5,
          nudge_y = .005) +
        scale_x_date(
          breaks = dates,
          date_labels = "%b %d\n%Y") +
        scale_y_continuous(
          breaks = scales::breaks_pretty(6),
          expand = expansion()) +
        coord_cartesian(xlim = x_lims, ylim = y_lims) +
        scale_fill_manual(
          breaks = c(T, F),
          values = c("#FFA168", "#40b0a6"),
          labels = c("Yes", "No")) +
        labs(x = NULL, y = "Total phosphorus (mg/L)", fill = "Exceeds 0.075 mg/L limit?") +
        common_theme +
        theme(legend.position = "bottom")

      return(plt)
    }


    # Thermistor ----

    if (type == "thermistor") {

      daily_min <- df %>%
        slice_min(order_by = temp_c, by = date) %>%
        select(date_time, min = temp_c)
      daily_max <- df %>%
        slice_max(order_by = temp_c, by = date) %>%
        select(date_time, max = temp_c)
      daily_range <- bind_rows(daily_min, daily_max) %>%
        arrange(date_time) %>%
        mutate(across(c(min, max), ~zoo::na.spline(.x))) %>%
        mutate(mean = (min + max) / 2) %>%
        na.omit()
      temp_labels <- tibble(
        x = as.POSIXct(Inf),
        y = c(20.7, 24.6),
        label = c(
          "Cold-cool transition\n(20.7°C / 69.3°F)",
          "Cool-warm transition\n(24.6°C / 76.3°F)"))

      plt <- daily_range %>%
        ggplot(aes(x = date_time)) +
        addRectDatetime(-Inf, 20.7, "blue") +
        addRectDatetime(20.7, 24.6, "cornflowerblue") +
        addRectDatetime(24.6, Inf, "darkorange") +
        ggrepel::geom_text_repel(
          data = temp_labels,
          aes(x, y, label = label),
          size = 2.5) +
        geom_ribbon(
          aes(ymin = min, ymax = max),
          color = NA, fill = alpha("lightblue", .1)) +
        geom_line(
          data = df,
          aes(y = temp_c),
          color = alpha("#1f77b4", .5),
          linewidth = .25) +
        geom_ribbon(
          aes(ymin = min, ymax = max),
          color = alpha("#2590da", .25), fill = NA) +
        geom_line(
          aes(y = mean),
          color = "orange",
          linewidth = 1) +
        scale_x_datetime(
          breaks = "weeks",
          date_labels = "%b %d") +
        scale_y_continuous(
          breaks = scales::pretty_breaks(),
          labels = ~sprintf("%s°C\n(%s°F)", .x, round(c_to_f(.x), 1))) +
        labs(x = NULL, y = "Water temperature") +
        common_theme +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))

      return(plt)
    }

  })
}
