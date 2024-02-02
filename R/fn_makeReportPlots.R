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

    # coldwater < 69.3 F
    # cool-cold 69.3 - 72.5 (middle 70.9)
    # cool-warm 72.5 - 76.3 (middle 74.4)
    # warmwater > 76.3
    temp_labels <- tibble(
      y = c(68, 70.9, 74.4, 78),
      label = c(
        "Coldwater stream (< 69.3°F)",
        "Cold-cool stream (69.3 - 72.5°F)",
        "Cool-warm stream (72.5 - 76.3°F)",
        "Warmwater stream (> 76.3°F)")
      )


    # Temperature ----


    if (type == "temp") {

      df <- df %>%
        select(date, Air = air_temp, Water = water_temp,) %>%
        filter(!is.na(Air) | !is.na(Water)) %>%
        pivot_longer(c(Air, Water), names_to = "measure", values_to = "temp") %>%
        mutate(temp = c_to_f(temp)) %>%
        mutate(label = paste0(temp, "°F")) %>%
        mutate(measure = paste(measure, "temperature"))
      if (nrow(df) == 0) return("No data")
      n_dates <- n_distinct(df$date)
      x_lims <- setReportDateRange(df$date, pad_right = T)
      y_lims <- setAxisLimits(df$temp, 50, 80)
      air <- df %>% filter(measure == "Air temperature") %>% drop_na(temp)
      water <- df %>% filter(measure == "Water temperature") %>% drop_na(temp)

      plt <- df %>%
        ggplot(aes(x = date, y = temp)) +
        addRectDate(-Inf, 69.3, "blue") +
        addRectDate(69.3, 72.5, "cornflowerblue") +
        addRectDate(72.5, 76.3, "lightsteelblue") +
        addRectDate(76.3, Inf, "darkorange") +
        geom_text(
          data = temp_labels,
          aes(y = y, label = label),
          x = as.Date(Inf),
          hjust = 1.05,
          size = 2.5,
          alpha = .8) +
        { if (nrow(air) > 1) geom_line(data = air, aes(color = measure), linewidth = 1.5) } +
        { if (nrow(water) > 1) geom_line(data = water, aes(color = measure), linewidth = 1.5) } +
        geom_point(aes(fill = measure), size = 4, shape = 21) +
        ggrepel::geom_text_repel(
          aes(label = label),
          size = 3,
          box.padding = .5,
          min.segment.length = 0) +
        scale_x_date(
          breaks = "months",
          date_labels = "%b\n%Y") +
        scale_y_continuous(
          breaks = scales::breaks_pretty(6),
          labels = ~sprintf("%s°F\n(%s°C)", .x, round(f_to_c(.x), 1)),
          expand = expansion()) +
        coord_cartesian(xlim = x_lims, ylim = y_lims) +
        scale_color_manual(values = c("orange", "lightsteelblue")) +
        scale_fill_manual(values = c("orange", "lightsteelblue")) +
        labs(x = NULL, y = "Temperature", fill = "Measurement", color = "Measurement") +
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
        mutate(across(c(min, max), ~zoo::na.approx(.x, na.rm = F))) %>%
        drop_na()
      daily_means <- df %>%
        summarize(mean = mean(temp_f), .by = date) %>%
        mutate(date_time = as.POSIXct(paste(date, "12:00:00")))

      plt <- daily_range %>%
        ggplot(aes(x = date_time)) +
        addRectDatetime(-Inf, 69.3, "blue") +
        addRectDatetime(69.3, 72.5, "cornflowerblue") +
        addRectDatetime(72.5, 76.3, "lightsteelblue") +
        addRectDatetime(76.3, Inf, "darkorange") +
        geom_text(
          data = temp_labels,
          aes(y = y, label = label),
          x = as.POSIXct(Inf),
          hjust = 1.05,
          size = 2.5,
          alpha = .8) +
        geom_ribbon(
          aes(ymin = min, ymax = max),
          color = NA, fill = alpha("lightblue", .1)) +
        geom_line(
          data = df,
          aes(y = temp_f),
          color = alpha("#1f77b4", .5),
          linewidth = .25) +
        geom_ribbon(
          aes(ymin = min, ymax = max),
          color = alpha("#2590da", .25), fill = NA) +
        geom_line(
          data = daily_means,
          aes(y = mean),
          color = "orange",
          linewidth = 1) +
        scale_x_datetime(
          breaks = date_breaks,
          date_labels = date_format) +
        scale_y_continuous(
          breaks = scales::pretty_breaks(),
          labels = ~sprintf("%s°F\n(%s°C)", .x, round(f_to_c(.x), 1))) +
        labs(x = NULL, y = "Water temperature") +
        common_theme +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))

      return(plt)
    }


    # Dissolved oxygen ----

    if (type == "do") {

      df <- df %>%
        select(date, d_o, do_sat = d_o_percent_saturation) %>%
        drop_na(d_o) %>%
        mutate(do_color = map_chr(d_o, do_color))
      n_dates <- n_distinct(df$date)
      df <- df %>% mutate(
        sat_label = if_else(is.na(do_sat), "", paste0("\n(", do_sat, ifelse(n_dates < 8, "% sat)", "%)"))),
        label = paste0(d_o, ifelse(n_dates < 8, " mg/L", ""), sat_label))
      x_lims <- setReportDateRange(df$date, pad_right = T)
      y_lims <- setAxisLimits(df$d_o, 0, 8)
      col_width <- ifelse(n_dates > 8, 10, 15)
      do_labels <- tibble(
        y = c(1, 3, 5, 6, 7),
        label = c(
          "Aquatic life minimum\n(1 mg/L) ",
          "Limited forage fish\n(>3 mg/L) ",
          "Warmwater fish\n(>5 mg/L) ",
          "Coldwater fish\n(>6 mg/L) ",
          "Coldwater spawning\n(>7 mg/L) "))

      plt <- df %>%
        ggplot(aes(x = date, y = d_o)) +
        addRectDate(-Inf, 1, "red") +
        addRectDate(1, 3, "orange") +
        addRectDate(3, 5, "gold") +
        addRectDate(5, 6, "lightblue") +
        addRectDate(6, 7, "steelblue") +
        addRectDate(7, Inf, "blue") +
        geom_text(
          data = do_labels,
          aes(y = y, label = label),
          x = as.Date(Inf),
          hjust = 1.1,
          size = 2.5,
          lineheight = 1,
          alpha = .8) +
        geom_col(
          aes(fill = do_color),
          position = "identity",
          color = "black",
          width = col_width) +
        ggrepel::geom_text_repel(
          aes(label = label),
          size = 3.5,
          nudge_y = .25,
          min.segment.length = 0) +
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
      if (nrow(df) == 0) return("No data")
      x_lims <- setReportDateRange(df$date, pad_right = T)
      y_lims <- setAxisLimits(df$ph, 6, 9)
      ph_labels <- tibble(
        y = c(6, 7.5, 9),
        label = c(
          "Minimum water quality\nstandard (pH 6.0) ",
          "Optimal for fish\n(pH 7.5) ",
          "Maximum water quality\nstandard (pH 9.0) "))

      plt <- df %>%
        ggplot(aes(x = date, y = ph)) +
        addRectDate(-Inf, 6, "orange") +
        addRectDate(6, 9, "chartreuse") +
        addRectDate(9, Inf, "purple") +
        geom_text(
          data = ph_labels,
          aes(y = y, label = label),
          x = as.Date(Inf),
          hjust = 1.1,
          size = 2.5,
          alpha = .8) +
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
        mutate(label = round(cond, 1))
      if (nrow(df) == 0) return("No data")
      n_dates <- n_distinct(df$date)
      df <- df %>% mutate(
        label = paste0(round(cond, 1), if_else(n_dates < 8, " uS/cm", "")))
      x_lims <- setReportDateRange(df$date, pad_right = T)
      y_lims <- setAxisLimits(df$cond, 400, 700)
      cond_labels <- tibble(
        y = c(800, 1500, 2000),
        label = c(
          "High conductivity\n(> 800 uS/cm)\n\n",
          "Potentially toxic chloride\nlevels (1500-2000 uS/cm)\n\n",
          "Likely toxic chloride\nlevel (> 2000 uS/cm)\n\n")) %>%
        filter(y < y_lims[2])

      plt <- df %>%
        ggplot(aes(x = date, y = cond)) +
        addRectDate(-Inf, 800, "turquoise") +
        addRectDate(800, 1500, "orange") +
        addRectDate(1500, 2000, "tomato") +
        addRectDate(2000, Inf, "red") +
        geom_text(
          data = cond_labels,
          aes(y = y, label = label),
          x = as.Date(Inf),
          hjust = 1.1,
          size = 2.5,
          alpha = .8) +
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
      if (nrow(df) == 0) return("No data")
      n_dates <- n_distinct(df$date)
      df <- df %>% mutate(
        label = paste0(trans, if_else(trans == tube, "+", ""), if_else(n_dates < 8, " cm", "")))
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
          size = 3, nudge_y = 1,
          min.segment.length = 0) +
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
        drop_na(flow)
      if (nrow(df) == 0) return("No data")
      n_dates <- n_distinct(df$date)
      df <- df %>% mutate(
        label = paste0(round(flow, 1), if_else(n_dates < 8, " cfs", ""))
      )
      x_lims <- setReportDateRange(df$date, pad_right = T)
      y_lims <- setAxisLimits(df$flow, 0, 1)
      flow_labels <- tibble(
        y = c(.03, 3, 150),
        max = c(3, 150, Inf),
        label = c(
          "Headwater stream (0.03-3 cfs)\nEphemeral stream (< 0.03 cfs)  ",
          "Mainstem stream (3-150 cfs)\nHeadwater stream (0.03-3 cfs)  ",
          "Large river (> 150 cfs)\nMainstem stream (3-150 cfs)  ")) %>%
        filter(y < y_lims[2], max > y_lims[2])

      plt <- df %>%
        ggplot(aes(x = date, y = flow)) +
        addRectDate(-Inf, .03, "#915119") +
        addRectDate(.03, 3, "#e3c283") +
        addRectDate(3, 150, "#73cdc1") +
        addRectDate(150, Inf, "#09968e") +
        geom_text(
          data = flow_labels,
          aes(y = y, label = label),
          x = as.Date(Inf),
          hjust = 1.05,
          size = 2.5,
          lineheight = 1.1,
          alpha = .8) +
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
          min.segment.length = 0) +
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
        mutate(exceeds = tp > phoslimit) %>%
        mutate(label = ifelse(tp == 0, "< LOD", signif(tp, 3)))
      x_lims <- c(dates[1] - 15, eoy_date + 15)
      y_lims <- setAxisLimits(df$tp, 0, .1)
      est <- getPhosEstimate(df$tp)
      est_labels <- tribble(
        ~value, ~label,
        est$lower, "Lower 80% CI",
        est$median, "Median value",
        est$upper, "Upper 80% CI",
        est$limit, "State criteria",
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
            size = 3, nudge_x = 1,
            box.padding = .5,
            min.segment.length = 0
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
        labs(x = NULL, y = "Total phosphorus (mg/L)", fill = "Exceeds 0.075 mg/L criteria?") +
        common_theme +
        theme(legend.position = "bottom")

      return(plt)
    }




  })
}
