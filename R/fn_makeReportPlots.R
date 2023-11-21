makeReportPlots <- function(df, type) {
  try({

    date_limits <- get_report_date_range(df$date)
    date_scale <- scale_x_date(
      breaks = "months",
      limits = date_limits,
      oob = scales::oob_keep,
      date_labels = "%b\n%Y")

    # Temperature ----
    if (type == "temp") {
      df <- df %>%
        select(date, Air = air_temp, Water = water_temp,) %>%
        filter(!is.na(Air) | !is.na(Water)) %>%
        pivot_longer(c(Air, Water), names_to = "measure", values_to = "temp_c") %>%
        mutate(label = paste0(temp_c, "°C\n(", c_to_f(temp_c), "°F)"))

      plt <- df %>%
        ggplot(aes(x = date, y = temp_c)) +
        geom_line(aes(color = measure), linewidth = 1.5) +
        geom_point(aes(fill = measure), size = 4, shape = 21) +
        ggrepel::geom_text_repel(
          aes(label = label),
          size = 3.5,
          box.padding = unit(.5, "lines"),
          min.segment.length = unit(0, "lines"),
          seed = 1) +
        date_scale +
        scale_y_continuous(
          limits = c(0, find_max(df$temp_c, 30)),
          breaks = scales::pretty_breaks(),
          labels = ~sprintf("%s°C\n(%s°F)", .x, c_to_f(.x)),
          expand = expansion()) +
        scale_color_manual(values = c("orange", "lightsteelblue")) +
        scale_fill_manual(values = c("orange", "lightsteelblue")) +
        labs(x = NULL, y = "Temperature", fill = "Measurement", color = "Measurement") +
        theme_classic() +
        theme(legend.position = "top")

      return(plt)
    }

    # Dissolved oxygen ----
    if (type == "do") {
      df <- df %>%
        select(date, d_o, do_sat = d_o_percent_saturation) %>%
        drop_na(d_o) %>%
        mutate(
          do_color = map_chr(d_o, do_color),
          sat_label = if_else(is.na(do_sat), "", paste0("\n(", do_sat, "% sat)")),
          label = paste0(d_o, " mg/L", sat_label))

      plt <- df %>%
        ggplot(aes(x = date, y = d_o)) +
        geom_col(aes(fill = do_color), color = "black", width = 15) +
        ggrepel::geom_text_repel(
          aes(label = label),
          size = 3.5,
          nudge_y = .25,
          min.segment.length = unit(0, "lines"),
          seed = 1) +
        date_scale +
        scale_y_continuous(
          limits = c(0, find_max(df$d_o, 10)),
          breaks = scales::pretty_breaks(),
          expand = expansion()) +
        scale_fill_identity() +
        labs(x = NULL, y = "Dissolved oxygen (mg/L)") +
        theme_classic()

      return(plt)
    }

    # Transparency ----
    if (type == "trans") {
      df <- df %>%
        select(date, trans = transparency, tube = transparency_tube_length) %>%
        drop_na(trans) %>%
        mutate(label = if_else(trans == tube, paste0(trans, "+ cm"), paste0(trans, " cm")))

      plt <- df %>%
        ggplot(aes(x = date, y = trans)) +
        geom_col(aes(y = tube), color = "grey50", fill = "grey95", width = 15) +
        geom_col(aes(fill = trans), color = "black", width = 15) +
        ggrepel::geom_text_repel(
          aes(label = label),
          size = 3.5,
          nudge_y = 1,
          min.segment.length = unit(0, "lines"),
          seed = 1) +
        date_scale +
        scale_y_continuous(
          breaks = seq(0, 120, 20),
          limits = c(0, find_max(df$trans, 120)),
          expand = expansion()) +
        scale_fill_distiller(palette = "BuGn", limits = c(0, 120)) +
        labs(x = NULL, y = "Transparency (cm)") +
        theme_classic() +
        theme(legend.position = "none")

      return(plt)
    }

    # Streamflow ----
    if (type == "flow") {
      df <- df %>%
        select(date, flow = streamflow) %>%
        drop_na(flow) %>%
        mutate(label = paste(flow, " cfs"))

      plt <- df %>%
        ggplot(aes(x = date, y = flow)) +
        geom_line(color = "cadetblue", linewidth = 2) +
        geom_point(color = "black", fill = "cadetblue", shape = 21, size = 4) +
        ggrepel::geom_text_repel(
          aes(label = label),
          size = 3.5,
          nudge_y = max(df$flow) / 20,
          min.segment.length = unit(0, "lines"),
          seed = 1) +
        date_scale +
        scale_y_continuous(
          limits = c(0, find_max(df$flow, 5)),
          breaks = scales::pretty_breaks(),
          expand = expansion()) +
        labs(x = NULL, y = "Streamflow (cfs)") +
        theme_classic() +
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
        filter(tp > 0) %>% # true zeros shouldn't exist in this data
        mutate(exceeds = tp > phoslimit) %>%
        mutate(label = paste(signif(tp, 3), "mg/L"))
      est <- getPhosEstimate(df$tp)
      est_labels <- tribble(
        ~value, ~label,
        est$lower, "Lower 90% CI",
        est$median, "Median value",
        est$upper, "Upper 90% CI",
        est$limit, "State limit",
      ) %>% mutate(date = eoy_date)

      ci <- est$n > 1
      plt <- df %>%
        ggplot(aes(x = date, y = tp)) +
        {
          if (ci) c(
            geom_rect(
              data = tibble(), inherit.aes = F,
              aes(fill = est$median > phoslimit),
              xmin = -Inf, xmax = Inf,
              ymin = est$lower, ymax = est$upper,
              alpha = .2),
            geom_hline(yintercept = est$lower),
            geom_hline(yintercept = est$upper),
            geom_hline(yintercept = est$median, linetype = "dashed")
          )
        } +
        geom_hline(yintercept = phoslimit, linewidth = 1, color = "red") +
        geom_col(aes(fill = exceeds), color = "black") +
        {
          if (ci) ggrepel::geom_text_repel(
            data = est_labels,
            aes(y = value, label = label),
            size = 3,
            nudge_x = 1,
            box.padding = unit(.5, "lines"),
            min.segment.length = unit(0, "lines"),
            seed = 1
          )
        } +
        ggrepel::geom_text_repel(
          aes(label = label),
          size = 3,
          nudge_y = .001,
          seed = 1) +
        scale_x_date(
          breaks = dates,
          limits = c(dates[1] - 15, eoy_date + 15),
          date_labels = "%b\n%Y") +
        scale_y_continuous(
          limits = c(0, find_max(df$tp, .2)),
          breaks = scales::pretty_breaks(),
          expand = expansion()) +
        scale_fill_manual(
          breaks = c(T, F),
          values = c("#ff8e68", "#52c2a6"),
          labels = c("Yes", "No")) +
        labs(x = NULL, y = "Total phosphorus (mg/L)", fill = "Exceeds 0.075 mg/L limit?") +
        theme_classic() +
        theme(legend.position = "bottom")

      return(plt)
    }

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

      add_rect <- function(ymin, ymax, color) {
        annotate("rect",
          xmin = as.POSIXct(-Inf), xmax = as.POSIXct(Inf),
          ymin = ymin, ymax = ymax, fill = color,
          alpha = .08
        )
      }

      transition_labels <- tibble(
        x = as.POSIXct(Inf),
        y = c(22.2, 25),
        label.size = 2,
        label = c(
          "Cold-cool transition\n(22.2°C / 72°F)",
          "Cool-warm transition\n(25°C / 77°F)"
        )
      )

      plt <- daily_range %>%
        ggplot(aes(x = date_time)) +
        add_rect(-Inf, 22.2, "blue") +
        add_rect(22.2, 25, "cornflowerblue") +
        add_rect(25, Inf, "darkorange") +
        geom_hline(yintercept = 22.2, color = alpha("blue", .25)) +
        geom_hline(yintercept = 25, color = alpha("cornflowerblue", .25)) +
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
        ggrepel::geom_text_repel(
          data = transition_labels,
          aes(x, y, label = label),
          size = 2.5,
          seed = 1) +
        scale_x_datetime(
          breaks = "weeks",
          date_labels = "%b %d") +
        scale_y_continuous(
          breaks = scales::pretty_breaks(),
          labels = ~sprintf("%s°C\n(%s°F)", .x, c_to_f(.x))) +
        labs(x = NULL, y = "Water temperature") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))

      return(plt)
    }
  })
}
