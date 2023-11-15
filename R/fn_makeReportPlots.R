makeReportPlots <- function(df, type) {
  try({
    date_limits <- get_report_date_range(df$date)

    # Temperature ----
    if (type == "temp") {
      df <- df %>%
        select(
          date,
          Air = ambient_air_temp,
          Water = water_temp,
        ) %>%
        filter(!is.na(Air) | !is.na(Water)) %>%
        pivot_longer(c(Air, Water), names_to = "measure", values_to = "temp_c") %>%
        mutate(temp_f = c_to_f(temp_c)) %>%
        mutate(label = paste0(temp_c, "°C\n(", temp_f, "°F)"))

      plt <- df %>%
        ggplot(aes(x = date, y = temp_c)) +
        geom_line(aes(color = measure), linewidth = 1.5) +
        geom_point(aes(fill = measure), size = 4, shape = 21) +
        ggrepel::geom_text_repel(
          aes(label = label),
          box.padding = unit(.5, "lines"),
          min.segment.length = unit(0, "lines"),
          size = 3.5,
          seed = 1) +
        scale_x_date(
          breaks = "months",
          limits = date_limits,
          oob = scales::oob_keep,
          date_labels = "%b\n%Y") +
        scale_y_continuous(
          limits = c(0, find_max(df$temp_c, 30)),
          expand = expansion()) +
        scale_color_manual(values = c("orange", "lightsteelblue")) +
        scale_fill_manual(values = c("orange", "lightsteelblue")) +
        labs(x = NULL, y = "Temperature (°C)", fill = "Measurement", color = "Measurement") +
        theme_classic() +
        theme(legend.position = "top")

      return(plt)
    }

    # Dissolved oxygen ----
    if (type == "do") {
      df <- df %>%
        select(
          date,
          do = d_o,
          do_sat = d_o_percent_saturation
        ) %>%
        filter(!is.na(do) | !is.na(do_sat)) %>%
        mutate(
          do_color = map_chr(do, do_color),
          label = paste0(do, "mg/L\n(", do_sat, "% sat)"))

      plt <- df %>%
        ggplot(aes(x = date, y = do)) +
        geom_col(aes(fill = do_color), color = "black", width = 15) +
        ggrepel::geom_text_repel(
          aes(label = label),
          vjust = -.25,
          min.segment.length = unit(0, "lines"),
          seed = 1) +
        scale_x_date(
          breaks = "months",
          limits = date_limits,
          oob = scales::oob_keep,
          date_labels = "%b\n%Y") +
        scale_y_continuous(
          limits = c(0, find_max(df$do, 10)),
          expand = expansion()) +
        scale_fill_identity() +
        labs(x = NULL, y = "Dissolved oxygen (mg/L)") +
        theme_classic()

      return(plt)
    }

    if (type == "trans") {
      df <- df %>%
        select(
          date,
          trans = transparency_average,
          tube = transparency_tube_length
        ) %>%
        mutate(label = if_else(
          trans == tube,
          paste0(trans, "+ cm"),
          paste0(trans, " cm")))

      plt <- df %>%
        ggplot(aes(x = date, y = trans)) +
        geom_col(aes(y = tube), color = "grey50", fill = "grey95", width = 15) +
        geom_col(aes(fill = trans), color = "black", width = 15) +
        ggrepel::geom_text_repel(
          aes(label = label),
          nudge_y = 1,
          min.segment.length = unit(0, "lines"),
          seed = 1) +
        scale_x_date(
          breaks = "months",
          limits = date_limits,
          oob = scales::oob_keep,
          date_labels = "%b\n%Y") +
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

    if (type == "flow") {
      df <- df %>%
        select(date, flow = streamflow_cfs) %>%
        mutate(label = paste(flow, " cfs"))

      plt <- df %>%
        ggplot(aes(x = date, y = flow)) +
        geom_line(color = "cadetblue", linewidth = 2) +
        geom_point(color = "black", fill = "cadetblue", shape = 21, size = 4) +
        ggrepel::geom_text_repel(
          aes(label = label),
          nudge_y = .5,
          min.segment.length = unit(0, "lines"),
          seed = 1) +
        scale_x_date(
          breaks = "months",
          limits = date_limits,
          oob = scales::oob_keep,
          date_labels = "%b\n%Y") +
        scale_y_continuous(
          limits = c(0, find_max(df$flow, 10)),
          expand = expansion()) +
        labs(x = NULL, y = "Streamflow (cfs)") +
        theme_classic() +
        theme(legend.position = "none")

      return(plt)
    }
  })
}
