



# absolute changes ----
BioShockImpact_Box_AgEcon_NetStockTrade <-
  function(.ImpactData = ImpactData_cross,
           .scenario_storage = "Dynamic",
           .Dist_Reg_Year = "Year"){

    ImpactData <- .ImpactData

    Ele <- c("Stock variation_Pcal", "Stock variation", "Net export")
    EleLab <- c("Stock variation (Pcal)", "Stock variation (Mt)", "Net export (Mt)")

    theme0 <- theme(
      panel.grid.minor = element_line(linewidth = 0.2, linetype = 2, colour = "grey75"),
      panel.grid.major = element_line(linewidth = 0.2, linetype = 2, colour = "grey75"),
      panel.border = element_rect(colour = "black", size=1),
      text = element_text(family= fontfamily, size = 14),
      axis.text.y = element_text(size = 13, color = "black"),
      axis.text.x = element_text(angle = 0, color = "black", size = 13, margin = margin(t = 5), vjust= 0.5),
      axis.title.x = element_text(size = 14),
      strip.background = element_rect(fill="grey95"),
      strip.text = element_text(size = 14),
      plot.title = element_text(hjust = 0, face = "bold", margin = margin(t = 5, r = 0, b = 8, l = 0)),
      plot.margin = margin(t = 10, r = 15, b = 10, l = 10),
      panel.spacing = unit(0.8, "lines"),
      legend.key.width = unit(2,"line"),
      legend.text = element_text(margin = margin(t=0, b=0), size = 14), legend.margin=margin(t=0, b=0)
    )

    if (.Dist_Reg_Year == "Year") {

      df1 <- ImpactData %>%
        filter(region != "World",
               scenario_storage == .scenario_storage,
               element %in% Ele) %>%
        mutate(
          element = factor(element, levels = Ele, labels = EleLab),
          year_f  = factor(year),
          sector  = factor(sector)
        )

      df_line_world_grain <- ImpactData %>%
        filter(region == "World",
               scenario_storage == .scenario_storage,
               element %in% Ele) %>%
        mutate(
          element = factor(element, levels = Ele, labels = EleLab),
          year_f  = factor(year),
          sector  = factor(sector)
        )

      # Box stats by (element, year)
      box_stats <- df1 %>%
        group_by(element, year_f) %>%
        summarise(
          p05 = quantile(value, 0.05, na.rm = TRUE),
          q25 = quantile(value, 0.25, na.rm = TRUE),
          p50 = quantile(value, 0.50, na.rm = TRUE),
          q75 = quantile(value, 0.75, na.rm = TRUE),
          p95 = quantile(value, 0.95, na.rm = TRUE),
          .groups = "drop"
        )

      df_pts <- df1 %>%
        left_join(box_stats, by = c("element", "year_f")) %>%
        filter(value >= p05, value <= p95)

      # optional: a nice palette for sectors (Set2 tends to work well)
      nsec <- nlevels(df1$sector)
      pal_sec <- brewer.pal(min(max(nsec, 3), 8), "Set1")  # up to 8
      # If you have >8 sectors, I can switch this to colorspace/hue palette.

      ggplot() +
        geom_hline(yintercept = 0, linewidth = 0.6, linetype = 2, color = "blue") +
        geom_boxplot(
          data = box_stats,
          aes(
            x = year_f,
            ymin = p05, lower = q25, middle = p50, upper = q75, ymax = p95,
            group = year_f
          ),
          stat = "identity",
          fill = "grey85",
          color = "black",
          linewidth = 0.3,
          width = 0.65
        ) +
        geom_point(
          data = df_pts,
          aes(x = year_f, y = value, fill = sector),
          position = position_jitter(width = 0.15, height = 0),
          shape = 21,
          color = "black",
          alpha = 0.5,
          size = 1.2,
          stroke = 0.3
        ) +
        geom_line(
          data = df_line_world_grain,
          aes(x = year_f, y = value, group = interaction(element, sector), color = sector),
          linetype = 1, linewidth = 0.5, alpha = 0.7
        ) +
        facet_wrap(~element, scales = "free_y", ncol = 4)+
        #scale_y_continuous(labels = percent_format(accuracy = 1)) +
        scale_x_discrete(breaks = c("2025", "2050", "2075", "2100")) +
        scale_color_manual(values = pal_sec) +
        theme_bw() + theme0 +
        labs(
          x = "Year",
          y = "Impact of Biophysical Shocks\nAbsolute Differences (Pcal or Mt)",
          title = "Absolute impacts",
          color = "Sector", fill = "Sector"
        ) +
        theme(legend.position = "top") ->
        BioShockImpact_Box_Year_AgEcon_NetStockTrade

      return(BioShockImpact_Box_Year_AgEcon_NetStockTrade)
    }

    if (.Dist_Reg_Year == "Reg") {

      df1 <- ImpactData %>%
        filter(#region != "World",
          scenario_storage == .scenario_storage,
          element %in% Ele) %>%
        mutate(
          element = factor(element, levels = Ele, labels = EleLab),
          year_f  = factor(year),
          sector  = factor(sector)
        )

      # Box stats by (element, year)
      box_stats <- df1 %>%
        group_by(element, region) %>%
        summarise(
          p05 = quantile(value, 0.05, na.rm = TRUE),
          q25 = quantile(value, 0.25, na.rm = TRUE),
          p50 = quantile(value, 0.50, na.rm = TRUE),
          q75 = quantile(value, 0.75, na.rm = TRUE),
          p95 = quantile(value, 0.95, na.rm = TRUE),
          .groups = "drop"
        )

      df_pts <- df1 %>%
        left_join(box_stats, by = c("element", "region")) %>%
        filter(value >= p05, value <= p95)

      ggplot() +
        geom_hline(yintercept = 0, linewidth = 0.6, linetype = 2, color = "blue") +
        geom_boxplot(
          data = box_stats,
          aes(
            x = region,
            ymin = p05, lower = q25, middle = p50, upper = q75, ymax = p95,
            group = region
          ),
          stat = "identity",
          fill = "grey85",
          color = "black",
          linewidth = 0.3,
          width = 0.65
        ) +
        geom_point(
          data = df_pts,
          aes(x = region, y = value, fill = sector),
          position = position_jitter(width = 0.15, height = 0),
          shape = 21,
          color = "black",
          alpha = 0.5,
          size = 1.2,
          stroke = 0.3
        ) +
        # geom_line(
        #   data = df_line_world_grain,
        #   aes(x = year_f, y = value, group = interaction(element, sector), color = sector),
        #   linetype = 1, linewidth = 0.5, alpha = 0.7
        # ) +
        facet_wrap(~element, scales = "free_y", ncol = 4) +
        scale_color_manual(values = pal_sec) +
        theme_bw() + theme0 +
        labs(
          x = "Region",
          y = "Impact of Biophysical Shocks\nAbsolute Differences (Pcal or Mt)",
          title = "Absolute impacts",
          color = "Sector", fill = "Sector"
        ) +
        theme(axis.text.x = element_text(size = 13, margin = margin(t = 2),
                                         angle = 30, hjust = 0.8, vjust= 0.8)) +
        theme(legend.position = "top") ->
        BioShockImpact_Box_Reg_AgEcon_NetStockTrade;

      return(BioShockImpact_Box_Reg_AgEcon_NetStockTrade)

    }

  }



