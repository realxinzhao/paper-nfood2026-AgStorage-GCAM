
DIR_MODULE <- "YieldShockTransmission"

BioShockImpact_Response <-
  function(.ImpactData = ImpactData_cross,
           .ImpactYear = seq(2025, 2100, 5),
           .TimeStep = "period-to-period",
           .ImpactSector = c("Grains", "Oil crops", "Sugar crops", "Other crops"),
           .SmoothMethod = "lm"){

    ImpactData <- .ImpactData


    theme0 <- theme(
      panel.grid.minor = element_line(linewidth = 0.2, linetype = 2, colour = "grey75"),
      panel.grid.major = element_line(linewidth = 0.2, linetype = 2, colour = "grey75"),
      panel.border = element_rect(colour = "black", linewidth = 1),
      text = element_text(family= fontfamily, size = 14),
      axis.text.y = element_text(size = 13, color = "black"),
      axis.text.x = element_text(angle = 0, color = "black", size = 13, margin = margin(t = 5, b = 5), vjust= 0.5),
      axis.title.x = element_text(size = 14),
      strip.background = element_rect(fill="grey95"),
      strip.text = element_text(size = 14),
      plot.title = element_text(hjust = 0, face = "bold", margin = margin(t = 5, r = 0, b = 8, l = 0)),
      plot.margin = margin(t = 10, r = 15, b = 10, l = 10),
      panel.spacing = unit(0.8, "lines"),
      legend.key.width = unit(1.5,"line"),
      legend.text = element_text(margin = margin(t=0, b=0), size = 14), legend.margin=margin(t=0, b=0),
    )


    Ele <- c("BioYield_Land",
             "Yield_Land", "Area", "Production",
             "Export",
             "Closing stocks", "Closing stocks_Pcal",
             "Stock loss", "Stock loss_Pcal",
             "Import", "Food", "Feed", "Bioenergy", #"Other use",
             "Consumption", "Price", "Revenue")

    EleLab <- c("Biophysical Yield",
                "Realized Yield", "Area harvested", "Production",
                "Export",
                "Closing stocks (Mt)", "Closing stocks (Pcal)",
                "Stock loss (Mt)", "Stock loss (Pcal)",
                "Import", "Food", "Feed", "Bioenergy", #"Other use",
                "Consumption (Total)", "Price", "Revenue")

    ImpactData %>%
      filter(region != "World") %>%
      filter(year %in% .ImpactYear,
             sector %in% .ImpactSector) %>%
      filter(element %in% Ele) %>%
      mutate(element = factor(element, levels = Ele, labels = EleLab)) %>%
      left_join(
        ImpactData %>% filter(element == "BioYield_Land") %>%
          spread(element, value)
      ) %>% filter(!is.na(value)) %>%
      arrange(factor(scenario_storage,
                     levels = c("Hidden", "Fixed-Ratio", "Competitive" ))) %>%
      ggplot(aes(x = BioYield_Land, y = value)) +
      facet_wrap(~element, scale = "free_y", nrow = 4) +
      geom_hline(yintercept = 0, linetype = 1, color = "grey") +
      geom_vline(xintercept = 0, linetype = 1, color = "grey") +
      geom_abline(slope = 1, linetype = 1, color = "grey") +
      geom_point(aes(color = scenario_storage, size = scenario_storage), alpha = 0.2) +
      scale_size_manual(values = c(1.2, 1.3, 1.5)) +
      geom_smooth(aes(color = scenario_storage, fill = scenario_storage), linewidth = 0.5,
                  alpha = 0.1, method= .SmoothMethod, span = 1,
                  se = TRUE,
                  level = 0.95) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme_bw() + theme0 +
      labs(y = "Relative responses to biophysical shocks",
           x = "Biophysical yield shock",
           color = "Storage Scenario", fill = "Storage Scenario", size = "Storage Scenario") +
      theme(panel.grid = element_blank()) ->
      BioShockImpact_Response_AgEcon


    Ele <- c("Stock variation_Pcal", "Stock variation", "Net export")
    EleLab <- c("Stock variation (Pcal)", "Stock variation (Mt)", "Net export (Mt)")

    ImpactData %>%
      filter(region != "World") %>%
      filter(year %in% .ImpactYear,
             sector %in% .ImpactSector) %>%
      filter(element %in% Ele) %>%
      mutate(element = factor(element, levels = Ele, labels = EleLab)) %>%
      left_join(
        ImpactData %>% filter(element == "BioYield_Land") %>%
          spread(element, value)
      ) %>% filter(!is.na(value)) %>%
      arrange(factor(scenario_storage,
                     levels = c("Hidden", "Fixed-Ratio", "Competitive"))) %>%
      ggplot(aes(x = BioYield_Land, y = value)) +
      facet_wrap(~element, scale = "free_y") +
      geom_hline(yintercept = 0, linetype = 1, color = "grey") +
      geom_vline(xintercept = 0, linetype = 1, color = "grey") +
      geom_point(aes(color = scenario_storage, size = scenario_storage), alpha = 0.2) +
      scale_size_manual(values = c(1.2, 1.3, 1.5)) +
      geom_smooth(aes(color = scenario_storage, fill = scenario_storage), linewidth = 0.5,
                  alpha = 0.1, method= .SmoothMethod, span = 1,
                  se = TRUE,
                  level = 0.95) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme_bw() + theme0 +
      labs(y = "Absolute responses (Pcal or Mt) to biophysical shocks",
           x = "Biophysical yield shock",
           color = "Storage Scenario", fill = "Storage Scenario", size = "Storage Scenario") +
      theme(panel.grid = element_blank()) ->
      BioShockImpact_Box_Year_AgEcon_NetStockTrade


    patchwork::wrap_plots(
      (
        BioShockImpact_Response_AgEcon +
          labs(title = paste0("(A) Transmission of ", .TimeStep, " yield shocks (percent responses)"),
               y = "Percent response (%)",
               x = "Biophysical yield shock")+
          theme(legend.position = "top")
      ),
      (
        BioShockImpact_Box_Year_AgEcon_NetStockTrade +
          labs(title = paste0("(B) Transmission of ", .TimeStep, " yield shocks (absolute responses)"),
               y = "Difference",
               x = "Biophysical yield shock") +
          theme(legend.position = "none")
      ),
      nrow = 2,
      heights = c(4, 1.4)
    ) ->
      MainResults_BioShockImpact_Response

    return(MainResults_BioShockImpact_Response)

    }


# Export ----

## Main pooled ----
BioShockImpact_Response(
  .ImpactData = ImpactData_cross) ->
  MainResults_BioShockImpact_Response

TimeFrame <- "period-to-period"

MainResults_BioShockImpact_Response %>%
  Write_png(paste0("FigSIResults_BioShockImpact_Response_InterPeriod_", Scenario_YieldShocks),
    .svg = F, .DIR_MODULE = DIR_MODULE, w = 16, h = 17, r = 300)




## Cumulative 2050 & 2100  ----
BioShockImpact_Response(
  .ImpactData = ImpactData_cum, .TimeStep = "cumulative",
  .ImpactYear = 2050) ->
  MainResults_BioShockImpact_Response_2050

MainResults_BioShockImpact_Response_2050 %>%
  Write_png(paste0("FigSIResults_BioShockImpact_Response_Cum_2050_", Scenario_YieldShocks),
            .svg = F, .DIR_MODULE = DIR_MODULE, w = 16, h = 17, r = 300)


BioShockImpact_Response(
  .ImpactData = ImpactData_cum, .TimeStep = "cumulative",
  .ImpactYear = 2100) ->
  MainResults_BioShockImpact_Response_2100

MainResults_BioShockImpact_Response_2100 %>%
  Write_png(paste0("FigSIResults_BioShockImpact_Response_Cum_2100_", Scenario_YieldShocks),
            .svg = F, .DIR_MODULE = DIR_MODULE, w = 16, h = 17, r = 300)



# # Regression FE
#
#
# .ImpactYear <- seq(2025, 2100, 5); length(.ImpactYear)
# .ImpactSector <- c("Grains", "Oil crops", "Sugar crops", "Other crops")
# #.SmoothMethod <- "lm" # "loess
#
#
#
# ImpactData <- ImpactData_cross
# ImpactData <- ImpactData_cum
#
# ImpactData %>% distinct(sector)
#
#
# Ele = c(#"BioYield_Land",
#   "Yield_Land", "Area", "Production",
#   #"Net export", "Stock variation",
#   "Export", #"Opening stocks",
#   "Closing stocks", #"Stock loss",
#   "Import", "Consumption",
#   #"Bioenergy", "Consumption_NB",
#   "Price"#, "Revenue"
# )
# EleLab = c(#"Biophysical Yield",
#   "Realized Yield", "Area harvested", "Production",
#   #"Net export", "Stock variation",
#   "Export", #"Opening stocks",
#   "Closing stocks", #"Stock loss",
#   "Import", "Consumption",
#   #"Bioenergy", "Consumption_NB",
#   "Price"#, "Revenue"
# )
