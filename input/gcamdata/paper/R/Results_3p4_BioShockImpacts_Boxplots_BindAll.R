
library(patchwork)


ImpactData <- ImpactData_cross
ImpactData <- ImpactData_cum


pal_sec <- brewer.pal(4, "Set1")


# FigMainResults_BioShockImpact_Box_Year_BioYieldShocks ----

# FigMainResults_BioShockImpact_Box_Year_Dynamic ----

## InterPeriod ----

### InterPeriod across years ----

BioShockImpact_Box_AgEcon(.ImpactData = ImpactData_cross,
                          .Dist_Reg_Year = "Year",
                          .scenario_storage = "Dynamic") ->
  BioShockImpact_Box_Year_Competitive


BioShockImpact_Box_AgEcon_NetStockTrade(.ImpactData = ImpactData_cross,
                                        .Dist_Reg_Year = "Year",
                                        .scenario_storage = "Dynamic") ->
  BioShockImpact_Box_Year_AgEcon_NetStockTrade_Competitive

patchwork::wrap_plots(
  (
    BioShockImpact_Box_Year_Competitive +
    labs(title = "(A) Period-to-period percent impacts (regional x sectoral distribution)",
         y = "Percent impact of biophysical shocks") +
      theme(legend.position = "bottom")
   ),
  (
    BioShockImpact_Box_Year_AgEcon_NetStockTrade_Competitive +
      labs(title = "(B) Period-to-period absolute impacts (regional x sectoral distribution)",
           y = "Absolute impact of \nbiophysical shocks (Mt or Pcal)") +
      theme(legend.position = "none")
    ),
  nrow = 2,
  heights = c(4, 1)
) -> MainResults_BioShockImpact_Box_Year_Competitive

MainResults_BioShockImpact_Box_Year_Competitive %>%
  Write_png(paste0("FigSIResults_BioShockImpact_Box_Year_Dynamic_InterPeriod_", Scenario_YieldShocks),
    .svg = F, .DIR_MODULE = DIR_MODULE, w = 16, h = 16, r = 300)


### InterPeriod across regions ----


BioShockImpact_Box_AgEcon(.ImpactData = ImpactData_cross,
                          .Dist_Reg_Year = "Reg",
                          .scenario_storage = "Dynamic") ->
  BioShockImpact_Box_Reg_Competitive

BioShockImpact_Box_AgEcon_NetStockTrade(.ImpactData = ImpactData_cross,
                                        .Dist_Reg_Year = "Reg",
                                        .scenario_storage = "Dynamic") ->
  BioShockImpact_Box_Reg_AgEcon_NetStockTrade_Competitive


patchwork::wrap_plots(
  (
    BioShockImpact_Box_Reg_Competitive +
      labs(title = "(A) Period-to-period percent impacts (model period x sectoral distribution)",
           y = "Percent impact of biophysical shocks") +
      theme(legend.position = "bottom")+
      theme(axis.text.x = element_text(size = 11, margin = margin(t = 2),
                                       angle = 30, hjust = 0.8, vjust= 0.8))
  ),
  (
    BioShockImpact_Box_Reg_AgEcon_NetStockTrade_Competitive +
      labs(title = "(B) Period-to-period absolute impacts (model period x sectoral distribution)",
           y = "Absolute impact of \nbiophysical shocks (Mt or Pcal)") +
      theme(legend.position = "none")
  ),
  nrow = 2,
  heights = c(4, 1)
) -> MainResults_BioShockImpact_Box_Reg_Competitive

MainResults_BioShockImpact_Box_Reg_Competitive %>%
  Write_png(paste0("FigSIResults_BioShockImpact_Box_Reg_Dynamic_InterPeriod_", Scenario_YieldShocks),
            .svg = F, .DIR_MODULE = DIR_MODULE, w = 16, h = 16, r = 300)

## Cumulative ----

BioShockImpact_Box_AgEcon(.ImpactData = ImpactData_cum,
                          .Dist_Reg_Year = "Year",
                          .scenario_storage = "Dynamic") ->
  BioShockImpact_Box_Year_Competitive


BioShockImpact_Box_AgEcon_NetStockTrade(.ImpactData = ImpactData_cum,
                                        .Dist_Reg_Year = "Year",
                                        .scenario_storage = "Dynamic") ->
  BioShockImpact_Box_Year_AgEcon_NetStockTrade_Competitive


patchwork::wrap_plots(
  (
    BioShockImpact_Box_Year_Competitive +
      labs(title = "(A) Cumulative percent impacts since 2021 (regional x sectoral distribution)",
           y = "Percent impact of biophysical shocks")+
      theme(legend.position = "bottom")
  ),
  (
    BioShockImpact_Box_Year_AgEcon_NetStockTrade_Competitive +
      labs(title = "(B) Cumulative absolute impacts since 2021 (regional x sectoral distribution)",
           y = "Absolute impact of \nbiophysical shocks (Mt or Pcal)") +
      theme(legend.position = "none")
  ),
  nrow = 2,
  heights = c(4, 1)
) -> MainResults_BioShockImpact_Box_Year_Competitive

MainResults_BioShockImpact_Box_Year_Competitive %>%
  Write_png(paste0("FigSIResults_BioShockImpact_Box_Year_Dynamic_Cumulative_", Scenario_YieldShocks),
            .svg = F, .DIR_MODULE = DIR_MODULE, w = 16, h = 16, r = 300)



