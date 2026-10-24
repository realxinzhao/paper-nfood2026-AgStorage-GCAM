# NATURE FOOD FIGURES 4: long term MSD impacts of dynamic storage representation ----


DIR_OUTPUT <- "paper/output"
DIR_MODULE = "LongTermImpacts"
Project <- ""


AgElement_SUA_Storage %>%  distinct(year)
AgElement_SUA_Storage %>% distinct(element)
AgElement_SUA_Storage %>% distinct(scenario)

# Define scenarios ----
# S1 vs. S2
Scenario_Storage <- "S1_Dynamic"
Scenario_NoStorage <- "S2_Hidden"

# Scenario_Storage <- "S1_Dynamic_StaticInterPeriod"
# Scenario_NoStorage <- "S2_Hidden"


# Other sensitivity comparisons
# Scenario_Storage <- "S3_Dynamic_HG"
# Scenario_NoStorage <- "S4_Hidden_HG"
#
# Scenario_Storage <- "S5_FixedRatio"
# Scenario_NoStorage <- "S2_Hidden"
#


theme0 <- theme(
  panel.grid.minor = element_line(linewidth = 0.2, linetype = 2, colour = "grey75"),
  panel.grid.major = element_line(linewidth = 0.2, linetype = 2, colour = "grey75"),
  panel.border = element_rect(colour = "black", linewidth=1),
  text = element_text(family= fontfamily, size = 14),
  axis.text.y = element_text(size = 13, color = "black"),
  axis.text.x = element_text(angle = 0, color = "black", size = 13, margin = margin(t = 5), vjust= 0.5),
  axis.title.x = element_text(size = 14, margin = margin(t = 2, r = 0, b = 3, l = 0)),
  strip.background = element_rect(fill="grey95"),
  strip.text = element_text(size = 14),
  plot.title = element_text(hjust = 0, face = "bold", margin = margin(t = 5, r = 0, b = 8, l = 0))#,
  #plot.margin = margin(t = 10, r = 15, b = 10, l = 10) #panel.spacing = unit(1, "lines"),
)





# Stocks ----


## Main Results  4A  World Agricultural stock by sector ----
AgElement_SUA_Storage %>%
  filter(element == "Closing stocks_Pcal") %>% filter(scenario == Scenario_Storage) %>%
  filter(region == "World") %>% mutate(value = value / 1000) %>%
  Agg_reg(element, region, sector) %>%
  ggplot() +
  geom_bar(aes(x = year, y = value, fill = sector), linewidth = 0.3,
           stat = "identity", position = "stack", color = "black") +
  geom_bar(data = AgElement_SUA_Storage %>%
             filter(element == "Stock loss_Pcal") %>%
             filter(region == "World") |>
             Agg_reg(element) %>%
             filter(scenario == Scenario_Storage) %>%
             mutate(value = -value / 1000, ele = "Total loss"),
           aes(x = year, y = value, alpha = ele), fill = "grey", stat = "identity",
           linewidth = 0.3, color = "black", linetype = 2) +
  scale_alpha_manual(values = 1) +
  geom_hline(yintercept = 0, color = "grey20", linetype = 1, linewidth = 0.5) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  labs(x= "Year", y = "1000 petacalories (Pcal)",
       fill = "Stock sector", alpha = "Stock loss\n(negative values)",
       title = "(A) World stock by sector"
  ) +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(size = 13, margin = margin(t = 2),
                                   angle = 30, hjust = 0.8, vjust= 0.8),
        axis.title.y = element_text(margin = margin(r = 8, l = 10))) +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1),
         alpha = guide_legend(order = 3)) ->
  p1_World_Sec ;p1_World_Sec

## Main Results  4B Agricultural stock by sector and region ----
AgElement_SUA_Storage %>%
  filter(element == "Closing stocks_Pcal") %>% filter(scenario == Scenario_Storage) %>%
  filter(region != "World") %>% mutate(value = value / 1000) %>%
  Agg_reg(element, region, sector) %>%
  #filter(year %in% c(2021, 2050, 2075, 2100)) %>%
  ggplot() + facet_wrap(~sector, scale = "free_y", nrow = 1) +
  geom_hline(yintercept = 0.8, color = NA) +
  geom_hline(yintercept = -0.05, color = NA ) +
  geom_area(aes(x = year, y = value, fill = region), linewidth = 0.3,
            stat = "identity", position = "stack", color = "black") +
  geom_area(data = AgElement_SUA_Storage %>%
              filter(element == "Stock loss_Pcal") %>%
              filter(region == "World") |>
              Agg_reg(element, sector) %>%
              filter(scenario == Scenario_Storage) %>%
              mutate(value = -value / 1000, ele = "Total loss"),
            aes(x = year, y = value#, alpha = ele
            ), fill = "grey", stat = "identity",
            linewidth = 0.3, color = "black", linetype = 2) +
  scale_alpha_manual(values = 1) +
  geom_hline(yintercept = 0, color = "grey20", linetype = 1, linewidth = 0.5) +
  scale_fill_brewer(palette = "Paired") +
  labs(x= "Year", y = "1000 Petacalories (Pcal)",
       fill = "Region (R10)", alpha = "Stock loss\n(negative values)",
       title = "(B) Agricultural stock by sector and region"
  ) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(size = 13, margin = margin(t = 2),
                                   angle = 30, hjust = 0.8, vjust= 0.8),
        axis.title.y = element_text(margin = margin(r = 8, l = 10))) +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1),
         alpha = guide_legend(order = 3)) ->
  p2;p2

###  Adding SI ----

AgElement_SUA_Storage %>%
  filter(element == "Closing stocks_Pcal") %>%
  filter(scenario == Scenario_Storage) %>%
  #filter(region != "World") %>%
  mutate(value = value ) %>%
  Agg_reg(element, region, sector) %>%
  left_join_error_no_match(

    L101.Pop_thous_Scen_R_Y |>
      filter(year >= 2021, scenario == "SSP2") |>
      select(-scenario) |>
      left_join_error_no_match(GCAM_region_names) |>
      select(-GCAM_region_ID) |>
      Reg_Clean() |> rename(pop = value) |>
      ungroup()
  ) |>
  mutate(value = value * 10^12  / (pop*1000) / 365  ) |>
  filter(year %in% c(2025, 2050, 2075, 2100)) %>%
  ggplot() + facet_wrap(~region, scale = "free_y", nrow = 2) +
  geom_hline(yintercept = 0.8, color = NA) +
  geom_bar(aes(x = year, y = value, fill = sector), linewidth = 0.3,
           stat = "identity", position = "stack", color = "black") +
  #scale_y_continuous(expand = c(0, 0)) +
  scale_alpha_manual(values = 1) +
  geom_hline(yintercept = 0, color = "grey20", linetype = 1, linewidth = 0.5) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x= "Year", y = "Kilocalories per capita per day",
       fill = "Stock sector", alpha = "Stock loss\n(negative values)"
  ) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(size = 13, margin = margin(t = 2),
                                   angle = 30, hjust = 0.8, vjust= 0.8),
        axis.title.y = element_text(margin = margin(r = 8, l = 10))) +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1),
         alpha = guide_legend(order = 3)) ->
  p0;p0

lemon::reposition_legend(p0, 'center', panel = 'panel-6-2') -> p

# p %>% Write_png(.name = "Fig_4_SI_pcStock", .svg = F,
#                 .DIR_MODULE = DIR_MODULE, w = 13, h = 7, r = 300)



## Main Results  4C global stock variation by region (thousand Pcal) ----
AgElement_SUA_Storage %>%
  spread(element, value) %>%
  mutate(StockVar = `Closing stocks` - `Opening stocks`,
         StockVar_Pcal = `Closing stocks_Pcal` - `Opening stocks_Pcal`) %>%
  gather(element, value, -branch:-sector) %>%
  filter(scenario == Scenario_Storage) %>%
  filter(element %in% c("StockVar_Pcal", "Stock loss_Pcal") ) %>%
  mutate(element = factor(element, levels = c("StockVar_Pcal", "Stock loss_Pcal"),
                          labels = c("Stock variation (closing - opening)", "Stock loss") )) %>%
  filter(region == "World") %>% mutate(value = value / 1000) %>%
  Agg_reg(element, region, sector)->
  df

df |>
  filter(element %in% c("Stock variation (closing - opening)") ) %>%
  Agg_reg(element, region) |>
  arrange(-value)


df %>%
  ggplot() + facet_wrap(~element) +
  geom_bar(aes(x = year, y = value, fill = sector), linewidth = 0.3,
           stat = "identity", position = "stack", color = "black") +
  geom_point(data = df %>% Agg_reg(element) %>% mutate(ss = "Net total (dynamic)"),
             aes(x = year, y = value, size = ss), fill = "blue", alpha = 0.5,
             stat = "identity",
             color = "black", shape = 21) +
  scale_size_manual(values = c(2.5), name = NULL) +
  geom_hline(data = df %>% filter(year == 2021) %>% Agg_reg(element) %>%
               mutate(sss = "Net total (static)"),
             aes(yintercept = value, alpha = sss), color = "blue", linetype= 3, linewidth = 0.8) +
  scale_alpha_manual(values = 1) +
  geom_hline(yintercept = 0, color = "grey20", linetype = 1, linewidth = 0.5) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  labs(x= "Year", y = "1000 Petacalories (Pcal)", linewidth = NULL,
       fill = "Stock sector", alpha = "",
       title = "(C) World stock variation and loss by sector"
  ) +
  theme_bw() + theme0 + #theme_leg +
  theme(axis.text.x = element_text(size = 13, margin = margin(t = 2),
                                   angle = 30, hjust = 0.8, vjust= 0.8),
        axis.title.y = element_text(margin = margin(r = 8, l = 10))) +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1),
         size = guide_legend(order = 3, title = NULL,
                             title.position = "top",),
         alpha = guide_legend(order = 4)) ->
  p3_World_Sec_StockVar ;p3_World_Sec_StockVar

## Main Results  4D Grain stock variation by region (Mt) ----
AgElement_SUA_Storage %>% #distinct(element)
  spread(element, value) %>%
  mutate(StockVar = `Closing stocks` - `Opening stocks`,
         StockVar_Pcal = `Closing stocks_Pcal` - `Opening stocks_Pcal`) %>%
  gather(element, value, -branch:-sector) %>%
  filter(scenario == Scenario_Storage) %>%
  filter(element %in% c("StockVar", "Stock loss"),
         region != "World", sector == "Grains") %>%
  mutate(element = factor(element, levels = c("StockVar", "Stock loss"),
                          labels = c("Stock variation (closing - opening)", "Stock loss") )) %>%
  mutate(value = value) %>%
  Agg_reg(element, region, sector) ->
  df

AgElement_SUA_Storage %>%
  filter(scenario == Scenario_Storage) %>%
  filter(region == "World", year %in% c(2021, 2050, 2100)) |>
  filter(sector == "Grains") |>
  Agg_reg(element)

AgElement_SUA_Storage %>%
  filter(scenario == Scenario_Storage) %>%
  filter(region == "World", year %in% c(2021, 2100)) |>
  #filter(sector == "Grains") |>
  filter(element == "Closing stocks") |>
  Agg_reg(element, sector) |>
  spread(year, value) |>
  mutate(value = (`2100` - `2021`)/79) |>
  bind_rows(
    AgElement_SUA_Storage %>%
      filter(scenario == Scenario_Storage) %>%
      spread(element, value) %>%
      mutate(StockVar = `Closing stocks` - `Opening stocks`,
             StockVar_Pcal = `Closing stocks_Pcal` - `Opening stocks_Pcal`) %>%
      gather(element, value, -branch:-sector) %>%
      filter(region == "World", element %in% c("Stock loss")) |>
      Agg_reg(element, sector) |>
      group_by(element, sector) |>
      summarize(value = mean(value))
  ) |>
  group_by(sector) |>
  summarize(value = sum(value))





df %>% ggplot() +
  facet_wrap(~element, scale = "fixed") +
  geom_bar(aes(x = year, y = value, fill = region), linewidth = 0.3,
           stat = "identity", position = "stack", color = "black") +
  geom_point(data = df %>% Agg_reg(element),
             aes(x = year, y = value), fill = "blue", alpha = 0.5,
             stat = "identity",
             size = 2.5, color = "black", shape = 21) +
  geom_hline(data = df %>% filter(year == 2021) %>% Agg_reg(element),
             aes(yintercept = value), color = "blue", linetype= 3, linewidth = 0.8) +
  geom_hline(yintercept = 0, color = "grey20", linetype = 1, linewidth = 0.5) +
  scale_fill_brewer(palette = "Paired") +
  labs(x= "Year", y = "Million tonnes (Mt)",
       fill = "Region (R10)",
       title = "(D) Grains stock variation and loss by region (Mt)"
  ) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 + #theme_leg +
  theme(axis.text.x = element_text(size = 13, margin = margin(t = 2),
                                   angle = 30, hjust = 0.8, vjust= 0.8),
        axis.title.y = element_text(margin = margin(r = 8, l = 10))) +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1),
         alpha = guide_legend(order = 3)) ->
  p4;p4

### patch plots A-D----



(p1_World_Sec + p2 + plot_layout(widths = c(0.31, 0.69))) /
  (p3_World_Sec_StockVar + (p4 + theme(legend.position = "none")) + plot_layout(widths = c(0.5, 0.5))) +
  plot_layout(guides = "collect") ->
  MainResults_Stock_Patch_CollectLeg

legend_MainResults_Stock_Patch <-
  cowplot::get_legend(MainResults_Stock_Patch_CollectLeg +
                        theme(legend.position = "right"))


((p1_World_Sec + theme(legend.position = "none")) +
    (p2 + theme(legend.position = "none")) +
    plot_layout(widths = c(0.31, 0.69))  ) /
  ((p3_World_Sec_StockVar + theme(legend.position = "none")) +
     (p4 + theme(legend.position = "none")) +
     plot_layout(widths = c(0.5, 0.5)))  ->
  MainResults_Stock_Patch_NoLeg


MainResults_Stock_Patch <-
  patchwork::wrap_plots(
    MainResults_Stock_Patch_NoLeg,
    patchwork::wrap_elements(legend_MainResults_Stock_Patch),
    ncol = 2,
    widths = c(0.9, 0.13)
  )

# MainResults_Stock_Patch %>%
#   Write_png(.name = "FigMainResults_Stock", .svg = F,
#             .DIR_MODULE = DIR_MODULE, w = 16, h = 9, r = 300)


## Main Results  4E  World Agricultural production by sector ----
AgElement_SUA_ProdValue %>%
  filter(sector %in% c("Grains", "Oil crops", "Sugar crops", "Other crops")) %>%
  #group_by_at(vars(-value, -year)) %>%
  #Fill_annual(CUMULATIVE = T, CUM_YEAR_START = 2021) %>%
  filter(element == "Production", region == "World", year >= 2020) %>%
  filter(scenario %in% c(Scenario_NoStorage, Scenario_Storage)#, year %in% c(2025, 2050, 2075, 2100)
  ) %>%
  group_by_at(vars(-scenario, -value)) %>%
  mutate(value = value -value[scenario == Scenario_NoStorage]) %>%
  filter(scenario != Scenario_NoStorage) %>% ungroup() %>%
  filter(year %in% seq(2025, 2100, 25)) %>%
  ggplot() +
  geom_bar(aes(x = year, y = value, fill = sector), linewidth = 0.5,
           stat = "identity", position = "stack", color = "black") +
  geom_hline(yintercept = 0, color = "grey20", linetype = 1, linewidth = 0.5) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  labs(x= "Year", y = "Million tonnes",
       fill = "Sector", title = "(E) Production impact"
  ) +
  theme_bw() + theme0 + #theme_leg +
  theme(axis.text.x = element_text(size = 13, margin = margin(t = 2),
                                   angle = 30, hjust = 0.8, vjust= 0.8),
        axis.title.y = element_text(margin = margin(r = 8, l = 10))) +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1),
         alpha = guide_legend(order = 3)) ->
  p5_World_ProdSec ;p5_World_ProdSec


AgElement_SUA_ProdValue %>%
  filter(sector %in% c("Grains", "Oil crops", "Sugar crops", "Other crops")) %>%
  filter(element == "Production") %>%
  filter(scenario %in% c(Scenario_NoStorage, Scenario_Storage)) %>%
  #filter(region == "World") %>%
  ggplot() + facet_wrap(~region, scale = "free_y") +
  geom_line(aes(x = year, y = value, linetype = scenario, color = sector))


## Main Results  4F  World irrigation water withdrawal by sector ----

Pwater %>%
  filter(scenario %in% c(Scenario_NoStorage, Scenario_Storage)) %>%
  filter(sector %in% c("Grains", "Oil crops", "Sugar crops", "Other crops")) %>%
  group_by_at(vars(-scenario, -value)) %>%
  mutate(value = value -value[scenario == Scenario_NoStorage]) %>%
  filter(scenario != Scenario_NoStorage) %>% ungroup() %>%
  filter(region == "World", year >= 2020) %>%
  filter(year %in% seq(2025, 2100, 25)) %>%
  ggplot +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = sector),
           stat = "identity", position = "stack", linewidth = 0.5,
           color = "black") +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  labs(x = "Year", y = "Billion cubic meter", fill = "Sector",
       title = "(F) Irrigation water impact") +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  theme_bw() + theme0 + #theme_leg +
  theme(axis.text.x = element_text(size = 13, margin = margin(t = 2),
                                   angle = 30, hjust = 0.8, vjust= 0.8),
        axis.title.y = element_text(margin = margin(r = 8, l = 10))) +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1),
         alpha = guide_legend(order = 3)) ->
  p6_World_IrrWater; p6_World_IrrWater


## Main Results  4G  World land use by sector ----

PLand %>%
  filter(scenario %in% c(Scenario_NoStorage, Scenario_Storage)#, year %in% c(2025, 2050, 2075, 2100)
  ) %>%
  filter(region == "World", year >= 2020) %>%
  group_by_at(vars(-scenario, -value)) %>%
  mutate(value = value -value[scenario == Scenario_NoStorage]) %>%
  filter(scenario != Scenario_NoStorage) %>% ungroup() %>%
  filter(year %in% seq(2025, 2100, 25)) %>%
  ggplot +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = land),
           stat = "identity", position = "stack", linewidth = 0.5,
           color = "black") +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  labs(x = "Year", y = "Million hectare", fill = "Land",
       title = "(G) Land use impact") +
  scale_fill_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme0 + #theme_leg +
  theme(axis.text.x = element_text(size = 13, margin = margin(t = 2),
                                   angle = 30, hjust = 0.8, vjust= 0.8),
        axis.title.y = element_text(margin = margin(r = 8, l = 10))) +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1),
         alpha = guide_legend(order = 3)) ->
  p7_World_Land; p7_World_Land

## Main Results  4H  World agricultural emissions by sector ----

EMs_NCEM_Ag %>%
  filter(scenario %in% c(Scenario_NoStorage, Scenario_Storage) ) |>
  mutate(sector = factor(sector, levels = rev(c("CH4_Ag", "N2O_Ag", "LULUCF")))) %>%
  filter(region == "World") %>%
  group_by_at(vars(-value, -year)) %>%
  Fill_annual(CUMULATIVE = T, CUM_YEAR_START = 2021) %>% ungroup() %>%
  filter(year %in% seq(2025, 2100, 25)) %>%
  group_by_at(vars(-scenario, -value)) %>%
  mutate(value = value -value[scenario == Scenario_NoStorage]) %>%
  filter(scenario != Scenario_NoStorage) %>% ungroup() %>%
  ggplot +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = sector),
           stat = "identity", position = "stack",
           color = "black") +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  labs(x = "Year", fill = "Emission", y = expression(paste(GtCO[2]-eq)),
       title = "(H) Emission impact") +
  scale_fill_brewer(palette = "BuPu", direction = -1,
                    breaks = c( "CH4_Ag", "N2O_Ag", "LULUCF"),
                    labels = c( "CH4_Ag" = expression(paste(CH[4], " Agriculture")),
                                "N2O_Ag" = expression(paste(N[2], "O Agriculture")),
                                "LULUCF" = expression(paste(CO[2], " LULUCF"))
                    )) +
  theme_bw() + theme0 +
  theme(axis.text.x = element_text(size = 13, margin = margin(t = 2),
                                   angle = 30, hjust = 0.8, vjust= 0.8),
        axis.title.y = element_text(margin = margin(r = 8, l = 10))) +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1),
         alpha = guide_legend(order = 3)) ->
  p8_World_EMs; p8_World_EMs

### patch plots E-H ----
(p5_World_ProdSec + theme(legend.position = "none")) +
  (p6_World_IrrWater  + theme(legend.position = "none")) +
  p7_World_Land +
  p8_World_EMs +
  plot_layout(guides = "collect", nrow = 1) &
  theme(axis.title.x = element_blank()) ->
  MainResults_StockImpact; MainResults_StockImpact

legend_MainResults_StockImpact <-
  cowplot::get_legend(MainResults_StockImpact +
                        theme(legend.position = "right"))

(p5_World_ProdSec + theme(legend.position = "none")) +
  (p6_World_IrrWater  + theme(legend.position = "none")) +
  p7_World_Land +
  p8_World_EMs +
  plot_layout(nrow = 1) &
  theme(axis.title.x = element_blank(), legend.position = "none") ->
  MainResults_StockImpact_NoLeg; MainResults_StockImpact_NoLeg

MainResults_StockImpact <-
  patchwork::wrap_plots(
    MainResults_StockImpact_NoLeg,
    patchwork::wrap_elements(legend_MainResults_StockImpact),
    ncol = 2,
    widths = c(0.9, 0.13)
  )


# Patch All ----

# source MainResults_StockImpactMaps from the map script
source("paper/R/Results_2p1_StockImpacts_Maps.R")

Get_MainResults_StockImpactMaps(
  .Scenario_Storage = Scenario_Storage,
  .Scenario_NoStorage = Scenario_NoStorage) ->
  MainResults_StockImpactMaps

patchwork::wrap_plots(
  MainResults_Stock_Patch,
  MainResults_StockImpact,
  MainResults_StockImpactMaps,
  nrow = 3,
  heights = c(1, 0.37, 0.38)
) -> MainResults_StockImpact_updated

# Save plots ----
# S1 vs. S2
# Scenario_Storage <- "S1_Dynamic"
# Scenario_NoStorage <- "S2_Hidden"

MainResults_StockImpact_updated %>%
  Write_png(.name = "FigMainResults_Stock_Patched_DynvsHidden", .svg = F,
            .DIR_MODULE = DIR_MODULE, w = 16, h = 16, r = 300)

## Other sensitivity comparisons ----
# need to update scenario def in the beginning

# MainResults_StockImpact_updated %>%
#   Write_png(.name = "FigMainResults_Stock_Patched_Dynamic_StaticInterPeriodvsHidden", .svg = F,
#             .DIR_MODULE = DIR_MODULE, w = 16, h = 16, r = 300)

# MainResults_StockImpact_updated %>%
#   Write_png(.name = "FigMainResults_Stock_Patched_HG", .svg = F,
#             .DIR_MODULE = DIR_MODULE, w = 16, h = 16, r = 300)
#
# MainResults_StockImpact_updated %>%
#   Write_png(.name = "FigMainResults_Stock_Patched_FRvsHidden", .svg = F,
#             .DIR_MODULE = DIR_MODULE, w = 16, h = 16, r = 300)
#



