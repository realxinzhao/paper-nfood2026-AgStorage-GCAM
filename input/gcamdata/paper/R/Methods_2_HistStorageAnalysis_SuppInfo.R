# supplementary figures associated with Figs. 1 & 2


DIR_OUTPUT <- "paper/output"
DIR_MODULE = "StorageHist"
Project <- ""

library(lemon)
library(ggtext)

## load data need from drake (run driver_drake() if not already) ----

MODULE_INPUTS <-
  c("GCAM_AgLU_SUA_APE")

MODULE_INPUTS %>% load_from_cache() -> all_data
get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)



# Define constants sets ----

c("Corn", "Wheat", "Rice", "OtherGrain",
  "Soybean", "OilPalm", "OilCrop",
  "SugarCrop", "FiberCrop",
  "RootTuber", "Legumes", "NutsSeeds", "MiscCrop") ->
  COMM_Storage

c("Vegetables", "Fruits", "Beef", "Dairy", "SheepGoat", "Pork", "Poultry",
  "OtherMeat_Fish", "FodderHerb", "FodderGrass") ->
  COMM_NoStorage

COMM_All <- c(COMM_Storage, COMM_NoStorage)


fontfamily = "sans"
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



# Supply utilization accounts ----

## fill element colors ----
RegDemendEle <- c("Closing stocks", "Export", "Other uses", "Loss", "Feed", "Food")
RegSupplyEle <- c("Production", "Import", "Opening stocks")
Ele_level <- c(RegDemendEle, RegSupplyEle)

#colorRampPalette( brewer.pal(11,"RdYlBu")[c(1:4,8:11)] )(length(Ele)) -> Bar_Col
colorRampPalette( brewer.pal(11,"Spectral")[c(1:5,9:11)] )(length(Ele_level)) -> Bar_Col

# Global figure SUA ----
GCAM_AgLU_SUA_APE %>% distinct(element)


GCAM_AgLU_SUA_APE %>%
  filter(element %in% Ele_level) %>%
  mutate(SD_Group = if_else(element %in% RegSupplyEle, "Supply", "Demand")) %>%
  mutate(SD_Group = factor(SD_Group, levels = c("Supply", "Demand"))) %>%
  group_by(GCAM_commodity, year, SD_Group, element) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  mutate(value = value / 1000) |>
  mutate(GCAM_commodity = factor(GCAM_commodity, levels = COMM_All))->
  GCAM_APE_World

GCAM_APE_World %>% filter(year >= 2010) |>
  #filter(GCAM_commodity == "Corn") %>%
  mutate(value = if_else(
    element %in% c("Opening stocks", "Production", "Import"),
    value, -value)) -> df0

c("white",Bar_Col[1:length(RegDemendEle)], "white", "white",
  (Bar_Col[(length(RegDemendEle)+1) :length(Ele_level)] )) -> Bar_Col1
LegAdd0 <- " "
LegAdd1 <- "Supply"
LegAdd2 <- "Demand"

names(Bar_Col1) <- c(LegAdd1, RegDemendEle, LegAdd0, LegAdd2, RegSupplyEle)

legend_order <- c(LegAdd1,
                  "Opening stocks", "Import", "Production", LegAdd0, LegAdd2,
                  RegDemendEle |> rev())
legend_order1 <- c(paste0("**",LegAdd1,"**"),
                   "Opening stocks", "Import", "Production", LegAdd0, paste0("**",LegAdd2,"**"),
                   RegDemendEle |> rev())


df0 %>%
  mutate(element = factor(element, levels = c(RegSupplyEle %>% rev(), RegDemendEle) )) %>%
  ggplot() +
  facet_wrap(~GCAM_commodity, scale = "free_y", ncol = 4) +
  geom_bar(aes(x = year, y = value, fill = element),
           stat = "identity", linewidth = 0.2, color = "black") +
  geom_hline(yintercept = 0, color = "grey20", linetype = 1, linewidth = 0.6) +
  scale_shape_manual(values = 21) +
  scale_x_continuous(breaks = seq(2010, 2022, 2)) +
  scale_y_continuous(n.breaks = 8) +
  labs(x= "Year", y = "Mt", fill = "SUA element") +
  scale_fill_manual(values = Bar_Col1 %>% rev,
                    limits = legend_order,
                    labels = legend_order1) +
  theme_bw() + theme0 +
  guides(fill = guide_legend(override.aes = list(colour = NA)) ) +
  theme(axis.text.x = element_text(angle = 30, size = 13, margin = margin(t = 0),
                                   hjust = 0.8,
                                   vjust= 0.8),
        legend.text = element_markdown(),
        axis.title.x = element_blank()) ->
  p;p


p %>% Write_png(.name = "Fig2_SI_Global_Hist_SUA", .svg = F,
                .DIR_MODULE = DIR_MODULE, w = 16, h = 16, r = 300)





## Fig 1 Mean Year ----
# GCAM_APE_World %>%
#   filter(year %in% c(2020:2022)) %>%
#   group_by_at(vars(-year, -value)) %>%
#   summarize(value = sum(value) / 3) %>%
#   ungroup() ->
#   GCAM_APE_World_MeanYear
#
#
# c("white",Bar_Col[1:length(RegDemendEle)], "white", "white",
#   (Bar_Col[(length(RegDemendEle)+1) :length(Ele_level)] )) -> Bar_Col1
# LegAdd0 <- " "
# LegAdd1 <- "Supply"
# LegAdd2 <- "Demand"
#
# names(Bar_Col1) <- c(LegAdd1, RegDemendEle, LegAdd0, LegAdd2, RegSupplyEle)
#
# legend_order <- c(LegAdd1,
#                   "Opening stocks", "Import", "Production", LegAdd0, LegAdd2,
#                   "Closing stocks", "Export", "Other uses", "Loss", "Feed", "Food")
# legend_order1 <- c(paste0("**",LegAdd1,"**"),
#                    "Opening stocks", "Import", "Production", LegAdd0, paste0("**",LegAdd2,"**"),
#                    "Closing stocks", "Export", "Other uses", "Loss", "Feed", "Food")
#
# GCAM_APE_World_MeanYear %>%
#   filter(GCAM_commodity %in% COMM_Storage) %>%
#   mutate(GCAM_commodity = factor(GCAM_commodity, levels = COMM_Storage)) %>%
#   mutate(value = value /1000) %>%
#   mutate(element = factor(element, levels = c(RegSupplyEle %>% rev(), RegDemendEle) )) %>%
#   ggplot() +
#   facet_wrap(~GCAM_commodity, scale = "free_y", ncol = 7) +
#   geom_bar(aes(x = SD_Group, y = value, fill = element), position = position_stack(),
#            stat = "identity", linewidth = 0.4, color = "black") +
#   geom_hline(yintercept = 0, color = "grey20", linetype = 1, size = 0.5) +
#   scale_fill_manual(values = Bar_Col1 %>% rev,
#                     limits = legend_order,
#                     labels = legend_order1) +
#   scale_y_continuous(n.breaks = 4) +
#   labs(x= "Year", y = "Billion tonnes per year",
#        fill = "" #,title = "(B) Global Supply Utilization Accounts for GCAM Agricultural Commodities"
#   ) +
#   theme_bw() + theme0 +
#   guides(fill = guide_legend(override.aes = list(colour = NA)) ) +
#   theme(axis.text.x = element_text(angle = 30, size = 13, margin = margin(t = 0),
#                                    hjust = 0.8,
#                                    vjust= 0.8),
#         legend.text = element_markdown(),
#         axis.title.x = element_blank()) ->
#   p;p
#
#
# reposition_legend(p0, 'center', panel = 'panel-7-2') -> p
#
# p %>% Write_png(.name = "Fig_Global_Hist_SUA", .svg = F,
#                 .DIR_MODULE = DIR_MODULE, w = 15, h =8, r = 300)



# Stock variation ----

GCAM_AgLU_SUA_APE %>%
  Reg_Clean %>%
  #filter(element %in% Ele_level) %>%
  mutate(SD_Group = if_else(element %in% RegSupplyEle, "Supply", "Demand")) %>%
  mutate(SD_Group = factor(SD_Group, levels = c("Supply", "Demand"))) %>%
  mutate(value = value / 1000) ->
  GCAM_AgLU_SUA_APE_Reg_Mt



## SI all commodities ----
GCAM_AgLU_SUA_APE_Reg_Mt %>%
  filter(GCAM_commodity %in% COMM_All) %>%
  mutate(GCAM_commodity = factor(GCAM_commodity, levels = COMM_All)) %>%
  filter(year %in% c(2010:2022),
         grepl("Stock Variation", element)) |>
  filter(GCAM_commodity %in% c(COMM_Storage))->
  df2

df2 %>%
  ggplot() +
  facet_wrap(~GCAM_commodity, scale = "free_y", nrow = 4) +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = region, y = value, fill = year),
           color = "black", linewidth = 0.3, stat = "identity") +
  geom_point(data = df2 %>%
               group_by_at(vars(-value, -year)) %>%
               summarize(value = sum(value)),
             aes(x = region, y = value),
             fill = "red", shape = 21, color = "black", size = 2) +
  scale_fill_viridis_c() +
  labs(x= "Region", y = "Million Tonnes",
       fill = "Year" #,title = "(B) Global Supply Utilization Accounts for GCAM Agricultural Commodities"
  ) +
  theme_bw() + theme0 + #theme_leg +
  scale_y_continuous(n.breaks = 4) +
  theme(axis.text.x = element_text(angle = 30, size = 10, margin = margin(t = 0),
                                   hjust = 0.8,
                                   vjust= 0.8)) -> p


p %>% Write_png(.name = "Fig2_SI_Global_Hist_StockVariation", .svg = F,
                .DIR_MODULE = DIR_MODULE, w = 16, h = 16, r = 300)



# Stock level ----

## SI Stock level all commodities ----

GCAM_AgLU_SUA_APE %>%
  Reg_Clean %>%
  #filter(element %in% Ele_level) %>%
  mutate(SD_Group = if_else(element %in% RegSupplyEle, "Supply", "Demand")) %>%
  mutate(SD_Group = factor(SD_Group, levels = c("Supply", "Demand"))) %>%
  mutate(value = value / 1000) ->
  GCAM_AgLU_SUA_APE_Reg_Mt


GCAM_AgLU_SUA_APE_Reg_Mt %>%
  rename(sector = GCAM_commodity) %>%
  AgSector_Clean %>%
  #filter(region == "World") %>%
  filter(year %in% c(2010:2022),
         grepl("stocks|Varia", element)) %>%
  mutate(element = factor(element,
                          levels = c("Opening stocks", "Closing stocks", "Stock Variation"),
                          labels = c("Opening stocks", "Closing stocks", "Stock variation"))) %>%
  filter(sector %in% c("Grains", "Oil crops", "Sugar crops", "Other crops")) |>
  ggplot() +
  facet_wrap(sector~region, scale = "free_y", ncol = 11) +
  geom_hline(yintercept = 0) +
  geom_line(aes(x = year, y = value, color = element), linewidth = 1) +
  geom_point(aes(x = year, y = value), size = 1, color = "black") +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(2010, 2022, by = 4)) +
  labs(x= "Year", y = "Million Tonnes Per Year",
       color = "Account" #,title = "(B) Global Supply Utilization Accounts for GCAM Agricultural Commodities"
  ) +
  theme_bw() + theme0 + theme_leg +
  theme(text = element_text(family= fontfamily, size = 12),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 30, size = 12, margin = margin(t = 0),
                                   hjust = 0.8,
                                   vjust= 0.9),
        legend.position = "top",
        legend.box = "horizontal",
        legend.box.just = "right",
        legend.justification = "right") -> p;p

p %>% Write_png(.name = "Fig2_SI_Global_Hist_Stocks", .svg = F,
                .DIR_MODULE = DIR_MODULE, w = 20, h = 16, r = 300)


## SI Stock to use ratio all commodities ----



GCAM_AgLU_SUA_APE_Reg_Mt %>% filter(year >= 2010) %>%
  rename(sector = GCAM_commodity) %>%
  AgSector_Clean %>%
  select(-SD_Group) %>%
  spread(element, value) %>%
  mutate(Net_Export = Export - Import,
         Consumption = Production - Net_Export,
         Consumption = Production - Net_Export - `Stock Variation`,
         Consumption1 = Production - Net_Export - `Stock Variation`) %>%
  # Adj Africa & Europe 2016 sugar crops
  mutate(Consumption = if_else(region %in% c("Africa", "Europe") & sector == "Sugar crops" & year == 2016,
                               Consumption - `Other uses`,
                               Consumption)) %>%
  mutate(StockToGrossUse = `Closing stocks`/(Consumption + `Closing stocks`),
         StockToGrossSupply = `Opening stocks`/(Production + `Opening stocks`),
         LossRate_Prod = Loss / Production,
         LossRate_Cons = Loss / Consumption,
         LossRate = if_else(Net_Export >= 0, LossRate_Prod, LossRate_Cons),
         LossRate = pmin(LossRate_Prod, LossRate_Cons)
         ) %>%
  gather(element, value, -year:-sector) ->
  GCAM_AgLU_SUA_APE_Reg_Mt_Metrics


GCAM_AgLU_SUA_APE_Reg_Mt_Metrics %>%
  filter(element %in% c("StockToGrossUse", "StockToGrossSupply")) %>%
  mutate(element = factor(element, levels = c("StockToGrossUse", "StockToGrossSupply"),
                          labels = c("(Closing) Stock-to-gross-use", "(Opening) stock-to-gross-supply"))) %>%
  filter(sector %in% c("Grains", "Oil crops", "Sugar crops", "Other crops")) |>
  ggplot() +
  facet_grid(sector~region, scale = "free_y") +
  #geom_hline(yintercept = 0) +
  geom_line(aes(x = year, y = value, color = element), size = 1) +
  geom_point(aes(x = year, y = value), size = 1, color = "black") +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(2010, 2022, by = 4)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x= "Year", y = "Share",
       color = "Metric") +
  theme_bw() + theme0 + theme_leg +
  theme(text = element_text(family= fontfamily, size = 12),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 40, size = 12, margin = margin(t = 0),
                                   hjust = 0.8,
                                   vjust= 0.9),
        panel.spacing.y = unit(0.4, "lines"),
        panel.spacing.x = unit(0.5, "lines"),
        legend.position = "top",
        legend.box = "horizontal",
        legend.box.just = "right",
        legend.justification = "right") -> p;p


p %>% Write_png(.name = "Fig2_SI_Global_Hist_StocksToSupplyDemand", .svg = F,
                .DIR_MODULE = DIR_MODULE, w = 20, h = 16, r = 300)


GCAM_AgLU_SUA_APE_Reg_Mt_Metrics %>%
  filter(element %in% c("LossRate_Cons", "LossRate_Prod", "LossRate")) %>%
  mutate(element = factor(element, levels = c("LossRate_Cons", "LossRate_Prod", "LossRate"),
                          labels = c("Loss-to-demand-ratio", "Loss-to-supply-ratio", "Regional storage loss ratio"))) %>%
  filter(sector %in% c("Grains", "Oil crops", "Sugar crops", "Other crops")) |>
  ggplot() +
  facet_grid(sector~region, scale = "free_y") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 0.05, color = "grey", linetype = 2, size = 1) +
  geom_line(aes(x = year, y = value, color = element, size = element)) +
  geom_point(aes(x = year, y = value), size = 1, color = "black") +
  scale_size_manual(values = c(1.5, 1.5, 1)) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(2010, 2022, by = 4)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x= "Year", y = "Share", size = "Metric",
       color = "Metric") +
  theme_bw() + theme0 + theme_leg +
  theme(text = element_text(family= fontfamily, size = 12),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 40, size = 12, margin = margin(t = 0),
                                   hjust = 0.8,
                                   vjust= 0.9),
        panel.spacing.y = unit(0.5, "lines"),
        panel.spacing.x = unit(0.5, "lines"),
        legend.position = "top",
        legend.box = "horizontal",
        legend.box.just = "right",
        legend.justification = "right") -> p;p


p %>% Write_png(.name = "Fig2_SI_Global_Hist_LossRate", .svg = F,
                .DIR_MODULE = DIR_MODULE, w = 20, h = 16, r = 300)



