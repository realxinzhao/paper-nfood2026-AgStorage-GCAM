# NATURE FOOD FIGURES 1 & 2: Global Storage System Analysis ----
# Purpose: Comprehensive analysis of global food storage systems (2010–2022)
#
# Figure 1: Grain Storage Decomposition (Panels A–D)
#   Panel A: Global grain closing stocks by region (2022)
#   Panel B: Regional grain storage variation by year (temporal decomposition)
#   Panel C: Sectoral decomposition of grain storage change
#   Panel D: Complete grain storage decomposition (temporal + regional + sectoral)
#
# Figure 2: Food Storage & Loss Analysis (Panels E–H)
#   Panel E: Global stock variation by commodity type (stacked by year)
#   Panel F: Stock-to-gross-use ratio across regions and sectors (2022)
#   Panel G: Storage loss rate distribution by region and sector (2022)
#   Panel H: Dietary energy from stocks vs. total food availability (2022)
#
# Output:
#   Fig1.png/pdf (11" x 11" at 300 dpi)
#   Fig2.png/pdf (13" x 11" at 300 dpi)

# SETUP: Load libraries and define constants ----

# Load packages and data ----
devtools::load_all()
library(lemon)
library(ggtext)
library(ggplot2)
library(RColorBrewer)
library(ggnewscale)
source("paper/func/LoadPackagesFuncs.R")
source("paper/func/AgLU_DashBoard_Data_Func.R")

DIR_MODULE <- "StorageHist"

## load data need from drake (run driver_drake() if not already) ----

MODULE_INPUTS <-
  c(FILE = "aglu/AGLU_ctry",
    FILE = "common/iso_GCAM_regID",
    FILE = "common/GCAM_region_names",
    FILE = "aglu/FAO/Mapping_SUA_PrimaryEquivalent",
    FILE = "aglu/FAO/SUA_item_code_map",
    FILE = "aglu/FAO/GCAMFAOSTAT_SUA",
    "GCAM_AgLU_SUA_APE",
    "FAO_Stock_Macronutrient_All",
    "FAO_Food_Macronutrient_All",
    "L101.Pop_thous_Scen_R_Y")

MODULE_INPUTS %>% load_from_cache() -> all_data
get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)





# Define grain commodities
grain_sectors <- c("Corn", "Wheat", "Rice", "OtherGrain")

# Define commodity sets
c("Corn", "Wheat", "Rice", "OtherGrain",
  "Soybean", "OilPalm", "OilCrop",
  "SugarCrop", "FiberCrop",
  "RootTuber", "Legumes", "NutsSeeds", "MiscCrop") ->
  COMM_Storage

c("Vegetables", "Fruits", "Beef", "Dairy", "SheepGoat", "Pork", "Poultry",
  "OtherMeat_Fish", "FodderHerb", "FodderGrass") ->
  COMM_NoStorage

COMM_All <- c(COMM_Storage, COMM_NoStorage)

fontfamily <- "sans"
theme0 <- theme(
  panel.grid.minor = element_line(linewidth = 0.2, linetype = 2, colour = "gray50"),
  panel.grid.major = element_line(linewidth = 0.2, linetype = 2, colour = "gray50"),
  panel.border = element_rect(colour = "black", linewidth = 1),
  text = element_text(family = fontfamily, size = 14),
  axis.text.y = element_text(size = 13, color = "black"),
  axis.text.x = element_text(angle = 0, color = "black", size = 13, margin = margin(t = 5), vjust = 0.5),
  axis.title.x = element_text(size = 14, margin = margin(t = 9, r = 0, b = 0, l = 0)),
  strip.background = element_rect(fill = "grey95"),
  strip.text = element_text(size = 14),
  plot.title = element_text(hjust = 0, face = "bold", margin = margin(t = 5, r = 0, b = 8, l = 0))
)


RegDemendEle <- c("Closing stocks", "Export", "Other uses", "Loss", "Feed", "Food")
RegSupplyEle <- c("Production", "Import", "Opening stocks")
Ele_level <- c(RegDemendEle, RegSupplyEle)

GCAM_AgLU_SUA_APE %>%
  Reg_Clean %>%
  #filter(element %in% Ele_level) %>%
  mutate(SD_Group = if_else(element %in% RegSupplyEle, "Supply", "Demand")) %>%
  mutate(SD_Group = factor(SD_Group, levels = c("Supply", "Demand"))) %>%
  mutate(value = value / 1000) |> select(-unit) ->
  GCAM_AgLU_SUA_APE_Reg_Mt

# Get primary stocks in 2022 for grains
Mapping_SUA_PrimaryEquivalent %>%
  filter(source_primary == T) %>%
  distinct(GCAM_commodity, item = source_item) %>%
  left_join_error_no_match(SUA_item_code_map, by = "item") %>%
  select(GCAM_commodity, item_code, item) ->
  Source_Item

Mapping_SUA_PrimaryEquivalent %>%
  filter(source_primary == F) %>%
  distinct(GCAM_commodity, item = source_item) %>%
  left_join_error_no_match(SUA_item_code_map, by = "item") %>%
  select(GCAM_commodity, item_code, item) ->
  Processed_Item
Processed_Item |>
  filter(GCAM_commodity %in% grain_sectors)


GCAMFAOSTAT_SUA |>
  filter(element == "Closing stocks", year == 2022) |>
  inner_join(
    Source_Item |>
      filter(GCAM_commodity %in% grain_sectors)
  ) |>
  left_join_error_no_match(AGLU_ctry %>% distinct(area_code = FAO_country_code, iso), by = c("area_code")) %>%
  left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
  left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") |>
  group_by(region, element, year, GCAM_commodity) |>
  summarize(value = sum(value) / 1000, .groups = "drop") %>%
  Reg_Clean ->
  GCAM_AgLU_GrainStocks_Reg_Mt_Primary

GCAMFAOSTAT_SUA |>
  filter(element == "Closing stocks", year == 2022) |>
  inner_join(
    Source_Item |>
      filter(GCAM_commodity %in% grain_sectors)
  ) |>
  left_join_error_no_match(
    AGLU_ctry %>% distinct(area_code = FAO_country_code, iso),
                           by = c("area_code")) %>%
  left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
  left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") |>
  group_by(region, element, year, GCAM_commodity) |>
  summarize(value = sum(value) / 1000, .groups = "drop") %>%
  Reg_Clean |> distinct(region, GCAM_commodity, element)


GCAM_AgLU_SUA_APE_Reg_Mt |>
  rename(sector = GCAM_commodity) |>
  AgSector_Clean() |>
  filter(year %in% c(2020:2022))



# Figure 1 ----


# PANEL A (Updated to 1D): Regional grain stocks in 2022 ----
# Get closing stocks in 2022
GCAM_AgLU_SUA_APE_Reg_Mt %>%
  filter(GCAM_commodity %in% grain_sectors) %>%
  group_by_at(vars(-GCAM_commodity, -value)) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  mutate(GCAM_commodity = "Grain") %>%
  filter(year %in% c(2022),
         grepl("Closing stocks", element)) |>
  left_join_error_no_match(
    GCAM_AgLU_GrainStocks_Reg_Mt_Primary |>
      group_by(year, region) |>
      summarize(Primary = sum(value))
  ) |>
  mutate(Processed = value - Primary) |>
  rename(total = value) |>
  gather(proc, value, total, Primary, Processed) |> ungroup() |>
  # Join population in 2022
  left_join_error_no_match(
    L101.Pop_thous_Scen_R_Y |>
      filter(year == 2022, scenario == "SSP2") |>
      left_join_error_no_match(GCAM_region_names) |>
      select(-GCAM_region_ID) |>
      Reg_Clean() |> rename(pop = value) |> ungroup() ) |>
  # kg per ca
  mutate(value = value * 10^9  / (pop * 1000) )->
  Fig1_PanelA_df

Fig1_PanelA_df |> filter(proc == "total") |>
  filter(region != "World") |> arrange(-value) |>
  pull(region) ->
  PanelA_Reg_Order

Fig1_PanelA_df |> filter(proc != "total") |>
  left_join_error_no_match(Fig1_PanelA_df |> filter(proc == "total") |>
                             spread(proc, value)) |>
  mutate(proc = factor(proc, levels = c("Processed", "Primary"))) |>
  mutate(region = factor(region, levels = c(PanelA_Reg_Order, "World"))) |>
  ggplot() +
  geom_bar(aes(x = region, y = value, fill = proc),
           color = "black", linewidth = 0.3, stat = "identity") +
  scale_fill_brewer(palette = "Set3", direction = -1) +
  scale_y_continuous(limits = c(0, 380), breaks = seq(0, 400, 100),
                     expand = c(0, 0)) +
  labs(x = "Region", y = "Stocks, kilograms per capita", fill = "Stocks",
       title = "(D) Grain stocks per capita in 2022") +
  theme_bw() + theme0 +
  theme(#panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 25, size = 11, margin = margin(t = 0),
                                   hjust = 0.8, vjust = 0.8)) ->
  panel_A0; panel_A0

panel_A <- panel_A0 + theme_leg +
  theme(legend.position = c(0.65, 0.95),   # upper-left
        legend.justification = c(0, 1),
        #legend.direction = "horizontal",
        legend.title = element_text(margin = margin(r = 10, b = 6), vjust = 0.8),
        legend.background = element_rect(
          fill = scales::alpha("white", 0.5), color = "NA"),
        legend.key.size = unit(0.9, "cm"),
        legend.text = element_text(size = 12)); panel_A



# PANEL B (Updated to 1C): Regional grain storage variation by year ----
# Decomposes year-over-year changes in grain stocks by region (2010–2022).
# Regions ordered by decomposition slope steepness (steepest first).
# Shows both supply-side (with stock variation) and demand-side elements.
GCAM_AgLU_SUA_APE_Reg_Mt %>%
  filter(GCAM_commodity %in% c("Wheat", "Corn", "Rice", "OtherGrain")) %>%
  group_by_at(vars(-GCAM_commodity, -value)) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  mutate(GCAM_commodity = "Grain") %>%
  filter(year %in% c(2011:2022),
         grepl("Stock Variation", element)) ->
  df2


# Prepare data for red dot (net total change by region)
df2_total <- df2 %>%
  group_by_at(vars(-value, -year)) %>%
  summarize(value = sum(value), .groups = "drop")

df2 %>%
  mutate(region = factor(region, levels = c(PanelA_Reg_Order, "World"))) |>
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = region, y = value, fill = year),
           color = "black", linewidth = 0.3, stat = "identity") +
  geom_point(data = df2_total, aes(x = region, y = value),
             fill = "red", shape = 21, color = "black", size = 2.5) +
  annotate("text", x = 1.85, y = 390,
           label = "Net change \n(2010–2022)", size = 4.5, hjust = 0, vjust = 1,
           color = "darkred", fontface = "italic") +
  scale_fill_viridis_c() +
  scale_y_continuous(limits = c(-150, 700), breaks = seq(0, 600, 200), expand = c(0, 0)) +
  labs(x = "Region", y = "Stock variation, million tonnes", fill = "Year",
       title = "(C) Grain stock variation, 2010–2022") +
  theme_bw() + theme0 +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 25, size = 11, margin = margin(t = 0),
                                   hjust = 0.8, vjust = 0.8)) -> panel_B0; panel_B0

panel_B0 + theme_leg +
  theme(legend.position = c(0.22, 0.95),   # upper-left
        legend.justification = c(0, 1),
        legend.direction = "horizontal",
        legend.title = element_text(margin = margin(r = 20), vjust = 0.8),
        legend.background = element_rect(
          fill = scales::alpha("white", 0.5), color = "NA"),
        legend.key.size = unit(0.9, "cm"),
        legend.text = element_text(size = 12)) ->
  panel_B



# PANEL C (Updated to 1A): Sectoral decomposition of grain storage ----
# Shows sector-level (crop type) drivers of grain stock changes (2010–2022).
# Sectors ordered by decomposition slope steepness (shallowest to steepest).
# Illustrates which crop sectors dominate temporal and regional changes.

# Data preparation: version WITH Stock Variation
GCAM_AgLU_SUA_APE_Reg_Mt %>%
  filter(year >= 2010) %>%
  rename(sector = GCAM_commodity) %>%
  AgSector_Clean %>%
  select(-SD_Group) %>%
  spread(element, value) %>%
  mutate(Net_Export = Export - Import,
         Consumption = Production - Net_Export - `Stock Variation`) %>%
  gather(element, value, -year:-sector) %>%
  filter(element %in% c("Closing stocks", "Consumption")) %>%
  group_by(sector, year, region) %>%
  mutate(stock = value[element == "Closing stocks"]) %>%
  filter(element != "Closing stocks") %>%
  ungroup() %>%
  filter(sector == "Grains", region != "World") %>%
  group_by_at(vars(-region, -value, -stock)) %>%
  summarize(value = sum(value), stock = sum(stock), .groups = "drop") ->
  fig1a_data

# Year-by-year segments (with Stock Variation)
fig1a_data %>%
  arrange(year) %>%
  mutate(x_end = lead(value), y_end = lead(stock)) %>%
  filter(!is.na(x_end)) -> fig1a_segments

# Data preparation: version WITHOUT Stock Variation
GCAM_AgLU_SUA_APE_Reg_Mt %>%
  filter(year >= 2010) %>%
  rename(sector = GCAM_commodity) %>%
  AgSector_Clean %>%
  select(-SD_Group) %>%
  spread(element, value) %>%
  mutate(Net_Export = Export - Import,
         Consumption = Production - Net_Export) %>%
  gather(element, value, -year:-sector) %>%
  filter(element %in% c("Closing stocks", "Consumption")) %>%
  group_by(sector, year, region) %>%
  mutate(stock = value[element == "Closing stocks"]) %>%
  filter(element != "Closing stocks") %>%
  ungroup() %>%
  filter(sector == "Grains", region != "World") %>%
  group_by_at(vars(-region, -value, -stock)) %>%
  summarize(value = sum(value), stock = sum(stock), .groups = "drop") ->
  panel_B_data_no_sv

# Year-by-year segments (without Stock Variation)
panel_B_data_no_sv %>%
  arrange(year) %>%
  mutate(x_end = lead(value), y_end = lead(stock)) %>%
  filter(!is.na(x_end)) -> panel_B_segments_no_sv

# Prepare band data for year ranges
panel_B_band_data <- bind_rows(
  fig1a_segments %>%
    select(year, value, stock) %>%
    rename(value_with_sv = value, stock_with_sv = stock) %>%
    left_join(
      panel_B_segments_no_sv %>%
        select(year, value, stock) %>%
        rename(value_without_sv = value, stock_without_sv = stock),
      by = "year"
    ),
  fig1a_data %>% filter(year == 2022) %>%
    select(year, value, stock) %>%
    rename(value_with_sv = value, stock_with_sv = stock) %>%
    left_join(
      panel_B_data_no_sv %>% filter(year == 2022) %>%
        select(year, value, stock) %>%
        rename(value_without_sv = value, stock_without_sv = stock),
      by = "year"
    )
)

# Regional breakdown: WITH Stock Variation
GCAM_AgLU_SUA_APE_Reg_Mt %>%
  filter(year >= 2010) %>%
  rename(sector = GCAM_commodity) %>%
  AgSector_Clean %>%
  select(-SD_Group) %>%
  spread(element, value) %>%
  mutate(Net_Export = Export - Import,
         Consumption = Production - Net_Export - `Stock Variation`) %>%
  gather(element, value, -year:-sector, -region) %>%
  filter(element %in% c("Closing stocks", "Consumption")) %>%
  group_by(sector, year, region) %>%
  mutate(stock = value[element == "Closing stocks"]) %>%
  filter(element != "Closing stocks") %>%
  ungroup() %>%
  filter(sector == "Grains") -> fig1a_data_reg

# World aggregate reference (2010 and 2022)
fig1a_data %>% filter(year %in% c(2010, 2022)) -> fig0_combined_ref

# Create reference endpoints for panel B
fig1a_data %>% filter(year %in% c(2010, 2022)) -> panel_B_ref_endpoints
fig0_combined_ref %>% filter(year %in% c(2010, 2022)) -> panel_B_combined_ref

# World values for arrow starting points
world_2010 <- fig0_combined_ref %>% filter(year == 2010) %>% select(value, stock) %>% as.list()
world_2022 <- fig0_combined_ref %>% filter(year == 2022) %>% select(value, stock) %>% as.list()

# Add missing 2021→2022 segment to fig1a_segments (since lead() drops the last year)
missing_segment <- fig1a_data %>%
  filter(year == 2021) %>%
  mutate(x_end = fig1a_data %>% filter(year == 2022) %>% pull(value),
         y_end = fig1a_data %>% filter(year == 2022) %>% pull(stock))

fig1a_segments <- bind_rows(fig1a_segments, missing_segment) %>% distinct()

## ---- Reorder regional decomposition by arrow slope (steepest first) ----
# Calculate slope for each regional arrow (dy/dx) to order by steepness
fig1a_data_reg %>%
  filter(year %in% c(2010, 2022), region != "World") %>%
  select(year, region, value, stock) %>%
  pivot_wider(names_from = year, values_from = c(value, stock), names_sep = "_") %>%
  mutate(d_value = value_2022 - value_2010,
         d_stock = stock_2022 - stock_2010,
         slope = d_stock / d_value) %>%  # slope of regional arrow
  arrange(desc(slope)) %>%  # steepest first (highest stock-to-consumption ratio increase)
  pull(region) -> region_order_by_slope

# Recompute fig1b_decomp with new ordering
fig1a_data_reg %>%
  filter(year %in% c(2010, 2022), region != "World") %>%
  select(year, region, value, stock) %>%
  pivot_wider(names_from = year, values_from = c(value, stock), names_sep = "_") %>%
  mutate(d_value = value_2022 - value_2010,
         d_stock = stock_2022 - stock_2010) %>%
  arrange(match(region, region_order_by_slope)) %>%  # Use slope-based ordering
  mutate(
    x_start = world_2010$value + lag(cumsum(d_value), default = 0),
    y_start = world_2010$stock + lag(cumsum(d_stock), default = 0),
    x_end   = world_2010$value + cumsum(d_value),
    y_end   = world_2010$stock + cumsum(d_stock)
  ) -> fig1b_decomp_slope_ordered

# Update labels for the reordered decomposition
fig1b_decomp_slope_ordered %>%
  mutate(magnitude = sqrt(d_value^2 + d_stock^2),
         x_mid = (x_start + x_end) / 2,
         y_mid = (y_start + y_end) / 2,
         dx = x_end - x_start, dy = y_end - y_start,
         norm = sqrt(dx^2 + dy^2),
         x_label = x_mid + 25 * (-dy / norm),
         y_label = y_mid + 25 * ( dx / norm),
         # Wrap text for region names
         region_label = case_when(
           region == "Latin America" ~ "Latin\nAmerica",
           region == "South Asia" ~ "South\nAsia",
           region == "Rest of Asia" ~ "Rest of\nAsia",
           region == "North America" ~ "North\nAmerica",
           region == "Middle East" ~ "Middle\nEast",
           TRUE ~ region
         ),
         # Adjust y_label to move upper regions up to avoid overlap
         y_label = case_when(
           region == "Latin America" ~ y_label + 15,
           region == "South Asia" ~ y_label + 10,
           region == "North America" ~ y_label + 20,
           TRUE ~ y_label
         )) %>%
  filter(region %in% c("China", "Africa", "South Asia", "Europe", "Reforming", "Latin America", "North America", "Rest of Asia", "Middle East")) -> fig1b_labels_slope_ordered

# Update fig1b_decomp and fig1b_labels for use in panel B
fig1b_decomp <- fig1b_decomp_slope_ordered
fig1b_labels <- fig1b_labels_slope_ordered

## ---- Sector decomposition ordered by slope (shallowest to steepest) ----
# Calculate slope for each sector arrow (dy/dx)
GCAM_AgLU_SUA_APE_Reg_Mt %>%
  filter(GCAM_commodity %in% grain_sectors, region != "World",
         year %in% c(2010, 2022)) %>%
  select(-SD_Group) %>%
  spread(element, value) %>%
  mutate(Net_Export = Export - Import,
         Consumption = Production - Net_Export - `Stock Variation`) %>%
  gather(element, value, -year, -GCAM_commodity, -region) %>%
  filter(element %in% c("Closing stocks", "Consumption")) %>%
  group_by(GCAM_commodity, year, region) %>%
  mutate(stock = value[element == "Closing stocks"]) %>%
  filter(element != "Closing stocks") %>%
  ungroup() %>%
  group_by(GCAM_commodity, year) %>%
  summarise(value = sum(value), stock = sum(stock), .groups = "drop") %>%
  select(year, GCAM_commodity, value, stock) %>%
  pivot_wider(names_from = year, values_from = c(value, stock), names_sep = "_") %>%
  mutate(d_value = value_2022 - value_2010,
         d_stock = stock_2022 - stock_2010,
         slope = d_stock / d_value) %>%
  arrange(slope) %>%
  pull(GCAM_commodity) -> sector_order_by_slope

# Recompute fig1c_decomp with slope-based ordering
GCAM_AgLU_SUA_APE_Reg_Mt %>%
  filter(GCAM_commodity %in% grain_sectors, region != "World",
         year %in% c(2010, 2022)) %>%
  select(-SD_Group) %>%
  spread(element, value) %>%
  mutate(Net_Export = Export - Import,
         Consumption = Production - Net_Export - `Stock Variation`) %>%
  gather(element, value, -year, -GCAM_commodity, -region) %>%
  filter(element %in% c("Closing stocks", "Consumption")) %>%
  group_by(GCAM_commodity, year, region) %>%
  mutate(stock = value[element == "Closing stocks"]) %>%
  filter(element != "Closing stocks") %>%
  ungroup() %>%
  group_by(GCAM_commodity, year) %>%
  summarise(value = sum(value), stock = sum(stock), .groups = "drop") %>%
  select(year, GCAM_commodity, value, stock) %>%
  pivot_wider(names_from = year, values_from = c(value, stock), names_sep = "_") %>%
  mutate(d_value = value_2022 - value_2010,
         d_stock = stock_2022 - stock_2010) %>%
  arrange(match(GCAM_commodity, sector_order_by_slope)) %>%
  mutate(
    x_start = world_2010$value + lag(cumsum(d_value), default = 0),
    y_start = world_2010$stock + lag(cumsum(d_stock), default = 0),
    x_end   = world_2010$value + cumsum(d_value),
    y_end   = world_2010$stock + cumsum(d_stock)
  ) -> fig1c_decomp_slope_ordered

# Update labels for the reordered sectoral decomposition
fig1c_decomp_slope_ordered %>%
  mutate(x_mid = (x_start + x_end) / 2,
         y_mid = (y_start + y_end) / 2,
         dx = x_end - x_start, dy = y_end - y_start,
         norm = sqrt(dx^2 + dy^2),
         x_label = x_mid + 25 * (-dy / norm),
         y_label = y_mid + 25 * ( dx / norm)) -> fig1c_labels_slope_ordered

# Update fig1c_decomp and fig1c_labels for use in panel B
fig1c_decomp <- fig1c_decomp_slope_ordered
fig1c_labels <- fig1c_labels_slope_ordered

# Panel C visualization: temporal paths with consumption formula comparison
panel_C <- ggplot() +
  # 45-degree reference lines (stock = consumption)
  geom_abline(slope = 1, intercept = -1400, linetype = 2, color = "gray50", linewidth = 0.2) +
  geom_abline(slope = 1, intercept = -1600, linetype = 2, color = "gray50", linewidth = 0.2) +
  geom_abline(slope = 1, intercept = -1800, linetype = 2, color = "gray50", linewidth = 0.2) +
  geom_abline(slope = 1, intercept = -2000, linetype = 2, color = "gray50", linewidth = 0.2) +

  scale_x_continuous(limits = c(2400, 3100), breaks = seq(2500, 3000, 100), expand = c(0,0)) +
  scale_y_continuous(limits = c(600, 1450), breaks = seq(600, 1400, 100), expand = c(0,0)) +

  geom_text(data = filter(panel_B_combined_ref, year == 2010),
            aes(x = value, y = stock, label = year),
            size = 4.5, vjust = 2.2, hjust = 1, fontface = "bold", color = "black") +
  # Year labels with viridis color mapping (using palette indices)
  annotate("text", x = 2600, y = 740, label = "2012",
           size = 4, hjust = 0, vjust = 0.5, color = scales::viridis_pal()(13)[3]) +
  annotate("text", x = 2850, y = 1002, label = "2014",
           size = 4, hjust = 0, vjust = 0.5, color = scales::viridis_pal()(13)[5]) +
  annotate("text", x = 2935, y = 1150, label = "2016",
           size = 4, hjust = 0, vjust = 0.5, color = scales::viridis_pal()(13)[7]) +
  annotate("text", x = 2975, y = 1259, label = "2018",
           size = 4, hjust = 0, vjust = 0.5, color = scales::viridis_pal()(13)[9]) +
  annotate("text", x = 3025, y = 1293, label = "2020",
           size = 4, hjust = 0, vjust = 0.5, color = scales::viridis_pal()(13)[11]) +

  # Horizontal bands colored by year showing consumption difference at each year
  geom_rect(data = panel_B_band_data,
            aes(xmin = pmin(value_with_sv, value_without_sv),
                xmax = pmax(value_with_sv, value_without_sv),
                ymin = stock_with_sv - 10,
                ymax = stock_with_sv + 10,
                fill = year),
            alpha = 0.4) +

  # WITH Stock Variation path (solid) - temporal decomposition
  geom_segment(data = fig1a_segments,
               aes(x = value, y = stock, xend = x_end, yend = y_end, color = year),
               linewidth = 0.9, linetype = 1, lineend = "square",
               arrow = arrow(length = unit(0.15, "cm"), angle = 12, type = "closed", ends = "last")) +

  # WITHOUT Stock Variation path (dotted) - temporal decomposition
  geom_segment(data = panel_B_segments_no_sv,
               aes(x = value, y = stock, xend = x_end, yend = y_end, color = year),
               linewidth = 0.9, linetype = 3, lineend = "square",
               arrow = arrow(length = unit(0.15, "cm"), angle = 12, type = "closed", ends = "last")
               ) +

  # Reference line connecting WITH SV endpoints
  geom_path(data = panel_B_combined_ref, aes(x = value, y = stock),
            linetype = 1, color = "black", linewidth = 1.2, lineend = "square",
            arrow = arrow(length = unit(.3, "cm"), angle = 15, type = "closed", ends = "last")) +

  scale_color_viridis_c(name = "Year", labels = function(x) as.character(as.integer(x)), aesthetics = c("color", "fill")) +
  new_scale_color() +

  # Endpoint dots and labels (drawn on top for visibility)
  geom_point(data = filter(panel_B_combined_ref, year == 2010),
             aes(x = value, y = stock), size = 2.5, color = "black", shape = 21, fill = "red") +
  geom_point(data = filter(panel_B_combined_ref, year == 2022),
             aes(x = value, y = stock), size = 2.5, color = "black", shape = 21, fill = "red") +
  geom_text(data = filter(panel_B_combined_ref, year == 2022),
            aes(x = value-5, y = stock, label = year),
            size = 4.5, vjust = -0.9, hjust = 0, fontface = "bold") +

  # Custom legend for line types and colors
  annotate("text", x = 2560, y = 810, label = "Cumulative change\n vs. consumption",
           size = 4.5, hjust = 0.5, vjust = 0, angle = 46, fontface = "bold.italic") +

  annotate("text", x = 2710, y = 1100, label = "Yearly trajectory\n vs. consumption",
           size = 4.5, hjust = 1, vjust = 0.5, fontface = "bold.italic", color = "deepskyblue4") +

  annotate("text", x = 2810, y = 900, label = "Yearly trajectory\n vs. production",
           size = 4.5, hjust = 0, vjust = 0.5, fontface = "bold.italic", color = "deepskyblue4") +

  labs(x = "Consumption / production, million tonnes",
       y = "Stocks, million tonnes",
       title = "(A) Global grain stock–use trajectories") +
  theme_bw() + theme0 +
  theme(panel.grid.major = element_line(linetype = 2, color = "gray50", linewidth = 0.2),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 5, r = 10, b = 15, l = 5, unit = "pt")); panel_C

# PANEL D (Updated to 1B): Complete grain storage decomposition ----
# Combines temporal, regional, and sectoral dimensions of grain stock changes (2010–2022).
# Integrates insights from Panels A–C into unified visualization framework.
panel_D <- ggplot() +
  # 45-degree reference lines (stock = consumption)
  geom_abline(slope = 1, intercept = -1400, linetype = 2, color = "gray50", linewidth = 0.2) +
  geom_abline(slope = 1, intercept = -1600, linetype = 2, color = "gray50", linewidth = 0.2) +
  geom_abline(slope = 1, intercept = -1800, linetype = 2, color = "gray50", linewidth = 0.2) +
  geom_abline(slope = 1, intercept = -2000, linetype = 2, color = "gray50", linewidth = 0.2) +

  scale_x_continuous(limits = c(2400, 3100), breaks = seq(2500, 3000, 100), expand = c(0,0)) +
  scale_y_continuous(limits = c(600, 1450), breaks = seq(600, 1400, 100), expand = c(0,0)) +

  # Temporal decomposition: stock vs consumption (back layer - thin, size 0.8)
  geom_segment(data = fig1a_segments,
               aes(x = value, y = stock, xend = x_end, yend = y_end, color = year),
               linewidth = 0.9, linetype = 1, lineend = "square",
               arrow = arrow(length = unit(0.15, "cm"), angle = 12, type = "closed", ends = "last")) +

  scale_color_viridis_c(name = "Year", labels = function(x) as.character(as.integer(x)), aesthetics = c("color", "fill")) +
  new_scale_color() +

  # Reference line connecting 2010 and 2022
  geom_path(data = panel_B_combined_ref, aes(x = value, y = stock),
            linetype = 1, color = "black", linewidth = 1.2, lineend = "square",
            arrow = arrow(length = unit(.3, "cm"), angle = 15, type = "closed", ends = "last")) +

  # Regional decomposition (2010 vs 2022) - dashed lines
  geom_segment(data = fig1b_decomp,
               aes(x = x_start, y = y_start, xend = x_end, yend = y_end, color = region),
               linewidth = 0.9, linetype = "dashed", lineend = "round",
               alpha = 1,
               arrow = arrow(length = unit(0.1, "cm"), angle = 10, type = "closed", ends = "last")) +
  geom_text(data = fig1b_labels %>% filter(region %in% c("China", "Europe")),
            aes(x = x_label + 15, y = y_label, label = region_label, color = region),
            size = 4, hjust = 1, vjust = 0.5, show.legend = FALSE, lineheight = 0.9) +
  geom_text(data = fig1b_labels %>% filter(region == "Reforming"),
            aes(x = x_label - 40, y = y_label, label = region_label, color = region),
            size = 4, hjust = 0.5, vjust = 0.5, show.legend = FALSE, lineheight = 0.9) +
  geom_text(data = fig1b_labels %>% filter(region %in% c("Latin America", "South Asia", "North America")),
            aes(x = x_label, y = y_label, label = region_label, color = region),
            size = 2.8, hjust = 0.5, vjust = 0.5, show.legend = FALSE, lineheight = 0.9) +
  # Rest of Asia: slightly left
  geom_text(data = fig1b_labels %>% filter(region == "Rest of Asia"),
            aes(x = x_label - 20, y = y_label, label = region_label, color = region),
            size = 3, hjust = 0.5, vjust = 0.5, show.legend = FALSE, lineheight = 0.9) +
  # Middle East: moved to left side
  geom_text(data = fig1b_labels %>% filter(region == "Middle East"),
            aes(x = x_label - 30, y = y_label - 20, label = region_label, color = region),
            size = 2.5, hjust = 0.5, vjust = 0.5, show.legend = FALSE, lineheight = 0.9) +
  # Africa: slightly left
  geom_text(data = fig1b_labels %>% filter(region == "Africa"),
            aes(x = x_label - 20, y = y_label, label = region_label, color = region),
            size = 4, hjust = 0.5, vjust = 0.5, show.legend = FALSE, lineheight = 0.9) +
  scale_color_brewer(palette = "Paired", name = "Region") +
  new_scale_color() +

  # Sectoral decomposition (2010 vs 2022) - dotted lines (size = 1, alpha = 1)
  geom_segment(data = fig1c_decomp,
               aes(x = x_start, y = y_start, xend = x_end, yend = y_end,
                   color = GCAM_commodity),
               linewidth = 0.9, linetype = "dotted", lineend = "round", alpha = 1,
               arrow = arrow(length = unit(0.1, "cm"), angle = 10, type = "closed", ends = "last")) +
  # Sectoral labels: all moved to right side
  # Corn: further right, moved down slightly, size 4
  geom_text(data = fig1c_labels %>% filter(GCAM_commodity == "Corn"),
            aes(x = x_label + 40, y = y_label - 15, label = GCAM_commodity, color = GCAM_commodity),
            size = 4, hjust = 0, vjust = 0.5, show.legend = FALSE, lineheight = 0.9) +
  # Wheat, Rice: further right, size 4
  geom_text(data = fig1c_labels %>% filter(GCAM_commodity %in% c("Wheat", "Rice")),
            aes(x = x_label + 40, y = y_label-10, label = GCAM_commodity, color = GCAM_commodity),
            size = 4, hjust = 0, vjust = 0.5, show.legend = FALSE, lineheight = 0.9) +
  # OtherGrain: right side, moved down, smaller size and wrapped
  geom_text(data = fig1c_labels %>% filter(GCAM_commodity == "OtherGrain"),
            aes(x = x_label + 25, y = y_label - 30, label = "Other\nGrain", color = GCAM_commodity),
            size = 3, hjust = 0, vjust = 0.5, show.legend = FALSE, lineheight = 0.9) +

  scale_color_manual(
    values = head(RColorBrewer::brewer.pal(11, "BrBG"), 4),
    name = "Sector") +

  # Endpoint dots and labels (drawn on top for visibility)
  geom_point(data = filter(panel_B_combined_ref, year == 2010),
             aes(x = value, y = stock), size = 2.5, color = "black", shape = 21, fill = "red") +
  geom_text(data = filter(panel_B_combined_ref, year == 2010),
            aes(x = value, y = stock, label = year),
            size = 4.5, vjust = 2.2, hjust = 1, fontface = "bold", color = "black") +
  geom_point(data = filter(panel_B_combined_ref, year == 2022),
             aes(x = value, y = stock), size = 2.5, color = "black", shape = 21, fill = "red") +
  geom_text(data = filter(panel_B_combined_ref, year == 2022),
            aes(x = value-5, y = stock, label = year),
            size = 4.5, vjust = -0.9, hjust = 0, fontface = "bold") +

  annotate("text", x = 2650, y = 1150, label = "Regional \ntrajectory",
           size = 4.5, hjust = 1, vjust = 0.5, fontface = "bold.italic", color = "black") +

  annotate("text", x = 2810, y = 900, label = "Commodity \ntrajectory",
           size = 4.5, hjust = 0, vjust = 0.5, fontface = "bold.italic", color = "black") +

  labs(x = "Consumption, million tonnes", y = "Stocks, million tonnes",
       title = "(B) Regional & commodity trajectories") +
  theme_bw() + theme0 +
  theme(panel.grid.major = element_line(linetype = 2, color = "gray50", linewidth = 0.2),
        panel.grid.minor = element_blank(),
        plot.margin = margin(t = 5, r = 10, b = 15, l = 5, unit = "pt")); panel_D



# ASSEMBLY: Combine panels in 2-row layout ----
library(patchwork)

# Row 1: Panels A, B
row2 <- (panel_B + theme(plot.margin = margin(t = 5, r = 10, b = 15, l = 5, unit = "pt")) |
           panel_A + theme(plot.margin = margin(t = 5, r = 10, b = 15, l = 5, unit = "pt")) ) +
  plot_layout(ncol = 2, widths = c(1, 1))

# Row 2: Panel D + Panels B & C as placeholders for E & F
row1 <- (panel_C + theme(plot.margin = margin(t = 5, r = 10, b = 15, l = 5, unit = "pt"), legend.position = "none")|
         panel_D + theme(plot.margin = margin(t = 5, r = 10, b = 15, l = 5, unit = "pt"), legend.position = "none")) +
  plot_layout(ncol = 2, widths = c(1, 1))

# Combine rows
Fig_1 <- row1 / row2 + plot_layout(heights = c(1, 0.95))

# OUTPUT: Save figures ----

Fig_1 %>%
  Write_png("Fig1", .svg = F, .DIR_MODULE = DIR_MODULE, w = 11, h = 11, r = 300)



# Figure 2 ----

# PANEL E (Updated to 2A): Global stock variation by commodity ----
# Shows year-by-year stock changes for all storable commodities (2010–2022).
# Stacked bar chart with red dot indicating net change across full period.
# Final version used: panel_E (with legend positioning for Figure 2).
GCAM_AgLU_SUA_APE_Reg_Mt %>%
  filter(GCAM_commodity %in% COMM_Storage, year %in% c(2011:2022),
         region == "World",
         grepl("Stock Variation", element)) %>%
  select(-SD_Group) %>%
  mutate(GCAM_commodity = factor(GCAM_commodity, levels = COMM_Storage)) ->
  df_panel_E

GCAM_AgLU_SUA_APE_Reg_Mt %>%
  filter(GCAM_commodity %in% COMM_Storage) %>%
  mutate(GCAM_commodity = factor(GCAM_commodity, levels = COMM_Storage)) %>%
  filter(year %in% c(2010:2022),
         region == "World",
         grepl("Stock Variation", element)) ->
  df2


# Net total change by commodity for red dot annotation
df_panel_E %>%
  group_by_at(vars(-value, -year)) %>%
  summarize(value = sum(value), .groups = "drop") -> df_panel_E_total

panel_E0 <- df_panel_E %>%
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = GCAM_commodity, y = value, fill = year),
           color = "black", linewidth = 0.3, stat = "identity") +
  geom_point(data = df_panel_E_total,
             aes(x = GCAM_commodity, y = value),
             fill = "red", shape = 21, color = "black", size = 2.5) +
  annotate("text", x = 3, y = 180,
           label = "Net change\n(2010–2022)", size = 4.5, hjust = 0, vjust = 0,
           color = "darkred", fontface = "italic") +
  scale_fill_viridis_c(name = "Year", labels = function(x) as.character(as.integer(x))) +
  scale_y_continuous(limits = c(-100, 300), breaks = seq(-100, 300, 100), expand = c(0, 0)) +
  labs(x = "Commodity", y = "Stock variation, million tonnes", fill = "Year",
       title = "(A) Global stock variation by commodity"
  ) +
  theme_bw() + theme0 +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 25, size = 11, margin = margin(t = 0),
                                   hjust = 0.8, vjust = 0.8),
        legend.title = element_text(margin = margin(b = 12))
        ); panel_E0

# Create panel_E with legend positioning optimized for Figure 2 assembly
panel_E0 + theme_leg +
  theme(legend.position = c(0.35, 0.95),   # upper-left
        legend.justification = c(0, 1),
        legend.direction = "horizontal",
        legend.title = element_text(margin = margin(r = 20), vjust = 0.8),
        legend.background = element_rect(
          fill = scales::alpha("white", 0.5), color = "NA"),
        legend.key.size = unit(0.9, "cm"),
        legend.text = element_text(size = 12)) ->
  panel_E



# PANEL F (Updated to 2B): Stock-to-gross-use ratio ----
# Calculates closing stocks / (production + imports - exports) by region and sector (2022).
# Ratio indicates storage buffer relative to annual commodity flow.
# Adjusted for Africa & Europe sugar crops (2016 anomaly in other uses).


GCAM_AgLU_SUA_APE_Reg_Mt %>% filter(year >= 2010) %>%
  rename(sector = GCAM_commodity) %>%
  AgSector_Clean %>%
  select(-SD_Group) %>% #filter(region == "China", year == 2022, sector == "Oil crops") |>
  spread(element, value) %>%
  mutate(Net_Export = Export - Import,
         # consumption1 = Food + Feed + Loss + `Other uses`,
         Consumption = Production - Net_Export - `Stock Variation`,
         Consumption1 = Production - Net_Export - `Stock Variation`) |>
  # Adj Africa & Europe 2016 sugar crops
  mutate(Consumption = if_else(region %in% c("Africa", "Europe") & sector == "Sugar crops" & year == 2016,
                               Consumption - `Other uses`,
                               Consumption)) %>%
  #filter(year == 2022, sector == "Oil crops") -> A
  mutate(StockToGrossUse = `Closing stocks`/(Consumption + `Closing stocks`),
         StockToGrossSupply = `Opening stocks`/(Production + `Opening stocks`),
         LossRate_Prod = Loss / Production,
         LossRate_Cons = Loss / Consumption1,
         LossRate = if_else(Net_Export >= 0, LossRate_Prod, LossRate_Cons)) %>%
  gather(element, value, -year:-sector) ->
  GCAM_AgLU_SUA_APE_Reg_Mt_Metrics


GCAM_AgLU_SUA_APE_Reg_Mt_Metrics |>
  filter(region == "World", sector == "Grains",
         element %in% c("LossRate", "Closing stocks", "Stock Variation", "Production")) |>
  spread(element, value) |>
  mutate(Loss = LossRate * `Closing stocks`) |>
  filter(year >= 2011)  |>
  summarize(SB = sum(`Stock Variation`),
            stocks = sum(`Closing stocks`),
            Loss = sum(Loss),
            Production = sum(Production))


GCAM_AgLU_SUA_APE_Reg_Mt_Metrics %>%
  filter(sector %in% c("Grains", "Oil crops", "Sugar crops", "Other crops")) %>%
  mutate(sector = factor(sector, levels = c("Grains", "Oil crops", "Sugar crops", "Other crops"),
                         labels = c("Grains", "Oil\ncrops", "Sugar\ncrops", "Other\ncrops"))) |>
  filter(element %in% c("StockToGrossUse")) %>%
  mutate(element = factor(element, levels = c("StockToGrossUse"),
                          labels = c("Stocks-to-gross-use-ratio"))) %>%
  filter(year == 2022) -> Panel_F_data



# Prepare data for boxplot: exclude World, keep for World separately
Panel_F_regional <- Panel_F_data %>%
  filter(region != "World") %>%
  select(region, sector, value)

Panel_F_world <- Panel_F_data %>%
  filter(region == "World") %>%
  select(sector, value) %>%
  rename(ratio = value)

Panel_F_regional <- Panel_F_regional %>%
  rename(ratio = value)

# Create boxplot with custom whiskers (0-100 percentiles) and box (25-75)
Panel_F_regional %>%
  ggplot(aes(x = sector, y = ratio)) +
  stat_boxplot(geom = "errorbar", aes(ymin = after_stat(ymin), ymax = after_stat(ymax)),
               width = 0, linewidth = 0.5) +
  geom_boxplot(aes(fill = sector), alpha = 0.1, outlier.shape = NA,
               coef = Inf,  # This makes whiskers extend to min/max (0-100 percentiles)
               width = 0.6) +
  scale_fill_brewer(palette = "Dark2", name = "Sector") + guides(fill = "none") +
  new_scale_fill() +
  geom_jitter(aes(fill = region), shape = 21, size = 3.5, alpha = 0.9, width = 0.15) +
  scale_fill_brewer(palette = "Paired", name = "Region") +
  # Add World values as red dots
  geom_errorbar(data = Panel_F_world, aes(x = sector, ymax = ratio, ymin = ratio),
                linetype = 2, color = "blue", linewidth = 0.9) +
  scale_y_continuous(labels = scales::label_percent()) +

  annotate("text", x = 1, y = .45, label = "China",
           size = 4, hjust = 0.5, vjust = 0, color = RColorBrewer::brewer.pal(n = 12, name = "Paired")[2]) +
  annotate("text", x = 1, y = .12, label = "North\nAmerica",
           size = 4, hjust = .5, vjust = 0, color = RColorBrewer::brewer.pal(n = 12, name = "Paired")[6]) +
  annotate("text", x = 3, y = .05, label = "Latin\nAmerica",
           size = 4, hjust = .5, vjust = 0, color = RColorBrewer::brewer.pal(n = 12, name = "Paired")[4]) +
  annotate("text", x = 4, y = .35, label = "Middle\nEast",
           size = 4, hjust = 0.5, vjust = 0, color = RColorBrewer::brewer.pal(n = 12, name = "Paired")[5]) +
  annotate("text", x = 1.6, y = .03, label = "Pacific\nOECD",
           size = 4, hjust = 0.5, vjust = 0, color = RColorBrewer::brewer.pal(n = 12, name = "Paired")[7]) +

  annotate("text", x = 4, y = .06, label = "Rest of\nAsia",
           size = 4, hjust = 0.5, vjust = 0, color = RColorBrewer::brewer.pal(n = 12, name = "Paired")[9]) +

  annotate("text", x = 2.4, y = .23, label = "World",
           size = 4, hjust = 0, vjust = 0, color = "blue") +

  labs(x = "Sector", y = "Stock-to-gross-use ratio",
       title = "(B) Stock-to-gross-use ratio (2022)",
       fill = "Sector") +
  theme_bw() + theme0 +
  theme(# legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5))-> panel_F; panel_F


# PANEL G (Updated to 2C): Storage loss rate by region and sector ----
# Displays distribution of regional loss rates for major commodity sectors (2022).
# Boxplot shows min/max range (whiskers to full data range) with individual points by region.
# Dashed line indicates world average loss rate.
# Final version used: panel_G (region on x-axis, sector colors for points).
# Alternative version: panel_G1 (sector on x-axis; retained for reference).

GCAM_AgLU_SUA_APE_Reg_Mt_Metrics %>%
  filter(sector %in% c("Grains", "Oil crops", "Sugar crops", "Other crops")) %>%
  # mutate(sector = factor(sector, levels = c("Grains", "Oil crops", "Sugar crops", "Other crops"),
  #                        labels = c("Grains", "Oil\ncrops", "Sugar\ncrops", "Other\ncrops"))) |>
  filter(element %in% c("LossRate")) %>%
  mutate(element = factor(element, levels = c("LossRate"),
                          labels = c("Storage loss rate"))) %>%
  filter(year == 2022) -> Panel_G_data


# Prepare data for boxplot: exclude World, keep for World separately
Panel_G_regional <- Panel_G_data %>%
  filter(region != "World") %>%
  select(region, sector, value) |>
  mutate(sector = factor(sector, levels = c("Grains", "Oil crops", "Sugar crops", "Other crops"),
                         labels = c("Grains", "Oil\ncrops", "Sugar\ncrops", "Other\ncrops")))

Panel_G_world <- Panel_G_data %>%
  filter(region == "World") %>%
  select(sector, value) |>
  mutate(sector = factor(sector, levels = c("Grains", "Oil crops", "Sugar crops", "Other crops"),
                         labels = c("Grains", "Oil\ncrops", "Sugar\ncrops", "Other\ncrops")))



Panel_G_regional %>%
  ggplot(aes(x = sector, y = value)) +
  stat_boxplot(geom = "errorbar", aes(ymin = after_stat(ymin), ymax = after_stat(ymax)),
               width = 0, linewidth = 0.7, coef = Inf) +
  geom_boxplot(aes(fill = sector), alpha = 0.1, outlier.shape = NA,
               coef = Inf,  # This makes whiskers extend to min/max (0-100 percentiles)
               width = 0.6) +
  scale_fill_brewer(palette = "Dark2", name = "Sector") + guides(fill = "none") +
  new_scale_fill() +
  geom_jitter(aes(fill = region), shape = 21, size = 3.5, alpha = 0.9, width = 0.15) +
  scale_fill_brewer(palette = "Paired", name = "Region") +
  # Add World values as red dots
  geom_errorbar(data = Panel_G_world, aes(x = sector, ymax = value, ymin = value),
                linetype = 2, color = "blue", linewidth = 0.9) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Sector", y = "Loss rate",
       title = "(C) Storage loss rate (2022)") +
  theme_bw() + theme0 +
  theme(# legend.position = "none",
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)) -> panel_G; panel_G


# ALTERNATIVE: Version with region on x-axis (panel_G1)
# This alternative shows loss rates ordered by Grains sector values.
# Retained for reference but not used in final Figure 2.

Panel_G_data %>%
  group_by(region, year) |>
  mutate(value1 = value[sector == "Grains"]) |> ungroup() |>
  #mutate(value1 = median(value)) |> ungroup() |>
  ggplot(aes(x = reorder(region, -value1), y = value)) +
  stat_boxplot(geom = "errorbar", aes(ymin = after_stat(ymin), ymax = after_stat(ymax)),
               width = 0, linewidth = 0.7, coef = Inf) +
  # geom_boxplot(aes(fill = region), alpha = 0.1, outlier.shape = NA,
  #              coef = Inf,  # This makes whiskers extend to min/max (0-100 percentiles)
  #              width = 0.6)+
  scale_color_brewer(palette = "Paired", name = "Region") + guides(fill = "none") +
  new_scale_fill() +
  geom_jitter(aes(fill = sector), shape = 22, size = 3, alpha = 0.9, width = 0.15) +
  scale_fill_brewer(palette = "Dark2", name = "Sector") +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Region", y = "Loss rate",
       title = "(C) Storage loss rate (2022)") +
  theme_bw() + theme0 +
  theme(# legend.position = "none",
    axis.text.x = element_text(angle = 25, size = 11, margin = margin(t = 0),
                               hjust = 0.8, vjust = 0.8)) -> panel_G1; panel_G1




# PANEL H (Updated to 2D): Dietary energy from stocks vs. total food availability ----
# Dual visualization comparing energy content of stored crops (stacked bars) with
# total food availability (overlaid points) by region (2022).
# Stacked bars show caloric contribution by commodity sector (Grains, Oil crops, etc.).
# Points indicate total food availability per capita per day for comparison.

FAO_Food_Macronutrient_All %>%
  filter(macronutrient == "MKcal") %>%
  left_join_error_no_match(GCAM_region_names) %>%
  group_by(region, year, GCAM_commodity, macronutrient) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  spread(macronutrient, value)->
  Food_Calories

FAO_Stock_Macronutrient_All %>%
  filter(macronutrient == "MKcal") %>%
  left_join_error_no_match(GCAM_region_names) %>%
  group_by(region, year, GCAM_commodity, macronutrient) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  spread(macronutrient, value)->
  Stock_Calories

Stock_Calories %>%
  rename(value = MKcal, sector = GCAM_commodity) %>%
  Reg_Clean() %>%
  AgSector_Clean() %>% mutate(source = "Stocks") %>%
  filter(sector %in% c("Grains", "Oil crops", "Sugar crops", "Other crops")) %>%
  bind_rows(
    Food_Calories %>%
      rename(value = MKcal, sector = GCAM_commodity) %>%
      Reg_Clean() %>%
      #filter(sector != "NEC") %>%
      #AgSector_Clean() %>%
      mutate(source = "Food", sector = "AllFood")
  ) %>%
  group_by_at(vars(-value)) %>%
  summarize(value = sum(value)) %>%
  left_join_error_no_match(
    L101.Pop_thous_Scen_R_Y %>%
      filter(year >= 2010, scenario == "SSP2") %>%
      left_join_error_no_match(GCAM_region_names) %>%
      select(region, year, value) %>%
      Reg_Clean %>%
      rename(pop = value)
  ) %>%
  # MKcal to kcal and k person to person
  mutate(kcalPcPd = value * 10^6  / (pop*1000) / 365 ) ->
  Stock_Food_Calories



Stock_Food_Calories %>% filter(year == 2022, source == "Stocks") %>%
  group_by(region) %>%
  mutate(tot = sum(kcalPcPd)) %>% ungroup() %>%
  mutate(sector = factor(sector, levels = c("Grains", "Oil crops", "Sugar crops", "Other crops")
                         )) |>
  ggplot() +

  geom_bar(aes(x = reorder(region, -tot), y = kcalPcPd, fill =  sector),
           color = "black", linewidth = 0.3, stat = "identity", alpha = 0.9) +
  scale_fill_brewer(palette = "Dark2",name = "Sector") +

  new_scale_fill() +
  geom_point(data = Stock_Food_Calories %>% filter(year == 2022, source == "Food"),
             aes(x = region, y = kcalPcPd, fill = region), alpha = 0.9,
             color = "black", shape = 21, size = 4) +
  scale_fill_brewer(palette = "Paired", name = "Region") +

  annotate("text", x = 7, y = 2100,
           label = "Stocks by crop sectors",
           size = 4.5, hjust = 0, vjust = 0.5, angle = -25,
           color = "black", fontface = "bold.italic") +

  annotate("text", x = 5.7, y = 3500,
           label = "Total food availability",
           size = 4.5, hjust = 0, vjust = 0.5,
           color = "black", fontface = "bold.italic") +

  annotate("text", x = 0.7, y = 2400,
           label = "Grains",
           size = 4.5, hjust = 0, vjust = 0.5,
           color = "darkgreen") +

  annotate("text", x = 0.7, y = 1200,
           label = "Oil crops",
           size = 4.5, hjust = 0, vjust = 0.5,
           color = "darkgoldenrod4") +

  annotate("text", x = 0.7, y = 600,
           label = "Sugar crops",
           size = 4.5, hjust = 0, vjust = 0.5,
           color = "darkblue") +


  annotate("text", x = 0.7, y = 130,
           label = "Other crops",
           size = 4.5, hjust = 0, vjust = 0.5,
           color = "deeppink4") +


  scale_y_continuous(limits = c(0, 4500), expand = c(0, 0)) +
  labs(x = "Region", y = "Kilocalories per capita per day", fill = "Region",
       title = "(D) Stocks in dietary energy (2022)") +
  theme_bw() + theme0 +
  theme(#panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 25, size = 11, margin = margin(t = 0),
                               hjust = 0.8, vjust = 0.8)) ->
  panel_H; panel_H




# ASSEMBLY: Combine panels in 2-row layout ----
library(patchwork)
library(cowplot)

# Row 1: Panels E & F
row1 <- (panel_E0 +
           theme(plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt")) |
           panel_F +
           theme(plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt"),
                 legend.position = "none")) +
  plot_layout(ncol = 2, widths = c(1, 1))

# Row 2: Panels G & H
row2 <- (panel_G + theme(plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt"),
                         legend.position = "none") |
           panel_H + theme(plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt"))) +
  plot_layout(ncol = 2, widths = c(1, 1))

# Combine rows and collect legends
Fig_2 <- row1 / row2 + plot_layout(guides = "collect", heights = c(1, 1)) &
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.margin = margin(l = 10, unit = "pt"),
        legend.spacing.y = unit(50, "pt"),
        legend.key.height = unit(20, "pt"))

# OUTPUT: Save figures ----


Fig_2 %>%
  Write_png("Fig2", .svg = F, .DIR_MODULE = DIR_MODULE, w = 13, h = 11, r = 300)

