
# ImpactData ----

library(forcats)
library(stringr)


Scenario_YieldShocks <- "HG"
Scenario_YieldShocks_All <-
  c("S1_Dynamic", "S3_Dynamic_HG", "S2_Hidden", "S4_Hidden_HG", "S5_FixedRatio", "S6_FixedRatio_HG" )

# Scenario_YieldShocks <- "GL"
# Scenario_YieldShocks_All <-
#   c("S1_Dynamic", "S3_Dynamic_GL", "S2_Hidden", "S4_Hidden_GL", "S5_FixedRatio", "S6_FixedRatio_GL")

# Get impact data ready ----
## Bind elements needed ----
bind_rows(AgElement_SUA_Demand,
          AgElement_SUA_Storage,
          AgElement_SUA_Trade,
          AgElement_SUA_ProdValue,
          AgElement_Input_LandYield,
          AgElement_Input_LandBioYield) %>%
  filter(!sector %in% c("Forest", "Fodder crops", "Energy crops", "Pasture")) %>%
  select(branch, scenario, element, year, region, sector, value) ->
  AgElement0


bind_rows(
  AgElement0 %>%
    filter(element %in% c("Export", "Import")) %>%
    spread(element, value) %>%
    mutate(`Net export` = Export - Import) %>%
    gather(element, value, -branch:-sector) %>%
    filter(!element %in% c("Export", "Import")),
  AgElement0 %>%
    filter(element %in% c("Closing stocks", "Opening stocks", "Stock loss")) %>%
    spread(element, value) %>%
    mutate(`Stock variation` = `Closing stocks` - `Opening stocks`) %>%
    gather(element, value, -branch:-sector) %>%
    filter(!element %in% c("Closing stocks", "Opening stocks", "Stock loss")),
  AgElement0 %>%
    filter(element %in% c("Closing stocks_Pcal", "Opening stocks_Pcal")) %>%
    spread(element, value) %>%
    mutate(`Stock variation_Pcal` = `Closing stocks_Pcal` - `Opening stocks_Pcal`) %>%
    gather(element, value, -branch:-sector) %>%
    filter(element %in% c("Stock variation_Pcal")),
  AgElement0 %>%
    filter(element %in% c("Bioenergy", "Food", "Feed", "Other use")) %>%
    group_by_at(vars(-element, -value)) %>%
    summarize(value = sum(value, na.rm = T), .groups = "drop") %>%
    mutate(element = "Consumption"),
  AgElement0 %>%
    filter(element %in% c("Food", "Feed", "Other use")) %>%
    group_by_at(vars(-element, -value)) %>%
    summarize(value = sum(value, na.rm = T), .groups = "drop") %>%
    mutate(element = "Consumption_NB"),
  AgElement0) ->
  AgElement


AgElement %>% distinct(element) %>% pull
AgElement %>% distinct(sector) %>% pull
AgElement |> distinct(scenario)


AgElement |>
  filter(scenario %in% Scenario_YieldShocks_All) ->
  AgElement

# Cumulative impact ----


ImpactData_pct <-
  AgElement %>%
  filter(!element %in% c("Stock variation_Pcal", "Stock variation", "Net export"),
         sector %in% c("Grains", "Oil crops", "Sugar crops", "Other crops"),
         year >= 2020) %>%
  mutate(
    scenario_shock   = if_else(grepl(Scenario_YieldShocks, scenario), Scenario_YieldShocks, "Smooth"),
    scenario_storage = case_when(
      grepl("FixedRatio", scenario) ~ "Fixed-Ratio",
      grepl("Hidden", scenario)     ~ "Hidden",
      TRUE                          ~ "Dynamic"
    )
  ) %>%
  group_by(scenario_storage, region, year, sector, element) %>%
  summarise(
    value = {
      v_smooth <- value[scenario_shock == "Smooth"]
      v_hg     <- value[scenario_shock != "Smooth"]
      #if (length(v_smooth) == 1 && length(v_hg) == 1) v_hg / v_smooth - 1 else NA_real_
      if (length(v_smooth) == 1 && length(v_hg) == 1 &&
          is.finite(v_smooth) && is.finite(v_hg) &&
          v_smooth > 0 && v_hg > 0) {
        log(v_hg / v_smooth)
      } else {
        NA_real_
      }
    },
    .groups = "drop"
  ) %>%
  filter(!is.na(value)) %>%
  mutate(scenario = Scenario_YieldShocks)  # optional placeholder if you still want scenario column


ImpactData_abs <-
  AgElement %>%
  filter(element %in% c("Stock variation_Pcal", "Stock variation", "Net export"),
         sector %in% c("Grains", "Oil crops", "Sugar crops", "Other crops"),
         year >= 2020) %>%
  mutate(
    scenario_shock   = if_else(grepl(Scenario_YieldShocks, scenario), Scenario_YieldShocks, "Smooth"),
    scenario_storage = case_when(
      grepl("FixedRatio", scenario) ~ "Fixed-Ratio",
      grepl("Hidden", scenario)     ~ "Hidden",
      TRUE                          ~ "Dynamic"
    )
  ) %>%
  group_by(scenario_storage, region, year, sector, element) %>%
  summarize(
    value = {
      v_smooth <- value[scenario_shock == "Smooth"]
      v_hg     <- value[scenario_shock != "Smooth"]
      if (length(v_smooth) == 1 && length(v_hg) == 1) v_hg - v_smooth else NA_real_
    },
    .groups = "drop"
  ) %>%
  filter(!is.na(value)) %>%
  spread(element, value) %>%
  # Zero stock variation impacts in Hidden
  replace_na(list(`Stock variation` = 0)) %>%
  gather(element, value, `Net export`, `Stock variation`, `Stock variation_Pcal`) %>%
  mutate(scenario = Scenario_YieldShocks)



## Bind impact data ----
ImpactData_cum <-
  bind_rows(ImpactData_pct, ImpactData_abs) %>%
  mutate(scenario_storage = factor(scenario_storage,
                                   levels = c("Dynamic", "Hidden","Fixed-Ratio")))


# Temporally marginal impacts

# ImpactData_pct_cross ----

ImpactData_pct_cross <-
  AgElement %>%
  filter(!element %in% c("Stock variation_Pcal", "Stock variation", "Net export"),
         sector %in% c("Grains", "Oil crops", "Sugar crops", "Other crops"),
         year >= 2015) %>%
  mutate(
    scenario_shock = if_else(grepl(Scenario_YieldShocks, scenario), Scenario_YieldShocks, "Smooth"),
    scenario_storage = case_when(
      grepl("FixedRatio", scenario) ~ "Fixed-Ratio",
      grepl("Hidden", scenario)     ~ "Hidden",
      TRUE                          ~ "Dynamic"
    )
  ) %>%
  group_by_at(vars(-year, -value)) %>%
  mutate(
    base_value = lag(value),
    value_norm = value / base_value
  ) %>%
  ungroup() %>% filter(year >= 2020) %>%
  select(-base_value) %>%
  group_by(scenario_storage, region, year, sector, element) %>%
  summarize(
    value = {
      v_hg     <- value_norm[scenario_shock == Scenario_YieldShocks]
      v_smooth <- value_norm[scenario_shock == "Smooth"]

      if (length(v_hg) == 1 && length(v_smooth) == 1 &&
          is.finite(v_hg) && is.finite(v_smooth) &&
          v_hg > 0 && v_smooth > 0) {
        log(v_hg / v_smooth)
      } else {
        NA_real_
      }
    },
    .groups = "drop"
  )
  # summarize(
  #   value =
  #     value_norm[scenario_shock == Scenario_YieldShocks] /
  #     value_norm[scenario_shock == "Smooth"] -1,
  #   .groups = "drop"
  # ) %>%



ImpactData_abs_cross <-
  AgElement %>%
  filter(element %in% c("Stock variation_Pcal", "Stock variation", "Net export"),
         sector %in% c("Grains", "Oil crops", "Sugar crops", "Other crops"),
         year >= 2021) %>%
  mutate(
    scenario_shock = if_else(grepl(Scenario_YieldShocks, scenario), Scenario_YieldShocks, "Smooth"),
    scenario_storage = case_when(
      grepl("FixedRatio", scenario) ~ "Fixed-Ratio",
      grepl("Hidden", scenario)     ~ "Hidden",
      TRUE                          ~ "Dynamic"
    )
  ) %>%
  group_by_at(vars(-year, -value)) %>%
  mutate(
    base_value = lag(value),
    value_norm = value - base_value
  ) %>%
  ungroup() %>%
  group_by(scenario_storage, region, year, sector, element) %>%
  summarise(
    value =
      value_norm[scenario_shock == Scenario_YieldShocks] -
      value_norm[scenario_shock == "Smooth"],
    .groups = "drop"
  ) %>% filter(year >= 2025)

ImpactData_cross <-
  bind_rows(ImpactData_pct_cross, ImpactData_abs_cross)%>%
  mutate(scenario_storage = factor(scenario_storage,
                                   levels = c("Dynamic", "Hidden", "Fixed-Ratio")))


# Done ----



# Ele <- c("BioYield_Land",
#          "Yield_Land", "Area", "Production",
#          "Export",
#          "Closing stocks", "Stock loss",
#          "Import", "Consumption",
#          "Bioenergy", "Consumption_NB",
#          "Price", "Revenue")
#
# EleLab <- c("Biophysical Yield",
#             "Realized Yield", "Area harvested", "Production",
#             "Export",
#             "Closing stocks", "Stock loss",
#             "Import", "Consumption",
#             "Bioenergy", "Consumption_NB",
#             "Price", "Revenue")
#
# year0 <- 2050  # pick your year
#
# df0 <- ImpactData_cross %>%
#   filter(region != "World", year == year0, element %in% Ele) %>%
#   mutate(
#     element = factor(element, levels = Ele, labels = EleLab),
#     scenario_storage = factor(scenario_storage, levels = c("Hidden", "Dynamic", "Fixed-Ratio"))
#   )
#
# # Box stats: 5/25/50/75/95
# box_stats <- df0 %>%
#   group_by(element, scenario_storage) %>%
#   summarise(
#     ymin   = quantile(value, 0.05, na.rm = TRUE),
#     lower  = quantile(value, 0.25, na.rm = TRUE),
#     middle = quantile(value, 0.50, na.rm = TRUE),
#     upper  = quantile(value, 0.75, na.rm = TRUE),
#     ymax   = quantile(value, 0.95, na.rm = TRUE),
#     .groups = "drop"
#   )
#
# # Points: only 5th and 95th percentiles (consistent with whiskers)
# pt_stats <- box_stats %>%
#   select(element, scenario_storage, p05 = ymin, p95 = ymax) %>%
#   pivot_longer(c(p05, p95), names_to = "pt", values_to = "value")
#
# shape_map <- c("Hidden" = 21, "Dynamic" = 22, "Fixed-Ratio" = 23)
#
# # Set1 palette mapped to your scenario_storage levels
# pal <- brewer.pal(n = 3, name = "Set1")
# names(pal) <- levels(df0$scenario_storage)
#
# ggplot() +
#   geom_boxplot(
#     data = box_stats,
#     aes(
#       x = element,
#       ymin = ymin, lower = lower, middle = middle, upper = upper, ymax = ymax,
#       color = scenario_storage,
#       group = interaction(element, scenario_storage)
#     ),
#     stat = "identity",
#     position = position_dodge(width = 0.75),
#     fill = NA,
#     linewidth = 0.35,
#     width = 0.6
#   ) +
#   geom_point(
#     data = pt_stats,
#     aes(
#       x = element,
#       y = value,
#       color = scenario_storage,
#       shape = scenario_storage,
#       group = interaction(element, scenario_storage)
#     ),
#     position = position_dodge(width = 0.75),
#     fill = NA,     # no fill, border only
#     alpha = 0.3,
#     size = 2.0,
#     stroke = 0.9
#   ) +
#   scale_color_manual(values = pal) +
#   scale_shape_manual(values = shape_map) +
#   theme_bw() + theme0 +
#   labs(
#     x = NULL,
#     y = "Climate vs No-Climate",
#     title = paste0("Impacts in ", year0, " (p05–p95 whiskers; p05/p95 points)")
#   ) +
#   theme(
#     panel.grid = element_blank(),
#     legend.title = element_blank(),
#     axis.text.x = element_text(angle = 35, hjust = 1)
#   )
