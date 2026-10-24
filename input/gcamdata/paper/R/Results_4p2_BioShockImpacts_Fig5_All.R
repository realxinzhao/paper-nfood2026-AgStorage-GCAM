# =============================================================================
# Results_4p2_BioShockImpacts_Fig5_AB.R
#
# Figure 5: Yield shock transmission across storage scenarios
#
# Panel A — Nelson et al. (2014)-style scatter + OLS regression
#   Data  : ImpactData_cum, Dynamic scenario, cumulative to 2050
#   x-axis: cumulative biophysical yield shock
#   y-axis: cumulative relative response (%)
#   Color : outcome element (8 variables); lines fitted on full data,
#           displayed within ±25% window (slopes unaffected by clipping)
#
# Panel B — same format as A but period-to-period (ImpactData_cross, all years)
#
# Panel C — transmission coefficients β by storage scenario
#   FE (region×sector) estimate ± 95% CI + IQR of pair-level slopes
#   Colors match Panels A & B; no legend (facet label identifies element)
#
# Requires:
#   ImpactData_cum, ImpactData_cross, theme0
#   results_beta2 for Panel C (auto-sourced if absent)
# =============================================================================

library(dplyr)
library(purrr)
library(ggplot2)
library(scales)
library(patchwork)
library(cowplot)


# =============================================================================
# 0. Shared constants ----
# =============================================================================

.ImpactSector <- c("Grains", "Oil crops", "Sugar crops", "Other crops")
.ImpactYear   <- seq(2025, 2100, 5)

# Element mapping: raw GCAM names → display labels (order = legend order)
Ele_A    <- c("Yield_Land", "Area", "Production", "Export",
              "Closing stocks", "Import", "Consumption", "Price")
EleLab_A <- c("Realized Yield", "Area harvested", "Production", "Export",
              "Closing stocks", "Import", "Consumption", "Price")

# Okabe-Ito colorblind-safe palette (8 colors incl. black for Realized Yield)
# Alternative: ggsci::pal_npg()(8) for the NPG house palette
ele_colors <- c(
  "Realized Yield" = "#000000",  # black   (sits on 1:1 reference line)
  "Area harvested" = "#E69F00",  # orange
  "Production"     = "#009E73",  # bluish green
  "Export"         = "#0072B2",  # blue
  "Closing stocks" = "#D55E00",  # vermillion (Okabe-Ito)
  "Import"         = "#56B4E9",  # sky blue
  "Consumption"    = "#999999",  # grey
  "Price"          = "#CC79A7"   # reddish purple
)

# Display window for Panels A & B.
# OLS fitted on ALL data; predict() evaluated on this grid → slopes unaffected.
view_lim <- c(-0.25, 0.25)

out_dir <- "paper/output/YieldShockTransmission/Regression_WaldTest"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# =============================================================================
# 1. Panel A data — cumulative to 2050 ----
# =============================================================================

x_cum <- ImpactData_cum |>
  filter(element == "BioYield_Land",
         region  != "World",
         year    == 2050,
         sector  %in% .ImpactSector,
         scenario_storage == "Dynamic") |>
  select(region, year, sector, scenario_storage, BioYield_Land = value)

panelA_data <- ImpactData_cum |>
  filter(region  != "World",
         year    == 2050,
         sector  %in% .ImpactSector,
         scenario_storage == "Dynamic",
         element %in% Ele_A) |>
  mutate(element = factor(element, levels = Ele_A, labels = EleLab_A)) |>
  left_join(x_cum, by = c("region", "year", "sector", "scenario_storage")) |>
  filter(!is.na(value), !is.na(BioYield_Land))


# =============================================================================
# 2. Panel B data — period-to-period (all years 2025–2100) ----
# =============================================================================

x_cross <- ImpactData_cross |>
  filter(element == "BioYield_Land",
         region  != "World",
         year    %in% .ImpactYear,
         sector  %in% .ImpactSector,
         scenario_storage == "Dynamic") |>
  select(region, year, sector, scenario_storage, BioYield_Land = value)

panelB_data <- ImpactData_cross |>
  filter(region  != "World",
         year    %in% .ImpactYear,
         sector  %in% .ImpactSector,
         scenario_storage == "Dynamic",
         element %in% Ele_A) |>
  mutate(element = factor(element, levels = Ele_A, labels = EleLab_A)) |>
  left_join(x_cross, by = c("region", "year", "sector", "scenario_storage")) |>
  filter(!is.na(value), !is.na(BioYield_Land))


# =============================================================================
# 3. Pre-fit OLS per element (full data) → predict on ±25% display grid ----
# =============================================================================
# Fitting on full data ensures slopes are unbiased by the display window.
# The ±25% grid is only the range over which we evaluate predictions for display.

fit_lines <- function(df) {
  x_seq <- seq(view_lim[1], view_lim[2], length.out = 200)
  df |>
    group_by(element) |>
    group_modify(~{
      m    <- lm(value ~ BioYield_Land, data = .x)
      pred <- predict(m, newdata = tibble(BioYield_Land = x_seq),
                      interval = "confidence", level = 0.95)
      tibble(BioYield_Land = x_seq,
             fit = pred[, "fit"], lwr = pred[, "lwr"], upr = pred[, "upr"])
    }) |>
    ungroup()
}

lines_A <- fit_lines(panelA_data)
lines_B <- fit_lines(panelB_data)


# =============================================================================
# 4. Panel builder — shared by A and B ----
# =============================================================================

make_panel <- function(raw_data, fitted_lines, title_str, xlab, ylab,
                       stocks_label = EleLab_A[Ele_A == "Closing stocks"]) {
  ggplot() +
    geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
    geom_vline(xintercept = 0, color = "grey60", linewidth = 0.4) +
    # 1:1 reference line
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                color = "grey50", linewidth = 0.5) +
    geom_abline(slope = -1, intercept = 0, linetype = "dashed",
                color = "grey50", linewidth = 0.5) +
    # Scatter points for Closing stocks only.
    # Map a new variable (obs_label) to shape so it gets its own legend section,
    # separate from the color/line legend. color = element keeps the right color.
    geom_point(
      data = filter(raw_data, element == stocks_label) |>
               mutate(obs_label = stocks_label),
      aes(x = BioYield_Land, y = value, color = element, shape = obs_label),
      alpha = 0.25, size = 2.5, show.legend = c(color = FALSE, shape = TRUE)
    ) +
    # 95% CI ribbon from pre-fitted OLS (clipped at ±25% by scale limits)
    geom_ribbon(
      data = fitted_lines,
      aes(x = BioYield_Land, ymin = lwr, ymax = upr, fill = element),
      alpha = 0.12, show.legend = FALSE
    ) +
    # OLS fitted line
    geom_line(
      data = fitted_lines,
      aes(x = BioYield_Land, y = fit, color = element),
      linewidth = 0.8
    ) +
    scale_color_manual(values = ele_colors, limits = names(ele_colors),
                       guide  = guide_legend(order = 1)) +
    scale_fill_manual(values = ele_colors, name = NULL,
                      limits = names(ele_colors), guide = "none") +
    scale_shape_manual(values = setNames(16L, stocks_label),
                       name   = "Observations",
                       labels = stocks_label,
                       guide  = guide_legend(order = 2,
                         override.aes = list(size = 3, alpha = 0.7,
                                             linetype = 0))) +
    scale_x_continuous(labels = percent_format(accuracy = 1),
                       limits = view_lim,
                       breaks = seq(-0.2, 0.2, by = 0.1),
                       expand = expansion(mult = 0.02)) +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       limits = view_lim,
                       breaks = seq(-0.2, 0.2, by = 0.1),
                       expand = expansion(mult = 0.02)) +
    labs(title = title_str, x = xlab, y = ylab, color = "Fitted responses") +
    theme_bw() + theme0 + theme_leg +
    theme(aspect.ratio = 1) +
    theme(panel.grid.minor = element_blank())
}


# =============================================================================
# 5. Build Panel A ----
# =============================================================================

fig5_panelA <- make_panel(
  panelA_data, lines_A,
  title_str = "(A) Cumulative yield shock transmission",
  xlab      = "Cumulative biophysical yield shock, 2021-2050",
  ylab      = "Relative response (cumulative)"
)

fig5_panelA


# =============================================================================
# 6. Build Panel B ----
# =============================================================================

fig5_panelB <- make_panel(
  panelB_data, lines_B,
  title_str = "(B) Period-to-period yield shock transmission",
  xlab      = "Period-to-period biophysical yield shock",
  ylab      = "Relative response (period-to-period)"
)

fig5_panelB


# =============================================================================
# 7. Panel C — transmission coefficients ----
#
# Source: regression script (auto-sourced if results_beta2 not in session).
# Element labels remapped to match Panels A & B (Stocks, Consumption).
# Color = element color from ele_colors; scenarios distinguished by x position.
# No legend — facet strip identifies element, x-axis labels identify scenario.
# =============================================================================

if (!exists("results_beta2"))
  source("paper/R/Results_4p2_BioShockImpacts_Regression_FitFourStat.R")

ele_remap_C     <- c("Closing stocks (Mt)" = "Closing stocks",
                     "Consumption (Total)" = "Consumption")
fig_ele_order_C <- c("Realized Yield", "Area harvested", "Production",
                     "Export", "Closing stocks", "Import", "Consumption", "Price")

coef_fe <- results_beta2 |>
  filter(element %in% c(names(ele_remap_C), fig_ele_order_C),
         is.na(sector), model == "fe_pair") |>
  mutate(
    element          = dplyr::recode(as.character(element), !!!ele_remap_C),
    element          = factor(element, levels = fig_ele_order_C),
    scenario_storage = factor(scenario_storage,
                              levels = c("Dynamic", "Hidden", "Fixed-Ratio")),
    ci_lo = beta - 1.96 * se,
    ci_hi = beta + 1.96 * se
  )

coef_ind <- results_beta2 |>
  filter(element %in% c(names(ele_remap_C), fig_ele_order_C),
         is.na(sector), model == "individual") |>
  mutate(
    element          = dplyr::recode(as.character(element), !!!ele_remap_C),
    element          = factor(element, levels = fig_ele_order_C),
    scenario_storage = factor(scenario_storage,
                              levels = c("Dynamic", "Hidden", "Fixed-Ratio"))
  )

fig5_panelC <-
  ggplot(mapping = aes(x = scenario_storage)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey55", linewidth = 0.5) +
  geom_linerange(
    data = coef_ind,
    aes(ymin = beta_p25, ymax = beta_p75,
        group = scenario_storage, color = element),
    linewidth = 4, alpha = 0.20,
    position = position_dodge(width = 0.6)
  ) +
  geom_pointrange(
    data = coef_fe,
    aes(y = beta, ymin = ci_lo, ymax = ci_hi, color = element),
    position = position_dodge(width = 0.6),
    linewidth = 0.7, size = 0.5
  ) +
  scale_color_manual(values = ele_colors, guide = "none") +
  facet_wrap(~element, scales = "free_y", ncol = 4) +
  labs(
    title = "(C) Period-to-period transmission elasticities by storage representation",
    x     = "Storage representation",
    y     = "Transmission elasticity"
  ) +
  theme_bw() + theme0 +
  theme(axis.text.x   = element_text(angle = 30, hjust = 1, vjust = 1),
        legend.position = "none")

fig5_panelC


# =============================================================================
# 8. Assemble Fig 5 (A + B + C) and export ----
#
# Alignment: A, B, C passed individually to cowplot::align_plots so each
# panel's gtable column widths are equalized before stacking. Passing A+B
# as a patchwork object would make them opaque to cowplot's alignment.
# =============================================================================



fig5_AB <- (fig5_panelA + fig5_panelB) + #theme_leg +
  plot_layout(nrow = 1, guides = "collect") &
  theme(legend.position = "right")

fig5_ABC <- fig5_AB/fig5_panelC

DIR_MODULE <- "YieldShockTransmission"
fig5_ABC %>%
  Write_png(paste0("Fig5", Scenario_YieldShocks),
            .svg = T, .DIR_MODULE = DIR_MODULE, w = 16, h = 16, r = 300)


# =============================================================================
# Coefficient table for Nature Food ----
#
# OLS regression: value ~ BioYield_Land, per scenario × element combination.
# Three datasets:
#   Cumulative 2050  — ImpactData_cum,   year == 2050, all scenarios/regions
#   Cumulative 2100  — ImpactData_cum,   year == 2100, all scenarios/regions
#   Period-to-period — ImpactData_cross, all years,    all scenarios/regions
#
# Columns: data_type, scenario_storage, element, beta, se, p_value, n
# =============================================================================

library(broom)

.AllScenarios <- c("Dynamic", "Hidden", "Fixed-Ratio")

# Helper: fit OLS per scenario × element and return tidy coefficients
coef_table_from <- function(impact_data, year_filter = NULL, data_label) {

  # Pre-filter by year if supplied (avoids magrittr-style '.' in |> pipes)
  d_filtered <- if (!is.null(year_filter))
    filter(impact_data, year %in% year_filter) else impact_data

  x_tbl_ct <- d_filtered |>
    filter(element == "BioYield_Land", region != "World",
           sector %in% .ImpactSector,
           scenario_storage %in% .AllScenarios) |>
    select(scenario_storage, region, year, sector, BioYield_Land = value)

  d_filtered |>
    filter(element %in% Ele_A, region != "World",
           sector %in% .ImpactSector,
           scenario_storage %in% .AllScenarios) |>
    mutate(element = factor(element, levels = Ele_A, labels = EleLab_A)) |>
    select(scenario_storage, region, year, sector, element, value) |>
    left_join(x_tbl_ct,
              by = c("scenario_storage", "region", "year", "sector")) |>
    filter(!is.na(value), !is.na(BioYield_Land)) |>
    group_by(scenario_storage, element) |>
    group_modify(~{
      d <- .x
      if (nrow(d) < 5 || sd(d$BioYield_Land, na.rm = TRUE) == 0)
        return(tibble(beta = NA_real_, se = NA_real_,
                      p_value = NA_real_, n = nrow(d)))
      m  <- lm(value ~ BioYield_Land, data = d)
      tt <- tidy(m) |> filter(term == "BioYield_Land")
      tibble(beta = tt$estimate, se = tt$std.error,
             p_value = tt$p.value, n = nrow(d))
    }) |>
    ungroup() |>
    mutate(data_type = data_label)
}

# Build the three components
coef_cum2050 <- coef_table_from(ImpactData_cum,   year_filter = 2050,
                                 data_label = "Cumulative 2050")
coef_cum2100 <- coef_table_from(ImpactData_cum,   year_filter = 2100,
                                 data_label = "Cumulative 2100")
coef_p2p     <- coef_table_from(ImpactData_cross, year_filter = .ImpactYear,
                                 data_label = "Period-to-period")

# Stack, order, and round
coef_table <- bind_rows(coef_cum2050, coef_cum2100, coef_p2p) |>
  mutate(
    data_type        = factor(data_type,
                              levels = c("Cumulative 2050",
                                         "Cumulative 2100",
                                         "Period-to-period")),
    scenario_storage = factor(scenario_storage, levels = .AllScenarios),
    element          = factor(element,          levels = EleLab_A)
  ) |>
  arrange(data_type, element, scenario_storage) |>
  mutate(across(c(beta, se), \(x) round(x, 4)),
         p_value = signif(p_value, 3)) |>
  select(data_type, element, scenario_storage, beta, se, p_value, n)

coef_table

# Export
write.csv(coef_table,
          file = file.path(out_dir, "Table_Coefficients_AllScenarios.csv"),
          row.names = FALSE)
message("Coefficient table saved to: ", out_dir)
