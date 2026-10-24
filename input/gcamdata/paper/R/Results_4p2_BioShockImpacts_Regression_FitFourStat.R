# =============================================================================
# Results_4p2_BioShockImpacts_Regression_FitFourStat.R
#
# Purpose:
#   Quantify the transmission of biophysical yield shocks to agricultural
#   outcomes (area, production, trade, stocks, price, consumption) using
#   linear regression, and test whether transmission coefficients differ
#   significantly across storage scenarios (Dynamic, Hidden, Fixed-Ratio).
#
# Modelling approach:
#   X = biophysical yield shock (BioYield_Land) for each region × sector ×
#       year × scenario observation.
#   Y = outcome element (see Ele below), same indexing.
#   Four specifications are fitted across all scenario × element groups:
#     1. pooled           — OLS across all region-crop pairs
#     2. fe_region        — OLS absorbing region intercepts (1-way FE)
#     3. fe_pair          — OLS absorbing region×sector pair intercepts (2-way FE)
#     4. individual       — separate OLS per pair; reports median + IQR of slopes
#   Plus two sector-specific variants:
#     5. pooled_by_sector — pooled OLS run within each crop sector
#     6. fe_pair_by_sector— OLS absorbing region intercepts, within each sector
#
#   Scenario-slope differences are tested with Wald F-tests
#   (car::linearHypothesis) via wald_three(), covering:
#     • Dynamic vs. Hidden        (pairwise F, df = 1)
#     • Dynamic vs. Fixed-Ratio   (pairwise F, df = 1)
#     • Hidden  vs. Fixed-Ratio   (pairwise F, df = 1; stored for completeness)
#     • Omnibus: all 3 equal      (joint F, df = 2)
#   Wald models tested: pooled, fe_region, fe_pair (individual cannot be
#   Wald-tested as it has no pooled slope).
#
# Key outputs:
#   results_beta2    — β, SE, p-value for every model × scenario × element
#   individual_dist  — per-pair slope distribution (median, IQR, min/max)
#   wald_results_all — Wald test results for every element × model
#   Figures saved to: paper/output/YieldShockTransmission/test/
#     Fig_YST_Coefficients.png  — β ± 95% CI, all 4 models, Nature quality
#     Fig_YST_Heatmap.png       — significance heatmap (element × comparison)
#
# Requires:
#   ImpactData_cross  (built in Results_3_BioShockImpacts_Data.R)
# =============================================================================


# =============================================================================
# 0. Libraries and output directory ----
# =============================================================================

library(dplyr)
library(broom)
library(purrr)
library(tidyr)
library(ggplot2)
library(scales)
library(car)

out_dir <- "paper/output/YieldShockTransmission/Regression_WaldTest"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# =============================================================================
# 1. Variable definitions ----
# =============================================================================

# --- Filters -----------------------------------------------------------------
.ImpactYear     <- seq(2025, 2100, 5)
.ImpactSector   <- c("Grains", "Oil crops", "Sugar crops", "Other crops")
.ImpactScenario <- c("Dynamic", "Hidden", "Fixed-Ratio")

# --- Outcome elements --------------------------------------------------------
# Ele  : values as they appear in ImpactData_cross$element
# EleLab: display labels (same order)
#
# -- (Reference) full exploratory list, no longer active --
# Ele <- c("BioYield_Land",
#          "Yield_Land", "Area", "Production",
#          "Export",
#          "Closing stocks", "Closing stocks_Ecal",
#          "Stock loss", "Stock loss_Ecal",
#          "Import", "Food", "Feed", "Bioenergy",
#          "Consumption", "Price", "Revenue")
# EleLab <- c("Biophysical Yield",
#             "Realized Yield", "Area harvested", "Production",
#             "Export",
#             "Closing stocks (Mt)", "Closing stocks (Ecal)",
#             "Stock loss (Mt)", "Stock loss (Ecal)",
#             "Import", "Food", "Feed", "Bioenergy",
#             "Consumption (Total)", "Price", "Revenue")

# -- Active list --
Ele <- c("BioYield_Land",
         "Yield_Land", "Area", "Production",
         "Export",
         "Closing stocks",
         "Import",
         "Consumption", "Price")

EleLab <- c("Biophysical Yield",
            "Realized Yield", "Area harvested", "Production",
            "Export",
            "Closing stocks (Mt)",
            "Import",
            "Consumption (Total)", "Price")

# --- Shared visual constants -------------------------------------------------
# Colour palette for the three storage scenarios (colour-blind tested)
scen_colors <- c("Dynamic"     = "#1b7837",
                 "Hidden"      = "#762a83",
                 "Fixed-Ratio" = "#e08214")

# Shape palette for four model specifications (coefficient figure)
model_shapes <- c("Pooled" = 16, "FE-R" = 17, "FE-RS" = 15, "Pairs" = 18)

# --- Figure element ordering -------------------------------------------------
# Biophysical Yield is the x-variable; excluded from result figures.
fig_ele_order <- c("Realized Yield", "Area harvested", "Production",
                   "Export", "Closing stocks (Mt)", "Import",
                   "Consumption (Total)", "Price")


# =============================================================================
# 2. Prepare regression data ----
# =============================================================================

# x-variable: biophysical yield shock for each region × sector × year × scenario
x_tbl <- ImpactData_cross %>%
  filter(element == "BioYield_Land",
         region  != "World",
         year    %in% .ImpactYear,
         sector  %in% .ImpactSector,
         scenario_storage %in% .ImpactScenario) %>%
  transmute(scenario_storage, region, year, sector,
            x = value)

# y-variables: all outcome elements joined to x on matching keys
df_xy <- ImpactData_cross %>%
  filter(region  != "World",
         year    %in% .ImpactYear,
         sector  %in% .ImpactSector,
         element %in% Ele,
         scenario_storage %in% .ImpactScenario) %>%
  mutate(element = factor(element, levels = Ele, labels = EleLab)) %>%
  transmute(scenario_storage, region, year, sector, element,
            y = value) %>%
  left_join(x_tbl, by = c("scenario_storage", "region", "year", "sector")) %>%
  filter(!is.na(x), !is.na(y))

# Factor scenario_storage for consistent ordering in models and plots
df_xy2 <- df_xy %>%
  mutate(scenario_storage = factor(scenario_storage,
                                   levels = c("Hidden", "Dynamic", "Fixed-Ratio")))


# =============================================================================
# 3. Regression coefficients and SE ----
# =============================================================================
#
# fit_four() fits six specifications for one data slice
# (one element × one scenario combination).
#
# Arguments
#   d          data frame: must contain x, y, region, sector
#   min_n      minimum rows for pooled / FE models (default 10)
#   min_n_pair minimum rows per pair for individual-slope model (default 5)
#
# Returns a tibble with columns:
#   model, sector, beta, se, p_value, beta_p25, beta_p75, n, n_pairs
#   (beta_p25 / beta_p75 are only populated for model = "individual")

fit_four <- function(d, min_n = 10, min_n_pair = 5) {

  d <- d %>%
    filter(is.finite(x), is.finite(y)) %>%
    mutate(pair = interaction(region, sector, drop = TRUE))

  n_pairs <- n_distinct(d$pair)

  # Guard: too few observations or degenerate x variance
  if (nrow(d) < min_n || sd(d$x) == 0) {
    return(tibble(
      model    = c("pooled", "fe_region", "fe_pair", "individual",
                   "pooled_by_sector", "fe_pair_by_sector"),
      sector   = NA_character_,
      beta     = NA_real_, se = NA_real_, p_value  = NA_real_,
      beta_p25 = NA_real_, beta_p75 = NA_real_,
      n = nrow(d), n_pairs = n_pairs
    ))
  }

  # 1) Pooled OLS -----------------------------------------------------------
  m_pooled <- lm(y ~ x, data = d)
  pooled <- tidy(m_pooled) %>%
    filter(term == "x") %>%
    transmute(model = "pooled", sector = NA_character_,
              beta = estimate, se = std.error, p_value = p.value,
              beta_p25 = NA_real_, beta_p75 = NA_real_,
              n = nrow(d), n_pairs = n_pairs)

  # 2) Region fixed effects (1-way FE) --------------------------------------
  m_fe_reg <- lm(y ~ x + factor(region), data = d)
  fe_reg <- tidy(m_fe_reg) %>%
    filter(term == "x") %>%
    transmute(model = "fe_region", sector = NA_character_,
              beta = estimate, se = std.error, p_value = p.value,
              beta_p25 = NA_real_, beta_p75 = NA_real_,
              n = nrow(d), n_pairs = n_pairs)

  # 3) Region × sector fixed effects (2-way FE) -----------------------------
  m_fe <- lm(y ~ x + pair, data = d)
  fe <- tidy(m_fe) %>%
    filter(term == "x") %>%
    transmute(model = "fe_pair", sector = NA_character_,
              beta = estimate, se = std.error, p_value = p.value,
              beta_p25 = NA_real_, beta_p75 = NA_real_,
              n = nrow(d), n_pairs = n_pairs)

  # 4) Individual pair slopes → distribution summary ------------------------
  # Fit y ~ x separately for each region-crop pair; report the cross-pair
  # median and interquartile range (no pooled SE; bootstrap if SE needed).
  ind <- d %>%
    group_by(pair) %>%
    group_modify(~{
      dd <- .x
      if (nrow(dd) < min_n_pair || sd(dd$x) == 0) return(tibble(beta = NA_real_))
      tibble(beta = as.numeric(coef(lm(y ~ x, data = dd))["x"]))
    }) %>%
    ungroup() %>%
    filter(!is.na(beta))

  individual <- tibble(
    model    = "individual", sector = NA_character_,
    beta     = ifelse(nrow(ind) > 0, median(ind$beta), NA_real_),
    se       = NA_real_, p_value = NA_real_,
    beta_p25 = ifelse(nrow(ind) > 0, unname(quantile(ind$beta, 0.25)), NA_real_),
    beta_p75 = ifelse(nrow(ind) > 0, unname(quantile(ind$beta, 0.75)), NA_real_),
    n = nrow(d), n_pairs = n_pairs
  )

  # 5a) Pooled OLS within each crop sector ----------------------------------
  pooled_by_sector <- d %>%
    group_by(sector) %>%
    group_modify(~{
      dd <- .x
      if (nrow(dd) < min_n || sd(dd$x) == 0)
        return(tibble(beta = NA_real_, se = NA_real_, p_value = NA_real_,
                      n = nrow(dd), n_pairs = n_distinct(dd$pair)))
      mm <- lm(y ~ x, data = dd)
      tt <- tidy(mm) %>% filter(term == "x")
      tibble(beta = tt$estimate, se = tt$std.error, p_value = tt$p.value,
             n = nrow(dd), n_pairs = n_distinct(dd$pair))
    }) %>%
    ungroup() %>%
    transmute(model = "pooled_by_sector", sector = as.character(sector),
              beta, se, p_value, beta_p25 = NA_real_, beta_p75 = NA_real_,
              n, n_pairs)

  # 5b) Region FE within each crop sector -----------------------------------
  # Sector is held fixed by group_by; only region intercepts are absorbed.
  # This gives: y = beta*x + alpha_region + e  (within-sector)
  fe_by_sector <- d %>%
    group_by(sector) %>%
    group_modify(~{
      dd <- .x
      if (nrow(dd) < min_n || sd(dd$x) == 0)
        return(tibble(beta = NA_real_, se = NA_real_, p_value = NA_real_,
                      n = nrow(dd), n_pairs = n_distinct(dd$pair)))
      mm <- lm(y ~ x + factor(region), data = dd)
      tt <- tidy(mm) %>% filter(term == "x")
      tibble(beta = tt$estimate, se = tt$std.error, p_value = tt$p.value,
             n = nrow(dd), n_pairs = n_distinct(dd$pair))
    }) %>%
    ungroup() %>%
    transmute(model = "fe_pair_by_sector", sector = as.character(sector),
              beta, se, p_value, beta_p25 = NA_real_, beta_p75 = NA_real_,
              n, n_pairs)

  bind_rows(pooled, fe_reg, fe, individual, pooled_by_sector, fe_by_sector)
}


# Run fit_four() across every scenario × element combination
results_beta2 <- df_xy %>%
  group_by(scenario_storage, element) %>%
  group_modify(~ fit_four(.x)) %>%
  ungroup()


# Per-pair slope distribution (extends results_beta2 "individual" rows
# with range and exact pair count)
individual_dist <- df_xy %>%
  mutate(pair = interaction(region, sector, drop = TRUE)) %>%
  group_by(scenario_storage, element, pair) %>%
  summarise(
    beta = if (n() >= 5 && sd(x) > 0) as.numeric(coef(lm(y ~ x))["x"]) else NA_real_,
    .groups = "drop"
  ) %>%
  filter(!is.na(beta)) %>%
  group_by(scenario_storage, element) %>%
  summarise(
    beta_med = median(beta),
    beta_p25 = quantile(beta, 0.25),
    beta_p75 = quantile(beta, 0.75),
    beta_min = min(beta),
    beta_max = max(beta),
    n_pairs  = n(),
    .groups  = "drop"
  )

individual_dist


# =============================================================================
# 4. Coefficient figure — Nature quality ----
# =============================================================================
#
# Dot plot of beta +/- 95% CI (or IQR for pair medians) from all four model
# specifications, faceted by outcome element (free x-scales).
# Color = storage scenario, shape = model specification.
# Stars indicate beta != 0 based on the model's own t-test p-value.
#
# Biophysical Yield is excluded: y = x by construction (beta ~ 1, RSS ~ 0).
#
# Model labels in figure:
#   Pooled = pooled OLS (circle)
#   FE-R   = region-only FE, 1-way (triangle)
#   FE-RS  = region x sector FE, 2-way (square)
#   Pairs  = median of per-pair slopes, IQR bars (diamond)

model_order  <- c("pooled", "fe_region", "fe_pair", "individual")
model_labels <- c("Pooled", "FE-R", "FE-RS", "Pairs")

coef_df_all <- results_beta2 %>%
  filter(element %in% fig_ele_order, is.na(sector),
         model %in% model_order) %>%
  mutate(
    element          = factor(element, levels = fig_ele_order),
    scenario_storage = factor(scenario_storage,
                              levels = c("Dynamic", "Hidden", "Fixed-Ratio")),
    # Reverse so Pooled is at bottom of y-axis (most familiar spec first)
    model_lab = factor(model, levels = rev(model_order),
                       labels = rev(model_labels)),
    # 95% CI for OLS models; IQR for pair medians
    ci_lo = ifelse(model == "individual", beta_p25, beta - 1.96 * se),
    ci_hi = ifelse(model == "individual", beta_p75, beta + 1.96 * se),
  )

fig_coef <-
  ggplot(coef_df_all,
         aes(x = beta, y = model_lab,
             color = scenario_storage, shape = model_lab)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey55", linewidth = 0.3) +
  geom_pointrange(
    aes(xmin = ci_lo, xmax = ci_hi),
    position  = position_dodge(width = 0.7),
    linewidth = 0.4, size = 0.35, fatten = 2.8
  ) +

  scale_color_manual(
    values = scen_colors,
    name   = "Storage scenario",
    guide  = guide_legend(order = 1, nrow = 1,
                          override.aes = list(shape = 16, linewidth = 0.5))
  ) +
  scale_shape_manual(
    values = model_shapes,
    name   = "Model specification",
    labels = c(
      "Pooled" = "Pooled OLS",
      "FE-R"   = "FE: region intercepts",
      "FE-RS"  = "FE: region\u00d7sector intercepts",
      "Pairs"  = "Pair medians (IQR bars)"
    ),
    guide = guide_legend(order = 2, nrow = 2,
                         override.aes = list(color = "grey40", linewidth = 0))
  ) +
  scale_y_discrete(expand = expansion(add = 0.8)) +
  scale_x_continuous(labels = number_format(accuracy = 0.01), n.breaks = 4,
                     expand = expansion(mult = c(0.05, 0.22))) +
  facet_wrap(~element, scales = "free_x", ncol = 2) +
  labs(
    title    = "Yield shock transmission coefficients by storage scenario",
    subtitle = paste(
      "Slope \u03b2 of outcome ~ biophysical yield shock, across four model specifications.",
      "Y-axis groups (top to bottom): Pairs \u25c6, FE-RS \u25a0, FE-R \u25b2, Pooled \u25cf."
    ),
    x       = expression(paste("Regression coefficient ", beta)),
    y       = NULL,
    caption = paste(
      "Error bars: 95\u202f% CI (\u00b11.96\u00d7SE) for OLS models; IQR for pair medians.",
      "All OLS coefficients are significant at p<0.001.",
      sep = "\n"
    )
  ) +
  theme_classic(base_size = 8, base_family = "sans") +
  theme(
    strip.text           = element_text(face = "bold", size = 8),
    strip.background     = element_blank(),
    panel.grid.major.x   = element_line(color = "grey93", linewidth = 0.25),
    axis.line            = element_line(linewidth = 0.3),
    axis.ticks.x         = element_line(linewidth = 0.3),
    axis.ticks.y         = element_blank(),
    axis.text.x          = element_text(size = 6.5),
    axis.text.y          = element_blank(),
    axis.title.x         = element_text(size = 7.5, margin = margin(t = 4)),
    plot.title           = element_text(face = "bold", size = 9,
                                        margin = margin(b = 2)),
    plot.subtitle        = element_text(size = 6.8, color = "grey35",
                                        lineheight = 1.3, margin = margin(b = 6)),
    plot.caption         = element_text(size = 6, color = "grey45", hjust = 0,
                                        lineheight = 1.3, margin = margin(t = 6)),
    legend.position      = "bottom",
    legend.box           = "vertical",
    legend.title         = element_text(size = 7, face = "bold"),
    legend.text          = element_text(size = 6.5),
    legend.key.size      = unit(0.35, "cm"),
    legend.spacing.y     = unit(0.1, "cm"),
    panel.spacing        = unit(0.5, "cm"),
    plot.margin          = margin(10, 14, 8, 8, "pt")
  )

fig_coef


# =============================================================================
# 5. Wald tests: are scenario transmission coefficients significantly different?
# =============================================================================
#
# Strategy: fit y ~ x:scenario_storage (+ FE terms) so each scenario gets its
# own slope. Then test pairwise equality with linearHypothesis (F-test).
#
# Primary comparisons:
#   Dynamic vs. Hidden       — does hiding stocks change shock transmission?
#   Dynamic vs. Fixed-Ratio  — does a fixed stock ratio change transmission?
# Additional comparisons stored in wald_results_all for completeness:
#   Hidden vs. Fixed-Ratio   (pairwise)
#   Omnibus: all 3 equal     (joint F, df = 2)
#
# NOTE on structural NAs: "Hidden" has no closing-stock observations in GCAM
# (stocks are hidden from agents and not reported as a SUA balance item).
# The corresponding slope coefficient is absent from the model, so
# linearHypothesis returns an error that is caught and recorded as NA.


# --- Diagnostic check on a single element (Import) --------------------------
# Manual verification that wald_three() results agree with direct calls.

m_compare <- lm(
  y ~ x:scenario_storage + interaction(region, sector),
  data = df_xy %>% filter(element == "Import")
)
summary(m_compare)

linearHypothesis(m_compare,
                 "x:scenario_storageHidden = x:scenario_storageDynamic")
linearHypothesis(m_compare,
                 "x:scenario_storageDynamic = x:scenario_storageFixed-Ratio")


# --- wald_three() ------------------------------------------------------------
# Runs all Wald tests for a single fitted lm object.
# Returns 4 rows (3 pairwise + omnibus). NA is returned when:
#   (a) RSS ~ 0  (degenerate fit; e.g. BioYield where y = x)
#   (b) A scenario coefficient is absent (Hidden x stock variables)

wald_three <- function(m, eps_rss = 1e-12) {

  na_out <- function(note) {
    tibble(
      comp    = c("Dynamic vs Hidden", "Dynamic vs Fixed-Ratio",
                  "Hidden vs Fixed-Ratio", "Omnibus (all equal)"),
      F       = NA_real_, df1 = NA_integer_, p_value = NA_real_, note = note
    )
  }

  rss <- tryCatch(sum(residuals(m)^2), error = function(e) NA_real_)
  if (!is.finite(rss) || rss < eps_rss)
    return(na_out("RSS ~ 0; test undefined"))

  run_lh <- function(hyp)
    tryCatch(car::linearHypothesis(m, hyp, test = "F"), error = function(e) NULL)

  t1 <- run_lh("x:scenario_storageDynamic = x:scenario_storageHidden")
  t2 <- run_lh("x:scenario_storageDynamic = x:scenario_storageFixed-Ratio")
  t3 <- run_lh("x:scenario_storageHidden = x:scenario_storageFixed-Ratio")
  t4 <- run_lh(c("x:scenario_storageDynamic = x:scenario_storageHidden",
                  "x:scenario_storageDynamic = x:scenario_storageFixed-Ratio"))

  ex <- function(t) {
    if (is.null(t)) return(list(F = NA_real_, df1 = NA_integer_, p = NA_real_))
    list(F = t$F[2], df1 = as.integer(t$Df[2]), p = t$`Pr(>F)`[2])
  }
  e1 <- ex(t1); e2 <- ex(t2); e3 <- ex(t3); e4 <- ex(t4)

  tibble(
    comp    = c("Dynamic vs Hidden", "Dynamic vs Fixed-Ratio",
                "Hidden vs Fixed-Ratio", "Omnibus (all equal)"),
    F       = c(e1$F, e2$F, e3$F, e4$F),
    df1     = c(e1$df1, e2$df1, e3$df1, e4$df1),
    p_value = c(e1$p, e2$p, e3$p, e4$p),
    note    = NA_character_
  )
}


# --- run_models_one_element() ------------------------------------------------
# Fits pooled, 1-way region FE, and 2-way region x sector FE for one element
# (all scenarios pooled), then runs wald_three() on each.
# Results should be stable across all three — treat as robustness check.

run_models_one_element <- function(d_el) {

  na_rows <- function(mod_label) {
    tibble(comp    = c("Dynamic vs Hidden", "Dynamic vs Fixed-Ratio",
                       "Hidden vs Fixed-Ratio", "Omnibus (all equal)"),
           F       = NA_real_, df1 = NA_integer_, p_value = NA_real_,
           note    = "insufficient data",
           model   = mod_label, sector = NA_character_)
  }

  if (nrow(d_el) < 10 || sd(d_el$x, na.rm = TRUE) == 0)
    return(bind_rows(na_rows("pooled"), na_rows("fe_region"), na_rows("fe_pair")))

  m_pooled  <- lm(y ~ x:scenario_storage, data = d_el)
  m_fe_reg  <- lm(y ~ x:scenario_storage + factor(region), data = d_el)
  m_fe_pair <- lm(y ~ x:scenario_storage + interaction(region, sector), data = d_el)

  bind_rows(
    wald_three(m_pooled)  |> mutate(model = "pooled",    sector = NA_character_),
    wald_three(m_fe_reg)  |> mutate(model = "fe_region", sector = NA_character_),
    wald_three(m_fe_pair) |> mutate(model = "fe_pair",   sector = NA_character_)
  )
}


# --- run_models_by_sector() --------------------------------------------------
# Within each crop sector, fits pooled and region-FE models and runs Wald tests.
# Sector is held fixed by group_by(), so only region intercepts are needed.

run_models_by_sector <- function(d_el) {

  na_rows_sec <- function(mod_label) {
    tibble(comp    = c("Dynamic vs Hidden", "Dynamic vs Fixed-Ratio",
                       "Hidden vs Fixed-Ratio", "Omnibus (all equal)"),
           F       = NA_real_, df1 = NA_integer_, p_value = NA_real_,
           note    = "insufficient data", model = mod_label)
  }

  pooled_sec <- d_el |>
    group_by(sector) |>
    group_modify(~{
      dd <- .x
      if (nrow(dd) < 10 || sd(dd$x) == 0) return(na_rows_sec("pooled_by_sector"))
      wald_three(lm(y ~ x:scenario_storage, data = dd))
    }) |>
    ungroup() |>
    mutate(model = "pooled_by_sector")

  fe_sec <- d_el |>
    group_by(sector) |>
    group_modify(~{
      dd <- .x
      if (nrow(dd) < 10 || sd(dd$x) == 0) return(na_rows_sec("fe_region_by_sector"))
      wald_three(lm(y ~ x:scenario_storage + factor(region), data = dd))
    }) |>
    ungroup() |>
    mutate(model = "fe_region_by_sector")

  bind_rows(pooled_sec, fe_sec)
}


# --- Run Wald tests for all elements -----------------------------------------
wald_results_all <-
  df_xy2 %>%
  group_by(element) %>%
  group_modify(~{
    d_el <- .x %>% ungroup()
    bind_rows(run_models_one_element(d_el),
              run_models_by_sector(d_el))
  }) %>%
  ungroup() %>%
  arrange(element, model, sector, comp)

wald_results_all


# =============================================================================
# 6. Significance heatmap ----
# =============================================================================
#
# Colour encodes -log10(p) from all three Wald-testable models
# (pooled, fe_region, fe_pair), shown as side-by-side facets.
# Only the two primary scenario comparisons are shown on the x-axis:
#   Dynamic vs. Hidden  |  Dynamic vs. Fixed-Ratio
# (Hidden vs. Fixed-Ratio and Omnibus are stored in wald_results_all.)
#
# Grey tiles: structural n/a — Hidden has no closing-stock observations,
# so x:scenario_storageHidden is absent from the model and cannot be tested.

comp_order  <- c("Dynamic vs Hidden", "Dynamic vs Fixed-Ratio", "Hidden vs Fixed-Ratio")
comp_labels <- c("Dyn\u2013Hid", "Dyn\u2013FR", "Hid\u2013FR")

heatmap_df_all <- wald_results_all %>%
  filter(
    element %in% fig_ele_order,
    is.na(sector),
    model %in% c("pooled", "fe_region", "fe_pair"),
    comp  %in% comp_order
  ) %>%
  mutate(
    element   = factor(element, levels = rev(fig_ele_order)),
    comp      = factor(comp, levels = comp_order, labels = comp_labels),
    model_lab = factor(model,
                       levels = c("pooled", "fe_region", "fe_pair"),
                       labels = c("Pooled OLS",
                                  "FE: Region",
                                  "FE: Region\u00d7Sector")),
    fill_val  = ifelse(!is.na(p_value), pmin(-log10(p_value), 3), NA_real_),
    sig_lbl   = case_when(
      is.na(p_value)  ~ "n/a",
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ ""
    )
  )

fig_heatmap <-
  ggplot(heatmap_df_all, aes(x = comp, y = element)) +
  geom_tile(aes(fill = fill_val), color = "white", linewidth = 0.55) +
  geom_text(aes(label = sig_lbl), size = 3.0, fontface = "bold", color = "grey15") +
  scale_fill_gradientn(
    colors   = c("#f7f7f7", "#fdcc8a", "#fc8d59", "#d7301f"),
    values   = scales::rescale(c(0, -log10(0.05), -log10(0.01), 3)),
    limits   = c(0, 3),
    na.value = "grey88",
    name     = expression(-log[10](italic(p))),
    breaks   = c(0, -log10(0.05), -log10(0.01), 3),
    labels   = c("1.0", "0.05", "0.01", "\u22640.001"),
    guide    = guide_colorbar(title.position = "top", title.hjust = 0.5,
                              barwidth = 5.5, barheight = 0.6)
  ) +
  facet_wrap(~model_lab, nrow = 1) +
  labs(
    title    = "Significance of scenario differences in yield shock transmission",
    subtitle = paste(
      "Wald F-test of H0: transmission slope (\u03b2) equal across storage scenarios.",
      "Dyn = Dynamic; FR = Fixed-Ratio; Hid = Hidden.",
      "Grey = structural n/a (Hidden scenario has no stock observations)."
    ),
    x       = NULL,
    y       = NULL,
    caption = "* p<0.05   ** p<0.01   *** p<0.001"
  ) +
  theme_minimal(base_size = 9, base_family = "sans") +
  theme(
    panel.grid    = element_blank(),
    strip.text    = element_text(face = "bold", size = 8.5),
    axis.text.x   = element_text(size = 8.5, lineheight = 1.1),
    axis.text.y   = element_text(size = 8.5),
    legend.position = "bottom",
    plot.title    = element_text(face = "bold", size = 9, margin = margin(b = 2)),
    plot.subtitle = element_text(size = 7, color = "grey35",
                                 lineheight = 1.3, margin = margin(b = 6)),
    plot.caption  = element_text(size = 7, color = "grey45",
                                 hjust = 0, margin = margin(t = 4)),
    panel.spacing = unit(0.5, "cm"),
    plot.margin   = margin(10, 10, 8, 8, "pt")
  )

fig_heatmap


# =============================================================================
# 7. Export figures ----
# =============================================================================
#
# Target dimensions for Nature:
#   Coefficient figure: double-column (183 mm = 7.2 in wide)
#   Heatmap:           double-column (183 mm wide), shorter height

ggsave(
  filename = file.path(out_dir, "Fig_YST_Coefficients.png"),
  plot     = fig_coef,
  width = 7.2, height = 9.0, dpi = 300, bg = "white"
)

ggsave(
  filename = file.path(out_dir, "Fig_YST_Heatmap.png"),
  plot     = fig_heatmap,
  width = 8.0, height = 5.5, dpi = 300, bg = "white"
)

message("Figures saved to: ", out_dir)


# =============================================================================
# Data tables for Nature Food ----
#
# Two CSV files are exported:
#
#   (1) Table_YST_Coefficients.csv
#       Source objects: results_beta2, individual_dist
#       Content: Yield shock transmission coefficients (beta) for every
#       scenario × element × model combination, with standard errors,
#       confidence intervals, p-values, and pair-slope distribution summaries.
#
#   (2) Table_YST_WaldTests.csv
#       Source object: wald_results_all
#       Content: Wald F-tests comparing whether beta differs across storage
#       scenarios, for every element × model × comparison combination.
#
# ── Statistical methods ──────────────────────────────────────────────────────
#
# COEFFICIENT MODELS (results_beta2)
#   For each scenario × element group, the following OLS models are fitted:
#
#   pooled
#     y ~ x
#     Single global intercept. Gives the average transmission slope across
#     all region-crop pairs, assuming no pair-level heterogeneity in levels.
#
#   fe_region   (1-way FE)
#     y ~ x + factor(region)
#     Region fixed effects absorb systematic between-region differences in
#     baseline levels. Slope is identified from within-region variation.
#
#   fe_pair     (2-way FE)  ← primary specification in the paper
#     y ~ x + interaction(region, sector)
#     Region × crop-sector pair fixed effects. The most conservative
#     specification; slope identified purely from within-pair variation.
#
#   individual
#     Separate OLS (y ~ x) fitted for each region × crop pair independently.
#     No pooling of slopes across pairs. Reports the cross-pair distribution
#     of slopes: median (beta), P25 (beta_p25), P75 (beta_p75),
#     min (beta_min), max (beta_max). SE and p_value are NA for this model
#     because there is no single pooled estimate.
#
#   pooled_by_sector / fe_pair_by_sector
#     Sector-specific variants of pooled and FE-pair, run separately within
#     each crop sector.
#
#   Variables:
#     x = BioYield_Land  (biophysical yield shock, fractional change)
#     y = outcome element (fractional change, same units as x)
#     beta     : OLS slope coefficient (transmission elasticity)
#     se       : standard error of beta (White-robust SE not applied; OLS SE)
#     p_value  : two-tailed p-value for H0: beta = 0
#     ci_lo/hi : 95% CI = beta ± 1.96 × se
#
# WALD TESTS (wald_results_all)
#   Tests whether the transmission slope (beta) differs across storage scenarios.
#   Implemented as linear hypothesis F-tests via car::linearHypothesis().
#
#   Model specification:
#     y ~ x : scenario_storage + [FE terms]
#   Each scenario gets its own slope (interaction of x with scenario factor).
#   The intercept varies by pair (FE models) or is global (pooled).
#
#   Hypotheses tested (comp column):
#     "Dynamic vs Hidden"       H0: beta_Dynamic = beta_Hidden
#     "Dynamic vs Fixed-Ratio"  H0: beta_Dynamic = beta_Fixed-Ratio
#     "Hidden vs Fixed-Ratio"   H0: beta_Hidden  = beta_Fixed-Ratio
#     "Omnibus (all equal)"     H0: beta_D = beta_H AND beta_D = beta_FR  (df=2)
#
#   F = Wald F-statistic; df1 = restriction degrees of freedom (1 or 2)
#   p_value = probability of observing F >= observed under H0
#   NA values: (a) RSS ≈ 0 (BioYield where y ≡ x); (b) scenario coefficient
#              absent (Hidden has no Closing stocks observations in GCAM).
#
# NOTE on structural NA for Closing stocks × Hidden:
#   In the Hidden storage scenario, closing stocks are excluded from agent
#   decisions and are not reported as a supply-utilisation balance item.
#   The coefficient x:scenario_storageHidden therefore does not exist in
#   the model for Closing stocks, and the Wald test returns NA.
# =============================================================================

# --- (1) Coefficient table ---------------------------------------------------

# Merge results_beta2 with individual_dist to add beta_min, beta_max
# (individual_dist has additional range statistics not stored in results_beta2)
coef_export <- results_beta2 |>
  left_join(
    individual_dist |>
      select(scenario_storage, element, beta_min, beta_max),
    by = c("scenario_storage", "element")
  ) |>
  # Compute 95% CI for OLS-based models (NA for individual model)
  mutate(
    ci_lo = ifelse(!is.na(se), round(beta - 1.96 * se, 5), NA_real_),
    ci_hi = ifelse(!is.na(se), round(beta + 1.96 * se, 5), NA_real_)
  ) |>
  # Round
  mutate(
    across(c(beta, se, ci_lo, ci_hi, beta_p25, beta_p75,
             beta_min, beta_max), \(x) round(x, 5)),
    p_value = signif(p_value, 3)
  ) |>
  select(
    scenario_storage, element, model, sector,
    beta, se, ci_lo, ci_hi, p_value,
    beta_p25, beta_p75, beta_min, beta_max,
    n, n_pairs
  ) |>
  arrange(element, scenario_storage, model, sector)

write.csv(coef_export,
          file      = file.path(out_dir, "Table_YST_Coefficients.csv"),
          row.names = FALSE,
          na        = "")

message("Coefficient table: ", nrow(coef_export), " rows")


# --- (2) Wald test table -----------------------------------------------------

wald_export <- wald_results_all |>
  mutate(
    F       = round(F,       3),
    p_value = signif(p_value, 3)
  ) |>
  select(element, model, sector, comp, F, df1, p_value, note) |>
  arrange(element, model, sector, comp)

write.csv(wald_export,
          file      = file.path(out_dir, "Table_YST_WaldTests.csv"),
          row.names = FALSE,
          na        = "")

message("Wald test table:   ", nrow(wald_export), " rows")
message("Tables saved to:   ", out_dir)
