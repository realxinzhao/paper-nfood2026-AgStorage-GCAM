# ==============================================================================
# DataProc_LandProductivityShocks.R
# ==============================================================================
# Generate yield shock scenarios for GCAM by applying external crop yield
# impacts (from ISIMIP ESM x crop model combinations) to GCAM reference
# yield growth trajectories.
#
# Workflow:
#   Part 1 — Load & prep external yield shocks (ISIMIP)
#     - Source: paper/data/ISIMIP_yieldshocks/ag_impacts_5YrMA_Upper2.rds
#     - Fix GLU name typos, rebase impact to 2021 = 1
#     - Result: agbio_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears
#       Cols: rcp, gcm, cropmodel, irr, GLU, GLU_name, GCAM_region_ID,
#             base, GCAM_commodity, year, impact
#       ESM x crop model combos: {gfdl-esm2m, hadgem2-es} x {gepic, lpjml}
#
#   Part 2 — Load & prep GCAM reference yield growth
#     - Source: gcamdata cache (L2053.AgProdChange_ag_irr_ref, _bio_irr_ref)
#     - Convert annualized AgProdChange to cumulative index (2021 = 1)
#     - Result: AgProdChangeIndex_ref
#       Cols: region, AgSupplySector, AgSupplySubsector,
#             AgProductionTechnology, GLU_name, irr, mgmt, year,
#             timestep, AgProdChangeIndex_cum, GCAM_region_ID
#
#   Part 3 — Apply shocks to reference (apply_yield_shocks function)
#     - Joins impact multipliers onto ref using fallback hierarchy:
#       1. Exact: Region + GLU + Crop + IRR + Year
#       2. Mean over IRR: Region + GLU + Crop + Year
#       3. Mean over IRR + GLU: Region + Crop + Year
#       4. Global crop mean: Crop + Year
#       5. Default = 1 (no shock; e.g., FodderGrass has no external data)
#     - Shocked index = AgProdChangeIndex_cum * impact
#     - Loops over all 4 ESM x crop model combos
#     - Result: AgProdChange_all_scenarios
#
#   Part 4 — Diagnostic visualizations
#     - Per-scenario: impact distributions, ref vs. shocked, regional
#       spaghetti, region x crop heatmaps
#     - Cross-scenario: ensemble comparison, spread ribbon, disagreement
#       heatmap, IRR vs. RFD comparison
#     - Saved to: paper/output/diag_yieldshocks/
#
#   Part 5 — Export GCAM-ready XML files
#     - Converts shocked cumulative index back to annualized AgProdChange
#     - Exports CSV + XML per scenario to paper/output/diag_yieldshocks/xml/
#     - File naming: ag_prodchange_rcp60_SSP2_{esm}_{cm}.{xml,csv}
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)


# ==============================================================================
# Part 1: Load & prep external yield shocks (ISIMIP)
# ==============================================================================

# 5-year moving average yield impacts from ISIMIP crop model intercomparison
readRDS("paper/data/ISIMIP_yieldshocks/ag_impacts_5YrMA_Upper2.rds") ->
  agbio_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears

# Fix GLU name typo
agbio_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears %>%
  mutate(GLU_name = if_else(GLU_name == "Madasgacar", "Madagascar", GLU_name)) ->
  agbio_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears

# Rebase impacts so 2021 = 1 (interpolate to 2021 if not present, then normalize)
agbio_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears %>%
  group_by_at(vars(-year, -impact)) %>%
  complete(year = 2021) %>% arrange(year) %>%
  mutate(impact = approx_fun(year, impact),
         impact = impact / impact[year == 2021]) %>%
  ungroup() %>%
  filter(year >= 2021) ->
  agbio_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears


# ==============================================================================
# Part 2: Load & prep GCAM reference yield growth
# ==============================================================================

MODULE_INPUTS <-
  c(FILE = "common/GCAM_region_names",
    FILE = "L2053.AgProdChange_ag_irr_ref",
    FILE = "L2053.AgProdChange_bio_irr_ref")

MODULE_INPUTS %>% load_from_cache() -> all_data
get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

# Convert annualized AgProdChange to cumulative index
# AgProdChange is an annual rate; we compound it over each timestep
L2053.AgProdChange_ag_irr_ref %>%
  bind_rows(L2053.AgProdChange_bio_irr_ref) %>%
  group_by_at(vars(-year, -AgProdChange)) %>%
  mutate(timestep = if_else(year == min(MODEL_FUTURE_YEARS),
                            min(MODEL_FUTURE_YEARS) - MODEL_FINAL_BASE_YEAR, year - lag(year)),
         AgProdChangeIndex = (AgProdChange + 1)^timestep,
         AgProdChangeIndex_cum = cumprod(AgProdChangeIndex)) %>%
  ungroup() %>%
  left_join(GCAM_region_names, by = "region") %>%
  separate(AgProductionTechnology, into = c("Crop", "GLU_name", "irr", "mgmt"), remove = F) %>%
  select(-AgProdChange, -AgProdChangeIndex, -Crop) ->
  AgProdChangeIndex_ref


# ==============================================================================
# Part 3: Apply shocks to reference yield growth
# ==============================================================================

OUTPUT_DIR <- "paper/output/diag_yieldshocks"
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

region_lookup <- GCAM_region_names

# Core function: apply yield shocks for one ESM x crop model combo
apply_yield_shocks <- function(ref_data, shock_data, esm_name, cm_name) {

  scenario_label <- paste0(esm_name, " x ", cm_name)
  cat("Processing:", scenario_label, "\n")

  # -- Filter & prep shock data ------------------------------------------------
  agbio_filtered <- shock_data |>
    mutate(year = as.integer(year)) |>
    filter(gcm == esm_name, cropmodel == cm_name) |>
    select(irr, GLU_name, GCAM_region_ID,
           AgSupplySector = GCAM_commodity, year, impact)

  # -- Build fallback mean tables ----------------------------------------------
  agbio_R_C_GLU_Y <- agbio_filtered |>
    summarize(impact_R_C_GLU_Y = mean(impact),
              .by = c(GCAM_region_ID, AgSupplySector, GLU_name, year))

  agbio_R_C_Y <- agbio_filtered |>
    summarize(impact_R_C_Y = mean(impact),
              .by = c(GCAM_region_ID, AgSupplySector, year))

  agbio_C_Y <- agbio_filtered |>
    summarize(impact_C_Y = mean(impact),
              .by = c(AgSupplySector, year))

  # -- Join with fallback hierarchy --------------------------------------------
  agbio_future <- agbio_filtered |> filter(year >= 2025)

  result <- ref_data |>
    left_join(agbio_future,
              by = c("GCAM_region_ID", "GLU_name", "AgSupplySector", "irr", "year")) |>
    left_join(agbio_R_C_GLU_Y |> filter(year >= 2025),
              by = c("GCAM_region_ID", "AgSupplySector", "GLU_name", "year")) |>
    mutate(impact = coalesce(impact, impact_R_C_GLU_Y)) |>
    select(-impact_R_C_GLU_Y) |>
    left_join(agbio_R_C_Y |> filter(year >= 2025),
              by = c("GCAM_region_ID", "AgSupplySector", "year")) |>
    mutate(impact = coalesce(impact, impact_R_C_Y)) |>
    select(-impact_R_C_Y) |>
    left_join(agbio_C_Y |> filter(year >= 2025),
              by = c("AgSupplySector", "year")) |>
    mutate(impact = coalesce(impact, impact_C_Y, 1)) |>
    select(-impact_C_Y) |>
    mutate(
      AgProdChangeIndex_cum_impact = AgProdChangeIndex_cum * impact,
      gcm = esm_name,
      cropmodel = cm_name,
      scenario = scenario_label
    )

  cat("  ->", nrow(result), "rows,",
      sum(is.na(result$AgProdChangeIndex_cum_impact)), "NAs\n")

  result
}


# -- Run all scenarios ---------------------------------------------------------
scenario_combos <- agbio_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears |>
  distinct(gcm, cropmodel)

AgProdChange_all_scenarios <- lapply(seq_len(nrow(scenario_combos)), function(i) {
  apply_yield_shocks(
    ref_data = AgProdChangeIndex_ref,
    shock_data = agbio_impacts_rcp_gcm_gcm_R_GLU_C_IRR_allyears,
    esm_name = scenario_combos$gcm[i],
    cm_name = scenario_combos$cropmodel[i]
  )
}) |> bind_rows()

cat("\nAll scenarios combined:", nrow(AgProdChange_all_scenarios), "rows\n")
cat("Scenarios:\n")
AgProdChange_all_scenarios |> distinct(scenario) |> print()


# ==============================================================================
# Part 4: Diagnostic Visualizations
# ==============================================================================

# -- Per-Scenario Diagnostics --------------------------------------------------

for (scen in unique(AgProdChange_all_scenarios$scenario)) {

  scen_data <- AgProdChange_all_scenarios |> filter(scenario == scen)
  scen_tag <- gsub(" x ", "_", scen)  # file-safe name

  # -- Diag 1: Impact distribution by crop and year ----------------------------
  p1 <- scen_data |>
    filter(year %in% c(2030, 2050, 2075, 2100)) |>
    ggplot(aes(x = factor(year), y = impact)) +
    geom_boxplot(outlier.size = 0.3) +
    facet_wrap(~AgSupplySector, scales = "free_y") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.3) +
    labs(title = paste0("Yield shock distribution (", scen, ")"),
         x = "Year", y = "Impact multiplier") +
    theme_bw(base_size = 10)

  ggsave(file.path(OUTPUT_DIR, paste0("diag1_impact_dist_", scen_tag, ".png")),
         p1, width = 14, height = 10, dpi = 200)

  # -- Diag 2: Ref vs. shocked for major crops --------------------------------
  diag2 <- scen_data |>
    filter(AgSupplySector %in% c("Corn", "Wheat", "Rice", "Soybean",
                                  "OilCrop", "SugarCrop")) |>
    summarize(ref = mean(AgProdChangeIndex_cum),
              shocked = mean(AgProdChangeIndex_cum_impact),
              .by = c(AgSupplySector, year)) |>
    pivot_longer(c(ref, shocked), names_to = "type", values_to = "index")

  p2 <- ggplot(diag2, aes(x = year, y = index, color = type)) +
    geom_line() +
    facet_wrap(~AgSupplySector, scales = "free_y") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.3) +
    labs(title = paste0("Ref vs. shocked yield index (", scen, ")"),
         x = "Year", y = "Cumulative yield index", color = "Scenario") +
    theme_bw(base_size = 10)

  ggsave(file.path(OUTPUT_DIR, paste0("diag2_ref_vs_shocked_", scen_tag, ".png")),
         p2, width = 12, height = 7, dpi = 200)

  # -- Diag 3: Shocked yield by region for key crops ---------------------------
  diag3 <- scen_data |>
    filter(AgSupplySector %in% c("Corn", "Wheat", "Rice", "Soybean")) |>
    left_join(region_lookup, by = "GCAM_region_ID") |>
    summarize(shocked = mean(AgProdChangeIndex_cum_impact),
              .by = c(region.y, AgSupplySector, year))

  p3 <- ggplot(diag3, aes(x = year, y = shocked, group = region.y)) +
    geom_line(alpha = 0.4, linewidth = 0.4) +
    facet_wrap(~AgSupplySector) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.3) +
    labs(title = paste0("Shocked yield by region (", scen, ")"),
         x = "Year", y = "Cumulative yield index (shocked)",
         caption = "Each line = one GCAM region") +
    theme_bw(base_size = 10)

  ggsave(file.path(OUTPUT_DIR, paste0("diag3_by_region_", scen_tag, ".png")),
         p3, width = 10, height = 7, dpi = 200)

  # -- Diag 4: Heatmap at 2050 ------------------------------------------------
  diag4 <- scen_data |>
    filter(year == 2050) |>
    left_join(region_lookup, by = "GCAM_region_ID") |>
    summarize(impact = mean(impact), .by = c(region.y, AgSupplySector))

  p4 <- ggplot(diag4, aes(x = AgSupplySector, y = region.y, fill = impact)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue",
                         midpoint = 1, name = "Impact") +
    labs(title = paste0("Mean yield shock at 2050 (", scen, ")"),
         x = "Crop", y = "Region") +
    theme_bw(base_size = 9) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(file.path(OUTPUT_DIR, paste0("diag4_heatmap_2050_", scen_tag, ".png")),
         p4, width = 12, height = 10, dpi = 200)

  cat("  Saved diag1-4 for", scen, "\n")
}


# -- Cross-Scenario Diagnostics ------------------------------------------------

# -- Diag 5: Scenario comparison — global mean shocked index by crop -----------
diag5 <- AgProdChange_all_scenarios |>
  filter(AgSupplySector %in% c("Corn", "Wheat", "Rice", "Soybean",
                                "OilCrop", "SugarCrop")) |>
  summarize(shocked = mean(AgProdChangeIndex_cum_impact),
            .by = c(AgSupplySector, year, scenario)) |>
  # Add reference (same across scenarios)
  bind_rows(
    AgProdChangeIndex_ref |>
      filter(AgSupplySector %in% c("Corn", "Wheat", "Rice", "Soybean",
                                    "OilCrop", "SugarCrop")) |>
      summarize(shocked = mean(AgProdChangeIndex_cum),
                .by = c(AgSupplySector, year)) |>
      mutate(scenario = "reference")
  )

p5 <- ggplot(diag5, aes(x = year, y = shocked, color = scenario, linewidth = scenario)) +
  geom_line() +
  scale_linewidth_manual(values = c("reference" = 1.2,
                                     setNames(rep(0.5, 4), unique(AgProdChange_all_scenarios$scenario))),
                         guide = "none") +
  facet_wrap(~AgSupplySector, scales = "free_y") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.3) +
  labs(title = "Cross-scenario comparison: shocked yield index (global mean)",
       x = "Year", y = "Cumulative yield index",
       color = "Scenario") +
  theme_bw(base_size = 10)

ggsave(file.path(OUTPUT_DIR, "diag5_cross_scenario_comparison.png"),
       p5, width = 14, height = 8, dpi = 200)


# -- Diag 6: Scenario spread — range across scenarios by crop & year -----------
diag6 <- AgProdChange_all_scenarios |>
  filter(AgSupplySector %in% c("Corn", "Wheat", "Rice", "Soybean",
                                "OilCrop", "SugarCrop")) |>
  summarize(shocked = mean(AgProdChangeIndex_cum_impact),
            .by = c(AgSupplySector, year, scenario)) |>
  summarize(mean_shocked = mean(shocked),
            min_shocked = min(shocked),
            max_shocked = max(shocked),
            .by = c(AgSupplySector, year))

# Add reference line
diag6_ref <- AgProdChangeIndex_ref |>
  filter(AgSupplySector %in% c("Corn", "Wheat", "Rice", "Soybean",
                                "OilCrop", "SugarCrop")) |>
  summarize(ref = mean(AgProdChangeIndex_cum), .by = c(AgSupplySector, year))

p6 <- ggplot(diag6, aes(x = year)) +
  geom_ribbon(aes(ymin = min_shocked, ymax = max_shocked), alpha = 0.3, fill = "steelblue") +
  geom_line(aes(y = mean_shocked), color = "steelblue") +
  geom_line(data = diag6_ref, aes(y = ref), color = "black", linetype = "dashed") +
  facet_wrap(~AgSupplySector, scales = "free_y") +
  labs(title = "Scenario ensemble: range of shocked yield indices across 4 ESM x crop model combos",
       subtitle = "Blue ribbon = min-max range; blue line = mean; dashed = reference",
       x = "Year", y = "Cumulative yield index") +
  theme_bw(base_size = 10)

ggsave(file.path(OUTPUT_DIR, "diag6_scenario_spread.png"),
       p6, width = 14, height = 8, dpi = 200)


# -- Diag 7: Heatmap of impact spread (max - min) at 2050 by region x crop ----
# Shows where scenarios disagree most
diag7 <- AgProdChange_all_scenarios |>
  filter(year == 2050) |>
  left_join(region_lookup, by = "GCAM_region_ID") |>
  summarize(impact = mean(impact), .by = c(region.y, AgSupplySector, scenario)) |>
  summarize(spread = max(impact) - min(impact),
            mean_impact = mean(impact),
            .by = c(region.y, AgSupplySector))

p7 <- ggplot(diag7, aes(x = AgSupplySector, y = region.y, fill = spread)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Spread\n(max - min)", option = "inferno") +
  labs(title = "Scenario disagreement at 2050: impact spread across 4 ESM x crop model combos",
       subtitle = "Higher values = more uncertainty across scenarios",
       x = "Crop", y = "Region") +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(OUTPUT_DIR, "diag7_scenario_spread_heatmap_2050.png"),
       p7, width = 12, height = 10, dpi = 200)


# -- Diag 8: IRR vs RFD comparison across scenarios ----------------------------
diag8 <- AgProdChange_all_scenarios |>
  filter(AgSupplySector %in% c("Corn", "Wheat", "Rice", "Soybean")) |>
  summarize(impact = mean(impact),
            .by = c(AgSupplySector, irr, year, scenario))

p8 <- ggplot(diag8, aes(x = year, y = impact, color = irr, linetype = scenario)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~AgSupplySector, scales = "free_y") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.3) +
  labs(title = "Irrigated vs. rainfed impact comparison across scenarios",
       x = "Year", y = "Impact multiplier",
       color = "Water mgmt", linetype = "Scenario") +
  theme_bw(base_size = 10)

ggsave(file.path(OUTPUT_DIR, "diag8_irr_vs_rfd.png"),
       p8, width = 14, height = 8, dpi = 200)


cat("\nAll diagnostics saved to:", OUTPUT_DIR, "\n")


# ==============================================================================
# Part 5: Export GCAM-ready XML files
# ==============================================================================
# Convert shocked cumulative index back to annualized AgProdChange rate,
# matching the format of L2053.AgProdChange_ag_irr_ref, then export as XML.
#
# Back-conversion:
#   AgProdChangeIndex (period) = cum[t] / cum[t-1]  (first period: just cum[t])
#   AgProdChange (annual rate) = AgProdChangeIndex^(1/timestep) - 1

XML_DIR <- file.path(OUTPUT_DIR, "xml")
dir.create(XML_DIR, recursive = TRUE, showWarnings = FALSE)

for (scen in unique(AgProdChange_all_scenarios$scenario)) {

  scen_data <- AgProdChange_all_scenarios |> filter(scenario == scen)
  esm <- unique(scen_data$gcm)
  cm <- unique(scen_data$cropmodel)

  # Convert cumulative index back to annualized AgProdChange
  AgProdChange_export <- scen_data |>
    group_by(region, AgSupplySector, AgSupplySubsector,
             AgProductionTechnology, irr, mgmt, GLU_name) |>
    arrange(year) |>
    mutate(
      AgProdChangeIndex = if_else(
        year == first(year),
        AgProdChangeIndex_cum_impact,
        AgProdChangeIndex_cum_impact / lag(AgProdChangeIndex_cum_impact)
      ),
      AgProdChange = round(AgProdChangeIndex^(1 / timestep) - 1, aglu.DIGITS_AGPRODCHANGE)
    ) |>
    ungroup() |>
    select(region, AgSupplySector, AgSupplySubsector,
           AgProductionTechnology, year, AgProdChange)

  # File naming: ag_prodchange_rcp60_SSP2_{esm}_{cm}
  file_stem <- paste0("ag_prodchange_rcp60_SSP2_", esm, "_", cm)

  readr::write_csv(AgProdChange_export, file.path(XML_DIR, paste0(file_stem, ".csv")))

  create_xml(file.path(XML_DIR, paste0(file_stem, ".xml"))) %>%
    add_xml_data(AgProdChange_export, "AgProdChange") %>%
    run_xml_conversion()

  cat("Exported:", file_stem, "\n")
}

cat("\nAll XMLs saved to:", XML_DIR, "\n")
