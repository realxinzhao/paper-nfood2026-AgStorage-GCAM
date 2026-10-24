
Get_MainResults_StockImpactMaps <-
  function(.Scenario_Storage = "S1_Dynamic",
           .Scenario_NoStorage = "S2_Hidden"){


    # Map data ----
    basin <- read.csv(file = "paper/data/maps/basin_to_country_mapping.csv", header = T, comment.char = "#") %>%
      dplyr::select(GCAM_basin_ID, GLU_name)
    basin_noise <- read.csv(file = "paper/data/maps/basin_noise.csv", header = T, comment.char = "#")
    library(sf)

    map_401_sf <- st_read("paper/data/maps/gcam_shapefiles/GCAM_v9_401RegGLU_boundaries.shp") |>
      rename(region = new_reg_nm) |>
      left_join(basin %>% select(glu_id = GCAM_basin_ID, basin = GLU_name), by = "glu_id")

    assign(x = "map_401_sf", value = map_401_sf, envir = .GlobalEnv)

    # map_424_sf <- st_read("paper/data/maps/map_424.shp") %>%
    #   # Separate Taiwan
    #   mutate(cntry_n = if_else(basn_nm == "Taiwan", "Taiwan", cntry_n)) %>%
    #   left_join(basin %>% select(basn_ID = GCAM_basin_ID, GLU_name), by = "basn_ID") %>%
    #   rename(basin = GLU_name, region = cntry_n) %>%
    #   # remove Greenland
    #   filter(basin != "ArcticIsl") %>%
    #   # remove small noise regions
    #   anti_join(basin_noise) # 381 combinations ~10 small areas not included



    ## Land use emissions maps ----

    EMs_CLUC %>%
      filter(year >= 2010,
             scenario %in% c(.Scenario_NoStorage, .Scenario_Storage)) %>%
      separate(landleaf, into = c("land", "basin","IRR", "MGMT"))  %>%
      mutate(basin = if_else(basin == "Forest", IRR, basin))%>%
      Agg_reg(region, basin) ->
      EMs_CLUC_basin

    EMs_CLUC_basin %>%
      distinct(region, basin) %>%
      anti_join(map_401_sf %>% as.data.frame() %>%
                  distinct(region, basin), by = c("region", "basin") ) ->
      basin_missing

    assert_that(nrow(basin_missing) == 0)

    EMs_CLUC_basin %>%
      group_by_at(vars(-scenario, -value)) %>%
      mutate(value = value -value[scenario == .Scenario_NoStorage]) %>%
      filter(scenario != .Scenario_NoStorage) %>% ungroup() %>%
      group_by_at(vars(-value, -year)) %>%
      Fill_annual(CUMULATIVE = T, CUM_YEAR_START = 2021) %>%
      mutate(value = value /1000 *44/12) %>%
      filter(year == 2100) ->
      CLUC_cum_basin

    map_401_sf %>% select(region, basin) %>% st_set_geometry(NULL) %>%
      full_join(CLUC_cum_basin %>% anti_join(basin_missing)) ->
      CLUC_cum1

    CLUC_cum_basin %>% Agg_reg() %>%
      mutate(value = as.character(round(value * 1000, -2))) ->
      NetCLUCValue

    CLUC_cum1 %>% mutate(value = value * 1000) %>%
      ggcamMapBasin(Facet_Var = "scenario") +
      geom_label(data = NetCLUCValue, aes(label = paste0(value, " Mt"), x = -130, y = -40),
                 size = 9, label.size = NA) +
      theme(axis.title =  element_blank()) +
      scale_fill_fermenter(n.breaks = 3,
                           #palette = "PiYG",
                           palette = "PRGn",
                           na.value="white", direction = -1,
                           name = expression(paste(MtCO[2], "")),
                           guide = guide_coloursteps(
                             title.position = "top",
                             title.hjust = 0,
                             frame.colour = "black",
                             frame.linewidth = 0.3,
                             ticks.colour = "black"
                           )) +
      labs(title = "(J) Cumulative land use change emission impact") +
      theme(legend.key.width = unit(1.5,"line"), legend.key.height=unit(1.8,"line"),
            plot.title = element_text(hjust = 0, face = "bold"),
            legend.justification = "left",
            legend.direction = "vertical",   # keep each legend vertical
            legend.box = "vertical",
            legend.title = element_text(margin = margin(b = 4))) ->
      MainResults_CumCLUC_Map;MainResults_CumCLUC_Map




    ## Ag water by sector ----

    AgLU_Water_WithdrawTech %>%
      filter(year >= 2010,
             scenario %in% c(.Scenario_NoStorage, .Scenario_Storage)) %>%
      separate(technology, into = c("land", "basin","IRR", "MGMT"))  %>%
      #mutate(basin = if_else(basin == "Forest", IRR, basin))%>%
      Agg_reg(region, basin) ->
      AgLU_CropWater_basin

    AgLU_CropWater_basin %>%
      distinct(region, basin) %>%
      anti_join(map_401_sf %>% as.data.frame() %>%
                  distinct(region, basin) ) ->
      basin_missing


    AgLU_CropWater_basin %>%
      group_by_at(vars(-scenario, -value)) %>%
      mutate(value = value -value[scenario == .Scenario_NoStorage]) %>%
      filter(scenario != .Scenario_NoStorage) %>% ungroup() %>%
      group_by_at(vars(-value, -year)) %>%
      Fill_annual(CUMULATIVE = T, CUM_YEAR_START = 2021) %>%
      filter(year == 2100) ->
      AgWater_cum_basin

    map_401_sf %>% select(region, basin) %>% st_set_geometry(NULL) %>%
      full_join(AgWater_cum_basin %>% anti_join(basin_missing)) ->
      AgWater_cum_basin_SF

    AgWater_cum_basin %>%
      Agg_reg() %>%
      mutate(value = as.character(round(value, -2))) ->
      NetCLUCValue

    AgWater_cum_basin_SF %>%
      ggcamMapBasin(Facet_Var = "scenario") +
      geom_label(data = NetCLUCValue, aes(label = paste0(value, " bcm"), x = -130, y = -40),
                 size = 9, label.size = NA) +
      theme(axis.title =  element_blank()) +
      scale_fill_fermenter(n.breaks = 4,
                           palette = "PiYG", na.value="white", direction = -1,
                           guide = guide_coloursteps(
                             title.position = "top",
                             title.hjust = 0,
                             frame.colour = "black",
                             frame.linewidth = 0.3,
                             ticks.colour = "black" )) +
      labs(fill = "bcm",
           title = "(I) Cumulative irrigation water withdrawal impact") +
      theme(legend.key.width = unit(1.5,"line"), legend.key.height=unit(1.8,"line"),
            plot.title = element_text(hjust = 0, face = "bold"),
            legend.justification = "left",
            legend.direction = "vertical",   # keep each legend vertical
            legend.box = "vertical",
            legend.title = element_text(margin = margin(b = 8)) ) ->
      MainResults_CumIRRWater_Map; MainResults_CumIRRWater_Map


    ### patch plots I-J ----

    MainResults_CumIRRWater_Map + MainResults_CumCLUC_Map +
      plot_layout(guides = "collect", nrow = 1) &
      theme(legend.direction = "vertical",
            legend.box = "horizontal",
            legend.justification = "center") ->
      MainResults_StockImpactMaps; MainResults_StockImpactMaps

    legend_MainResults_StockImpactMaps <-
      cowplot::get_legend(MainResults_StockImpactMaps +
                            theme(legend.position = "right"))

    MainResults_CumIRRWater_Map + MainResults_CumCLUC_Map +
      plot_layout(guides = "collect", nrow = 1) &
      theme(legend.direction = "vertical",
            legend.box = "horizontal",
            legend.position = "none") ->
      MainResults_StockImpactMaps_NoLeg; MainResults_StockImpactMaps_NoLeg

    patchwork::wrap_plots(
      MainResults_StockImpactMaps_NoLeg,
      patchwork::wrap_elements(legend_MainResults_StockImpactMaps),
      ncol = 2,
      widths = c(0.9, 0.13) ) ->
      MainResults_StockImpactMaps;MainResults_StockImpactMaps

    return(MainResults_StockImpactMaps)

  }



