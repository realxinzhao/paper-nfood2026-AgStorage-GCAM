# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_batch_ag_storage_xml
#'
#' Construct XML data structure for \code{ag_storage.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_storage.xml}.
#' @author XZ 2023
module_aglu_batch_ag_storage_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L113.StorageTechAndPassThrough",
      "L201.Pop_Scen")

  MODULE_OUTPUTS <-
    c(XML = "ag_storage.xml",
      XML = "ag_storage_updateSecOutRatio.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # 0. data ready ----
    L113.StorageTechAndPassThrough %>%
      filter(storage_model == TRUE) ->
      L113.StorageTechTable

    L113.StorageTechAndPassThrough %>%
      filter(storage_model == FALSE) %>%
      rename(technology = food.storage.technology) %>%
      mutate(technology = "no-storage-pass-through",
             minicam.non.energy.input = "border-cost", input.cost = 0) ->
      L113.PassThroughTable

    # 0. Shared tables

    # Pull region and sector to prepare for tables for xml generating
    L113.StorageTechAndPassThrough %>%
      distinct(region, supplysector, food.storage.technology) ->
      L113.ag_Storage_region_supplysector

    # Tables for xmls
    supplysec <-
      L113.ag_Storage_region_supplysector %>%
      dplyr::transmute(region, supplysector,
                       output.unit = "Mt", input.unit = "Mt",
                       price.unit = "1975$/kg", logit.year.fillout = min(MODEL_BASE_YEARS),
                       logit.exponent = -3, logit.type = NA)

    subsec <-
      L113.ag_Storage_region_supplysector %>%
      dplyr::transmute(region, supplysector,
                       subsector = supplysector, logit.year.fillout = min(MODEL_BASE_YEARS),
                       logit.exponent = 0, logit.type = NA)

    subsec_sw <-
      L113.ag_Storage_region_supplysector %>%
      dplyr::transmute(region, supplysector,
                       subsector = supplysector,
                       year.fillout = min(MODEL_BASE_YEARS), share.weight = 1)

    subsec_shwt <-
      L113.StorageTechAndPassThrough %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwt"]])


    # 1. tables ready for StorageTech commodities ----

    fst_interp <-
      L113.StorageTechTable %>%
      distinct(region, supplysector, food.storage.technology) %>%
      dplyr::transmute(region, supplysector,
                       subsector = supplysector,
                       food.storage.technology,
                       apply.to = "share-weight",
                       from.year = MODEL_FINAL_BASE_YEAR, to.year = max(MODEL_YEARS), interpolation.function = "fixed") %>%
      select(LEVEL2_DATA_NAMES[["FoodTechInterp"]])


    fst_extra <-
      L113.StorageTechTable %>%
      select(LEVEL2_DATA_NAMES[["FoodTech"]])


    fst_coef <-
      L113.StorageTechTable %>%
      mutate(coefficient = 1, market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["FoodTechCoef"]])

    # The cost here is not really useful for storage. It is cost to main product
    # fst_cost <-
    #   L113.StorageTechTable %>% mutate(minicam.non.energy.input = "storage-cost", input.cost = 0) %>%
    #   select(LEVEL2_DATA_NAMES[["FoodTechCost"]])

    fst_RESSecOut <-
      L113.StorageTechTable %>%
      mutate(res.secondary.output = GCAM_commodity,
             output.ratio = 1, pMultiplier = 0) %>%
      select(LEVEL2_DATA_NAMES[["FoodTechRESSecOut"]])


    # 2. tables ready for PassThrough commodities ----


    passthrough_interp <-
      L113.PassThroughTable %>%
      distinct(region, supplysector, technology) %>%
      dplyr::transmute(region, supplysector,
                       subsector = supplysector,
                       technology,
                       apply.to = "share-weight",
                       from.year = MODEL_FINAL_BASE_YEAR, to.year = max(MODEL_YEARS), interpolation.function = "fixed") %>%
      select(LEVEL2_DATA_NAMES[["TechInterp"]])

    passthrough_techshrwt <-
      L113.PassThroughTable %>%
      select(LEVEL2_DATA_NAMES[["TechShrwt"]])

    passthrough_coef <-
      L113.PassThroughTable %>%
      mutate(coefficient = 1, market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["TechCoef"]])

    passthrough_cost <-
      L113.PassThroughTable %>%
      select(LEVEL2_DATA_NAMES[["TechCost"]])


    # Produce outputs ----
    ag_storage.xml <-
      create_xml("ag_storage.xml") %>%
      add_logit_tables_xml(supplysec, "Supplysector") %>%
      add_logit_tables_xml(subsec, "SubsectorLogit") %>%
      add_xml_data(subsec_sw, "SubsectorShrwtFllt") %>%
      add_xml_data(subsec_shwt, "SubsectorShrwt") %>%
      add_xml_data(fst_interp, "FoodTechInterp") %>%
      add_xml_data(fst_extra, "FoodTech") %>%
      #add_xml_data(fst_cost, "FoodTechCost") %>%
      add_xml_data(fst_coef, "FoodTechCoef") %>%
      add_xml_data(fst_RESSecOut, "FoodTechRESSecOut") %>%
      add_xml_data(passthrough_interp, "TechInterp") %>%
      add_xml_data(passthrough_techshrwt, "TechShrwt") %>%
      add_xml_data(passthrough_coef, "TechCoef") %>%
      add_xml_data(passthrough_cost, "TechCost") %>%
      add_precursors("L113.StorageTechAndPassThrough")


    # The default storage structure assumes no stockbuilding in-between periods
    # This would be fine with annual step
    # With multiple-year step modeling, we make additional adjustments here

    # Update fst_RESSecOut output ratio ----


    L201.Pop_Scen %>%
      filter(scenario == "SSP2", year >= MODEL_FINAL_BASE_YEAR) %>%
      arrange(region, year) %>%
      group_by(scenario, region) %>%
      mutate(PopGrowthRatio = lead(totalPop)/totalPop) %>%
      replace_na(list(PopGrowthRatio = 1)) %>%
      ungroup ->
      L113.InterPeriod_PopGrowthRatio

    # L113.InterPeriod_PopGrowthRatio %>%
    #   ggplot() +
    #   geom_hline(yintercept = 1) +
    #   geom_line(aes(x = year, y = PopGrowthRatio, group = region)) +
    #   theme_bw()


      bind_rows(
        # For region with initial growth >1 set a floor at 1
        L113.InterPeriod_PopGrowthRatio %>%
          group_by(region) %>%
          filter(PopGrowthRatio[year == MODEL_FINAL_BASE_YEAR] >1) %>%
          mutate(PopGrowthRatio = pmax(1, PopGrowthRatio)),
        # For region with initial growth >1 set a floor at initial value
        L113.InterPeriod_PopGrowthRatio %>%
          group_by(region) %>%
          filter(PopGrowthRatio[year == MODEL_FINAL_BASE_YEAR] < 1, region != "Ukraine") %>%
          mutate(PopGrowthRatio = pmax(PopGrowthRatio[year == MODEL_FINAL_BASE_YEAR], PopGrowthRatio)),
        # For the special case of Ukraine, only adjust after 2035
        L113.InterPeriod_PopGrowthRatio %>%
          group_by(region) %>%
          filter(region == "Ukraine") %>%
          mutate(PopGrowthRatio = if_else(year >= 2035,
                 pmax(PopGrowthRatio[year == 2035], PopGrowthRatio),
                 PopGrowthRatio)) ) %>% ungroup ->
        L113.InterPeriod_PopGrowthRatio_floor

      # L113.InterPeriod_PopGrowthRatio_floor %>%
      #   ggplot() +
      #   geom_hline(yintercept = 1) +
      #   geom_line(aes(x = year, y = PopGrowthRatio, group = region)) +
      #   theme_bw()

      L113.InterPeriod_PopGrowthRatio_floor %>%
        transmute(region, year, output.ratio_update = PopGrowthRatio) ->
        fst_RESSecOut_outratio


    fst_RESSecOut %>%
      filter(year < MODEL_FINAL_BASE_YEAR) %>%
      bind_rows(
        fst_RESSecOut %>%
          filter(year >= MODEL_FINAL_BASE_YEAR) %>%
          left_join_error_no_match(
            fst_RESSecOut_outratio, by = c("region", "year")
          )  %>%
          mutate(output.ratio = output.ratio_update) %>%
          select(names(fst_RESSecOut))
      ) ->
        fst_RESSecOut_updated


    # Produce outputs ----
    ag_storage_updateSecOutRatio.xml <-
      create_xml("ag_storage_updateSecOutRatio.xml") %>%
      add_logit_tables_xml(supplysec, "Supplysector") %>%
      add_logit_tables_xml(subsec, "SubsectorLogit") %>%
      add_xml_data(subsec_sw, "SubsectorShrwtFllt") %>%
      add_xml_data(subsec_shwt, "SubsectorShrwt") %>%
      add_xml_data(fst_interp, "FoodTechInterp") %>%
      add_xml_data(fst_extra, "FoodTech") %>%
      #add_xml_data(fst_cost, "FoodTechCost") %>%
      add_xml_data(fst_coef, "FoodTechCoef") %>%
      add_xml_data(fst_RESSecOut_updated, "FoodTechRESSecOut") %>%
      add_xml_data(passthrough_interp, "TechInterp") %>%
      add_xml_data(passthrough_techshrwt, "TechShrwt") %>%
      add_xml_data(passthrough_coef, "TechCoef") %>%
      add_xml_data(passthrough_cost, "TechCost") %>%
      add_precursors("L113.StorageTechAndPassThrough")



    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
