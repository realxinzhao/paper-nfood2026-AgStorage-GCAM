
## Ag Emission  ----
### Read in GWP mapping ----
readr::read_csv("paper/data/maps/GWP.csv") -> GWP

EMs_NCEM %>%
  Reg_Clean() %>%
  filter(grepl("CH4|N2O", ghg),
         grepl("AGR|AWB", ghg)) %>%
  rename(GHG = ghg) %>%
  left_join(GWP %>% replace_na(list(AR6all = 0)) %>% distinct(GHG, AR6all), by = "GHG") %>%
  mutate(GHGnc_AR6 = (value * AR6all)/1000) %>%
  transmute(branch, scenario, region,  sector = GHG, year, value = GHGnc_AR6) %>%
  mutate(sector = case_when(
    sector %in% c("CH4_AGR", "CH4_AWB") ~ "CH4_Ag",
    sector %in% c("N2O_AGR", "N2O_AWB") ~ "N2O_Ag",
    TRUE ~ sector)
  ) %>%
  group_by_at(vars(-value)) %>% summarize(value = sum(value), .groups = "drop") %>%
  bind_rows(
    EMs_CLUC %>%
      Agg_reg(branch, region) %>%
      Reg_Clean %>%
      mutate(sector = "LULUCF", value = value * 44/12 / 1000)) %>%
  Year_Clean() ->
  EMs_NCEM_Ag


## Agg land by sector ----

AgLU_Aggland %>%
  Reg_Clean() %>%
  left_join_error_no_match(
    LandMapping %>% distinct(landleaf = LandLeaf, land = LandCover4), by = "landleaf") %>%
  # to Mha
  mutate(value = value /10) %>%
  Agg_reg(branch, land, region) %>%
  mutate(land = factor(land, levels = c(
    "Cropland: Grains", "Cropland: Oil crops", "Cropland: Sugar crops",  "Cropland: Other crops",
    "Cropland: Purpose-grown", "Cropland: Others",
    "Pasture", "Forest", "Other natural", "Other land"),
    labels = c(
      "Grains", "Oil crops", "Sugar crops","Other crops",
      "Other cropland", "Other cropland",
      "Pasture", "Forest", "Other natural", "Other land"))) %>%
  filter(!grepl("Other land", land)) %>%
  Agg_reg(branch, region, land) %>%
  Year_Clean ->
  PLand

## Ag labor by sector ----
AgLU_LaborSec |>
  Agg_reg(branch, region, sector) %>%
  Reg_Clean() |>
  AgSector_Clean() |>
  Year_Clean() ->
  Plabor
## Ag capital by sector ----
AgLU_CapitalSec|>
  Agg_reg(branch, region, sector) %>%
  Reg_Clean() |>
  AgSector_Clean() |>
  Year_Clean() |>
  mutate(value = value * gdp_deflator(2021, 1975)) ->
  Pcapital

## Ag water by sector ----
AgLU_Water_WithdrawTech %>%
  Agg_reg(branch, region, sector) %>%
  Reg_Clean() |>
  AgSector_Clean() |>
  Year_Clean() ->
  Pwater

## Ag Fertilizer ----
AgLU_FertSec %>%
  Agg_reg(branch, region, sector) %>%
  Reg_Clean() |>
  AgSector_Clean() |>
  Year_Clean() ->
  Pfertilizer


# SUA data ----

# This module prepare all the data needed to generated SUA by GCAM commodity


## AgElement_SUA_Demand ----
AgLU_Bal_Ag %>%
  bind_rows(AgLU_Bal_Livestock) %>%
  filter(!input %in% c("regional sawnwood", "regional woodpulp", "OtherMeat_Fish", "Forest")) %>%
  mutate(input = gsub("regional |_", "", input) ) %>%
  mutate(sector = if_else(grepl("NonFoodDemand", sector), "Other use", sector),
         sector = if_else(grepl("ethanol|biomassOil", sector), "Bioenergy", sector),
         sector = if_else(grepl("FoodDemand", sector), "Food", sector),
         sector = if_else(grepl("FeedCrops|Fodder", sector), "Feed", sector)) %>%
  Agg_reg(branch, region, element = sector, sector = tolower(input)) %>%
  Reg_Clean %>%
  AgSector_Clean %>%
  Year_Clean ->
  AgElement_SUA_Demand

## AgElement_SUA_Storage ----

### First at GCAM region and sectors----
bind_rows(
  AgLU_AgStorageClose %>%
    select(-any_of("x1")) %>%
    transmute(branch, scenario, year, region, sector, value, element = "ClosingStockBeforeLoss"),
  AgLU_AgStorageOpen  %>%
    select(-any_of("x1")) %>%
    transmute(branch, scenario, year, region, sector, value, element = "Opening stocks"),
  AgLU_AgStorageCloseLoss %>%
    select(-any_of("x1")) %>%
    transmute(branch, scenario, year,region, sector, value, element = "Closing stocks")
) %>%
  mutate(sector = gsub("regional |_", "", sector)) %>%
  Year_Clean %>%
  spread(element, value) %>%
  mutate(`Stock loss` = ClosingStockBeforeLoss -`Closing stocks`) %>%
  select(-ClosingStockBeforeLoss) %>%
  gather(element, value, -branch:-sector) ->
  AgElement_SUA_Storage_R_C


# AgElement_SUA_Storage_R_C %>% saveRDS("AgElement_SUA_Storage_R_C.rds")



## load data need from drake (run driver_drake() if not already) ----

MODULE_INPUTS <-
  c(FILE = "common/GCAM_region_names",
    "FAO_Stock_Macronutrient_All",
    "FAO_Food_Macronutrient_All",
    "L101.Pop_thous_Scen_R_Y",
    "L2053.AgLand_BiophysicalProducitivityRatio_ag_R_C_Y_scen")

MODULE_INPUTS %>% load_from_cache() -> all_data
get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


FAO_Stock_Macronutrient_All %>%
  filter(macronutrient == "MKcal") %>%
  left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
  group_by(region, year, GCAM_commodity, macronutrient) %>%
  summarize(value = sum(value), .groups = "drop") %>%
  spread(macronutrient, value) %>%
  # base year average
  filter(year %in% 2020:2022) %>%
  group_by_at(vars(-year, -MKcal)) %>%
  summarize(MKcal = sum(MKcal) / 3, .groups = "drop") %>%
  # change unit to Ecal; Pcal indeed!! (updated)
  mutate(Pcal = MKcal *10^9/10^15) %>%
  select(-MKcal) ->
  Stock_Calories_BY_Pcal_R_C

# adding conversion here and Pcal results
AgElement_SUA_Storage_R_C %>%
  # base year
  filter(year == 2021,
         scenario == "S1_Dynamic") %>%
  filter(element %in% c("Closing stocks")) %>%
  left_join_error_no_match(Stock_Calories_BY_Pcal_R_C %>% mutate(sector = tolower(GCAM_commodity)),
                           by = c("sector", "region")) %>%
  transmute(region, sector, PcalPerMt = Pcal / value) %>%
  left_join(AgElement_SUA_Storage_R_C,
            ., by = c("region", "sector")) %>%
  mutate(Pcal = value * PcalPerMt) %>%
  rename(Mt = value) %>% select(-PcalPerMt) %>%
  gather(measure, value, Mt, Pcal) %>%
  # aggregation here
  Reg_Clean %>%
  AgSector_Clean %>%
  Year_Clean %>%
  mutate(element = if_else(measure == "Pcal", paste0(element, "_", measure), element)) %>%
  select(-measure) ->
  AgElement_SUA_Storage


## AgElement_SUA_Trade ----
bind_rows(
  AgLU_RegAgsource %>%
    filter(units == "Mt") %>%
    Reg_Clean %>%
    mutate(sector = gsub("total |_", "", sector)) %>%
    filter(grepl("imported", subsector)) %>%
    Agg_reg(branch, region, sector, element = "Import"),
  AgLU_TradedSource %>%
    filter(units == "Mt") %>%
    mutate(sector = gsub("traded |_", "", sector)) %>%
    filter(sector != "iron and steel") %>%
    group_by(sector) %>%
    mutate(subsector = gsub(unique(sector),"", subsector),
           subsector = gsub("nuts_seeds|root_tuber","", subsector),
           subsector = gsub(" traded ","", subsector)) %>% ungroup() %>%
    ungroup() %>%
    Agg_reg(branch, region = subsector, sector, element = "Export") %>%
    Reg_Clean) %>%
  AgSector_Clean %>%
  Year_Clean ->
  AgElement_SUA_Trade

## AgElement_SUA_ProdValue ----
AgLU_Agprod %>%
  Agg_reg(branch, region, sector = tolower(sector), element = "Production") %>%
  bind_rows(
    AgLU_MeatProd %>%
      Agg_reg(branch, region, sector = tolower(sector), element = "Production")) %>%
  spread(element, value) %>%
  left_join_error_no_match(
    AgLU_Agprices %>%
      bind_rows(AgLU_Meatprices) %>%
      select(-units) %>%
      mutate(sector = tolower(sector)) %>%
      rename(Price = value),
    by = c("branch", "scenario", "region", "sector", "year") ) %>%
  mutate(Revenue = Production * Price) %>% select(-Price) %>%
  gather(element, value, Production, Revenue) %>%
  Reg_Clean %>%
  AgSector_Clean %>%
  Year_Clean %>%
  spread(element, value) %>%
  mutate(Price = Revenue / Production) %>%
  gather(element, value, Production:Price)  ->
  AgElement_SUA_ProdValue

## AgElement_Input_LandValue ----
AgLU_Detailedland %>%
  rename(land = value) %>%
  left_join(
    AgLU_Agrental %>%
      select(-units) %>%
      rename(rental = value),
    by = c("branch", "scenario", "region", "landleaf", "year") ) %>%
  mutate(value = land * rental) %>%
  mutate(landleaf = gsub("_IRR|_hi|_lo|_RFD|Hardwood_|Softwood_", "", landleaf)) %>%
  # remove unmanaged
  filter(!grepl("Unmanaged|UrbanLand|Tundra|Protected|Rock|Grassland|Shrubland|OtherArableLand",
                landleaf)) %>%
  separate(col = landleaf, into = c("sector", "basin"), remove = F) %>%
  # fix sector names
  mutate(sector = gsub("C4$|Tree$", "", sector)) %>%
  mutate(sector = gsub("biomassGrass", "biomass", sector)) %>%
  group_by(branch, scenario, region, sector, year) %>%

  summarize(Rent = sum(value) / 10^9, # to billion $
            Land = sum(land) / 10, # to Mha
            .groups = "drop") ->
  ValueLand

ValueLand %>%
  gather(var, value, Rent, Land) %>%
  Reg_Clean %>%
  AgSector_Clean %>%
  Year_Clean %>%
  spread(var, value) %>%
  mutate(Rental = Rent / Land * 1000) %>%
  filter(!(sector == "Energy crops" & year <= 2025)) %>%
  filter(!is.na(Rental))->
  AgElement_Input_LandValue


## AgElement_Input_LandYield ----
AgElement_Input_LandValue %>%
  left_join(
    AgElement_SUA_ProdValue %>%
      #filter(sector != "Livestock") %>%
      spread(element, value),
    by = c("branch", "scenario", "year", "region", "sector") ) %>%
  transmute(branch, scenario, region, year, sector, Yield_Land = Production / Land, Area = Land) %>%
  gather(element, value, Yield_Land, Area) ->
  AgElement_Input_LandYield



## AgElement_Input_LandBioYield ----
AgLU_AgprodByTech %>%
  filter(year >= 2021) %>%
  separate(col = get("technology"), into = c("crop","basin", "water","tech")) %>%
  select(branch, scenario, region, sector, crop, basin, water, tech, year, ProdMt = value) %>%
  left_join_error_no_match(
    AgLU_Detailedland %>%
      separate(col = get("landleaf"), into = c("crop","basin", "water","tech")) %>%
      transmute(branch, scenario, region, crop, basin, water, tech, year, AreaMha = value/10),
    by = c("branch", "scenario", "region", "crop", "basin", "water", "tech", "year")
  ) -> ProdArea

ProdArea %>%
  group_by_at(vars(-year, -ProdMt, -AreaMha)) %>%
  mutate(AreaMha_initial = first(AreaMha)) %>%
  ungroup() %>%
  group_by(branch, scenario, region, sector, year) %>%
  summarize(ProdMt = sum(ProdMt / AreaMha * AreaMha_initial),
            AreaMha_initial = sum(AreaMha_initial),
            .groups = "drop") %>%
  gather(element, value, ProdMt, AreaMha_initial) %>%
  Reg_Clean %>%
  AgSector_Clean %>%
  Year_Clean %>%
  spread(element, value) %>%
  mutate(BioYield_Land = ProdMt / AreaMha_initial) %>%
  select(-AreaMha_initial, -ProdMt) %>%
  gather(element, value, BioYield_Land)->
  AgElement_Input_LandBioYield





# Bind SUA elements ----
c("Opening stocks", "Production", "Import") -> ElEMSupply
c("Export", "Food", "Feed", "Bioenergy", "Other use", "Stock loss",  "Closing stocks") -> ELEMDemand
c(ElEMSupply, ELEMDemand) -> ELEMAll
c(ElEMSupply, ELEMDemand %>% rev) -> ELEMLevel
c(paste0("Supply: ", ElEMSupply), paste0("Demand: ", ELEMDemand) %>% rev ) -> ELEMLabel
c(paste0("Supply: ", ElEMSupply), paste0("Demand: ", ELEMDemand) ) -> ELEMLabel1

bind_rows(AgElement_SUA_Demand,
          AgElement_SUA_Storage,
          AgElement_SUA_Trade,
          AgElement_SUA_ProdValue) %>%
  filter(!sector %in% c("Forest", "Fodder crops", "Energy crops"),
         element %in% ELEMAll) ->
  AgElement_SUA

assertthat::assert_that(
  AgElement_SUA %>% filter(sector != "Nonstorable crops") |>
    spread(element, value, fill = 0) %>%
    mutate(RegSupply = rowSums(across(all_of(ElEMSupply)), na.rm = TRUE),
           RegDemand = rowSums(across(all_of(ELEMDemand)), na.rm = TRUE)) %>%
    gather(element, value, -scenario:-sector) %>%
    filter(element %in% c("RegSupply", "RegDemand")) %>%
    spread(element, value) %>%
    mutate(diff = abs(RegDemand - RegSupply)) %>%
    filter(diff > 0.5) %>% nrow() == 0 )


AgElement_SUA |>
  filter(year == 2021, scenario == "S1_Dynamic", sector == "Grains") |>
  spread(element, value) -> A

#A |> readr::write_csv("Grain2021SUA.csv")



# Appendix ----
# GCAM AgLandProducitivityRatio (Not really used)
# This is just to confirm source data == output data in biophysical yield drivers

L2053.AgLand_BiophysicalProducitivityRatio_ag_R_C_Y_scen |>
  filter(scenario == "ref") ->
  L2053.AgLandProducitivityRatio_ag_R_C_Y



# Done ----






# library("RColorBrewer")
# display.brewer.pal(n = 8, name = 'RdBu')
#
# c(brewer.pal(n = length(ELEMAll), name = "Spectral")[1:length(ElEMSupply)],
#   brewer.pal(n = length(ELEMAll), name = "Spectral")[length(ElEMSupply)+1:length(ELEMDemand)]
# ) -> ColUpdate
#
#
# AgElement_SUA %>%
#   Agg_reg(branch, element, sector, region) %>%
#   #filter(sector %in% tolower(GCAMCOMM[1:13])) %>%
#   #mutate(sector = factor(sector, levels = tolower(GCAMCOMM))) %>%
#   mutate(DS = if_else(element %in% ElEMSupply, "Supply", "Demand")) %>%
#   mutate(value = if_else(!element %in% ElEMSupply, -value, value)) %>%
#   mutate(element = factor(element, levels = ELEMLevel, labels = ELEMLabel)) ->
#   df1
#
#
# df1 %>% filter(region == "World", year >= 2015) %>%  #distinct(scenario)
#   filter(#sector %in% "Staple crops",
#     scenario %in% c("S1_Dynamic", "S2_Hidden")) %>%
#   Proc_Diff(type = "A", -scenario) %>% filter(scenario != "S2_Hidden") %>%
#   ggplot + facet_wrap(~sector) +
#   guides(colour = guide_legend(order = 2),
#          fill = guide_legend(order = 1)) +
#   #facet_grid(policy~SSP, scales = "free_y") +
#   geom_hline(yintercept = 0) +
#   geom_bar(aes(x = year, y = value, fill = element), stat = "identity", position = "stack",
#            color = "black") +
#   scale_color_manual(values = "black") +
#   labs(x = "Year", y = "Mt", fill = "Element") +
#   scale_fill_manual(values = c("Supply: Opening stocks" = ColUpdate[1], "Supply: Production" = ColUpdate[2],
#                                "Supply: Import" = ColUpdate[3], "Demand: Export" = ColUpdate[4],
#                                "Demand: Food" = ColUpdate[5],"Demand: Feed" = ColUpdate[6], "Demand: Bioenergy" = ColUpdate[7],
#                                "Demand: Other use" = ColUpdate[8],"Demand: Stock loss" = ColUpdate[9],
#                                "Demand: Closing stocks" = ColUpdate[10] ) ) +
#   theme_bw() + theme0 +
#   theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
#         panel.spacing.y = unit(0.5, "lines"),
#         panel.spacing.x = unit(0.5, "lines"))+
#   ggtitle(paste0("Difference in SUA for ",  " , Updated - Master, global (not incl. storage)")) -> A2; A2














