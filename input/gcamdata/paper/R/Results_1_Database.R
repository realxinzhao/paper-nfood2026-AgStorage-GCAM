
Load_Cached_Data = T

if (Load_Cached_Data == T) {

  # cache for the 6 main scenarios (Fig3 scenario matrix)
  AgLU_cache <- readRDS("paper/cache/GCAM_queries_MainScenarios.rds")
  list2env(AgLU_cache, .GlobalEnv)

  }


if (Load_Cached_Data == F) {

  # Create cache

  # Load gcamdata and related packages

  devtools::load_all()

  library(dplyr)
  library(xml2)
  library(rgcam)

  source("paper/func/GCAM_DB_PullQuery.R")


  # Define queries (pull from Main_Query) ----

  Main_Query_Path <- paste0("../../output/queries")
  Main_Query_XML <- read_xml(file.path(Main_Query_Path, "Main_queries.xml"))

  # load scenario map from csv
  SCENARIO_DB_MAP <-
    readr::read_csv(file = "paper/data/SCENARIO_DB_MAP.csv", comment = "#") |>
    filter(incl == TRUE)


  Macro_NatAcc <- Batch_Query_Title("National Account")
  # Ag inputs
  AgLU_LaborSec <- Batch_Query_Title("Labor inputs by crop type")
  AgLU_CapitalSec <- Batch_Query_Title("Capital inputs by sector")
  AgLU_FertSec <- Batch_Query_Title("fertilizer consumption by crop type")


  ## Ag IRR water by tech ----
  AgLU_Water_WithdrawTech <- Batch_Query_Title("irrigation water withdrawals by ag tech")
  AgLU_Water_WithdrawAnTech <- Batch_Query_Title("water consumption by meat and dairy production")


  ## Ag Emissions ----
  EMs_CLUC <- Batch_Query_Title("LUC emissions by region")

  EMs_NCEM <- Batch_Query_Title("nonCO2 emissions by region")

  # AgElement_SUA  data ----

  # Bal
  AgLU_Bal_Ag <- Batch_Query_Title("demand balances by crop commodity")
  AgLU_Bal_Livestock <- Batch_Query_Title("demand balances by meat and dairy commodity")
  # Trade
  AgLU_RegAgsource <- Batch_Query_Title("ag import vs. domestic supply (Regional Armington competition)")
  AgLU_TradedSource <- Batch_Query_Title("ag export to the world center (USA) (Intl. Armington competition)")
  # Prod
  AgLU_Agprod <- Batch_Query_Title("ag production by crop type")
  AgLU_MeatProd <- Batch_Query_Title("meat and dairy production by tech")
  # Prices
  AgLU_Agprices <- Batch_Query_Title("ag commodity prices")
  AgLU_Meatprices <- Batch_Query_Title("meat and dairy prices")
  # Storage
  AgLU_AgStorageOpen <- Batch_Query_Title("opening stocks (supply from carryover)")
  AgLU_AgStorageClose <- Batch_Query_Title("closing stocks (before loss/adjustments)")
  AgLU_AgStorageCloseLoss <- Batch_Query_Title("closing stocks (after loss/adjustments; carried over)")
  # Detailed Land
  AgLU_Detailedland <- Batch_Query_Title("detailed land allocation")
  AgLU_Agrental <- Batch_Query_Title("profit rate")
  AgLU_AgprodByTech <- Batch_Query_Title("ag production by tech")
  ## Agg land by sector ----
  AgLU_Aggland <- Batch_Query_Title("land allocation by crop")




  ## Ag IRR water by tech ----
  AgLU_Water_WithdrawTech <- Batch_Query_Title("irrigation water withdrawals by ag tech")
  AgLU_Water_WithdrawAnTech <- Batch_Query_Title("water consumption by meat and dairy production")




  AgLU_cache <- list(
    # Macro
    Macro_NatAcc = Macro_NatAcc,

    # Ag inputs
    AgLU_LaborSec = AgLU_LaborSec,
    AgLU_CapitalSec = AgLU_CapitalSec,
    AgLU_FertSec = AgLU_FertSec,

    # Balances
    AgLU_Bal_Ag              = AgLU_Bal_Ag,
    AgLU_Bal_Livestock       = AgLU_Bal_Livestock,

    # Trade
    AgLU_RegAgsource         = AgLU_RegAgsource,
    AgLU_TradedSource        = AgLU_TradedSource,

    # Production
    AgLU_Agprod              = AgLU_Agprod,
    AgLU_MeatProd            = AgLU_MeatProd,
    AgLU_AgprodByTech        = AgLU_AgprodByTech,

    # Prices
    AgLU_Agprices            = AgLU_Agprices,
    AgLU_Meatprices          = AgLU_Meatprices,

    # Storage
    AgLU_AgStorageOpen       = AgLU_AgStorageOpen,
    AgLU_AgStorageClose      = AgLU_AgStorageClose,
    AgLU_AgStorageCloseLoss  = AgLU_AgStorageCloseLoss,

    # Water
    AgLU_Water_WithdrawTech  = AgLU_Water_WithdrawTech,
    AgLU_Water_WithdrawAnTech = AgLU_Water_WithdrawAnTech,

    # Emissions
    EMs_CLUC                 = EMs_CLUC,
    EMs_NCEM                 = EMs_NCEM,

    # Land
    AgLU_Detailedland        = AgLU_Detailedland,
    AgLU_Aggland             = AgLU_Aggland,
    AgLU_Agrental            = AgLU_Agrental,
    AgLU_Aggland             = AgLU_Aggland
  )


  saveRDS(
    AgLU_cache,
    file = "paper/cache/GCAM_queries_MainScenarios.rds",
    compress = "xz"   # strongest compression
  )


}

