
# Author: Xin Zhao (xin.zhao@pnnl.gov)
# The folder (paper) inside gcamdata include R code for visualization

# Load gcamdata and related packages
devtools::load_all()

# Ensure driver_drake() was completed!!!

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggsci)
library(scales)

library(purrr)
library(patchwork)
library(RColorBrewer)

source("paper/func/AgLU_DashBoard_Data_Func.R")
source("paper/func/LoadPackagesFuncs.R")
source("paper/func/GCAM_module_funcs.R")


DIR_DATA <- "data"
DIR_OUTPUT <- "paper/output"
Project <- ""



# Figs. 1 & 2 ----
source("./paper/R/Methods_1_HistStorageAnalysis.R")
## Supplementary Fig. 2-7 ----
source("./paper/R/Methods_2_HistStorageAnalysis_SuppInfo.R")


# Results Figs ----

## Load scenario data ----
## download from https://zenodo.org/records/23611035
AgLU_cache <- readRDS("paper/cache/GCAM_queries_MainScenarios.rds")
list2env(AgLU_cache, .GlobalEnv)

## To query and processing the data from the original scenario database,  see Results_1_Database.R

## Prepare scenario data
source("./paper/R/Results_1_PrepareData.R")

## Fig. 4  ----
# S1 vs. S2
source("./paper/R/Results_2_StockImpacts.R")
## Other sensitivity scenarios results can be generated in the same script
## by updating scenario parameters and file names when saving.


## Supplementary Fig. 8  ----
Scenario_Storage <- "S1_Dynamic"
source("./paper/R/Results_2_AgLUDashBoard_SingleScen.R")

## Supplementary Fig. 9  ----
Scenario_Storage <- "S2_Hidden"
source("./paper/R/Results_2_AgLUDashBoard_SingleScen.R")

## Supplementary Fig. 10 impact ----
Scenario_Storage <- "S1_Dynamic"
Scenario_NoStorage <- "S2_Hidden"
source("./paper/R/Results_2_AgLUDashBoard_DiffScen.R")


## Fig. 5 ----

## prepare data ----
source("./paper/R/Results_3_BioShockImpacts_Data.R")

## Supplementary Fig biophysical shocks ----
source("./paper/R/Results_3p1_BioShockImpacts_Boxplots_BioYield.R")
source("./paper/R/Results_3p2_BioShockImpacts_Boxplots_AgEcon.R")
source("./paper/R/Results_3p3_BioShockImpacts_Boxplots_NetStockTrade.R")

source("./paper/R/Results_3p4_BioShockImpacts_Boxplots_BindAll.R")


# relationship
source("./paper/R/Results_4p1_BioShockImpacts_Regression_PooledFig.R")
source("./paper/R/Results_4p2_BioShockImpacts_Fig5_All.R")













