
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






