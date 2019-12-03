## Load libraries ####
library(tidyverse)

## read in data ####
soil_health_raw_data <- read.csv("~/Desktop/research/UMD_org_soil_MA/Crystal-Ornelas_Thapa_Tully_datasheet - datasheet_v2.csv", header = TRUE)

raw_data_SOC <- filter(soil_health_raw_data, outcome_standardized == "SOC (g/kg)") 
raw_data_MBC <- filter(soil_health_raw_data, outcome_standardized == "MBC (ug/g)")
