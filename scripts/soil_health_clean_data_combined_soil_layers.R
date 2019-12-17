## Load libraries ####
library(tidyverse)
library(mice)

## read in data ####
soil_health_raw_data_combined_soil_layers <- read.csv("~/Desktop/research/UMD_org_soil_MA/Crystal-Ornelas_Thapa_Tully_datasheet - datasheet_combined_soil_depth_v1.csv", header = TRUE)
dim(soil_health_raw_data_combined_soil_layers)

# first, separate out SOC and MBC measurements
raw_data_SOC_combined <- filter(soil_health_raw_data_combined_soil_layers, outcome_standardized == "SOC (g/kg)") 
dim(raw_data_SOC_combined)
raw_data_MBC_combined <- filter(soil_health_raw_data_combined_soil_layers, outcome_standardized == "MBC (ug/g)")

raw_data_SOC_with_nas_combined <- dplyr::select(raw_data_SOC_combined, study_code,weighted_control_mean_standardized, weighted_control_sd_standardized, control_n,weighted_treatment_mean_standardized,weighted_treatment_sd_standardized,treatment_n)
raw_data_MBC_with_nas_combined <- dplyr::select(raw_data_MBC_combined, study_code,weighted_control_mean_standardized, weighted_control_sd_standardized, control_n,weighted_treatment_mean_standardized,weighted_treatment_sd_standardized,treatment_n)

# Imputation for SOC data ####
raw_data_SOC_with_nas_combined <- raw_data_SOC_with_nas_combined %>%
  mutate(
    study_code = as.factor(study_code),
    weighted_control_mean_standardized = as.numeric(weighted_control_mean_standardized),
    weighted_control_sd_standardized = as.numeric(weighted_control_sd_standardized),
    control_n = as.numeric(control_n),
    weighted_treatment_mean_standardized = as.numeric(weighted_treatment_mean_standardized),
    weighted_treatment_sd_standardized = as.numeric(weighted_treatment_sd_standardized),
    treatment_n = as.numeric(treatment_n)
  )

# Here's some boiler plate code for starting the imputation
init <- mice(raw_data_SOC_with_nas_combined, maxit = 0) 
init
meth <- init$method
meth
predM <- init$predictorMatrix
predM

# We can remove some meaningless variables as predictors, for example study code
predM
predM[, c("study_code")] = 0

# Skip any variables for imputation, this variable will be used for prediction
meth[c("weighted_control_mean_standardized","weighted_treatment_mean_standardized","control_n","treatment_n")]=""

# What are we going to impute, gotta pick different methods for ordinal, factors or continuous
meth[c("weighted_control_sd_standardized","weighted_treatment_sd_standardized")]="norm" 

# Now it's time to run the multiple imputation
imputed_sds_SOC_combined <- mice(raw_data_SOC_with_nas_combined, method="pmm", predictorMatrix=predM, m=5, seed = 100)
# impute data with probable means
class(imputed_sds_SOC_combined)

# Create a dataset after the imputation
imputed_sds_SOC_combined <- mice::complete(imputed_sds_SOC_combined)
imputed_sds_SOC_combined
# Do we still havy missing values? Hopefully not!
sapply(imputed_sds_SOC_combined, function(x) sum(is.na(x)))

# Now that we have imputed values, replace whole column in R
raw_data_SOC_combined$weighted_control_sd_standardized <- imputed_sds_SOC_combined$weighted_control_sd_standardized
raw_data_SOC_combined$weighted_treatment_sd_standardized <- imputed_sds_SOC_combined$weighted_treatment_sd_standardized

## Imputation for MBC data ####
raw_data_MBC_with_nas_combined <- raw_data_MBC_with_nas_combined %>%
  mutate(
    study_code = as.factor(study_code),
    weighted_control_mean_standardized = as.numeric(weighted_control_mean_standardized),
    weighted_control_sd_standardized = as.numeric(weighted_control_sd_standardized),
    control_n = as.numeric(control_n),
    weighted_treatment_mean_standardized = as.numeric(weighted_treatment_mean_standardized),
    weighted_treatment_sd_standardized = as.numeric(weighted_treatment_sd_standardized),
    treatment_n = as.numeric(treatment_n)
  )

# Here's some boiler plate code for starting the imputation
init <- mice(raw_data_MBC_with_nas_combined, maxit = 0) 
init
meth <- init$method
meth
predM <- init$predictorMatrix
predM

# We can remove some meaningless variables as predictors, for example study code
predM
predM[, c("study_code")] = 0

# Skip any variables for imputation, this variable will be used for prediction
meth[c("weighted_control_mean_standardized","weighted_treatment_mean_standardized","control_n","treatment_n")]=""

# What are we going to impute, gotta pick different methods for ordinal, factors or continuous
meth[c("weighted_control_sd_standardized","weighted_treatment_sd_standardized")]="norm" 

# Now it's time to run the multiple imputation
imputed_sds_MBC_combined <- mice(raw_data_MBC_with_nas_combined, method="pmm", predictorMatrix=predM, m=5, seed = 100)
# impute data with probable means
class(imputed_sds_MBC_combined)

# Create a dataset after the imputation
imputed_sds_MBC_combined <- mice::complete(imputed_sds_MBC_combined)
imputed_sds_MBC_combined
# Do we still havy missing values? Hopefully not!
sapply(imputed_sds_MBC_combined, function(x) sum(is.na(x)))

# Now that we have imputed values, replace whole column in R
raw_data_MBC_combined$weighted_control_sd_standardized <- imputed_sds_MBC_combined$weighted_control_sd_standardized
raw_data_MBC_combined$weighted_treatment_sd_standardized <- imputed_sds_MBC_combined$weighted_treatment_sd_standardized


