## Load libraries ####
library(tidyverse)
library(mice)

## read in data ####
soil_health_raw_data <- read.csv("~/Desktop/research/UMD_org_soil_MA/Crystal-Ornelas_Thapa_Tully_datasheet - datasheet_v2.csv", header = TRUE)

# first, separate out SOC and MBC measurements
raw_data_SOC <- filter(soil_health_raw_data, outcome_standardized == "SOC (g/kg)") 
raw_data_MBC <- filter(soil_health_raw_data, outcome_standardized == "MBC (ug/g)")

raw_data_SOC_with_nas <- dplyr::select(raw_data_SOC, study_code,control_mean_standardized, control_sd_standardized, control_n,treatment_mean_standardized,treatment_sd_standardized,treatment_n)
raw_data_MBC_with_nas <- dplyr::select(raw_data_MBC, study_code,control_mean_standardized, control_sd_standardized, control_n,treatment_mean_standardized,treatment_sd_standardized,treatment_n)

# Imputation for SOC data ####
raw_data_SOC_with_nas <- raw_data_SOC_with_nas %>%
  mutate(
    study_code = as.factor(study_code),
    control_mean_standardized = as.numeric(control_mean_standardized),
    control_sd_standardized = as.numeric(control_sd_standardized),
    control_n = as.numeric(control_n),
    treatment_mean_standardized = as.numeric(treatment_mean_standardized),
    treatment_sd_standardized = as.numeric(treatment_sd_standardized),
    treatment_n = as.numeric(treatment_n)
  )

# Here's some boiler plate code for starting the imputation
init <- mice(raw_data_SOC_with_nas, maxit = 0) 
init
meth <- init$method
meth
predM <- init$predictorMatrix
predM

# We can remove some meaningless variables as predictors, for example study code
predM
predM[, c("study_code")] = 0

# Skip any variables for imputation, this variable will be used for prediction
meth[c("control_mean_standardized","treatment_mean_standardized","control_n","treatment_n")]=""

# What are we going to impute, gotta pick different methods for ordinal, factors or continuous
meth[c("control_sd_standardized","treatment_sd_standardized")]="norm" 

# Now it's time to run the multiple imputation
imputed_sds_SOC <- mice(raw_data_SOC_with_nas, method="pmm", predictorMatrix=predM, m=5, seed = 100)
# impute data with probable means
class(imputed_sds_SOC)

# Create a dataset after the imputation
imputed_sds_SOC <- mice::complete(imputed_sds_SOC)
imputed_sds_SOC
# Do we still havy missing values? Hopefully not!
sapply(imputed_sds_SOC, function(x) sum(is.na(x)))

# Now that we have imputed values, replace whole column in R
raw_data_SOC$control_sd_standardized <- imputed_sds_SOC$control_sd_standardized
raw_data_SOC$treatment_sd_standardized <- imputed_sds_SOC$treatment_sd_standardized


## Imputation for MBC data ####
raw_data_MBC_with_nas <- raw_data_MBC_with_nas %>%
  mutate(
    study_code = as.factor(study_code),
    control_mean_standardized = as.numeric(control_mean_standardized),
    control_sd_standardized = as.numeric(control_sd_standardized),
    control_n = as.numeric(control_n),
    treatment_mean_standardized = as.numeric(treatment_mean_standardized),
    treatment_sd_standardized = as.numeric(treatment_sd_standardized),
    treatment_n = as.numeric(treatment_n)
  )

# Here's some boiler plate code for starting the imputation
init <- mice(raw_data_MBC_with_nas, maxit = 0) 
init
meth <- init$method
meth
predM <- init$predictorMatrix
predM

# We can remove some meaningless variables as predictors, for example study code
predM
predM[, c("study_code")] = 0

# Skip any variables for imputation, this variable will be used for prediction
meth[c("control_mean_standardized","treatment_mean_standardized","control_n","treatment_n")]=""

# What are we going to impute, gotta pick different methods for ordinal, factors or continuous
meth[c("control_sd_standardized","treatment_sd_standardized")]="norm" 

# Now it's time to run the multiple imputation
imputed_sds_MBC <- mice(raw_data_MBC_with_nas, method="pmm", predictorMatrix=predM, m=5, seed = 100)
# impute data with probable means
class(imputed_sds_MBC)

# Create a dataset after the imputation
imputed_sds_MBC <- mice::complete(imputed_sds_MBC)
imputed_sds_MBC
# Do we still havy missing values? Hopefully not!
sapply(imputed_sds_MBC, function(x) sum(is.na(x)))

# Now that we have imputed values, replace whole column in R
raw_data_MBC$control_sd_standardized <- imputed_sds_MBC$control_sd_standardized
raw_data_MBC$treatment_sd_standardized <- imputed_sds_MBC$treatment_sd_standardized




