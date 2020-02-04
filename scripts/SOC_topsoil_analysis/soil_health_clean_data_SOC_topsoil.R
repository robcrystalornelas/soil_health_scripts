## Load libraries ####
library(tidyverse)
library(mice)

## read in data ####
soil_health_raw_data_topsoil_SOC <- read.csv("~/Desktop/research/UMD_org_soil_MA/Crystal-Ornelas_Thapa_Tully_datasheet - datasheet_topsoil_SOC.csv", header = TRUE)
dim(soil_health_raw_data_topsoil_SOC)

# Get the columns of data we need
raw_data_SOC_with_nas_topsoil <- dplyr::select(soil_health_raw_data_topsoil_SOC, 
                                               study_code,
                                               first_author,
                                               control_mean_standardized_SOC_ten, 
                                               control_sd_standardized_SOC_ten, 
                                               control_n,
                                               treatment_mean_standardized_SOC_ten,
                                               treatment_sd_standardized_SOC_ten,
                                               treatment_n)

# Imputation for SOC data ####
raw_data_SOC_with_nas_topsoil <- raw_data_SOC_with_nas_topsoil %>%
  mutate(
    study_code = as.factor(study_code),
    control_mean_standardized_SOC_ten = as.numeric(control_mean_standardized_SOC_ten),
    control_sd_standardized_SOC_ten = as.numeric(control_sd_standardized_SOC_ten),
    control_n = as.numeric(control_n),
    treatment_mean_standardized_SOC_ten = as.numeric(treatment_mean_standardized_SOC_ten),
    treatment_sd_standardized_SOC_ten = as.numeric(treatment_sd_standardized_SOC_ten),
    treatment_n = as.numeric(treatment_n)
  )

# Here's some boiler plate code for starting the imputation
init <- mice(raw_data_SOC_with_nas_topsoil, maxit = 0) 
init
meth <- init$method
meth
predM <- init$predictorMatrix
predM

# We can remove some meaningless variables as predictors, for example study code
predM
predM[, c("study_code")] = 0
predM[, c("first_author")] = 0

# Skip any variables for imputation, this variable will be used for prediction
meth[c("control_mean_standardized_SOC_ten","treatment_mean_standardized_SOC_ten","control_n","treatment_n")]=""

# What are we going to impute, gotta pick different methods for ordinal, factors or continuous
meth[c("control_sd_standardized_SOC_ten","treatment_sd_standardized_SOC_ten")]="norm" 

# Now it's time to run the multiple imputation
imputed_sds_SOC_topsoil <- mice(raw_data_SOC_with_nas_topsoil, method="pmm", predictorMatrix=predM, m=5, seed = 100)
# impute data with probable means
class(imputed_sds_SOC_topsoil)

# Create a dataset after the imputation
imputed_sds_SOC_topsoil <- mice::complete(imputed_sds_SOC_topsoil)
imputed_sds_SOC_topsoil
# Do we still havy missing values? Hopefully not!
sapply(imputed_sds_SOC_topsoil, function(x) sum(is.na(x)))

# Now that we have imputed values, replace whole column in R
soil_health_raw_data_topsoil_SOC$control_sd_standardized_SOC_ten <- imputed_sds_SOC_topsoil$control_sd_standardized_SOC_ten
soil_health_raw_data_topsoil_SOC$treatment_sd_standardized_SOC_ten <- imputed_sds_SOC_topsoil$treatment_sd_standardized_SOC_ten
