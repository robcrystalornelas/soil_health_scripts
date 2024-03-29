## Load libraries ####
library(tidyverse)
library(mice)

## read in data ####
soil_health_raw_data_topsoil_MBC <- read.csv("~/Desktop/research/UMD_org_soil_MA/Crystal-Ornelas_Thapa_Tully_datasheet - datasheet_topsoil_MBC_v2.csv", header = TRUE)
dim(soil_health_raw_data_topsoil_MBC)

# get just the rows we need
raw_data_MBC_with_nas_topsoil <- dplyr::select(soil_health_raw_data_topsoil_MBC, 
                                               study_code, 
                                               control_mean, 
                                               control_sd, 
                                               control_n,
                                               treatment_mean,
                                               treatment_sd,
                                               treatment_n)


## Imputation for MBC data ####
raw_data_MBC_with_nas_topsoil <- raw_data_MBC_with_nas_topsoil %>%
  mutate(
    study_code = as.factor(study_code),
    control_mean = as.numeric(control_mean),
    control_sd = as.numeric(control_sd),
    control_n = as.numeric(control_n),
    treatment_mean = as.numeric(treatment_mean),
    treatment_sd = as.numeric(treatment_sd),
    treatment_n = as.numeric(treatment_n)
  )

# Here's some boiler plate code for starting the imputation
init <- mice(raw_data_MBC_with_nas_topsoil, maxit = 0) 
init
meth <- init$method
meth
predM <- init$predictorMatrix
predM

# We can remove some meaningless variables as predictors, for example study code
predM
predM[, c("study_code")] = 0

# Skip any variables for imputation, this variable will be used for prediction
meth[c("control_mean","treatment_mean","control_n","treatment_n")]=""

# What are we going to impute, gotta pick different methods for ordinal, factors or continuous
meth[c("control_sd","treatment_sd")]="norm" 

# Now it's time to run the multiple imputation
imputed_sds_topsoil <- mice(raw_data_MBC_with_nas_topsoil, method="pmm", predictorMatrix=predM, m=5, seed = 100)
# impute data with probable means
class(imputed_sds_topsoil)

# Create a dataset after the imputation
imputed_sds_topsoil <- mice::complete(imputed_sds_topsoil)
imputed_sds_topsoil

# Do we still havy missing values? Hopefully not!
sapply(imputed_sds_topsoil, function(x) sum(is.na(x)))

# Now that we have imputed values, replace whole column in R
soil_health_raw_data_topsoil_MBC$control_sd <- imputed_sds_topsoil$control_sd
soil_health_raw_data_topsoil_MBC$treatment_sd <- imputed_sds_topsoil$treatment_sd
soil_health_raw_data_topsoil_MBC
