## load libraries
library(metafor)
library(metaviz)

## import data
source(
  "~/Desktop/research/UMD_org_soil_MA/UMD_project/scripts/soil_health_clean_data.R"
)

## First, calculate effect sizes, based on the Ratio of Means (or Response Ratio) for each measurement  in our database
effect_sizes_SOC <-
  escalc(
    "ROM", # Specify the effect size we want to calculate
    m1i = raw_data_SOC$treatment_mean_standardized, # provide means for group 1
    n1i = raw_data_SOC$treatment_n, # treatment sample size
    sd1i = raw_data_SOC$treatment_sd_standardized, # treatment SD
    m2i = raw_data_SOC$control_mean_standardized, # control mean
    n2i = raw_data_SOC$control_n, # control sample size
    sd2i = raw_data_SOC$control_sd_standardized, # control SD
    data = raw_data_SOC
  )
head(effect_sizes_SOC)

# Run the random effects model
random_effects_model_SOC <- rma(
  yi = yi, # specify where effect sizes are located in dataframe
  vi = vi, # specify where variances are located in data frame
  method = "REML", # model fitting
  test = "knha", # Knapp and Hartung is commonly used to test for significance in random effects or mixed effects models
  data = effect_sizes_SOC
)
random_effects_model_SOC
